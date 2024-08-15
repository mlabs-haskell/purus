{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# HLINT ignore "Fuse foldr/<$>" #-}
{-# HLINT ignore "Use fewer imports" #-}
{-# LANGUAGE TypeApplications #-}
-- has to be here (more or less)
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Language.PureScript.CoreFn.Convert.ToPIR where

import Data.Map qualified as M
import Data.Text qualified as T
import Language.PureScript.CoreFn.FromJSON ()
import Language.PureScript.Names (
  Ident,
  ProperName (..),
  ProperNameType (..),
  Qualified (..),
  disqualify,
  runIdent,
 )
import Language.PureScript.PSString (prettyPrintString)
import Prelude

-- mainly for the module (we might need it for constructors? idk)

import Language.PureScript.CoreFn.Convert.IR.Utils
import Language.PureScript.CoreFn.Convert.IR qualified as IR
import PlutusCore (Unique (..), latestVersion, runQuoteT)
import PlutusCore qualified as PLC
import PlutusIR qualified as PIR

import Bound (Var (..))
import Bound.Scope (instantiateEither)
import Control.Lens.Operators ((^.))
import Control.Monad (
  foldM,
  join,
  void,
 )
import Control.Monad.Except (MonadError (..))
import Control.Monad.Trans.Except (ExceptT, runExceptT)
import Control.Monad.Trans.Reader (Reader, runReader)
import Control.Monad.Trans.State (evalState)
import Data.Bifunctor (Bifunctor (..))
import Data.Foldable (foldl', traverse_)
import Data.Kind qualified as GHC
import Data.List.NonEmpty qualified as NE
import Data.Maybe (fromJust)
import Data.Text (Text)
import Data.Void (Void)
import Language.PureScript.CoreFn.Convert.Datatypes (
  DatatypeDictionary,
  DatatypeM,
  PIRTerm,
  PIRType,
  bindTV,
  eliminateCaseExpressionsTrace,
  freshName,
  getConstructorName,
  mkKind,
  mkTypeBindDict,
  pirDatatypes,
  toPIRType,
 )
import Language.PureScript.CoreFn.Module (
  Datatypes,
  cdCtorFields,
  dDataArgs,
  getConstructorIndexAndDecl,
  lookupDataDecl,
 )
import Language.PureScript.CoreFn.TypeLike (TypeLike (..), getInstantiations, safeFunArgTypes)
import PlutusCore (getDefTypeCheckConfig)
import PlutusCore.Evaluation.Machine.ExBudgetingDefaults qualified as PLC
import PlutusIR.Compiler (CompilationCtx, Compiling, compileProgram, compileToReadable, toDefaultCompilationCtx)
import PlutusIR.Core.Instance.Pretty.Readable (prettyPirReadable)
import Type.Reflection (TypeRep, Typeable, typeRep)

-- mainly for the module (we might need it for constructors? idk)

import Bound.Scope (fromScope)
import Control.Exception (throwIO)
import Control.Exception qualified as E
import Data.Functor qualified
import Language.PureScript.Constants.PLC (defaultFunMap)
import Language.PureScript.CoreFn.Convert.Debug
import Language.PureScript.CoreFn.Convert.DesugarObjects (
  prepPIR,
  prettyStr,
  primData,
 )
import Language.PureScript.CoreFn.Convert.IR (
  BVar (..),
  BindE (..),
  Exp (..),
  FVar (..),
  Lit (CharL, IntL, StringL),
  Ty (TyCon),
  expTy,
  expTy',
 )
import PlutusCore.Default (
  DefaultFun,
  DefaultUni,
  Esc,
 )
import PlutusCore.Evaluation.Machine.Ck (
  EvaluationResult,
  unsafeEvaluateCk,
 )
import PlutusCore.Pretty (prettyPlcReadableDef)
import PlutusIR (Binding (TermBind), Name (Name), Program (Program), Recursivity (NonRec, Rec), Strictness (..), Term (Builtin), VarDecl (VarDecl))
import PlutusIR.Compiler.Provenance (Provenance (Original))
import PlutusIR.Error (Error)
import PlutusIR.MkPir (mkConstant)
import Prettyprinter

showType :: forall a. (Typeable a) => String
showType = show (typeRep :: TypeRep a)

tyBuiltinBool :: PIRType
tyBuiltinBool = PLC.TyBuiltin () (PLC.SomeTypeIn PLC.DefaultUniBool)

(#) :: PIRTerm -> PIRTerm -> PIRTerm
e1 # e2 = PIR.Apply () e1 e2

-- I think this is the right fixity? TODO: Check plutarch
infixl 9 #

sopUnit :: PIRType
sopUnit = PIR.TySOP () [[]]

pirTyInst :: PIRType -> PIRTerm -> PIRTerm
pirTyInst ty term = PIR.TyInst () term ty

tyInstMany :: PIRTerm -> [PIRType] -> PIRTerm
tyInstMany = foldl' (flip pirTyInst)

sopUnitTerm :: PIRTerm
sopUnitTerm = PIR.Constr () sopUnit 0 []

pirDelay :: PIRTerm -> DatatypeM PIRTerm
pirDelay term = do
  nm <- freshName
  pure $ PIR.LamAbs () nm sopUnit term

pirForce :: PIRTerm -> PIRTerm
pirForce term = PIR.Apply () term sopUnitTerm

pirEqInt :: PIRTerm -> PIRTerm -> PIRTerm
pirEqInt i1 i2 =
  let pirEq = PIR.Builtin () PLC.EqualsInteger
   in pirEq # i1 # i2

pirEqString :: PIRTerm -> PIRTerm -> PIRTerm
pirEqString s1 s2 =
  let pirEq = PIR.Builtin () PLC.EqualsString
   in pirEq # s1 # s2

-- delayed (which is literally always what we want). Not sure if need to force here?
pirError :: PIRType -> DatatypeM PIRTerm
pirError t = pirForce <$> pirDelay (PIR.Error () t)

-- for builtin booleans jfc why don't they have thiiiisss
pirAnd :: PIRTerm -> PIRTerm -> DatatypeM PIRTerm
pirAnd t1 t2 = do
  tBranch <- pirIfThen tyBuiltinBool t2 (mkConstant () True) (mkConstant () False)
  pirIfThen tyBuiltinBool t1 tBranch (mkConstant () False)

pirLetNonRec ::
  PIRType -> -- type of the expression we're let- binding
  PIRTerm ->
  (PIRTerm -> DatatypeM PIRTerm) ->
  DatatypeM PIRTerm
pirLetNonRec ty toLet f = do
  nm <- freshName
  let myvar = PIR.Var () nm
      varDecl = PIR.VarDecl () nm ty
      binding = TermBind () NonStrict varDecl toLet
  PIR.Let () NonRec (NE.singleton binding) <$> f myvar

argTy :: (TypeLike t) => t -> t
argTy = head . splitFunTyParts . snd . stripQuantifiers

-- FIXME: Something's broken here w/ the type abstraction/instantiation for delay/force -_-
--        switching to strict ifte for now
pirIfThen :: PIRType -> PIRTerm -> PIRTerm -> PIRTerm -> DatatypeM PIRTerm
pirIfThen resTy cond troo fawlse = do
  troo' <- pirDelay troo
  fawlse' <- pirDelay fawlse
  pure . pirForce $ pirTyInst (PIR.TyFun () sopUnit resTy) (PIR.Builtin () PLC.IfThenElse) # cond # troo' # fawlse'

-- utility for constructing LamAbs w/ a fresh variable name. We do this a lot in the case analysis stuff
freshLam ::
  Ty -> -- type of the fresh var being created
  (PIRType -> PIRTerm -> DatatypeM PIRTerm) -> -- fn from that fresh var to a term
  DatatypeM PIRTerm
freshLam t f = do
  name <- freshName
  t' <- toPIRType t
  PIR.LamAbs () name t' <$> f t' (PIR.Var () name)

type PLCProgram uni fun a = PLC.Program PLC.TyName PLC.Name uni fun (Provenance a)

type PIRTermBind = Binding PLC.TyName Name DefaultUni DefaultFun ()

runDatatypeM :: DatatypeDictionary -> DatatypeM a -> Either String a
runDatatypeM dict act = evalState (runExceptT act) dict

firstPass ::
  forall a.
  (Pretty a) =>
  Datatypes IR.Kind Ty ->
  (a -> Var (BVar Ty) (FVar Ty)) ->
  Exp WithoutObjects Ty a ->
  DatatypeM PIRTerm
firstPass _datatypes f _exp = do
  res <- firstPass' _datatypes f _exp
  let msg =
        "INPUT:\n"
          <> prettyStr (f <$> _exp)
          <> "\n\nOUTPUT:\n"
          <> prettyStr res
  doTraceM "firstPassInput" ("INPUT EXPR:\n" <> prettyStr (f <$> _exp) <> "\n\nEXPR TYPE:\n" <> prettyStr (expTy f _exp))
  doTraceM "firstPass" msg
  pure res

firstPass' ::
  forall a.
  (Pretty a) =>
  Datatypes IR.Kind Ty ->
  (a -> Var (BVar Ty) (FVar Ty)) ->
  Exp WithoutObjects Ty a ->
  DatatypeM PIRTerm
firstPass' _datatypes f _exp =
  doTraceM "firstPass'" (prettyStr _exp) >> case _exp of
    V x -> case f x of
      F (FVar _ ident) ->
        let nm = runIdent $ disqualify ident
         in case M.lookup (T.unpack nm) defaultFunMap of
              Just aBuiltinFun -> pure $ Builtin () aBuiltinFun
              Nothing ->
                getConstructorName ident >>= \case
                  Just aCtorNm -> pure $ PIR.Var () aCtorNm
                  Nothing ->
                    throwError $
                      T.unpack nm
                        <> " isn't a builtin, and it shouldn't be possible to have a free variable that's anything but a builtin"
      B (BVar bvix _ (runIdent -> nm)) -> pure $ PIR.Var () (Name nm $ Unique bvix)
    LitE _ lit -> firstPassLit lit
    lam@(LamE (BVar bvIx bvTy bvNm) body) -> do
      let lty = funTy bvTy (expTy' f body)
      ty' <- toPIRType bvTy
      let nm = Name (runIdent bvNm) $ Unique bvIx
          body' = instantiateEither (either (IR.V . B) (IR.V . F)) body
      body'' <- firstPass datatypes (>>= f) body'
      let result = PIR.LamAbs () nm ty' body''
          msg =
            "BVar:\n"
              <> prettyStr bvNm
              <> "\n\nInput Lam:\n"
              <> prettyStr lam
              <> "\n\nInferred Lam Ty:\n"
              <> prettyStr lty
              <> "\n\nRESULT: "
              <> prettyStr result
      doTraceM "firstPassLamTy" msg
      pure result
    AppE e1 e2 -> do
      e1' <- firstPass datatypes f e1
      e2' <- firstPass datatypes f e2
      pure $ PIR.Apply () e1' e2'
    LetE varMap binds body -> do
      boundTerms <- foldM convertBind [] binds -- FIXME: This is almost certainly wrong. We should remove the Var Map
      body' <- firstPass datatypes (>>= f) $ instantiateEither (either (IR.V . B) (IR.V . F)) body
      case NE.nonEmpty boundTerms of
        -- NOTE: For simplicity we assume here that all let bindings are mutually recursive.
        --       This might not be great for performance (depends on what the PIR compiler does)
        Just boundTerms' -> pure $ PIR.Let () PIR.Rec boundTerms' body'
        Nothing -> error "empty bindings"
    ce@CaseE {} -> error $ "Case expressions should be eliminated by now, but found:\n\n" <> prettyStr ce
    TyInstE t e -> do
      t' <- toPIRType t
      e' <- firstPass datatypes f e
      pure $ PIR.TyInst () e' t'
    TyAbs (BVar bvIx bvTy bvNm) e -> do
      let bvKind = mkKind bvTy
          bvNmTxt = runIdent bvNm
          tNm = PIR.TyName $ PIR.Name (runIdent bvNm) (Unique bvIx)
      bindTV bvNmTxt tNm
      e' <- firstPass datatypes f e
      pure $ PIR.TyAbs () tNm bvKind e'
  where
    datatypes = _datatypes <> primData

    mkDict = M.fromList . fmap (\(a, FVar _ ident) -> (disqualify ident, a)) . M.toList

    convertBind ::
      [PIRTermBind] ->
      BindE Ty (Exp WithoutObjects Ty) a ->
      DatatypeM [PIRTermBind]
    convertBind acc = \case
      NonRecursive ident bvix expr -> do
        let unscoped = fmap join . fromScope $ f <$> expr
        nonRec <- goBind (ident, bvix) unscoped
        pure $ nonRec : acc
      Recursive xs -> do
        xs' <- traverse (uncurry goBind . second (fmap join . fromScope . fmap f)) xs
        pure $ xs' <> acc
      where
        goBind :: (Ident, Int) -> Exp WithoutObjects Ty (Var (BVar Ty) (FVar Ty)) -> DatatypeM PIRTermBind
        goBind (ident, i) expr = do
          let nm = Name (runIdent ident) $ Unique i
          ty <- toPIRType (expTy id expr)
          expr' <- firstPass datatypes id expr
          -- NOTE: Not sure if this should always be strict?
          pure $ TermBind () Strict (VarDecl () nm ty) expr'

    firstPassLit ::
      Lit WithoutObjects (Exp WithoutObjects Ty a) ->
      DatatypeM PIRTerm
    firstPassLit = \case
      IntL i -> pure $ mkConstant () i
      StringL str ->
        -- REVIEW/TODO/FIXME: This is probably wrong?
        pure $ mkConstant () $ prettyPrintString str
      CharL c ->
        -- REVIEW/TODO/FIXME: Is this what we want?
        pure
          . mkConstant ()
          . toInteger
          . fromEnum
          $ c

convertTrueLit :: forall (x :: GHC.Type). DefaultUni (Esc x) -> Lit WithoutObjects Void -> DatatypeM x
convertTrueLit uni lit = case uni of
  PLC.DefaultUniData -> throwError "convertTrueLit: Data literals not supported at this time"
  PLC.DefaultUniByteString -> throwError "convertTrueLit: ByteString literals are not supported at this time (though I guess they could be?)"
  PLC.DefaultUniPair _ _ -> throwError "convertTrueLit: Tuple literals are not supported at this time"
  PLC.DefaultUniInteger -> case lit of
    CharL c -> pure . toInteger . fromEnum $ c
    IntL i -> pure i
    other -> throwError (mkError other)
  PLC.DefaultUniString -> case lit of
    StringL str -> pure . prettyPrintString $ str
    other -> throwError (mkError other)
  other -> throwError $ "Unsupported PLC Constant type " <> show other <> " for literal value: " <> prettyStr lit
  where
    mkError :: Lit WithoutObjects Void -> String
    mkError wrong = "convertTrueLit: Type mismatch. Expected a literal value of PLC DefaultUni type " <> show uni <> " but found: " <> prettyStr wrong

monoCtorFields ::
  Qualified (ProperName 'TypeName) ->
  Qualified (ProperName 'ConstructorName) ->
  Ty -> -- the type of the scrutinee
  Datatypes IR.Kind Ty ->
  (Int, [Ty]) -- Constructor index & list of field types
monoCtorFields tn cn t datatypes = (thisCtorIx, monoCtorArgs)
  where
    (thisCtorIx, thisCtorDecl) = either error id $ getConstructorIndexAndDecl cn datatypes
    ctorArgs = snd <$> thisCtorDecl ^. cdCtorFields
    thisDataDecl = fromJust $ lookupDataDecl tn datatypes
    declArgVars = uncurry IR.TyVar <$> thisDataDecl ^. dDataArgs
    dataTyCon = TyCon tn
    polyTy = foldl' applyType dataTyCon declArgVars
    polyCtorTy = foldr funTy polyTy ctorArgs
    instantiations = getInstantiations t polyTy
    -- This might not be exactly correct, it assumes that NO tyvars remain in the input type.
    -- If any remain, they might not line up with the var names in the declaration (probably won't
    -- since I think I generated the decl vars). Might require tweaking "instantiates" & co
    monoCtorTy = replaceAllTypeVars instantiations polyCtorTy
    -- if it's a unary constructor, we want to skip the error from funArgTypes being partial
    monoCtorArgs = safeFunArgTypes monoCtorTy

---- Compilation helpers/utilities

runPLCProgram :: PLCProgram DefaultUni DefaultFun () -> (EvaluationResult (PLC.Term PLC.TyName Name DefaultUni DefaultFun ()), [Text])
runPLCProgram (PLC.Program _ _ c) = unsafeEvaluateCk PLC.defaultBuiltinsRuntime $ void c

declToPIR ::
  FilePath ->
  Text ->
  IO PIRTerm
declToPIR path decl =
  prepPIR path decl >>= \case
    (mainExpr, datatypes) -> do
      case mkTypeBindDict datatypes mainExpr of
        Left err -> throwIO . userError $ err
        Right dict -> case runDatatypeM dict $ firstPass datatypes id =<< eliminateCaseExpressionsTrace datatypes mainExpr of
          Left err -> throwIO . userError $ err
          Right e -> do
            print (pretty e)
            let
              dtBinds = NE.fromList $ PIR.DatatypeBind () <$> M.elems (dict ^. pirDatatypes)
              result = PIR.Let () Rec dtBinds e
            putStrLn "-------\\/ PIR \\/ --------"
            print e
            print $ prettyPirReadable result
            pure result

printExpr :: FilePath -> Text -> IO ()
printExpr path decl =
  prepPIR path decl >>= \case
    (e, _) -> putStrLn ("\n\n\n" <> T.unpack decl <> " = \n" <> prettyStr e)

declToPLC :: FilePath -> Text -> IO (PLCProgram DefaultUni DefaultFun ())
declToPLC path main = declToPIR path main >>= compileToUPLC

evaluateDecl :: FilePath -> Text -> IO (EvaluationResult (PLC.Term PLC.TyName Name DefaultUni DefaultFun ()), [Text])
evaluateDecl path main = declToPLC path main Data.Functor.<&> runPLCProgram

compileToUPLC :: PIRTerm -> IO (PLCProgram DefaultUni DefaultFun ())
compileToUPLC e = do
  let input = Program (Original ()) latestVersion (Original <$> e)
      withErrors = either (throwIO . userError) pure
  readable <- withErrors . runCompile $ compileToReadable input
  let pretty = prettyPlcReadableDef readable
  putStrLn "-------\\/ PIR (2) \\/ --------"
  print pretty
  withErrors . runCompile $ compileProgram (void readable)

runCompile ::
  forall e m c b.
  ( e ~ Error DefaultUni DefaultFun (Provenance ())
  , c ~ CompilationCtx DefaultUni DefaultFun ()
  , m ~ ExceptT e (ExceptT e (PLC.QuoteT (Reader c)))
  ) =>
  (Compiling m e DefaultUni DefaultFun ()) =>
  m b ->
  Either String b
runCompile x =
  let
    res :: Either e b
    res = do
      plcConfig <- getDefTypeCheckConfig (Original ())
      let ctx = toDefaultCompilationCtx plcConfig
      join $ flip runReader ctx $ runQuoteT $ runExceptT $ runExceptT x
   in
    first show res

passing :: IO ()
passing = traverse_ eval passingTests
  where
    eval :: Text -> IO ()
    eval declName = E.catch @(E.SomeException) (void $ evaluateDecl "tests/purus/passing/Misc/output/Lib/index.cfn" declName) $
      \e ->
        let msg = "Failed to compile and evaluate '" <> T.unpack declName <> "''\n  Error message: " <> show e
         in error msg
    passingTests =
      [ "testTestClass"
      , "minus"
      , "testEq"
      , "workingEven"
      , "brokenEven"
      , "opt2Int"
      , "testOpt2Int"
      , "unIdentitee"
      , "testIdentitee"
      , "testEq2"
      , "nestedBinds"
      , "anIntLit"
      , "aStringLit"
      , "aVal"
      , "testTuple"
      , "testCons"
      , "cons"
      , "aList"
      , "aFunction2"
      , "polyInObjMatch"
      , "arrForall"
      , "testBinders"
      , "testBindersCase"
      , "testValidator"
      , "testasum"
      , "aBool"
      , "aFunction"
      , "aFunction3"
      , "testBuiltin"
      , "main"
      , "plus"
      , "testPlus"
      , "anObj"
      , "objUpdate"
      , "polyInObj"
      , "aPred"
      , "id"
      , "testId"
      , "objForall"
      , "arrForall"
      , "testValidatorApplied"
      , "guardedCase"
      ]

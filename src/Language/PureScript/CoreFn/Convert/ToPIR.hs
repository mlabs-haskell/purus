{-# OPTIONS_GHC -Wno-orphans #-} -- has to be here (more or less)
{-# LANGUAGE ScopedTypeVariables #-}





{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Fuse foldr/<$>" #-}
{-# HLINT ignore "Use fewer imports" #-}

module Language.PureScript.CoreFn.Convert.ToPIR  where

import Prelude
import Language.PureScript.Names (
  Qualified (..),
  ProperName(..), runIdent, disqualify, ProperNameType (..), Ident)
import Language.PureScript.CoreFn.FromJSON ()
import Data.Map (Map)
import Data.Map qualified as M
import Data.Text qualified as T
import Language.PureScript.PSString (prettyPrintString)
import Language.PureScript.Constants.Prim qualified as C
 -- mainly for the module (we might need it for constructors? idk)
import PlutusIR qualified as PIR
import PlutusCore (Unique (..), runQuoteT, latestVersion)
import Language.PureScript.CoreFn.Convert.IR qualified as IR
import PlutusCore qualified as PLC
import Language.PureScript.CoreFn.Convert.DesugarCore
    ( WithoutObjects )

import Data.Void ( Void )
import Data.Kind qualified as GHC
import Type.Reflection ( Typeable, TypeRep, typeRep )
import Bound.Scope (instantiateEither)
import Data.List.NonEmpty qualified as NE
import Language.PureScript.CoreFn.Desugar.Utils (showIdent', properToIdent)
import Language.PureScript.CoreFn.Convert.Datatypes
    ( getConstructorName,
      toPIRType,
      freshName,
      DatatypeDictionary,
      DatatypeM,
      PIRType,
      PIRTerm,
      mkTypeBindDict,
      pirDatatypes,
      mkNewTyVar, bindTV, mkKind, eliminateCaseExpressions )
import Control.Monad.Except (MonadError(..))
import Language.PureScript.CoreFn.Module (
  Datatypes,
  getConstructorIndexAndDecl,
  lookupDataDecl,
  cdCtorFields,
  dDataArgs)
import Language.PureScript.CoreFn.TypeLike (TypeLike (..), getInstantiations, safeFunArgTypes)
import Data.Foldable (foldl')
import Data.Maybe (fromJust)
import Control.Lens.Operators ((^.))
import Data.Text (Text)
import PlutusIR.Compiler (CompilationCtx, compileToReadable, compileProgram)
import PlutusCore.Evaluation.Machine.ExBudgetingDefaults qualified as PLC
import PlutusCore (getDefTypeCheckConfig)
import Control.Monad.Trans.Reader ( runReader, Reader )
import Control.Monad.Trans.Except ( ExceptT, runExceptT )
import Control.Monad.Trans.State ( evalState )
import PlutusIR.Compiler ( Compiling, toDefaultCompilationCtx )
import Data.Bifunctor (Bifunctor(..))
import PlutusIR.Core.Instance.Pretty.Readable (prettyPirReadable)
import Bound ( Var(..) )
import Control.Monad
    ( join, foldM, void, forM_ )
 -- mainly for the module (we might need it for constructors? idk)
import Language.PureScript.CoreFn.Convert.IR
    ( expTy,
      BindE(..),
      Lit(CharL, IntL, StringL),
      Exp(..),
      FVar(..),
      BVar(..),
      Ty(TyCon), unsafeAnalyzeApp )
import PlutusIR
    ( Name(Name),
      VarDecl(VarDecl),
      Recursivity(Rec, NonRec),
      Strictness(..),
      Binding(TermBind),
      Term(Builtin) )
import PlutusCore.Default
    ( DefaultFun,
      DefaultUni,
      Esc )
import Language.PureScript.Constants.PLC ( defaultFunMap )
import PlutusIR.MkPir ( mkConstant )
import Language.PureScript.CoreFn.Convert.DesugarObjects
    ( prettyStr, prepPIR, primData )
import PlutusIR.Compiler.Provenance ( Provenance(Original) )
import PlutusIR.Error ( Error )
import Control.Exception ( throwIO )
import PlutusCore.Evaluation.Machine.Ck
    ( EvaluationResult, unsafeEvaluateCk )
import Prettyprinter ( Pretty)
import PlutusIR (Program(Program))
import PlutusCore.Pretty (prettyPlcReadableDef)
import Language.PureScript.CoreFn.Convert.Monomorphize (isConstructorE)
import Data.Functor qualified
import Bound.Scope (fromScope)
import Language.PureScript.CoreFn.Convert.Debug

showType :: forall a. Typeable a => String
showType = show (typeRep :: TypeRep a)

tyBuiltinBool :: PIRType
tyBuiltinBool  = PLC.TyBuiltin () (PLC.SomeTypeIn PLC.DefaultUniBool)

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
  pure $ PIR.LamAbs () nm sopUnit  term

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

pirLetNonRec :: PIRType -- type of the expression we're let- binding
             -> PIRTerm
             -> (PIRTerm -> DatatypeM PIRTerm)
             -> DatatypeM PIRTerm
pirLetNonRec ty toLet f = do
  nm <- freshName
  let myvar = PIR.Var () nm
      varDecl = PIR.VarDecl () nm ty
      binding = TermBind () NonStrict varDecl toLet
  PIR.Let () NonRec (NE.singleton binding) <$> f myvar

argTy :: TypeLike t => t -> t
argTy =  head . splitFunTyParts . snd . stripQuantifiers

-- FIXME: Something's broken here w/ the type abstraction/instantiation for delay/force -_-
--        switching to strict ifte for now
pirIfThen :: PIRType -> PIRTerm -> PIRTerm -> PIRTerm -> DatatypeM PIRTerm
pirIfThen resTy cond troo fawlse = do
   troo' <- pirDelay troo
   fawlse' <- pirDelay fawlse
   pure . pirForce $ pirTyInst (PIR.TyFun () sopUnit resTy) (PIR.Builtin () PLC.IfThenElse) # cond # troo' # fawlse'


-- utility for constructing LamAbs w/ a fresh variable name. We do this a lot in the case analysis stuff
freshLam :: Ty -- type of the fresh var being created
         -> (PIRType -> PIRTerm -> DatatypeM PIRTerm) -- fn from that fresh var to a term
         -> DatatypeM PIRTerm
freshLam t f = do
  name <- freshName
  t' <- toPIRType t
  PIR.LamAbs () name t' <$> f t' (PIR.Var () name)

type PLCProgram uni fun a = PLC.Program PLC.TyName PLC.Name uni fun (Provenance a)

type PIRTermBind  = Binding PLC.TyName  Name DefaultUni DefaultFun ()

runDatatypeM :: DatatypeDictionary -> DatatypeM a -> Either String a
runDatatypeM dict act = evalState (runExceptT act) dict

firstPass :: forall a
           . Pretty a
          => Datatypes IR.Kind Ty
          -> (a -> Var (BVar Ty) (FVar Ty))
          -> Exp WithoutObjects Ty a
          -> DatatypeM PIRTerm
firstPass _datatypes f _exp = doTraceM "firstPass" (prettyStr _exp) >> case _exp of
  V x -> case f x of
    F (FVar _ ident) ->
      let nm = runIdent $ disqualify ident
      in case M.lookup (T.unpack nm) defaultFunMap of
        Just aBuiltinFun ->  pure $ Builtin () aBuiltinFun
        Nothing -> getConstructorName ident >>= \case
          Just aCtorNm -> pure $ PIR.Var () aCtorNm
          Nothing ->
            throwError
                     $ T.unpack nm
                     <> " isn't a builtin, and it shouldn't be possible to have a free variable that's anything but a builtin"
    B (BVar bvix _ (runIdent -> nm)) -> pure $ PIR.Var () (Name nm $ Unique bvix)
  LitE litTy lit -> firstPassLit litTy lit
  LamE lty (BVar bvIx bvTy bvNm) body -> do
    let used = usedTypeVariables lty
    forM_ used $ \(v,k) -> do
        v' <- mkNewTyVar v
        bindTV v v'
        pure $ PIR.TyVarDecl () v' (mkKind k)
    fTy <- toPIRType (quantify lty)
    -- error (prettyStr lty)
    ty' <- toPIRType (argTy bvTy)
    let nm = Name (runIdent bvNm) $ Unique bvIx
        body' = instantiateEither (either (IR.V . B) (IR.V . F)) body
    body'' <- firstPass datatypes (>>= f) body'
    pirLetNonRec fTy (PIR.LamAbs () nm ty' body'') $ \x -> pure x
  AppE e1 e2  ->  do
    e1' <- firstPass datatypes f e1
    e2' <- firstPass datatypes f e2
    pure $ PIR.Apply () e1' e2'
  LetE varMap binds body -> do
    boundTerms <- foldM (convertBind (mkDict varMap)) [] binds
    body' <- firstPass datatypes (>>= f) $ instantiateEither (either (IR.V . B) (IR.V . F)) body
    case NE.nonEmpty boundTerms of
      -- NOTE: For simplicity we assume here that all let bindings are mutually recursive.
      --       This might not be great for performance (depends on what the PIR compiler does)
      Just boundTerms' -> pure $ PIR.Let () PIR.Rec boundTerms' body'
      Nothing -> error "empty bindings"
  CaseE{} -> error "Case expressions should be eliminated by now. TODO: Enforce this w/ types"
  TyInstE t e -> do
    t' <- toPIRType t
    e' <- firstPass datatypes f e
    pure $ PIR.TyInst () e' t'
 where 
   datatypes = _datatypes <> primData

   mkDict = M.fromList . fmap (\(a,FVar _ ident) -> (disqualify ident,a)) . M.toList

   convertBind :: Map Ident Int
               -> [PIRTermBind]
               -> BindE Ty (Exp WithoutObjects Ty) a
               -> DatatypeM [PIRTermBind]
   convertBind dict acc = \case
     NonRecursive ident bvix expr -> do
       let unscoped = fmap join . fromScope $ f <$>  expr
       nonRec <- goBind (ident,bvix) unscoped
       pure $ nonRec:acc
     Recursive xs -> do
       xs' <- traverse (uncurry goBind . second (fmap join . fromScope . fmap f)) xs
       pure $ xs' <> acc
    where
      goBind :: (Ident ,Int) -> Exp WithoutObjects Ty (Var (BVar Ty) (FVar Ty)) -> DatatypeM PIRTermBind
      goBind (ident,i) expr = do
         let nm = Name (runIdent ident) $ Unique i
         ty <- toPIRType (expTy id expr)
         expr' <- firstPass datatypes id expr
         -- NOTE: Not sure if this should always be strict?
         pure $ TermBind () Strict (VarDecl () nm ty) expr'

   firstPassLit :: Ty
                -> Lit WithoutObjects (Exp WithoutObjects Ty a)
                -> DatatypeM PIRTerm
   firstPassLit litTy = \case
    IntL i -> pure $ mkConstant () i
    StringL str -> -- REVIEW/TODO/FIXME: This is probably wrong?
       pure $ mkConstant () $ prettyPrintString str
    CharL c -> -- REVIEW/TODO/FIXME: Is this what we want?
        pure
      . mkConstant ()
      . toInteger
      . fromEnum
      $ c

convertTrueLit :: forall (x :: GHC.Type). DefaultUni (Esc x) -> Lit WithoutObjects Void -> DatatypeM x
convertTrueLit uni lit = case uni of
  PLC.DefaultUniData -> throwError  "convertTrueLit: Data literals not supported at this time"
  PLC.DefaultUniByteString -> throwError  "convertTrueLit: ByteString literals are not supported at this time (though I guess they could be?)"
  PLC.DefaultUniPair _ _ -> throwError  "convertTrueLit: Tuple literals are not supported at this time"
  PLC.DefaultUniInteger -> case lit of
    CharL c -> pure . toInteger . fromEnum $ c
    IntL i  -> pure i
    other   -> throwError (mkError other)
  PLC.DefaultUniString -> case lit of
    StringL str -> pure . prettyPrintString $ str
    other -> throwError (mkError other)
  other -> throwError $ "Unsupported PLC Constant type " <> show other <> " for literal value: " <> prettyStr lit
 where
   mkError :: Lit WithoutObjects Void -> String
   mkError wrong = "convertTrueLit: Type mismatch. Expected a literal value of PLC DefaultUni type " <> show uni <> " but found: " <> prettyStr wrong


monoCtorFields
  :: Qualified (ProperName 'TypeName)
  -> Qualified (ProperName 'ConstructorName)
  -> Ty -- the type of the scrutinee
  -> Datatypes IR.Kind Ty
  -> (Int,[Ty]) -- Constructor index & list of field types
monoCtorFields tn cn t datatypes = (thisCtorIx,monoCtorArgs)
 where
   (thisCtorIx,thisCtorDecl) =  either error id $ getConstructorIndexAndDecl cn datatypes
   ctorArgs = snd <$> thisCtorDecl ^. cdCtorFields
   thisDataDecl = fromJust $ lookupDataDecl tn datatypes
   declArgVars = uncurry IR.TyVar <$> thisDataDecl ^. dDataArgs
   dataTyCon    = TyCon tn
   polyTy    = foldl' applyType dataTyCon declArgVars
   polyCtorTy = foldr funTy polyTy ctorArgs
   instantiations = getInstantiations t polyTy
   -- This might not be exactly correct, it assumes that NO tyvars remain in the input type.
   -- If any remain, they might not line up with the var names in the declaration (probably won't
   -- since I think I generated the decl vars). Might require tweaking "instantiates" & co
   monoCtorTy = replaceAllTypeVars instantiations polyCtorTy
   -- if it's a unary constructor, we want to skip the error from funArgTypes being partial
   monoCtorArgs = safeFunArgTypes monoCtorTy

---- Compilation helpers/utilities

runPLCProgram :: PLCProgram DefaultUni DefaultFun () -> (EvaluationResult (PLC.Term PLC.TyName Name DefaultUni DefaultFun ()),[Text])
runPLCProgram (PLC.Program _ _ c) = unsafeEvaluateCk PLC.defaultBuiltinsRuntime $ void c

declToPIR :: FilePath
           -> Text
           -> IO PIRTerm
declToPIR path decl = prepPIR path decl >>= \case
  (mainExpr,datatypes) -> do
    case mkTypeBindDict datatypes mainExpr of
      Left err -> throwIO . userError $ err
      Right dict -> case runDatatypeM dict $ firstPass datatypes id =<< eliminateCaseExpressions datatypes mainExpr of
        Left err -> throwIO . userError $ err
        Right e  -> do
          let
              dtBinds = NE.fromList $  PIR.DatatypeBind () <$> M.elems (dict ^. pirDatatypes)
              result = PIR.Let () Rec dtBinds e
          putStrLn "-------\\/ PIR \\/ --------"
          print e
          print $ prettyPirReadable result
          pure result

declToPLC :: FilePath -> Text -> IO (PLCProgram DefaultUni DefaultFun ())
declToPLC path main = declToPIR path main >>= compileToUPLC

evaluateDecl :: FilePath -> Text -> IO (EvaluationResult (PLC.Term PLC.TyName Name DefaultUni DefaultFun ()),[Text])
evaluateDecl path main = declToPLC path main Data.Functor.<&> runPLCProgram

compileToUPLC :: PIRTerm -> IO (PLCProgram DefaultUni DefaultFun ())
compileToUPLC e = do
  let input = Program (Original ()) latestVersion (Original <$> e)
      withErrors = either (throwIO . userError) pure
  readable <- withErrors . runCompile $ compileToReadable input
  let pretty = prettyPlcReadableDef readable
  putStrLn  "-------\\/ PIR (2) \\/ --------"
  print pretty
  withErrors . runCompile $ compileProgram (void readable)

runCompile ::
      forall e m c  b.
      (e ~ Error DefaultUni DefaultFun (Provenance ())
      , c ~ CompilationCtx DefaultUni DefaultFun ()
      , m ~ ExceptT e (ExceptT e (PLC.QuoteT (Reader c)))
      )
      => (Compiling m e DefaultUni DefaultFun ()) => m b -> Either String b
runCompile x  =
      let
        res :: Either e b
        res = do
            plcConfig <- getDefTypeCheckConfig (Original ())
            let ctx = toDefaultCompilationCtx plcConfig
            join $ flip runReader ctx $ runQuoteT $ runExceptT $ runExceptT x
      in first show res


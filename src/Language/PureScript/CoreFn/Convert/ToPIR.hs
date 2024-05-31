{-# OPTIONS_GHC -Wno-orphans #-} -- has to be here (more or less)
{-# LANGUAGE ScopedTypeVariables #-}





{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Language.PureScript.CoreFn.Convert.ToPIR  where

import Prelude
import Language.PureScript.Names (Qualified (..), ProperName(..), runIdent, disqualify, ProperNameType (..), Ident)
import Language.PureScript.CoreFn.FromJSON ()
import Data.Map (Map)
import Data.Map qualified as M
import Data.Text qualified as T
import Language.PureScript.PSString (prettyPrintString)
import Bound ( Var(..) )
import Control.Monad ( (>=>), foldM )
import Language.PureScript.Constants.Prim qualified as C
 -- mainly for the module (we might need it for constructors? idk)
import Language.PureScript.CoreFn.Convert.IR
    ( Exp(CaseE, V, LitE, CtorE, LamE, AppE, LetE),
      BindE(..),
      Lit(NumL, ArrayL, ConstArrayL, BoolL, CharL, IntL, StringL),
      FVar(..),
      BVar(..),
      Ty(TyCon),
      expTy )
import PlutusIR
    ( Type,
      Name(Name),
      VarDecl(VarDecl),
      Term(Builtin, Constant),
      Binding(TermBind),
      Strictness(Strict) )
import PlutusIR qualified as PIR
import PlutusCore.Default
    ( DefaultFun,
      pattern DefaultUniList,
      DefaultUni,
      Esc,
      ValueOf(ValueOf),
      Some(Some) )
import PlutusCore (Unique (..))
import Language.PureScript.Constants.PLC ( defaultFunMap )
import PlutusIR.MkPir ( mkConstant )
import Language.PureScript.CoreFn.Convert.IR qualified as IR
import Language.PureScript.CoreFn.Convert.DesugarObjects
    ( prettyStr )
import PlutusIR.Compiler.Provenance ( Provenance )
import PlutusCore qualified as PLC
import Language.PureScript.CoreFn.Convert.DesugarCore
    ( WithoutObjects )
import Language.PureScript.CoreFn.Convert.Datatypes
    ( SomeUni(SomeUni), ModuleDataTypes, toPIRType, extractUni )
import Data.Void ( Void )
import Data.Kind qualified as GHC
import Type.Reflection ( Typeable, TypeRep, typeRep )
import Bound.Scope (instantiateEither)
import Data.List.NonEmpty qualified as NE
import Language.PureScript.CoreFn.Desugar.Utils (showIdent')


showType :: forall a. Typeable a => String
showType = show (typeRep :: TypeRep a)

type PLCProgram uni fun a = PLC.Program PLC.TyName PLC.Name uni fun (Provenance a)

type PIRTerm tyName = PIR.Term tyName PLC.Name DefaultUni DefaultFun ()

type PIRType tyName = Type tyName DefaultUni ()

type PIRTermBind tyName = Binding tyName  Name DefaultUni DefaultFun ()

data FirstPass = FirstPass {
    dataTypes :: ModuleDataTypes
  }

-- this will change
type PIRConvertM a = Either String a

{- In the first pass, we convert IR to PIR terms
   but do not convert PS type names into PIR type names
-}
firstPass :: forall a
           . (a -> Var (BVar Ty) (FVar Ty))
          -> Exp WithoutObjects Ty a
          -> PIRConvertM (PIRTerm (Qualified (ProperName 'TypeName)))
firstPass f = \case
  V x -> case f x of
    F (FVar _ ident) ->
      let nm = runIdent $ disqualify ident
      in case M.lookup (T.unpack nm) defaultFunMap of
        Just aBuiltinFun ->  pure $ Builtin () aBuiltinFun
        Nothing -> error
         $ T.unpack nm <> " isn't a builtin, and it shouldn't be possible to have a free variable that's anything but a builtin"
    B (BVar bvix _ (runIdent -> nm)) -> pure $ PIR.Var () (Name nm $ Unique bvix)
  LitE litTy lit -> firstPassLit litTy lit
  CtorE a b c d -> error "I need to look at the output of the previous steps to figure out what to do here. The old thing doesn't work."
  LamE _ (BVar bvIx bvTy bvNm) body -> do
    ty' <- toPIRType bvTy
    let nm = Name (runIdent bvNm) $ Unique bvIx
        body' = instantiateEither (either (IR.V . B) (IR.V . F)) body
    body'' <- firstPass (>>= f) body'
    pure $ PIR.LamAbs () nm ty' body''
  AppE e1 e2 -> do
    e1' <- firstPass f e1
    e2' <- firstPass f e2
    pure $ PIR.Apply () e1' e2'
  LetE varMap binds body -> do
    boundTerms <- foldM (convertBind (mkDict varMap)) [] binds
    body' <- firstPass (>>= f) $ instantiateEither (either (IR.V . B) (IR.V . F)) body
    case NE.nonEmpty boundTerms of
      -- NOTE: For simplcity we assume here that all let bindings are mutually recursive.
      --       This might not be great for performance (depends on what the PIR compiler does)
      Just boundTerms' -> pure $ PIR.Let () PIR.Rec boundTerms' body'
      Nothing -> error "non empty bindings"
  CaseE{} -> error "TODO @klntsky"
 where
   mkDict = M.fromList . fmap (\(a,FVar _ ident) -> (disqualify ident,a)) . M.toList

   convertBind :: Map Ident Int
               -> [PIRTermBind (Qualified (ProperName 'TypeName))]
               -> BindE Ty (Exp WithoutObjects Ty) a
               -> PIRConvertM [PIRTermBind (Qualified (ProperName 'TypeName))]
   convertBind dict acc = \case
     NonRecursive ident expr -> do
       nonRec <- goBind ident expr
       pure $ nonRec:acc
     Recursive xs -> do
       xs' <- traverse (uncurry goBind) xs
       pure $ xs' <> acc
    where
      goBind ident expr = case M.lookup ident dict of
       Just i -> do
         let nm = Name (runIdent ident) $ Unique i
         ty <- toPIRType (expTy f expr)
         expr' <- firstPass f expr
         -- NOTE: Not sure if this should always be strict?
         pure $ TermBind () Strict (VarDecl () nm ty) expr'
       Nothing -> Left $ "convertBind: Could not determine variable index for let-bound identifier " <> showIdent' ident

   firstPassLit :: Ty
                -> Lit WithoutObjects (Exp WithoutObjects Ty a)
                -> PIRConvertM (PIRTerm (Qualified (ProperName 'TypeName)))
   firstPassLit litTy = \case
    IntL i -> pure $ mkConstant () i
    NumL _ -> error "TODO: Doubles? (We could actually represent them as a builtin pair of integers so there is some sense to keeping them)"
    StringL str -> -- REVIEW/TODO/FIXME: This is probably wrong?
       pure $ mkConstant () $ prettyPrintString str
    CharL c -> -- REVIEW/TODO/FIXME: Is this what we want?
        pure
      . mkConstant ()
      . toInteger
      . fromEnum
      $ c
    BoolL b -> pure $ mkConstant () b
    ArrayL arr -> error "We need a SOP list data type defined somewhere to codegen array literals that have non-PLC-constant expressions in them"
    ConstArrayL constArr -> firstPassConstArray litTy constArr

   firstPassConstArray :: Ty -> [Lit WithoutObjects Void] -> PIRConvertM (PIRTerm (Qualified (ProperName 'TypeName)))
   firstPassConstArray (IR.TyApp (TyCon C.Array) litTy) ls = case ls of
     [] -> do
       SomeUni uni <- (toPIRType >=> extractUni) litTy
       pure $ Constant () (Some (ValueOf (DefaultUniList uni) []))
     lits ->  do
       SomeUni uni <- (toPIRType >=> extractUni) litTy
       lits' <- traverse (convertTrueLit uni) lits
       pure $ Constant () (Some $ ValueOf (DefaultUniList uni) lits')
   firstPassConstArray ty lits =
     Left $ "firstPassConstArray: Expected an array type for the constant array:\n  "
            <> prettyStr lits
            <> "But received type: " <> prettyStr ty

convertTrueLit :: forall (x :: GHC.Type). DefaultUni (Esc x) -> Lit WithoutObjects Void -> PIRConvertM x
convertTrueLit uni lit = case uni of
  PLC.DefaultUniData -> Left "convertTrueLit: Data literals not supported at this time"
  PLC.DefaultUniByteString -> Left "convertTrueLit: ByteString literals are not supported at this time (though I guess they could be?)"
  PLC.DefaultUniPair _ _ -> Left "convertTrueLit: Tuple literals are not supported at this time"
  PLC.DefaultUniBool -> case lit of
    BoolL b -> pure b
    other -> Left (mkError other)
  PLC.DefaultUniInteger -> case lit of
    CharL c -> pure . toInteger . fromEnum $ c
    IntL i  -> pure i
    other   -> Left (mkError other)
  PLC.DefaultUniString -> case lit of
    StringL str -> pure . prettyPrintString $ str
    other -> Left (mkError other)
  PLC.DefaultUniList uni' -> case lit of
    ConstArrayL xs -> traverse (convertTrueLit uni') xs
    other -> Left (mkError other)
  other -> Left $ "Unsupported PLC Constant type " <> show other <> " for literal value: " <> prettyStr lit
 where
   mkError :: Lit WithoutObjects Void -> String
   mkError wrong = "convertTrueLit: Type mismatch. Expected a literal value of PLC DefaultUni type " <> show uni <> " but found: " <> prettyStr wrong

{-
fuckThisMonadStack ::
      forall e m c  b.
      (e ~ Error DefaultUni DefaultFun (Provenance ())
      , c ~ CompilationCtx DefaultUni DefaultFun ()
      , m ~ ExceptT e (ExceptT e (QuoteT (Reader c)))
      )
      => (Compiling m e DefaultUni DefaultFun ()) => m b -> Either String b
fuckThisMonadStack x  =
      let
        res :: Either e b
        res = do
            plcConfig <- getDefTypeCheckConfig (Original ())
            let ctx = toDefaultCompilationCtx plcConfig
            join $ flip runReader ctx $ runQuoteT $ runExceptT $ runExceptT x
      in first show res

runPLCProgram :: PLCProgram DefaultUni DefaultFun () -> (EvaluationResult (PLC.Term TyName Name DefaultUni DefaultFun ()),[Text])
runPLCProgram (PLC.Program _ _ c) = unsafeEvaluateCk PLC.defaultBuiltinsRuntime $ void c

runPLCProgramTest :: String
                  -> (EvaluationResult (PLC.Term TyName Name DefaultUni DefaultFun ()),[Text])
                  -> FilePath
                  -> Text
                  -> TestTree
runPLCProgramTest testName expected path decl  = testCase testName $ do
  prog <- declToUPLC path decl
  let out = runPLCProgram prog
  assertEqual "program output matches expected "  expected out

declToUPLC :: FilePath
           -> Text
           -> IO (PLCProgram  DefaultUni DefaultFun ())
declToUPLC path decl = prepPIR path decl >>= \case
  (mainExpr,dict) -> do
    tcMap <- rethrowIO $ mkTyConMap dict
    ctorMap <- rethrowIO $ mkConstructorMap dict
    let initialState = ConvertState 0 M.empty M.empty tcMap ctorMap
        result = evalState (toPIR F mainExpr) initialState
    putStrLn $ replicate 20 '-' <> "PIR" <> replicate 20 '-'
    print $ prettyPirReadable result
    putStrLn $ replicate 43 '-' <> "\n"
    let input = Program (Original ()) latestVersion (Original <$> result)
    readable <- aghhhh . fuckThisMonadStack $ compileToReadable input
    aghhhh . fuckThisMonadStack $ compileProgram (void readable)

 where
   aghhhh = either (throwIO . userError) pure
   rethrowIO = \case
     Left te@(TypeConvertError _ _ _ ) -> error (prettyErrorT te)
     Right x -> pure x

-- can't figure out if this is exported in w/e version of plutus-core we're on here
splitFunTyParts :: Type tyname uni a -> NE.NonEmpty (Type tyname uni a)
splitFunTyParts = \case
    TyFun _ t1 t2 -> t1 NE.<| splitFunTyParts t2
    t             -> pure t
-}

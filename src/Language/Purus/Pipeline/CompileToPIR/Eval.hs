{-# LANGUAGE TypeApplications #-}
module Language.Purus.Pipeline.CompileToPIR.Eval where

import Prelude

import Data.Text (Text)
import Data.Text qualified as T



import Data.Foldable (traverse_)

import Data.Bifunctor ( Bifunctor(first) )
import Data.Functor ((<&>)) 

import Control.Exception (throwIO)
import Control.Exception qualified as E

import Control.Monad ( join, void )
import Control.Monad.Reader ( runReader, Reader )
import Control.Monad.Trans.Except (ExceptT, runExceptT)

import Language.PureScript.CoreFn.Module ( Datatypes )

import Language.Purus.Pretty.Common (prettyStr)
import Language.Purus.IR ( Exp, FVar, BVar, Ty )
import Language.Purus.IR qualified as IR
import Language.Purus.IR.Utils ( WithoutObjects )
import Language.Purus.Types ( PIRTerm )

import Bound ( Var )

import PlutusCore
    ( latestVersion, runQuoteT, getDefTypeCheckConfig )
import PlutusCore qualified as PLC
import PlutusCore.Evaluation.Machine.ExBudgetingDefaults qualified as PLC
import PlutusIR.Compiler (CompilationCtx, Compiling, compileProgram, compileToReadable, toDefaultCompilationCtx)
import PlutusCore.Default (
  DefaultFun,
  DefaultUni,
 )
import PlutusCore.Evaluation.Machine.Ck (
  EvaluationResult,
  unsafeEvaluateCk,
 )
import PlutusCore.Pretty (prettyPlcReadableDef)
import PlutusIR (Name, Program (Program))
import PlutusIR.Compiler.Provenance (Provenance (Original))
import PlutusIR.Error (Error)



type PLCProgram uni fun a = PLC.Program PLC.TyName PLC.Name uni fun (Provenance a)
---- Compilation helpers/utilities

-- FIXME/TODO re-implement this when we have the pipeline reorganized and rebuilt
prepPIR :: String -> Text -> IO (Exp WithoutObjects Ty (Var (BVar Ty) (FVar Ty)), Datatypes IR.Kind Ty)
prepPIR = undefined


runPLCProgram :: PLCProgram DefaultUni DefaultFun () -> (EvaluationResult (PLC.Term PLC.TyName Name DefaultUni DefaultFun ()), [Text])
runPLCProgram (PLC.Program _ _ c) = unsafeEvaluateCk PLC.defaultBuiltinsRuntime $ void c

declToPIR ::
  FilePath ->
  Text ->
  IO PIRTerm
declToPIR _path _decl = undefined {-
  prepPIR path decl >>= \case
    (mainExpr, datatypes) -> do
      case mkTypeBindDict datatypes mainExpr of
        Left err -> throwIO . userError $ err
        Right dict -> case runPlutusContext  dict $ firstPass datatypes id =<< eliminateCaseExpressionsTrace datatypes mainExpr of
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
-}
printExpr :: FilePath -> Text -> IO ()
printExpr path decl =
  prepPIR path decl >>= \case
    (e, _) -> putStrLn ("\n\n\n" <> T.unpack decl <> " = \n" <> prettyStr e)

declToPLC :: FilePath -> Text -> IO (PLCProgram DefaultUni DefaultFun ())
declToPLC path main = declToPIR path main >>= compileToUPLC

evaluateTerm :: PIRTerm -> IO (EvaluationResult (PLC.Term PLC.TyName Name DefaultUni DefaultFun ()), [Text])
evaluateTerm term = runPLCProgram <$> compileToUPLC term 

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

passing :: [Text]
passing =
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

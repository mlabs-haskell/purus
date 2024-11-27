module Language.Purus.Eval (
  compileToUPLC,
  evaluateTerm,
  -- temporary for GHCI testing. TODO move these to the test suite
  passing,
) where

import Prelude

import Data.Text (Text)

import Data.Bifunctor (Bifunctor (first))

import Control.Exception (throwIO)

import Control.Monad (join, void)
import Control.Monad.Reader (Reader, runReader)
import Control.Monad.Trans.Except (ExceptT, runExceptT)

import Language.Purus.Types (PIRTerm, PLCTerm)

import PlutusCore (
  getDefTypeCheckConfig,
  latestVersion,
  runQuoteT,
 )
import PlutusCore qualified as PLC
import PlutusCore.Default (
  DefaultFun,
  DefaultUni,
 )
import PlutusCore.Evaluation.Machine.Ck (
  EvaluationResult (EvaluationFailure, EvaluationSuccess),
  unsafeToEvaluationResult,
  evaluateCk
 )
import PlutusCore.Evaluation.Machine.ExBudgetingDefaults qualified as PLC
import PlutusIR (Name, Program (Program))
import PlutusIR.Compiler (CompilationCtx, Compiling, compileProgram, compileToReadable, toDefaultCompilationCtx, ccOpts)
import PlutusIR.Compiler.Provenance (Provenance (Original))
import PlutusIR.Compiler.Types (coDoSimplifierRemoveDeadBindings)
import PlutusIR.Error (Error)
import Control.Lens (over, set)

type PLCProgram uni fun a = PLC.Program PLC.TyName PLC.Name uni fun (Provenance a)

-- TODO: Add a cpu budget so we can evaluate non-terminating functions in tests.
--       See plutarch: https://github.com/Plutonomicon/plutarch-plutus/blob/staging/Plutarch/Internal/Evaluate.hs#L28-L54
--       Thanks for the link Tomasz!

{- Evaluates a UPLC Program -}
runPLCProgram :: PLCProgram DefaultUni DefaultFun () -> (EvaluationResult PLCTerm, [Text])
runPLCProgram (PLC.Program _ _ c) = case evaluateCk PLC.defaultBuiltinsRuntimeForTesting . void $ c of 
  (result, logs) -> case result of 
    Left _ -> (EvaluationFailure, logs)
    Right t -> (EvaluationSuccess t, logs)

{- Evaluates a PIR Term -}
evaluateTerm :: PIRTerm -> IO (EvaluationResult (PLC.Term PLC.TyName Name DefaultUni DefaultFun ()), [Text])
evaluateTerm term = runPLCProgram <$> compileToUPLC term

{- Compile a PIR Term to a UPLC Program-}
compileToUPLC :: PIRTerm -> IO (PLCProgram DefaultUni DefaultFun ())
compileToUPLC e = do
  let input = Program (Original ()) latestVersion (Original <$> e)
      withErrors = either (throwIO . userError) pure
  readable <- withErrors . runCompile $ compileToReadable input
  withErrors . runCompile $ compileProgram (void readable)

{- lol -}
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
      let ctx = disableDeadCodeElimination $ toDefaultCompilationCtx plcConfig
      join $ flip runReader ctx $ runQuoteT $ runExceptT $ runExceptT x
   in
    first show res
 where
   disableDeadCodeElimination :: CompilationCtx DefaultUni DefaultFun ()
                              -> CompilationCtx DefaultUni DefaultFun ()
   disableDeadCodeElimination = set  (ccOpts . coDoSimplifierRemoveDeadBindings ) False

-- temporary list of test cases used to validate compiler behavior
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
  , "litPattern"
  , "irrPattern"
  ]

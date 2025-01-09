{-# LANGUAGE TypeApplications #-}
module Language.Purus.Eval (
  compileToUPLC,
  compileToUPLCTerm,
  convertToUPLCAndEvaluate,
  evaluateUPLCTerm,
  evaluateTerm,
  evaluateTermU_,
  evaluateTermU_',
  evaluateTermU,
  reasonablySizedBudget,
  parseData,
  (#),
  applyArgs,
  dummyData
) where

import Prelude

import Data.Text (Text)

import Data.Bifunctor (Bifunctor (first))

import Control.Exception (throwIO)

import Control.Monad (join, void)
import Control.Monad.Reader (Reader, runReader)
import Control.Monad.Trans.Except (ExceptT, runExceptT)

import Language.Purus.Types (PIRTerm, PLCTerm, UPLCTerm)

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
  evaluateCk
 )
import PlutusCore.Evaluation.Machine.ExBudgetingDefaults qualified as PLC
import PlutusIR (Name, Program (Program))
import PlutusIR.Compiler (CompilationCtx, Compiling, compileProgram, compileToReadable, toDefaultCompilationCtx, ccOpts)
import PlutusIR.Compiler.Provenance (Provenance (Original))
import PlutusIR.Compiler.Types (coDoSimplifierRemoveDeadBindings)
import PlutusIR.Error (Error)
import Control.Lens (set)
import PlutusCore.Data qualified as PLC
import PlutusCore.MkPlc (mkConstant)
import PlutusCore.Evaluation.Machine.ExBudget (
  ExBudget (ExBudget),
  ExRestrictingBudget (ExRestrictingBudget),
  minusExBudget,
 )
import PlutusCore.Compiler.Erase (eraseTerm)
import UntypedPlutusCore.DeBruijn ( deBruijnTerm )
import PlutusCore.Evaluation.Machine.ExBudgetingDefaults (defaultCekParametersForTesting)
import PlutusCore.Evaluation.Machine.ExMemory (ExCPU (ExCPU), ExMemory (ExMemory))
import UntypedPlutusCore.Evaluation.Machine.Cek qualified as Cek
import Language.Purus.Pretty.Common (prettyStr)
import PlutusCore.Pretty (prettyPlcReadableDef)
import Language.Purus.Pretty (docString)

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

(#) :: PLCTerm -> PLCTerm -> PLCTerm
f # a = PLC.Apply () f a

applyArgs :: PLCTerm -> [PLCTerm] -> PLCTerm
applyArgs f args = foldl (#) f args

-- Parse a file containing a "show'd" piece of Data into a PLC term.
-- Mainly for testing but might have some other uses.
parseData :: FilePath -> IO PLCTerm
parseData path = do
  raw <- readFile path
  pure $ mkConstant () (read @PLC.Data raw)

-- for when we just need *something* to apply as a dummy argument 
dummyData :: PLCTerm
dummyData = mkConstant () $ PLC.I 0

{- Evaluates a PIR Term -}
evaluateTerm :: PIRTerm -> IO (EvaluationResult (PLC.Term PLC.TyName Name DefaultUni DefaultFun ()), [Text])
evaluateTerm term = runPLCProgram <$> compileToUPLC term

convertToUPLCAndEvaluate :: PIRTerm -> IO ()
convertToUPLCAndEvaluate t = evaluateTerm t >>= \case
  (EvaluationSuccess plcTerm ,_) -> evaluateTermU_ reasonablySizedBudget plcTerm
  (EvaluationFailure,logs) -> error (show logs)
  
{- Compile a PIR Term to a UPLC Program-}
compileToUPLC :: PIRTerm -> IO (PLCProgram DefaultUni DefaultFun ())
compileToUPLC e = do
  let input = Program (Original ()) latestVersion (Original <$> e)
      withErrors = either (throwIO . userError) pure
  readable <- withErrors . runCompile $ compileToReadable input
  withErrors . runCompile $ compileProgram (void readable)

compileToUPLCTerm :: PIRTerm -> IO PLCTerm
compileToUPLCTerm e = compileToUPLC e >>= \case
  PLC.Program a b c -> pure (void c)

evaluateUPLCTerm :: PLCTerm -> IO (EvaluationResult PLCTerm, [Text])
evaluateUPLCTerm e = do
  let input = PLC.Program (Original ()) latestVersion (Original <$> e)
      withErrors = either (throwIO . userError) pure
  pure $ runPLCProgram input

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


toDeBruijnUPLC :: PLCTerm -> Either String UPLCTerm
toDeBruijnUPLC t =  first prettyStr x
  where
    x :: Either (Error DefaultUni DefaultFun ()) UPLCTerm
    x = deBruijnTerm (eraseTerm t)

reasonablySizedBudget :: ExBudget
reasonablySizedBudget = ExBudget (ExCPU 100000000) (ExMemory 100000)


-- stolen from plutarch

evaluateTermU ::
  ExBudget ->
  PLCTerm ->
  Either (String, Maybe ExBudget, [Text]) (UPLCTerm,ExBudget,[Text])
evaluateTermU budget t = case toDeBruijnUPLC t of
  Left err -> Left (err, Nothing, [])
  Right uplc -> case Cek.runCekDeBruijn defaultCekParametersForTesting (Cek.restricting (ExRestrictingBudget budget)) Cek.logEmitter uplc of
    (errOrRes, Cek.RestrictingSt (ExRestrictingBudget final), logs) -> case errOrRes of
      Left err -> Left (show err, Just $ budget `minusExBudget` final, logs)
      Right res -> Right (res, budget `minusExBudget` final, logs)

-- for tests
evaluateTermU_ ::
  ExBudget ->
  PLCTerm ->
  IO ()
evaluateTermU_ budget t = case evaluateTermU budget t of
  Left (msg,mBudg,logs) -> do
    let prettyErr = "Failed to evaluate term\nError Message:\n" <> msg <>
                    (case mBudg of
                      Nothing -> ""
                      Just resBudg -> "\nCost: " <> prettyStr resBudg <> "\nLog: " <> prettyStr logs)
    throwIO $ userError prettyErr
  Right _ -> pure ()

evaluateTermU_' ::
  ExBudget ->
  PLCTerm ->
  IO ()
evaluateTermU_' budget t = case evaluateTermU budget t of
  Left (msg,mBudg,logs) -> do
    let prettyErr = "Failed to evaluate term\nError Message:\n" <> msg <>
                    (case mBudg of
                      Nothing -> ""
                      Just resBudg -> "\nCost: " <> prettyStr resBudg <> "\nLog: " <> prettyStr logs)
    throwIO $ userError prettyErr
  Right res -> do
    putStrLn . docString $  prettyPlcReadableDef res
    pure ()

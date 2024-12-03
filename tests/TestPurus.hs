{-# LANGUAGE TypeApplications #-}
module TestPurus where

import Prelude
import Data.Text (Text)
import Command.Compile ( compileForTests, PSCMakeOptions(..) )
import Control.Monad (when,unless)
import System.FilePath
import Language.PureScript qualified as P
import Data.Set qualified as S
import Data.Foldable (traverse_)
import System.Directory 
import System.FilePath.Glob qualified as Glob
import Data.Function (on)
import Data.List (sortBy, stripPrefix, groupBy)
import Language.Purus.Make
import Language.Purus.Eval
import Language.Purus.Types
import PlutusCore.Evaluation.Result
import PlutusIR.Core.Instance.Pretty.Readable (prettyPirReadable)
import Test.Tasty 
import Test.Tasty.HUnit
import Language.Purus.Make.Prim (syntheticPrim)

shouldPassTests :: IO ()
shouldPassTests = do
  cfn <- coreFnTests
  pirNoEval <- pirTestsNoEval
  pirEval <- pirTestsEval
  let validatorTest = testCase "validator apply/eval" mkValidatorTest
      policyTest    = testCase "minting policy apply/eval" mkMintingPolicyTest
  defaultMain $ sequentialTestGroup "Purus Tests" AllFinish [cfn,pirNoEval,pirEval,validatorTest,policyTest]
 
runPurusCoreFn :: P.CodegenTarget -> FilePath ->  IO ()
runPurusCoreFn target dir =  do
    outDirExists <- doesDirectoryExist outputDir
    when (target /= P.CheckCoreFn) $ do
      when outDirExists $ do
        removeDirectoryRecursive outputDir
        createDirectory outputDir
      unless outDirExists $ createDirectory outputDir
    files <- concat <$> getTestFiles dir
    compileForTests (makeOpts files)
  where
    outputDir = dir </> "output"

    makeOpts :: [FilePath] -> PSCMakeOptions
    makeOpts files = PSCMakeOptions {
      pscmInput = files,
      pscmExclude = [],
      pscmOutputDir = outputDir,
      pscmOpts = purusOpts,
      pscmUsePrefix = False,
      pscmJSONErrors = False
    }

    purusOpts :: P.Options
    purusOpts = P.Options {
      optionsVerboseErrors = True,
      optionsNoComments = True,
      optionsCodegenTargets = S.singleton target
    }

-- TODO: Move modules into a directory specifically for PIR non-eval tests (for now this should be OK)
pirTestsNoEval :: IO TestTree
pirTestsNoEval = do
  let coreFnTestPath = "tests/purus/passing/CoreFn"
  allTestDirectories <- listDirectory coreFnTestPath
  trees <- mapM (\dir -> compileDirNoEvalTest (coreFnTestPath </> dir)) allTestDirectories-- allTestDirectories
  pure $ sequentialTestGroup  "PIR Tests (No Evaluation)" AllFinish trees

pirTestsEval :: IO TestTree
pirTestsEval = do
  let coreFnTestPath = "tests/purus/passing/CoreFn"
  allTestDirectories <- listDirectory coreFnTestPath
  trees <- mapM (\dir -> compileDirEvalTest (coreFnTestPath </> dir)) allTestDirectories-- allTestDirectories
  pure $ sequentialTestGroup "PIR Tests (Evaluation)" AllFinish trees
-- path to a Purus project directory, outputs serialized CoreFn 
compileToCoreFnTest :: FilePath -> TestTree
compileToCoreFnTest path = testCase (path) $ runPurusCoreFnDefault path

coreFnTests :: IO TestTree
coreFnTests = do
  let coreFnTestPath = "tests/purus/passing/CoreFn"
  allTestDirectories <- listDirectory coreFnTestPath
  let trees = map (\dir -> compileToCoreFnTest (coreFnTestPath </> dir)) allTestDirectories
  pure $ sequentialTestGroup "CoreFn Tests" AllFinish trees


runPurusCoreFnDefault :: FilePath -> IO ()
runPurusCoreFnDefault path = runPurusCoreFn P.CoreFn path

runPurusGolden :: FilePath -> IO ()
runPurusGolden path = runPurusCoreFn P.CheckCoreFn path

runFullPipeline_ :: FilePath -> Text -> Text -> IO ()
runFullPipeline_ targetDir mainModuleName mainFunctionName = do
  runPurusCoreFnDefault targetDir
  pir <- make targetDir mainModuleName mainFunctionName Nothing
  result <- evaluateTerm pir
  print $ prettyPirReadable result

runFullPipeline :: FilePath -> Text -> Text -> IO (EvaluationResult PLCTerm, [Text])
runFullPipeline targetDir mainModuleName mainFunctionName = do
  runPurusCoreFnDefault targetDir
  pir <- make targetDir mainModuleName mainFunctionName Nothing
  evaluateTerm pir

mkValidatorTest :: IO ()
mkValidatorTest = do
  scriptContext <- parseData "sampleContext"
  --  Data -> Data -> Data -> wBoolean
  validatorPIR <- make "tests/purus/passing/CoreFn/Validator" "Validator" "validate" (Just syntheticPrim)
  validatorPLC <- compileToUPLCTerm validatorPIR
  let validatorApplied = applyArgs validatorPLC [dummyData,dummyData,scriptContext]
  res <- evaluateUPLCTerm validatorApplied
  print res

mkMintingPolicyTest :: IO ()
mkMintingPolicyTest = do
  scriptContext <- parseData "sampleContext"
  policyPIR <- make "tests/purus/passing/CoreFn/MintingPolicy" "MintingPolicy" "oneAtATime" (Just syntheticPrim)
  policyPLC <- compileToUPLCTerm policyPIR
  let policyApplied = applyArgs policyPLC [dummyData,dummyData,scriptContext]
  res <- evaluateUPLCTerm policyApplied
  print res
{- These assumes that name of the main module is "Main" and the
   name of the main function is "Main".

   For now this recompiles everything from scratch
-}

runDefaultCheckEvalSuccess :: String -> FilePath ->  Assertion
runDefaultCheckEvalSuccess nm targetDir
  = (fst <$> runFullPipeline targetDir "Main" "main") >>= assertBool nm . isEvaluationSuccess

runDefaultEvalTest :: String -> FilePath -> PLCTerm -> Assertion
runDefaultEvalTest nm targetDir expected
  = (fst <$> runFullPipeline targetDir "Main" "main") >>= \case
      EvaluationSuccess resTerm -> assertEqual nm expected resTerm
      EvaluationFailure -> assertFailure nm


getTestFiles :: FilePath -> IO [[FilePath]]
getTestFiles testDir = do
  let dir =  testDir
  getFiles dir <$> testGlob dir
  where
  -- A glob for all purs and js files within a test directory
  testGlob :: FilePath -> IO [FilePath]
  testGlob = Glob.globDir1 (Glob.compile "**/*.purs")
  -- Groups the test files so that a top-level file can have dependencies in a
  -- subdirectory of the same name. The inner tuple contains a list of the
  -- .purs files and the .js files for the test case.
  getFiles :: FilePath -> [FilePath] -> [[FilePath]]
  getFiles baseDir
    = map (filter ((== ".purs") . takeExtensions) . map (baseDir </>))
    . groupBy ((==) `on` extractPrefix)
    . sortBy (compare `on` extractPrefix)
    . map (makeRelative baseDir)
  -- Extracts the filename part of a .purs file, or if the file is in a
  -- subdirectory, the first part of that directory path.
  extractPrefix :: FilePath -> FilePath
  extractPrefix fp =
    let dir = takeDirectory fp
        ext = reverse ".purs"
    in if dir == "."
       then maybe fp reverse $ stripPrefix ext $ reverse fp
       else dir

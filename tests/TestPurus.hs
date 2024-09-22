{-# LANGUAGE TypeApplications #-}
module TestPurus where

import Prelude
import Data.Text (Text)
import Command.Compile ( compileForTests, compileForTestsWith, PSCMakeOptions(..) )
import Control.Monad (when,unless)
import System.FilePath
import Language.PureScript qualified as P
import Data.Set qualified as S
import Data.Foldable (traverse_)
import System.Directory
import System.FilePath.Glob qualified as Glob
import System.Exit qualified as Exit
import Data.Function (on)
import Data.List (sortBy, stripPrefix, groupBy)
import Language.Purus.Make
import Language.Purus.Eval
import Language.Purus.Types
import PlutusCore.Evaluation.Result
import PlutusIR.Core.Instance.Pretty.Readable (prettyPirReadable)
import Test.Tasty
import Test.Tasty.HUnit
import Language.PureScript qualified as PureScript
import Language.PureScript.CST.Errors qualified as PureScript.CST.Errors
import Data.Maybe qualified as Maybe
import Control.Arrow qualified as Arrow

shouldPassTests :: IO ()
shouldPassTests = do
  cfn <- coreFnTests
  pir <- pirTests
  defaultMain $ testGroup "Purus Tests" [cfn,pir,parserTests]


parserTests :: TestTree
parserTests = testGroup "Parser" $ map go shouldParseError
  where
    -- Jared provides the directory names so we need to add the full (relative) path (esp since I moved them)

    go :: (FilePath,String) -> TestTree
    go (path,msg) = testCase path $ runPurusParseError (path,msg)

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
pirTests :: IO TestTree
pirTests = do
  let coreFnTestPath = "tests/purus/passing/CoreFn"
  allTestDirectories <- listDirectory coreFnTestPath
  let trees = map (\dir -> testCase dir $ compileDirNoEval (coreFnTestPath </> dir)) allTestDirectories
  pure $ testGroup "PIR Tests (No Evaluation)" trees

-- path to a Purus project directory, outputs serialized CoreFn 
compileToCoreFnTest :: FilePath -> TestTree
compileToCoreFnTest path = testCase (path) $ runPurusCoreFnDefault path

coreFnTests :: IO TestTree
coreFnTests = do
  let coreFnTestPath = "tests/purus/passing/CoreFn"
  allTestDirectories <- listDirectory coreFnTestPath
  let trees = map (\dir -> compileToCoreFnTest (coreFnTestPath </> dir)) allTestDirectories
  pure $ testGroup "CoreFn Tests" trees


runPurusCoreFnDefault :: FilePath -> IO ()
runPurusCoreFnDefault path = runPurusCoreFn P.CoreFn path

-- | Runs purus but succeeds only in the case of a parse error.
runPurusShouldParseErrorWithMessage ::
    P.CodegenTarget -> 
    FilePath -> 
    String -> 
    IO ()
runPurusShouldParseErrorWithMessage target dir errMsg = 
    runPurusWith target dir $ \_files _warnings errors ->
        case errors of
            Right _ -> do
                putStrLn "Compilation succeeded, but expected parse error"
                Exit.exitFailure
            Left PureScript.MultipleErrors{PureScript.runMultipleErrors = psErrors } -> 
                case Maybe.mapMaybe 
                        (\(PureScript.ErrorMessage _ simpleErrMsg) -> case simpleErrMsg of 
                            PureScript.ErrorParsingCSTModule parserError -> Just parserError
                            _ -> Nothing
                                ) psErrors of
                    [parserErrorInfo]
                        |  prettyErr == errMsg -> putStrLn $ "Parse failed with expected error: " ++ prettyErr
                        |  otherwise -> do
                            putStrLn $ "Unexpected parse error of:\n" ++ prettyErr
                            putStrLn $ "But expected:\n" ++ errMsg
                            Exit.exitFailure
                      where
                        prettyErr = PureScript.CST.Errors.prettyPrintError parserErrorInfo

                    _ -> putStrLn "Too many parser errors" >> Exit.exitFailure
                    
                

-- | Runs purus with a function with the output of the compiler. This is useful
-- to create test cases that should fail.
runPurusWith:: 
    P.CodegenTarget -> 
    FilePath -> 
    -- | Arguments are: files, warnings, errors
    ([(FilePath, Text)] -> PureScript.MultipleErrors -> Either PureScript.MultipleErrors [PureScript.ExternsFile] -> IO ()) -> 
    IO ()
runPurusWith target dir f =  do
    outDirExists <- doesDirectoryExist outputDir
    when (target /= P.CheckCoreFn) $ do
      when outDirExists $ do
        removeDirectoryRecursive outputDir
        createDirectory outputDir
      unless outDirExists $ createDirectory outputDir
    files <- concat <$> getTestFiles dir
    print files
    print ("Compiling " <> dir)
    compileForTestsWith (makeOpts files) f
    print ("Done with " <> dir)
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

runPurusDefault :: FilePath -> IO ()
runPurusDefault path = runPurusCoreFn P.CoreFn path

runPurusParseError :: (FilePath, String) -> IO ()
runPurusParseError (path, errMsg) = runPurusShouldParseErrorWithMessage P.CoreFn path errMsg

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


-- | 'shouldParseError' is a list of tuples of:
--
-- - the path to the directory of the PureScript project
--
-- - the parser error message when compiling the PureScript project
--
-- So, to add a test, normally one would add a tuple like
-- > ("NewTestCase", "dummy error message")
-- run the tests, and see what the failed test message is, then copy and paste
-- the failed test message in the second projection of the tuple.
--
-- TODO(jaredponn): it would be really great to have some sort of testing
-- framework (golden tests!) to automate this out
shouldParseError :: [(FilePath, String)]
shouldParseError = map (Arrow.first (prefix </>)) paths
  where
    prefix = "tests/purus/passing/invalid-parses"
    paths = [
        ("NestedConstructors", "Unexpected token 'S' at line 7, column 16"),
        ("CharLiteralInBinaryOp", "Unexpected token ''0'' at line 9, column 18"),
        ("NumericLiteralInBinaryOp", "Unexpected token '0' at line 9, column 16"),
        ("NumericLiteralInConstructor", "Unexpected token '0' at line 6, column 14"),
        ("CharLiteralInConstructor", "Unexpected token ''_'' at line 9, column 24"),
        ("NestedBinaryOps", "Unexpected token '_' at line 4, column 24"),
        ("VariableAsConstructor", "Unexpected token 'b' at line 4, column 13")
      ]

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

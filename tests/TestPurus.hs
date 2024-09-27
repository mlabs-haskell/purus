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

shouldPassTests :: IO ()
shouldPassTests = do
  cfn <- coreFnTests
  pir <- pirTests
  defaultMain $ testGroup "Purus Tests" [cfn,pir]

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


shouldPassCoreFn :: [FilePath]
shouldPassCoreFn = map (prefix </>) paths
  where
    prefix = "tests/purus/passing/CoreFn"
    paths = [
         "2018",
        "2138",
        "2609",
        "4035",
        "4101",
        "4105",
        "4200",
        "4310",
        "ClassRefSyntax",
        "Coercible",
        "DctorOperatorAlias",
        "Demo",
        "ExplicitImportReExport",
        "ExportExplicit",
        "ExportExplicit2",
        "ForeignKind",
        "Import",
        "ImportExplicit",
        "ImportQualified",
        "InstanceUnnamedSimilarClassName",
        "ModuleDeps",
         "Misc",
        "NonOrphanInstanceFunDepExtra",
        "NonOrphanInstanceMulti",
        "PendingConflictingImports",
        "PendingConflictingImports2",
        "RedefinedFixity",
        "ReExportQualified",
        "ResolvableScopeConflict",
        "ResolvableScopeConflict2",
        "ResolvableScopeConflict3",
        "RowSyntax",
        "ShadowedModuleName",
        "TransitiveImport",
        "Validator"
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

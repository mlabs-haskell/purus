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
import System.Directory (removeDirectoryRecursive, doesDirectoryExist, createDirectory)
import System.FilePath.Glob qualified as Glob
import System.Exit qualified as Exit
import Data.Function (on)
import Data.List (sortBy, stripPrefix, groupBy)
import Language.Purus.Make
import Language.Purus.Eval
import PlutusIR.Core.Instance.Pretty.Readable (prettyPirReadable)
import Language.PureScript qualified as PureScript
import Language.PureScript.CST.Errors qualified as PureScript.CST.Errors
import Data.Maybe qualified as Maybe
import Control.Arrow qualified as Arrow

shouldPassTests :: IO ()
shouldPassTests = do
  traverse_ runPurusDefault shouldPass
  traverse_ runPurusParseError shouldParseError
  -- let misc =  "./tests/purus/passing/Misc/output/Lib/index.cfn"
  {- UPLC tests disabled atm while we rewrite stuff

  uplc1 <- declToUPLC misc "main"
  writeFile "./tests/purus/passing/Misc/output/Lib/main.plc" (show uplc1)
  uplc2 <- declToUPLC misc "minus"
  writeFile "./tests/purus/passing/Misc/output/Lib/fakeminus.plc" (show uplc2)
  defaultMain $
    runPLCProgramTest
    "mainTest"
    (EvaluationSuccess (Constant () (Some (ValueOf DefaultUniInteger 2))),[])
    misc
    "main"
  -}

runPurus :: P.CodegenTarget -> FilePath ->  IO ()
runPurus target dir =  do
    outDirExists <- doesDirectoryExist outputDir
    when (target /= P.CheckCoreFn) $ do
      when outDirExists $ do
        removeDirectoryRecursive outputDir
        createDirectory outputDir
      unless outDirExists $ createDirectory outputDir
    files <- concat <$> getTestFiles dir
    print files
    print ("Compiling " <> dir)
    compileForTests (makeOpts files)
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
runPurusDefault path = runPurus P.CoreFn path

runPurusParseError :: (FilePath, String) -> IO ()
runPurusParseError (path, errMsg) = runPurusShouldParseErrorWithMessage P.CoreFn path errMsg

runPurusGolden :: FilePath -> IO ()
runPurusGolden path = runPurus P.CheckCoreFn path

runFullPipeline :: FilePath -> Text -> Text -> IO ()
runFullPipeline targetDir mainModuleName mainFunctionName = do
  runPurusDefault targetDir
  pir <- make targetDir mainModuleName mainFunctionName Nothing
  result <- evaluateTerm pir
  print $ prettyPirReadable result


shouldPass :: [FilePath]
shouldPass = map (prefix </>) paths
  where
    prefix = "tests/purus/passing"
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
        -- "prelude"
        "ValidPatterns"
      ]

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
    prefix = "tests/purus/invalid-parses"
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

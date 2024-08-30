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
import System.Directory (removeDirectoryRecursive, doesDirectoryExist, createDirectory)
import System.FilePath.Glob qualified as Glob
import Data.Function (on)
import Data.List (sortBy, stripPrefix, groupBy)
import Language.Purus.Make
import Language.Purus.Eval
import PlutusIR.Core.Instance.Pretty.Readable (prettyPirReadable)

shouldPassTests :: IO ()
shouldPassTests = do
  traverse_ runPurusDefault shouldPass
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
      when outDirExists $ removeDirectoryRecursive outputDir
      unless outDirExists $ createDirectory outputDir
    files <- concat <$> getTestFiles dir
    print files
    print ("Compiling " <> dir)
    compileForTests (makeOpts files)
    print ("Done with " <> dir)
  where
    outputDir = "tests" </> "purus" </> dir </> "output"

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
    prefix = "passing"
    paths = [
      {-  "2018",
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
        "TransitiveImport", -}
        "prelude"
      ]


getTestFiles :: FilePath -> IO [[FilePath]]
getTestFiles testDir = do
  let dir = "tests" </> "purus" </> testDir
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

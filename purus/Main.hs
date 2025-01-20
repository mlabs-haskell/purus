module Main where

import Prelude


import Data.Text (Text)
import Data.Text qualified as T
import Command.Compile ( compileForTests, PSCMakeOptions(..) )
import Control.Monad (when,unless, void)
import System.FilePath
    ( makeRelative, (</>), takeDirectory, takeExtensions )
import Language.PureScript qualified as P
import Data.Set qualified as S
import System.Directory
    ( createDirectory,
      createDirectoryIfMissing,
      doesDirectoryExist,
      listDirectory,
      removeDirectoryRecursive, createDirectoryIfMissing )
import System.FilePath.Glob qualified as Glob
import Data.Function (on)
import Data.List (sortBy, stripPrefix, groupBy)
import Language.Purus.Make ( allValueDeclarations, make )
import Language.Purus.Eval
import Language.Purus.Utils 
import Language.Purus.Types ( PIRTerm, PLCTerm, UPLCTerm )
import Language.Purus.Make.Prim (syntheticPrim)
import Language.PureScript (ModuleName(..), runModuleName)
import Control.Concurrent.STM
    ( atomically, TVar, newTVarIO, readTVarIO, modifyTVar' )
import Data.Map (Map)
import Data.Map qualified as M
import Unsafe.Coerce ( unsafeCoerce )
import Control.Exception (SomeException, try, Exception (displayException))
import Language.Purus.Pretty.Common (prettyStr, docString)
import System.IO
import PlutusIR.Core.Instance.Pretty.Readable (prettyPirReadable)
import UntypedPlutusCore (DefaultUni)
import PlutusCore.MkPlc (HasTermLevel)
import Data.Maybe (mapMaybe)
import UntypedPlutusCore.Core qualified as UPLC
import PlutusCore.Evaluation.Machine.Ck (
  EvaluationResult (EvaluationFailure, EvaluationSuccess),
  evaluateCk
 )
import System.Environment (getArgs)

{- We *could* bake this into the existing purs executable, but doing so would require fundamentally changing the
   way in which `purs compile` works. purs compiles *individual modules* whereas we compile *entire projects* into a
   single script. It's easier / less disruptive to just have our own command
-}

main :: IO ()
main = getArgs >>= \case
  [projectDirPath] -> do
    putStrLn $ "Compiling " <> projectDirPath <> " ..."
    runPurusCoreFnDefault projectDirPath
    mainFnPIR <- make projectDirPath "Main" "main" (Just syntheticPrim)
    evaluateTerm mainFnPIR >>= \case -- we probably *do* want to do this here, if nothing else it should strip unused data declarations which our winnower didn't catch
      (EvaluationFailure,logs) -> do
        let msg = "Error: Something went wrong during PIR compilation! Logs: " <> prettyStr logs
        error msg
      (EvaluationSuccess plcTerm,_) -> do
        let uplcTerm =  either error id . toDeBruijnUPLC $ plcTerm  -- really need to change the name to `compileToPLCTerm`
        let scriptDir = projectDirPath </> "output" </> "scripts"
            scriptOutPath = scriptDir </> "script.uplc"
        createDirectoryIfMissing True scriptDir -- 'output' has to be there if corefn worked
        serializePlc scriptOutPath uplcTerm
        putStrLn $ "Success! You can find your serialized UPLC script at: " <> scriptOutPath
  [] -> putStrLn "USER ERROR: You did not supply a project directory argument for the compiler."
  _  -> error "USER ERROR: You supplied multiple project directories. You must invoke Purus targeting a directory which contains PureScript files (they can be nested inside child directories of the target)"
-- All of this stuff is duplicated in the purus tests, which I should probably fix at some point but shouldn't be an issue for now

{- The PureScript -> CoreFn part of the pipeline. Need to run this to output the CoreFn
   files which the other tests depend upon. (The Purus pipeline starts by parsing those CoreFn files)
-}
runPurusCoreFn :: P.CodegenTarget -> FilePath ->  IO ()
runPurusCoreFn target dir =  do
    outDirExists <- doesDirectoryExist outputDir
    when (target /= P.CheckCoreFn) $ do
      when outDirExists $ do
        removeDirectoryRecursive outputDir
        createDirectory outputDir
      unless outDirExists $ createDirectory outputDir
    files <- concat <$> getFilesToCompile dir
    compileForTests (makeOpts files)
  where
    outputDir = dir </> "output"

    makeOpts :: [FilePath] -> PSCMakeOptions
    makeOpts files = PSCMakeOptions {
      pscmInput = files,
      pscmExclude = [],
      pscmOutputDir = outputDir,
      pscmOpts = unsafeCoerce purusOpts, -- IT IS THE RIGHT TYPE BUT HLS WILL NOT SHUT UP ABOUT IT
      pscmUsePrefix = False,
      pscmJSONErrors = False
    }

    purusOpts :: P.Options
    purusOpts = P.Options {
      optionsVerboseErrors = False,
      optionsNoComments = True,
      optionsCodegenTargets = S.singleton target
    }

getFilesToCompile :: FilePath -> IO [[FilePath]]
getFilesToCompile testDir = do
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

{- Runs the PureScript -> CoreFn part of the compiler pipeline in default mode (i.e. not in Golden mode, i.e.
   this actually writes output files in the project directories)
-}
runPurusCoreFnDefault :: FilePath -> IO ()
runPurusCoreFnDefault path = runPurusCoreFn P.CoreFn path

{-# LANGUAGE TypeApplications #-}
module TestPurus  where

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
      doesDirectoryExist,
      listDirectory,
      removeDirectoryRecursive )
import System.FilePath.Glob qualified as Glob
import Data.Function (on)
import Data.List (sortBy, stripPrefix, groupBy)
import Language.Purus.Make ( allValueDeclarations, make )
import Language.Purus.Eval
    ( applyArgs,
      parseData,
      dummyData,
      evaluateTerm,
      convertToUPLCAndEvaluate,
      compileToUPLCTerm,
      evaluateUPLCTerm )
import Language.Purus.Types ( PIRTerm )
import Test.Tasty
    ( TestTree,
      defaultMain,
      withResource,
      sequentialTestGroup,
      testGroup,
      DependencyType(AllFinish) )
import Test.Tasty.HUnit ( testCase, assertFailure )
import Language.Purus.Make.Prim (syntheticPrim)
import Language.PureScript (ModuleName, runModuleName)
import Control.Concurrent.STM
    ( atomically, TVar, newTVarIO, readTVarIO, modifyTVar' )
import Data.Map (Map)
import Data.Map qualified as M
import Unsafe.Coerce ( unsafeCoerce )
import Control.Exception (SomeException, try, Exception (displayException))
import Language.Purus.Pretty.Common (prettyStr)

shouldPassTests :: IO ()
shouldPassTests = do
  generatedTests <- mkShouldPassTests "tests/purus/passing/CoreFn"
  let allTests = testGroup "Passing" [generatedTests,validatorTest,mintingPolicyTest]
  defaultMain allTests

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
    files <- concat <$> getTestFiles dir
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

{- Generated PIR non-evaluation tests (i.e. only checks that the *Purus* pipeline reaches the PIR stage,
   does not typecheck/compile/evaluate PIR).

   The TVar should be passed in empty. The path should be the full path to the project directory. The
   list of declarations is passed in primarily to make the types line up (can't write this without
   returning an IO [TestTree] if we don't pass it in afaict)
-}
-- TODO? withResource? We can do it w/ the TVar arg I think
mkPIRNoEval :: TVar (Map (ModuleName,Text) PIRTerm) -> FilePath -> [(ModuleName,Text)] ->  [TestTree]
mkPIRNoEval tv path ioInput  =  mkCase <$> ioInput
  where
    mkCase :: (ModuleName, Text) -> TestTree
    mkCase (mn'@(runModuleName -> mn),dn) = testCase testName $ do
        term <-  make path mn dn (Just syntheticPrim)
        atomically $ modifyTVar' tv (M.insert (mn',dn) term)
     where
       testName = T.unpack mn <> "." <> T.unpack dn

{- Generates automated evaluation tests.

   The first argument is an evaluation function. We want this as parameter so we can use (e.g.)
   variants that don't throw an error if the execution budget is exceeded.

   Second argument is the name of the test tree being generated.

   Third argument is a TVar which should be *full* (i.e. non-empty)

   Fourth argument is a list of declarations, which, as before, is mainly used to make the types line up.
-}
mkPIREvalMany :: (PIRTerm -> IO ())
              -> String
              -> TVar (Map (ModuleName,Text) PIRTerm)
              -> [(ModuleName,Text)]
              -> TestTree
mkPIREvalMany f nm tv decls = withResource (readTVarIO tv) (\_ -> pure ()) $ \tvIO ->
  testGroup nm $ mkPIREval1 tvIO <$> decls
 where
  mkPIREval1 :: IO (Map (ModuleName,Text) PIRTerm) -> (ModuleName,Text) -> TestTree
  mkPIREval1 dict declNm@(runModuleName -> mn,dn) = do
    let testName =  T.unpack mn <> "." <> T.unpack dn
    testCase testName $ do
      dict' <- dict
      case M.lookup declNm dict' of
        Nothing -> error $ "failure: no PIRTerm compiled at " <> show testName
        Just term -> void $ f term

{- Full pipeline tests for things we expect will succeed at the CoreFn -> PIR -> PLC -> UPLC -> Evaluation
   path. Reads from the modules in the `tests/purus/passing/CoreFn` directory.

   All functions tested here *should terminate*.
-}
mkShouldPassTests :: FilePath -> IO TestTree
mkShouldPassTests testDirPath = do
  allProjectDirectories <- listDirectory testDirPath
  testGroup "Generated (Passing)" <$> traverse (go . (testDirPath </>)) allProjectDirectories
 where
   go :: FilePath -> IO TestTree
   go path = try @SomeException initialize >>= \case
     Left err -> pure .  testCase ("PIR: " <> show path) $ assertFailure ("Failed during CoreFn compilation with reason: " <> displayException err)
     Right decls -> do
      declDict <- newTVarIO M.empty
      let pirNoEval = testGroup "No Eval" $ mkPIRNoEval declDict path decls
          pirEvalPlc   =  mkPIREvalMany (void . evaluateTerm) "Eval (PLC)" declDict decls
          pirEvalUplc = mkPIREvalMany convertToUPLCAndEvaluate "Eval (UPLC)" declDict decls
      pure $ sequentialTestGroup  ("PIR: " <> show path) AllFinish [pirNoEval,pirEvalPlc] -- ,pirEvalUplc]
    where
      initialize :: IO [(ModuleName,Text)]
      initialize = do
         void $ runPurusCoreFnDefault path
         allValueDeclarations path

{- Runs the PureScript -> CoreFn part of the compiler pipeline in default mode (i.e. not in Golden mode, i.e.
   this actually writes output files in the project directories)
-}
runPurusCoreFnDefault :: FilePath -> IO ()
runPurusCoreFnDefault path = runPurusCoreFn P.CoreFn path

{- Manual tests for scripts. These require us to apply arguments and parse an example script context. 
-}
-- TODO: Change these so they run through the UPLC evaluator using the new machinery
validatorTest :: TestTree
validatorTest = testCase "Basic Validator Test" $ do
  scriptContext <- parseData "sampleContext"
  --  Data -> Data -> Data -> Boolean
  validatorPIR <- make "tests/purus/passing/CoreFn/Validator" "Validator" "validate" (Just syntheticPrim)
  validatorPLC <- compileToUPLCTerm validatorPIR
  let validatorApplied = applyArgs validatorPLC [dummyData,dummyData,scriptContext]
  res <- evaluateUPLCTerm validatorApplied
  print res

mintingPolicyTest :: TestTree
mintingPolicyTest = testCase "Basic Minting Policy Test" $ do
  scriptContext <- parseData "sampleContext"
  policyPIR <- make "tests/purus/passing/CoreFn/MintingPolicy" "MintingPolicy" "oneAtATime" (Just syntheticPrim)
  policyPLC <- compileToUPLCTerm policyPIR
  let policyApplied = applyArgs policyPLC [dummyData,dummyData,scriptContext]
  res <- evaluateUPLCTerm policyApplied
  print res

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

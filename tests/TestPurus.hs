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
      removeDirectoryRecursive, createDirectoryIfMissing )
import System.FilePath.Glob qualified as Glob
import Data.Function (on)
import Data.List (sortBy, stripPrefix, groupBy)
import Language.Purus.Make ( allValueDeclarations, make )
import Language.Purus.Eval
import Language.Purus.Types ( PIRTerm, PLCTerm, UPLCTerm )
import Test.Tasty
    ( TestTree,
      defaultMain,
      withResource,
      sequentialTestGroup,
      testGroup,
      DependencyType(AllFinish) )
import Test.Tasty.HUnit ( testCase, assertFailure, assertBool, assertEqual )
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

shouldPassTests :: IO ()
shouldPassTests = do
  generatedTests <- mkShouldPassTests "tests/purus/passing/CoreFn"
  expectedResTests <- expectedResultTests
  nonTerminatingTests <- mkNonTerminatingTests "tests/purus/passing/NonTerminating"
  let golden = testCase "CoreFn Golden" $ runPurusCoreFnGolden "tests/purus/passing/golden/Misc"
      allTests = sequentialTestGroup "Passing" AllFinish
                  [ generatedTests
                  , nonTerminatingTests 
                  , validatorTest
                  , mintingPolicyTest
                  , expectedResTests
                  , golden ]
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
mkPIRNoEval :: TVar (Map (FilePath,ModuleName,Text) PIRTerm) -> FilePath -> [(FilePath,ModuleName,Text)] ->  [TestTree]
mkPIRNoEval tv path ioInput  =  mkCase <$> ioInput
  where
    mkCase :: (FilePath,ModuleName, Text) -> TestTree
    mkCase (fpath,mn'@(runModuleName -> mn),dn) = testCase testName $ do
        let outDirPath = path </> "output" </> T.unpack mn
            outFilePath = outDirPath </> "pir_no_eval.txt"
        createDirectoryIfMissing True outDirPath
        term <-  make path mn dn (Just syntheticPrim)
        withFile (outFilePath) AppendMode $ \h -> do
          let prettyTerm = docString $ prettyPirReadable term
              toLog = "\n---------- " <> T.unpack dn <> " -------------\n"
                      <> prettyTerm
                      <> "\n-----------------------------------------\n"
          hPutStr h toLog
        atomically $ modifyTVar' tv (M.insert (fpath,mn',dn) term)
     where
       testName = T.unpack mn <> "." <> T.unpack dn

{- Generates automated evaluation tests.

   The first argument is an evaluation function. We want this as parameter so we can use (e.g.)
   variants that don't throw an error if the execution budget is exceeded.

   Second argument is the name of the test tree being generated.

   Third argument is a TVar which should be *full* (i.e. non-empty)

   Fourth argument is a list of declarations, which, as before, is mainly used to make the types line up.
-}
mkPIREvalMany :: ((FilePath,ModuleName,Text) -> PIRTerm -> IO ())
              -> String
              -> TVar (Map (FilePath,ModuleName,Text) PIRTerm)
              -> [(FilePath,ModuleName,Text)]
              -> TestTree
mkPIREvalMany f nm tv decls = withResource (readTVarIO tv) (\_ -> pure ()) $ \tvIO ->
  sequentialTestGroup  nm AllFinish $ mkPIREval1 tvIO <$> decls
 where
  mkPIREval1 :: IO (Map (FilePath,ModuleName,Text) PIRTerm) -> (FilePath,ModuleName,Text) -> TestTree
  mkPIREval1 dict declNm@(fpath,runModuleName -> mn,dn) = do
    let testName =  T.unpack mn <> "." <> T.unpack dn
    testCase testName $ do
      dict' <- dict
      case M.lookup declNm dict' of
        Nothing -> error $ "failure: no PIRTerm compiled at " <> show testName
        Just term -> void $ f declNm term


{- Full pipeline tests for things we expect will succeed at the CoreFn -> PIR -> PLC -> UPLC -> Evaluation
   path. Reads from the modules in the `tests/purus/passing/CoreFn` directory.

   All functions tested here *should terminate*.
-}
mkShouldPassTests :: FilePath -> IO TestTree
mkShouldPassTests testDirPath = do
  allProjectDirectories <- listDirectory testDirPath
  sequentialTestGroup "Generated (Passing)" AllFinish <$> traverse (go . (testDirPath </>)) allProjectDirectories
 where
   go :: FilePath -> IO TestTree
   go path = try @SomeException initialize >>= \case
     Left err -> pure .  testCase ("PIR: " <> show path) $ assertFailure ("Failed during CoreFn compilation with reason: " <> displayException err)
     Right decls -> do
      declDict <- newTVarIO M.empty
      let pirNoEval = sequentialTestGroup "No Eval" AllFinish $ mkPIRNoEval declDict path decls
          --pirEvalPlc   =  mkPIREvalMany ( const (void . evaluateTerm)) "Eval (PLC)" declDict decls
          pirEvalUplc = mkPIREvalMany (const (void . convertToUPLCAndEvaluate)) "Eval (UPLC)" declDict decls
      pure $ sequentialTestGroup  ("PIR: " <> show path) AllFinish [pirNoEval,pirEvalUplc]
    where
      initialize :: IO [(FilePath,ModuleName,Text)]
      initialize = do
         void $ runPurusCoreFnDefault path
         allDecls <- allValueDeclarations path
         pure $ (\(b,c) -> (path,b,c)) <$> allDecls

{- Variant of the test generator for modules expected to contain non-terminating functions.
   Compiles to UPLC then evaluates, recording a failure ONLY in cases where evaluation fails with
   an error OTHER THAN 'Cek Budget Exceeded'

   Mainly used to test the inliner. Because it is essential to ensure that the inliner does not recurse
   infinitely (i.e. by inlining forever when inlining recursive functions),
   most of the useful inliner tests are necessarily non-terminating.
-}

mkNonTerminatingTests :: FilePath -> IO TestTree
mkNonTerminatingTests testDirPath = do
  allProjectDirectories <- listDirectory testDirPath
  sequentialTestGroup "Non-terminating" AllFinish <$> traverse ( go . (testDirPath </>)) allProjectDirectories
 where
   go :: FilePath -> IO TestTree
   go path = do
     void $ runPurusCoreFnDefault path
     allDecls <- allValueDeclarations path
     trees <- traverse mkTest allDecls
     pure $ sequentialTestGroup "Non-terminating" AllFinish trees
    where
      mkTest :: (ModuleName, Text) -> IO TestTree
      mkTest (runModuleName -> mn, dn) = do
        term <- make path mn dn (Just syntheticPrim)
        pure $ testCase "Non-terminating eval test" (convertAndEvaluateNonTerminating reasonablySizedBudget term)




{- Runs the PureScript -> CoreFn part of the compiler pipeline in default mode (i.e. not in Golden mode, i.e.
   this actually writes output files in the project directories)
-}
runPurusCoreFnDefault :: FilePath -> IO ()
runPurusCoreFnDefault path = runPurusCoreFn P.CoreFn path

runPurusCoreFnGolden :: FilePath -> IO ()
runPurusCoreFnGolden path = runPurusCoreFn P.CheckCoreFn path

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


pattern UPLCTrue :: UPLCTerm
pattern UPLCTrue = UPLC.Constr () 1 []

pattern UPLCFalse :: UPLCTerm
pattern UPLCFalse = UPLC.Constr () 0 []


-- i just think this is more readable

(#=) :: a -> b -> (a,b)
a #= b = (a,b)
-- something is wrong w/ the sequencing if I try to write this using the other machinery. Things that *should* be in the TVar are missing.
-- think I have to do it like this. will increase test run time a bit (but probably not too much if we keep all of the expected value tests in one module)
--
expectedResultTests :: IO TestTree
expectedResultTests = do
  void $ runPurusCoreFnDefault miscPath
  let expectedDecls = M.keys expectedResults
  (pirTerms :: Map (FilePath,ModuleName,Text) PIRTerm) <- M.unions <$> (traverse makePIR expectedDecls)
  pure $ sequentialTestGroup "Expected Results" AllFinish (makeExpectedTest pirTerms <$> expectedDecls)

 where
  makeExpectedTest :: Map (FilePath, ModuleName, Text) PIRTerm
                   -> (FilePath,ModuleName,Text)
                   -> TestTree
  makeExpectedTest pirTerms targetDecl@(fpath,runModuleName -> mn, dn) = testCase (fpath <> " " <> T.unpack (mn <> "." <> dn)) $ do
    let pirCompiled = pirTerms M.! targetDecl
        uplcExpected = expectedResults M.! targetDecl
        unRight (Right x) = x
    (uplcCompiled,_,_) <- unRight . evaluateTermU reasonablySizedBudget <$> compileToUPLCTerm pirCompiled
    assertEqual "expected / actual"  uplcExpected uplcCompiled

  makePIR :: (FilePath, ModuleName, Text) -> IO (Map (FilePath, ModuleName, Text) PIRTerm)
  makePIR declId@(fpath,runModuleName -> mn, declNm) = do
    term <- make fpath mn declNm (Just syntheticPrim)
    pure $ M.singleton declId term

  expectedResults :: Map (FilePath,ModuleName,Text) UPLCTerm
  expectedResults = M.fromList [
        "testOpt2Int" `is` (3 :: Integer)
      , "testIdentitee" `is` (101 :: Integer)
      , "testBindersCase" `is` (2 :: Integer)
      , "polyInObjMatch" `is` (5 :: Integer)
      , decl "litPatternApplied" #= UPLCFalse
      , "testPrelude1" `is` (1 :: Integer)
      , "testForce" `is` (2 :: Integer)
      , decl "testLazy" #= UPLCTrue
      , decl "testLazy'" #= UPLCTrue
      , "main" `is` (101 :: Integer)
      , "testNestedApplied" `is` (101 :: Integer)
      , "testRedundantLitApplied" `is` (1 :: Integer)
      , "testErrorApplied" `is` (2 :: Integer)
      , "testIdConst" `is` (5 :: Integer)
      , "testObjUpdate" `is` (4 :: Integer)
      , "testBrokenEven" `is` (1 :: Integer)
      , "testAccessorA" `is` (1 :: Integer)
      , "testAccessorB" `is` (2 :: Integer)
      , "testAccessorC" `is` (3 :: Integer)
      ]
  miscPath :: FilePath
  miscPath = "tests/purus/passing/CoreFn/Misc"

  miscLibMod :: ModuleName
  miscLibMod = ModuleName "Lib"

  decl :: Text -> (FilePath,ModuleName,Text)
  decl declNm = (miscPath,miscLibMod,declNm)

  is :: forall (a :: *)
          . HasTermLevel DefaultUni a
         => Text
         -> a
         -> ((FilePath,ModuleName,Text),UPLCTerm)
  is declNm val = (decl declNm,embedHaskell val)

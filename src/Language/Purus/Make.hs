{-# LANGUAGE TypeApplications #-}
module Language.Purus.Make where

import Prelude

import Control.Exception (throwIO)

import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Lazy qualified as LT

import Data.Map qualified as M
import Data.Set qualified as S

import Data.Function (on)

import Control.Exception
import Control.Monad (forM)
import Data.Foldable (foldrM, forM_)
import Data.List (delete, foldl', groupBy, sortBy, stripPrefix, find)

import System.FilePath (
  makeRelative,
  takeBaseName,
  takeDirectory,
  takeExtensions,
  (</>),
 )
import System.IO

import Language.PureScript.CoreFn.Ann (Ann)
import Language.PureScript.CoreFn.Expr (Bind (..), PurusType, cfnBindIdents)
import Language.PureScript.CoreFn.Module (Module (..))
import Language.PureScript.Names (
  Ident (Ident),
  ModuleName (..),
  runIdent,
  runModuleName,
 )

import Language.Purus.Eval
import Language.Purus.IR.Utils (IR_Decl, foldBinds, stripSkolemsFromExpr)
import Language.Purus.Pipeline.CompileToPIR (compileToPIR)
import Language.Purus.Pipeline.DesugarCore (desugarCoreModule)
import Language.Purus.Pipeline.DesugarObjects (
  desugarObjects,
  desugarObjectsInDatatypes,
 )
import Language.Purus.Pipeline.EliminateCases (eliminateCases)
import Language.Purus.Pipeline.GenerateDatatypes (
  generateDatatypes,
 )
import Language.Purus.Pipeline.Inline (inline)
import Language.Purus.Pipeline.Instantiate (applyPolyRowArgs, instantiateTypes)
import Language.Purus.Pipeline.Lift (lift)
import Language.Purus.Pipeline.Monad (
  CounterT (runCounterT),
  DesugarCore,
  globalScope,
  runCounter,
  runDesugarCore,
  runInline,
  runPlutusContext,
 )
import Language.Purus.Pretty.Common (prettyStr, docString)
import Language.Purus.Prim.Data (primDataPS, primData)
import Language.Purus.Types (PIRTerm, PLCTerm, initDatatypeDict)
import Language.Purus.Utils (
  decodeModuleIO,
  findDeclBodyWithIndex, findMain,
 )
import Language.Purus.Make.Prim (syntheticPrim)
import Language.Purus.Pipeline.EliminateCases.EliminateNested (eliminateNestedCases)

import Control.Monad.Except (MonadError (throwError), when)
import Control.Monad.State (evalStateT)

import Control.Lens (At (at))
import Control.Lens.Combinators (folded)
import Control.Lens.Operators ((^?))

import Algebra.Graph.AdjacencyMap (stars)
import Algebra.Graph.AdjacencyMap.Algorithm (topSort)

import System.FilePath.Glob qualified as Glob

import PlutusCore.Evaluation.Result (EvaluationResult(..))
import PlutusIR.Core.Instance.Pretty.Readable (prettyPirReadable)


import Language.Purus.Pipeline.Lift.Types
import System.Directory


import Debug.Trace (traceM)
import Language.Purus.IR (expTy)

-- TODO: Move the stuff that needs this to the tests
import Test.Tasty
import Test.Tasty.HUnit
-- import PlutusIR.Core.Instance.Pretty.Readable (prettyPirReadable)

{-  Compiles a main function to PIR, given its module name, dependencies, and a
    Prim module that will be compiled before anything else. (This is kind of a hack-ey shim
    to let us write e.g. serialization functions and provide them by default without a
    more sophisticated build system).
-}
compile ::
  Module (Bind Ann) PurusType PurusType Ann -> -- The Prim Module, or, if there isn't one, the first module to be compiles
  [Module (Bind Ann) PurusType PurusType Ann] -> -- The rest of the modules, sorted in dependency order (e.g. so Main comes last)
  ModuleName -> -- Name of the module with the main function (will probably be hardcoded to "Main")
  Ident -> -- Name of the main function (will probably be hardcoded to "main")
  Either String PIRTerm
compile primModule orderedModules mainModuleName mainFunctionName =
  evalStateT (runCounterT go) 0
  where
    desugarCoreModules ::
      Module (Bind Ann) PurusType PurusType Ann ->
      [Module (Bind Ann) PurusType PurusType Ann] ->
      DesugarCore (Module IR_Decl PurusType PurusType Ann)
    desugarCoreModules prim rest = do
      desugaredPrim <- desugarCoreModule primDataPS mempty prim
      loop desugaredPrim rest
      where
        loop here [] = pure here
        loop here (r : rs) = do
          let hereDatatypes = moduleDataTypes here
              hereDecls = moduleDecls here
          r' <- desugarCoreModule hereDatatypes hereDecls r
          loop r' rs
    go :: CounterT (Either String) PIRTerm
    go = do
      (summedModule, dsCxt) <- runDesugarCore $ desugarCoreModules primModule orderedModules
      let
        traceBracket lbl msg =  pure () -- traceM ("\n" <> lbl <> "\n\n" <> msg <> "\n\n")
        decls = moduleDecls summedModule
        declIdentsSet = foldBinds (\acc nm _ -> S.insert nm acc) S.empty decls
        couldn'tFindMain n =
          "Error: Could not find a main function with the name ("
            <> show (n :: Int)
            <> ") '"
            <> T.unpack (runIdent mainFunctionName)
            <> "' in module "
            <> T.unpack (runModuleName mainModuleName)
            <> "\nin declarations:\n"
            <> prettyStr (S.toList declIdentsSet)
      mainFunctionIx <- note (couldn'tFindMain 1) $ dsCxt ^? globalScope . at mainModuleName . folded . at mainFunctionName . folded
      -- traceM $ "Found main function Index: " <> show mainFunctionIx
      mainFunctionBody <- note (couldn'tFindMain 2) $ findDeclBodyWithIndex mainFunctionName mainFunctionIx decls
      --traceBracket ("Found main function body for " <> prettyStr mainFunctionName <> ":") (prettyStr mainFunctionBody)
      --traceBracket ("main function type  ") $ prettyStr (expTy id  mainFunctionBody)

      inlined <- runInline summedModule $ do
        liftResult <- lift (mainFunctionName, mainFunctionIx) mainFunctionBody
        traceBracket "lift result" (prettyStr liftResult) --"free variables in lift result" (prettyStr . M.toList . fmap S.toList $ oosInLiftResult liftResult)
        inlineResult <- inline liftResult
        --traceBracket "free variables in inline result" (prettyStr .  S.toList $ findOutOfScopeVars inlineResult)
        pure inlineResult
      traceBracket "Done inlining. Result:" $  prettyStr inlined
      let !instantiated = applyPolyRowArgs $ instantiateTypes inlined
      --traceBracket "Done instantiating types. Result:" $ prettyStr instantiated
      withoutObjects <- instantiateTypes <$> runCounter (desugarObjects instantiated)

      --traceBracket  "Desugared objects. Result:\n" $ prettyStr withoutObjects
      datatypes <- runCounter $ desugarObjectsInDatatypes (primDataPS <> moduleDataTypes summedModule)
      --traceM "Desugared datatypes"
      runPlutusContext initDatatypeDict $ do
        noNestedCases <- eliminateNestedCases datatypes withoutObjects
        when (noNestedCases /= withoutObjects) $ do
          traceBracket "Eliminated nested cases:\n" (prettyStr noNestedCases)
        generateDatatypes (moduleName summedModule) mainFunctionName noNestedCases datatypes
        -- traceM "Generated PIR datatypes"
        withoutCases <- eliminateCases datatypes noNestedCases 
        traceBracket "Eliminated Cases. Result:" $  prettyStr withoutCases
        pir <- compileToPIR datatypes withoutCases
        traceBracket  "Compiled to PIR. Result:\n"  $ docString (prettyPirReadable pir)
        -- traceBracket "PIR Raw:" $ LT.unpack (pShowNoColor pir)
        pure pir 

-- traceM . docString $ prettyPirReadable pirTerm

modulesInDependencyOrder :: [[FilePath]] -> IO [Module (Bind Ann) PurusType PurusType Ann]
modulesInDependencyOrder (concat -> paths) = do
  modules <- traverse decodeModuleIO paths
  let modMap = foldl' (\acc m@Module {..} -> M.insert moduleName m acc) M.empty modules
      depGraph = stars . M.toList $ M.mapWithKey (\n m -> delete n . fmap snd $ moduleImports m) modMap
  case reverse <$> topSort depGraph of
    Left cyc -> throwIO . userError $ "Error: Cycles detected in module graph: " <> show cyc
    Right ordered -> do
      -- we ignore Builtin and Prim deps (b/c we add those later ourselves)
      let canResolve :: ModuleName -> Bool
          canResolve (ModuleName mn) = mn /= "Prim" && mn /= "Builtin"
      foldrM
        ( \mn acc ->
            if not (canResolve mn)
              then pure acc
              else case modMap ^? at mn . folded of
                Nothing -> throwIO . userError $ "Error: Module '" <> T.unpack (runModuleName mn) <> "' is required for compilation but could not be found"
                Just mdl -> pure $ mdl : acc
        )
        []
        ordered

getFilesToCompile :: FilePath -> IO [[FilePath]]
getFilesToCompile targdir = do
  getFiles targdir <$> testGlob targdir
  where
    -- A glob for all purs and js files within a test directory
    testGlob :: FilePath -> IO [FilePath]
    testGlob = Glob.globDir1 (Glob.compile "**/*.cfn")
    -- Groups the test files so that a top-level file can have dependencies in a
    -- subdirectory of the same name. The inner tuple contains a list of the
    -- .purs files and the .js files for the test case.
    getFiles :: FilePath -> [FilePath] -> [[FilePath]]
    getFiles baseDir =
      map (filter ((== ".cfn") . takeExtensions) . map (baseDir </>))
        . groupBy ((==) `on` extractPrefix)
        . sortBy (compare `on` extractPrefix)
        . map (makeRelative baseDir)
    -- Extracts the filename part of a .purs file, or if the file is in a
    -- subdirectory, the first part of that directory path.
    extractPrefix :: FilePath -> FilePath
    extractPrefix fp =
      let dir = takeDirectory fp
          ext = reverse ".cfn"
       in if dir == "."
            then maybe fp reverse $ stripPrefix ext $ reverse fp
            else dir

make ::
  FilePath -> -- Path to the directory containing the modules to be compiled
  Text -> -- Name of the Main module (we'll default to Main in production but its nice to configure for testing)
  Text -> -- Name of the `main` function we're compiling
  Maybe (Module (Bind Ann) PurusType PurusType Ann) -> -- Optional prim module to compile first. Should be required but we don't *have* it yet so atm can't be
  IO PIRTerm
make path mainModule mainFunction primModule = do
  ordered <- getFilesToCompile path >>= modulesInDependencyOrder
  case (primModule, ordered) of
    (Just prim, toCompile) ->
      either (throwIO . userError) pure $ compile prim toCompile (ModuleName mainModule) (Ident mainFunction)
    -- if we don't have a prim module we treat the first module in compilation order as prim
    -- (need to do this for development/testing)
    (Nothing, m : ms) -> either (throwIO . userError) pure $ compile m ms (ModuleName mainModule) (Ident mainFunction)
    _ -> throwIO . userError $ "Error: No modules found for compilation"

-- for exploration/repl testing, this hardcodes `tests/purus/passing/Lib` as the target directory and
-- we only select the name of the main function
makeForTest :: Text -> IO PIRTerm
makeForTest main = make "tests/purus/passing/CoreFn/Misc" "Lib" main  (Just syntheticPrim)



{- Takes a path to a Purus project directory, the name of
   a target module, and a list of expression names and
   compiles them to PIR.

   Assumes that the directory already contains an "output" directory with serialized
   CoreFn files
-}
evalForTest_ :: Text -> IO ()
evalForTest_ main = (fst <$> evalForTest main) >>= \case
  EvaluationSuccess res -> pure () -- print $ prettyPirReadable res
  _ -> error $ "failed to evaluate " <> T.unpack main

evalForTest :: Text -> IO (EvaluationResult PLCTerm, [Text])
evalForTest main = do
  pir <- makeForTest main
  -- traceM . docString $ prettyPirReadable pir
  evaluateTerm pir

-- TODO put this somewhere else
note :: (MonadError String m) => String -> Maybe a -> m a
note msg = \case
  Nothing -> throwError msg
  Just x -> pure x

makeTestModule :: FilePath -> IO [(Text,PIRTerm)]
makeTestModule path = do
  mdl@Module{..} <- decodeModuleIO path
  let toTest :: [Ident]
      toTest = concatMap cfnBindIdents moduleDecls
      nm     = runModuleName moduleName
      goCompile  = pure . compile syntheticPrim [mdl] moduleName
  forM toTest $  \x ->  goCompile x >>= \case
    Left err -> error $ "Error while compiling " <> T.unpack (runIdent x) <> "\nReason: " <> err
    Right res -> pure (runIdent x,res)

evalTestModule :: FilePath -> IO ()
evalTestModule path = do
  made <- makeTestModule path
  putStrLn $ "Starting evaluation of: " <> prettyStr (fst <$> made)
  evaluated <- flip traverse made $ \(x,e) -> do
      res <- fst <$> evaluateTerm e
      pure (x,res)
  forM_ evaluated $ \(eNm,eRes) -> case eRes of
    EvaluationFailure -> putStrLn $ "\n\n> FAIL: Failed to evaluate " <> T.unpack eNm
    EvaluationSuccess res -> do
      putStrLn $ "\n\n> SUCCESS: Evaluated " <> T.unpack eNm
      print $ prettyPirReadable res

sanityCheck :: IO () 
sanityCheck = evalTestModule "tests/purus/passing/CoreFn/Misc/output/Lib/Lib.cfn"

-- takes a path to a project dir, returns (ModuleName,Decl Identifier)
allValueDeclarations :: FilePath -> IO [(ModuleName,Text)]
allValueDeclarations path = do
  allModules <- getFilesToCompile path >>= \files -> print files >> modulesInDependencyOrder files 
  let allDecls = map (\mdl -> (moduleName mdl, moduleDecls mdl)) allModules
      go mn  = \case
        NonRec _ ident _ -> [(mn,runIdent ident)]
        Rec xs -> map (\((_,ident),_) -> (mn,runIdent ident)) xs
  pure $ concatMap (\(mdl,dcls) -> concatMap (go mdl)  dcls)  allDecls

compileDirNoEval :: FilePath -> IO ()
compileDirNoEval path = do
  allDecls <- allValueDeclarations path
  let allModuleNames = runModuleName . fst <$> allDecls
  forM_ allModuleNames $ \mn -> do
    let outFilePath = path </> T.unpack mn <> "_pir_no_eval.txt"
    outFileExists <- doesFileExist outFilePath
    when outFileExists $
      removeFile outFilePath
  forM_ allDecls $ \(runModuleName -> mn, declNm) -> do
    let outFilePath = path </> T.unpack mn <> "_pir_no_eval.txt"
    withFile outFilePath AppendMode $ \h -> do
      result <- make path mn declNm (Just syntheticPrim)
      let nmStr = T.unpack declNm
          pirStr = docString $ prettyPirReadable result
          msg = "\n------ " <> nmStr <> " ------\n"
               <>  pirStr
               <> "\n------------\n"
      -- putStrLn msg
      hPutStr h msg
      hClose h 

-- Makes a TestTree. Should probably be in the test dir but don't feel like sorting out imports there
compileDirNoEvalTest :: FilePath -> IO TestTree
compileDirNoEvalTest path = do
  allDecls <- allValueDeclarations path
  let allModuleNames = runModuleName . fst <$> allDecls
  forM_ allModuleNames $ \mn -> do
    let outFilePath = path </> T.unpack mn <> "_pir_no_eval.txt"
    outFileExists <- doesFileExist outFilePath
    when outFileExists $
      removeFile outFilePath
  testCases <-  forM allDecls $ \(runModuleName -> mn, declNm) -> do
    let outFilePath = path </> T.unpack mn <> "_pir_no_eval.txt"
        testNm = path <> " - " <> T.unpack mn <> ":" <> T.unpack declNm
    pure $ testCase testNm $ do
     withFile outFilePath AppendMode $ \h -> do
       result <- make path mn declNm (Just syntheticPrim)
       let nmStr = T.unpack declNm
           pirStr = docString $ prettyPirReadable result
           msg = "\n------ " <> nmStr <> " ------\n"
                <>  pirStr
                <> "\n------------\n"
       -- putStrLn msg
       hPutStr h msg
       hClose h
  pure $ testGroup "PIR Compilation (No Eval)" testCases 

compileDirEvalTest :: FilePath -> IO TestTree
compileDirEvalTest path = do
  allDecls <- allValueDeclarations path
  let allModuleNames = runModuleName . fst <$> allDecls
  forM_ allModuleNames $ \mn -> do
    let outFilePath = path </> T.unpack mn <> "_pir_eval.txt"
    outFileExists <- doesFileExist outFilePath
    when outFileExists $
      removeFile outFilePath
  testCases <-  forM allDecls $ \(runModuleName -> mn, declNm) -> do
    let outFilePath = path </> T.unpack mn <> "_pir_eval.txt"
        testNm = path <> " - " <> T.unpack mn <> ":" <> T.unpack declNm
    pure $ testCase testNm $ do
     withFile outFilePath AppendMode $ \h -> do
       result <- snd <$> (evaluateTerm =<< make path mn declNm (Just syntheticPrim))
       let nmStr = T.unpack declNm
           pirStr = prettyStr result
           msg = "\n------ " <> nmStr <> " ------\n"
                <>  pirStr
                <> "\n------------\n"
       hPutStr h msg
       hClose h
  pure $ testGroup "PIR Evaluation" testCases

compileModuleNoEval :: FilePath -> IO ()
compileModuleNoEval path = do
  let baseName = takeBaseName path
      pathDir  = takeDirectory path
      outFilePath = pathDir </> baseName </> "_pir_decls_no_eval.txt"
  mdl <- decodeModuleIO path
  removeFile outFilePath
  withFile outFilePath  AppendMode $ \h ->  do
   made <- makeTestModule path
   forM_ made $ \(nm,pirterm) -> do
     let nmStr = T.unpack nm
         pirStr = docString $ prettyPirReadable pirterm
         origStr = unsafeFindDeclStr mdl nm
         msg = "\n\n------ " <> nmStr <> " ------\n\n"
               <> "Original coreFn declaration:\n" <> origStr
               <> "\n\nCompiled PIR expression:\n" <> pirStr
               <> "\n------------\n"
     putStrLn msg
     hPutStr h msg
   hClose h
 where
   findDecl :: Ident -> [Bind Ann] -> Maybe (Bind Ann)
   findDecl _ [] = Nothing
   findDecl i (bnd@(NonRec _ i' _):bs)
     | i == i' = Just bnd
     | otherwise = findDecl i bs
   findDecl i (Rec xs:bs) = case find (\x -> snd (fst x) == i) xs  of
                              Nothing -> findDecl i bs
                              Just ((a,_),body) -> Just $ NonRec a i body

   unsafeFindDeclStr :: Module (Bind Ann) PurusType PurusType Ann
                  -> Text
                  -> String
   unsafeFindDeclStr mdl nm = case findDecl (Ident nm) (moduleDecls mdl) of
           Nothing -> "error: couldn't find a declaration for " <> prettyStr nm
           Just bnd  -> prettyStr bnd

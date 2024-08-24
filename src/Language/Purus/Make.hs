module Language.Purus.Make where

import Prelude

import Control.Monad.State

import Language.PureScript.Names

import Language.PureScript.CoreFn.Ann
import Language.PureScript.CoreFn.Module
import Language.PureScript.CoreFn.Expr

import Language.Purus.Pipeline.DesugarCore
import Language.Purus.Pipeline.Lift
import Language.Purus.Pipeline.Inline
import Language.Purus.Pipeline.Instantiate
import Language.Purus.Pipeline.DesugarObjects
import Language.Purus.Pipeline.GenerateDatatypes
import Language.Purus.Pipeline.EliminateCases
import Language.Purus.Pipeline.CompileToPIR
import Language.Purus.Pipeline.Monad 

import Language.Purus.Types
import Language.Purus.IR.Utils
import Language.Purus.IR
import Language.Purus.Utils 

import Control.Monad.Except 

{-
decodeModuleIR :: FilePath -> IO (Module IR_Decl SourceType SourceType Ann, (Int, M.Map Ident Int))
decodeModuleIR path = do
  myMod <- decodeModuleIO path
  case desugarCoreModule myMod of
    Left err -> throwIO $ userError err
    Right myModIR -> pure myModIR

testDesugarObjects :: FilePath -> Text -> IO (Exp WithoutObjects Ty (Vars Ty))
testDesugarObjects path decl = do
  (myMod, ds) <- decodeModuleIR path
  Just myDecl <- pure . fmap snd $ findDeclBody decl myMod
  case runMonomorphize myMod [] (toExp myDecl) of
    Left (MonoError msg) -> throwIO $ userError $ "Couldn't monomorphize " <> T.unpack decl <> "\nReason:\n" <> msg
    Right body -> case evalStateT (tryConvertExpr body) ds of
      Left convertErr -> throwIO $ userError convertErr
      Right e -> do
        putStrLn (ppExp e)
        pure e

prepPIR ::
  FilePath ->
  Text ->
  IO (Exp WithoutObjects Ty (Vars Ty), Datatypes Kind Ty)
prepPIR path decl = do
  (myMod@Module {..}, ds) <- decodeModuleIR path

  desugaredExpr <- case snd <$> findDeclBody decl myMod of
    Nothing -> throwIO $ userError "findDeclBody"
    Just expr -> pure expr
  case runMonomorphize myMod [] (toExp desugaredExpr) of
    Left (MonoError msg) ->
      throwIO $
        userError $
          "Couldn't monomorphize "
            <> T.unpack (runModuleName moduleName <> ".main")
            <> "\nReason:\n"
            <> msg
    Right body -> do
      putStrLn (ppExp body)
      case evalStateT (tryConvertExpr body) ds of
        Left convertErr -> throwIO $ userError convertErr
        Right e -> do
          moduleDataTypes' <-
            either (throwIO . userError) pure $
              bitraverseDatatypes
                tryConvertKind
                tryConvertType
                moduleDataTypes
          putStrLn $ "tryConvertExpr result:\n" <> ppExp e <> "\n" <> replicate 20 '-'
          pure (e, moduleDataTypes')
-}


{- Arguments are:
     - A CoreFn `Prim` module containing the *primitive-but-not-builtin-functions*
       (e.g. serialization and deserialization functions). This always gets processed first

     - The parsed set of CoreFn modules needed for compilation, *sorted in dependency order)
       (e.g. so that the module containing the `main` function comes *last* and all depdencies
        are prior to the modules that they depend upon)

     - The name of the module containing the main function

     - The name of the main function
-}
compile :: Module IR_Decl PurusType PurusType Ann
        -> [Module IR_Decl PurusType PurusType Ann]
        -> ModuleName
        -> Ident
        -> Either String PIRTerm
compile primModule orderedModules mainModuleName mainFunctionName
  = evalStateT (runCounterT go) 0
 where
   desugarCoreModules = undefined

   go :: CounterT (Either String) PIRTerm
   go = do
     Module{..} <- runDesugarCore $ desugarCoreModules (primModule:orderedModules)
     undefined -- too tired will do over weekend

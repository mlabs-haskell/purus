{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- has to be here (more or less)
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use if" #-}
{-# HLINT ignore "Use <&>" #-}
{-# LANGUAGE NumericUnderscores #-}
module Language.PureScript.CoreFn.Convert.Monomorphize where

import Prelude

import Bound.Scope (fromScope)
import Bound.Var (Var (..))
import Control.Concurrent (threadDelay)
import Control.Lens (
  (&),
 )
import Control.Lens.Plated (transform, transformM)
import Control.Monad (join, void)
import Control.Monad.RWS (RWST (..))
import Data.Map qualified as M
import Data.Text (Text)
import Data.Text qualified as T
import GHC.IO (throwIO)
import Language.PureScript.CoreFn.Ann (Ann)
import Language.PureScript.CoreFn.Convert.Debug (
  doTrace,
  doTraceM,
 )
import Language.PureScript.CoreFn.Convert.DesugarCore (
  desugarCoreModule
 )
import Language.PureScript.CoreFn.Convert.IR (
  BVar (..),
  Exp (..),
  FVar (..),
  expTy,
  ppExp,
  unsafeAnalyzeApp,
 )
import Language.PureScript.CoreFn.Convert.Monomorphize.Utils (
  MonoError (..),
  MonoState (MonoState),
  decodeModuleIO,
  findDeclBody,
  unsafeApply,
  Monomorphizer,
 )
import Language.PureScript.CoreFn.Convert.IR.Utils 
import Language.PureScript.CoreFn.Convert.Inline.Lift
import Language.PureScript.CoreFn.Expr (PurusType)
import Language.PureScript.CoreFn.FromJSON ()
import Language.PureScript.CoreFn.Module (Module (..))
import Language.PureScript.CoreFn.Pretty.Common (prettyAsStr)
import Language.PureScript.CoreFn.TypeLike (
  TypeLike (..),
  getAllInstantiations,
 )
import Language.PureScript.Environment (kindType)
import Language.PureScript.Names
import Prettyprinter (Pretty, pretty)

import Language.PureScript.CoreFn.Convert.Inline.Inline (inline)
import Language.PureScript.CoreFn.Convert.Inline.Instantiate 
import Debug.Trace (traceM)


testLift' :: Text -> IO (LiftResult,MonoState,Module IR_Decl PurusType PurusType Ann)
testLift' decl = do
  myModCoreFn <- decodeModuleIO "tests/purus/passing/Misc/output/Lib/index.cfn"
  (myMod@Module {..}, _) <- either (throwIO . userError) pure $ desugarCoreModule myModCoreFn
  Just (myExpNm,myExp') <- pure $ findDeclBody decl myMod
  let myExp = join <$> fromScope myExp'
  case runRWST (lift myExpNm myExp) (moduleName, moduleDecls) (MonoState 100000) of
    Left (MonoError msg) -> throwIO $ userError $ "Couldn't lift " <> T.unpack decl <> "\nReason:\n" <> msg
    Right (res, st, _) -> do
      let !res' = res
      case res' of
        LiftResult a b -> do
          threadDelay 500000
          !prettyString <- pure $ prettyAsStr (LiftResult a b)
          print (length prettyString)
          traceM prettyString
          putStrLn "\n----------DONE----------\n"
          pure (res',st,myMod)


testLift :: Text -> IO ()
testLift = void . testLift'

testInline' :: Text -> IO MonoExp
testInline' nm = do
  (liftRes,st,modl) <- testLift' nm
  runMonoM' modl st (inline liftRes)

testInline :: Text -> IO ()
testInline = void . testInline'

testInstantiate :: Text -> IO ()
testInstantiate decl = do
  myModCoreFn <- decodeModuleIO "tests/purus/passing/Misc/output/Lib/index.cfn"
  (myMod@Module {..}, _) <- either (throwIO . userError) pure $ desugarCoreModule myModCoreFn
  Just (myExpNm,myExp') <- pure $ findDeclBody decl myMod
  res <- runMonoM'  myMod (MonoState 1_000_000) $ do
             liftResult <- lift myExpNm (toExp myExp')
             inlined    <- inline liftResult
             pure $ monoMorph inlined
  traceM $  "------TestInstantiate Result---------\n"
         <> show (expTy id res)
         <> "\n------------------------------------"
         
runMonoM :: Pretty a
         => Module IR_Decl PurusType PurusType Ann
         -> MonoState
         -> Monomorphizer a
         -> IO ()
runMonoM mod st act = void $  runMonoM' mod st act

runMonoM' :: Pretty a
         => Module IR_Decl PurusType PurusType Ann
         -> MonoState
         -> Monomorphizer a
         -> IO a
runMonoM' Module{..} st act = case runRWST act (moduleName,moduleDecls) st of
  Left (MonoError msg) -> throwIO . userError $ "Monomorphizer Error: " <> msg
  Right (res,_,_) -> do
    traceM $  "------PRETTY RUNMONO RESULT---------\n"
              <>  (prettyAsStr res)
              <>  "\n------------------------------------"
    pure res

reduceRowTypes ::
  Exp WithObjects PurusType (Vars PurusType) ->
  Exp WithObjects PurusType (Vars PurusType)
reduceRowTypes = transform $ \case
  TyInstE t (TyAbs (BVar _ bvTy (Ident bvNm)) innerE) | bvTy /= kindType ->
    let f =  replaceAllTypeVars [(bvNm,t)]
    in transformTypesInExp f innerE --  [(bvNm, t)] innerE
  other -> other

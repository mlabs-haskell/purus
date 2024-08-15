{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- has to be here (more or less)
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use if" #-}
{-# HLINT ignore "Use <&>" #-}
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
import Language.PureScript.CoreFn.Convert.Monomorphize.Inline (inlineEverything)
import Language.PureScript.CoreFn.Convert.Monomorphize.Monomorphize (monomorphize)
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
import Debug.Trace (traceM)

{- Function for quickly testing/debugging monomorphization -}

testMono :: FilePath -> Text -> IO ()
testMono path decl = do
  myModCoreFn <- decodeModuleIO path
  (myMod, _) <- either (throwIO . userError) pure $ desugarCoreModule myModCoreFn
  Just myDecl <- pure $ findDeclBody decl myMod
  case runMonomorphize myMod [] (join <$> fromScope myDecl) of
    Left (MonoError msg) -> throwIO $ userError $ "Couldn't monomorphize " <> T.unpack decl <> "\nReason:\n" <> msg
    Right body -> do
      putStrLn $ "MONO RESULT: \n" <> ppExp body

testLift' :: Text -> IO (LiftResult,MonoState,Module IR_Decl PurusType PurusType Ann)
testLift' decl = do
  myModCoreFn <- decodeModuleIO "tests/purus/passing/Misc/output/Lib/index.cfn"
  (myMod@Module {..}, _) <- either (throwIO . userError) pure $ desugarCoreModule myModCoreFn
  Just myExp' <- pure $ findDeclBody decl myMod
  let myExp = join <$> fromScope myExp'
  case runRWST (lift myExp) (moduleName, moduleDecls) (MonoState 100000) of
    Left (MonoError msg) -> throwIO $ userError $ "Couldn't lift " <> T.unpack decl <> "\nReason:\n" <> msg
    Right (res, st, _) -> do
      let !res' = res
      case res' of
        LiftResult a b | length a >= 1 -> do
          threadDelay 500000
          !prettyString <- pure $ prettyAsStr (LiftResult a b)
          print (length prettyString)
          traceM prettyString
          putStrLn "\n----------DONE----------\n"
          pure (res',st,myMod)
        _ -> error "boom"

testLift :: Text -> IO ()
testLift = void . testLift'

testInline :: Text -> IO ()
testInline nm = do
  (liftRes,st,modl) <- testLift' nm
  runMonoM modl st (inline liftRes)


runMonoM :: Pretty a
         => Module IR_Decl PurusType PurusType Ann
         -> MonoState
         -> Monomorphizer a
         -> IO ()
runMonoM Module{..} st act = case runRWST act (moduleName,moduleDecls) st of
  Left (MonoError msg) -> throwIO . userError $ "Monomorphizer Error: " <> msg
  Right (res,_,_) -> do
    traceM $  "------PRETTY RUNMONO RESULT---------\n"
              <>  (prettyAsStr res)
              <>  "\n------------------------------------"



{- This is the top-level entry point for monomorphization. Typically,
   you will search the module for a 'main' decl and use its
   body as the Exp argument.
-}
runMonomorphize ::
  Module IR_Decl PurusType PurusType Ann ->
  [Module IR_Decl PurusType PurusType Ann] ->
  -- | Modules in scope, for declarations
  Exp WithObjects PurusType (Var (BVar PurusType) (FVar PurusType)) ->
  Either MonoError (Exp WithObjects PurusType (Var (BVar PurusType) (FVar PurusType)))
runMonomorphize Module {..} _modulesInScope expr =
  let res =
        runRWST
          ( transformM monomorphize expr
              >>= inlineEverything
              >>= (pure . instantiateAllConstructors)
              >>= (pure . reduceRowTypes)
              >>= transformM monomorphize
          )
          (moduleName, moduleDecls)
          (MonoState 100000)
   in case res of -- FIXME: That 0 needs to be a MaxBv or we need to pass the real value from the core desugarer state
        Left err -> Left err
        Right (a, _, _) -> do
          doTraceM "runMonomorphize" ("OUTPUT: \n" <> ppExp a <> "\n" <> replicate 20 '-')
          pure a

reduceRowTypes ::
  Exp WithObjects PurusType (Vars PurusType) ->
  Exp WithObjects PurusType (Vars PurusType)
reduceRowTypes = transform $ \case
  TyInstE t (TyAbs (BVar _ bvTy (Ident bvNm)) innerE) | bvTy /= kindType ->
    let f =  replaceAllTypeVars [(bvNm,t)]
    in transformTypesInExp f innerE --  [(bvNm, t)] innerE
  other -> other

{- Entry point for inlining monomorphization.

   Broadly, we deduce the monomorphic type for a polymorphic function
   by looking at the arguments the function is applied to. Without
   arguments, we cannot deduce the monomorphic type at all, and so
   this function is `pure` if the provided expression is anything other than
   than an `AppE`
-}
instantiateAllConstructors ::
  forall x t.
  (TypeLike t, Pretty t, Pretty (KindOf t)) =>
  Exp x t (Var (BVar t) (FVar t)) ->
  Exp x t (Var (BVar t) (FVar t))
instantiateAllConstructors = transform $ \case
  AppE e1 e2 -> case unsafeAnalyzeApp (AppE e1 e2) of
    (f, args)
      | isConstructorE f || isBuiltinE f ->
          let f' = instantiateConstructorWithArgs id f args
           in unsafeApply id f' args
    _ -> AppE e1 e2
  other -> other

-- should also work with builtins
instantiateConstructorWithArgs ::
  forall x t a.
  (TypeLike t, Pretty t, Pretty (KindOf t)) =>
  (a -> Var (BVar t) (FVar t)) ->
  Exp x t a -> -- a non-monomorphized constructor
  [Exp x t a] -> -- the arguments to which it is applied
  Exp x t a
instantiateConstructorWithArgs _ fun [] = fun
instantiateConstructorWithArgs f fun args = doTrace "instantiateConstructorWithArgs" msg result
  where
    quantifiedTyVars =
      fmap (\(_, b, c) -> (b, c))
        . fst
        . stripQuantifiers
        $ expTy f fun

    msg =
      "INPUT FUN:\n"
        <> prettyAsStr (f <$> fun)
        <> "\n\nINPUT FUN TY:\n"
        <> prettyAsStr (expTy f fun)
        <> "\n\nINPUT ARGS:\n"
        <> prettyAsStr (fmap f <$> args)
        <> "\n\nINPUT ARG TYPES:\n"
        <> prettyAsStr (expTy f <$> args)
        <> "\n\nRESULT FUN:\n"
        <> prettyAsStr (f <$> result)
        <> "\n\nRESULT FUN TY:\n"
        <> prettyAsStr (expTy f result)
        <> "\n\nQUANTIFIED TYVARS:\n"
        <> prettyAsStr quantifiedTyVars
    result = runInst fun (reverse quantifiedTyVars)
    instantiationDict = M.fromList $ getAllInstantiations (expTy f fun) (expTy f <$> args)
    runInst e [] = e
    runInst e ((t, _) : usedRest) = case M.lookup t instantiationDict of
      Just new -> runInst (TyInstE new e) usedRest
      Nothing -> runInst e usedRest -- maybe this should be an error?

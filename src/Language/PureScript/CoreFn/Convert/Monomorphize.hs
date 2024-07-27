{-# OPTIONS_GHC -Wno-orphans #-} -- has to be here (more or less)
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use if" #-}
{-# HLINT ignore "Use <&>" #-}
module Language.PureScript.CoreFn.Convert.Monomorphize  where

import Prelude

import Language.PureScript.CoreFn.Ann (Ann)
import Language.PureScript.CoreFn.Expr (PurusType)
import Language.PureScript.CoreFn.Module ( Module(..) )
import Language.PureScript.Environment (kindType)
import Language.PureScript.CoreFn.Convert.IR
    ( Exp(..),
      FVar(..),
      ppExp,
      unsafeAnalyzeApp,
      BVar(..),
      expTy )
import Language.PureScript.CoreFn.FromJSON ()
import Data.Text qualified as T
import Data.Map qualified as M
import Control.Lens
    ( (&) )
import Control.Monad (join)
import Control.Monad.RWS (RWST(..))
import Language.PureScript.CoreFn.Convert.DesugarCore
    ( desugarCoreModule, WithObjects, IR_Decl, type Vars )
import Bound.Var (Var(..))
import Bound.Scope (fromScope)
import Language.PureScript.CoreFn.TypeLike
    ( TypeLike(..), getAllInstantiations )
import Language.PureScript.CoreFn.Convert.Monomorphize.Utils
    ( decodeModuleIO,
      findDeclBody,
      isBuiltinE,
      isConstructorE, 
      unsafeApply,
      updateTypes,
      MonoError(..),
      MonoState(MonoState) )
import Data.Text (Text)
import GHC.IO (throwIO)
import Control.Lens.Plated ( transform, transformM )
import Prettyprinter (Pretty)
import Language.PureScript.CoreFn.Pretty.Common (prettyAsStr)
import Language.PureScript.CoreFn.Convert.Debug
    ( doTrace, doTraceM )
import Language.PureScript.CoreFn.Convert.Monomorphize.Inline (inlineEverything)
import Language.PureScript.CoreFn.Convert.Monomorphize.Monomorphize (monomorphize)
import Language.PureScript.Names 

{- Function for quickly testing/debugging monomorphization -}

testMono :: FilePath -> Text -> IO ()
testMono path decl = do
  myModCoreFn <- decodeModuleIO path
  (myMod,_) <- either (throwIO . userError) pure $ desugarCoreModule myModCoreFn
  Just myDecl <- pure $ findDeclBody decl myMod
  case runMonomorphize myMod [] (join <$> fromScope myDecl) of
    Left (MonoError msg ) -> throwIO $ userError $ "Couldn't monomorphize " <> T.unpack decl <> "\nReason:\n" <> msg
    Right body -> do
      putStrLn $ "MONO RESULT: \n" <>  ppExp body

{- This is the top-level entry point for monomorphization. Typically,
   you will search the module for a 'main' decl and use its
   body as the Exp argument.
-}
runMonomorphize ::
  Module IR_Decl PurusType PurusType Ann ->
  [Module IR_Decl PurusType PurusType Ann] -> -- | Modules in scope, for declarations
  Exp WithObjects PurusType (Var (BVar PurusType) (FVar PurusType)) ->
  Either MonoError (Exp WithObjects PurusType (Var (BVar PurusType) (FVar PurusType)))
runMonomorphize Module{..} _modulesInScope expr =
  let res = runRWST (transformM monomorphize expr
                     >>= inlineEverything
                     >>= (pure . instantiateAllConstructors)
                     >>= (pure . reduceRowTypes)
                     >>= transformM monomorphize) (moduleName, moduleDecls) (MonoState 100000)
  in case res of -- FIXME: That 0 needs to be a MaxBv or we need to pass the real value from the core desugarer state
    Left err -> Left err
    Right (a,_,_) -> do
      doTraceM "runMonomorphize" ("OUTPUT: \n" <> ppExp a <> "\n" <> replicate 20 '-')
      pure a


reduceRowTypes :: Exp WithObjects PurusType (Vars PurusType)
               -> Exp WithObjects PurusType (Vars PurusType)
reduceRowTypes = transform $ \case
  TyInstE t (TyAbs (BVar _ bvTy (Ident bvNm) ) innerE) | bvTy /= kindType -> updateTypes [(bvNm,t)] innerE
  other -> other 

{- Entry point for inlining monomorphization.

   Broadly, we deduce the monomorphic type for a polymorphic function
   by looking at the arguments the function is applied to. Without
   arguments, we cannot deduce the monomorphic type at all, and so
   this function is `pure` if the provided expression is anything other than
   than an `AppE`
-}
instantiateAllConstructors :: forall x t
                            . (TypeLike t, Pretty t, Pretty (KindOf t))
                           => Exp x t (Var (BVar t) (FVar t))
                           -> Exp x t (Var (BVar t) (FVar t))
instantiateAllConstructors = transform $ \case
  AppE e1 e2 -> case unsafeAnalyzeApp (AppE e1 e2) of
    (f,args) | isConstructorE f || isBuiltinE f ->
               let f' = instantiateConstructorWithArgs id f args
               in unsafeApply id f' args
    _ -> AppE e1 e2
  other -> other

-- should also work with builtins 
instantiateConstructorWithArgs :: forall x t a
                                . (TypeLike t, Pretty t, Pretty (KindOf t))
                               => (a -> Var (BVar t) (FVar t))
                               -> Exp x t a -- a non-monomorphized constructor
                               -> [Exp x t a] -- the arguments to which it is applied
                               -> Exp x t a
instantiateConstructorWithArgs _ fun [] = fun
instantiateConstructorWithArgs f fun args = doTrace "instantiateConstructorWithArgs" msg result
  where
    quantifiedTyVars = fmap (\(_,b,c) -> (b,c))
                       . fst
                       . stripQuantifiers
                       $ expTy f fun

    msg = "INPUT FUN:\n" <> prettyAsStr (f <$> fun)
          <> "\n\nINPUT FUN TY:\n" <> prettyAsStr (expTy f fun)
          <> "\n\nINPUT ARGS:\n" <> prettyAsStr (fmap f <$> args)
          <> "\n\nINPUT ARG TYPES:\n" <> prettyAsStr (expTy f <$> args)
          <> "\n\nRESULT FUN:\n" <> prettyAsStr (f <$> result)
          <> "\n\nRESULT FUN TY:\n" <> prettyAsStr (expTy f result)
          <> "\n\nQUANTIFIED TYVARS:\n" <> prettyAsStr quantifiedTyVars
    result = runInst fun (reverse quantifiedTyVars)
    instantiationDict = M.fromList $ getAllInstantiations (expTy f fun) (expTy f <$> args)
    runInst e [] = e
    runInst e ((t,_):usedRest) = case M.lookup t instantiationDict of
      Just new -> runInst (TyInstE new e) usedRest
      Nothing  -> runInst e usedRest -- maybe this should be an error?

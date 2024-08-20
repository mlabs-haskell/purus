{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use if" #-}
{-# HLINT ignore "Use <&>" #-}
module Language.PureScript.CoreFn.Convert.Monomorphize.Monomorphize where

import Prelude

import Bound.Var (Var (..))
import Control.Lens.Combinators (transformM)
import Data.Bifunctor
import Data.Foldable (Foldable (..))
import Data.Map qualified as M
import Data.Text (Text)
import Data.Traversable (for)
import Language.PureScript.CoreFn.Convert.Debug
import Language.PureScript.CoreFn.Convert.IR (
  BVar (..),
  Exp (..),
  FVar (..),
  expTy,
  ppExp,
  unsafeAnalyzeApp,
 )
import Language.PureScript.CoreFn.Convert.Monomorphize.Utils
import Language.PureScript.CoreFn.Expr (PurusType)
import Language.PureScript.CoreFn.FromJSON ()
import Language.PureScript.CoreFn.Pretty.Common (prettyAsStr)
import Language.PureScript.CoreFn.TypeLike (
  TypeLike (..),
 )
import Language.PureScript.Names (Ident (..), runIdent)
import Language.PureScript.CoreFn.Convert.IR.Utils
{- TODO: Rewrite this to work agnostically over Exp x t (Vars t)
         so we can avoid the ad-hoc stupidity in Datatypes

-}

{- Entry point for inlining monomorphization.

   Broadly, we deduce the monomorphic type for a polymorphic function
   by looking at the arguments the function is applied to. Without
   arguments, we cannot deduce the monomorphic type at all, and so
   this function is `pure` if the provided expression is anything other than
   than an `AppE`
-}
monomorphize ::
  Exp WithObjects PurusType (Var (BVar PurusType) (FVar PurusType)) ->
  Monomorphizer (Exp WithObjects PurusType (Var (BVar PurusType) (FVar PurusType)))
monomorphize = transformM $ \case
  appE@(AppE {}) -> do
    (f, args) <- traverse (traverse monomorphize) $ unsafeAnalyzeApp appE

    let qvars' =
          fmap (\(_, a, b) -> (a, b))
            . fst
            . stripQuantifiers
            $ expTy id f
    vars <- for qvars' $ \(a, b) -> (,Ident a,b) <$> freshUnique
    doTraceM "monomorphize" $
      prettify
        [ "f:f:\n" <> prettyAsStr f
        , "vars:\n" <> prettyAsStr vars
        , "args:\n" <> prettyAsStr args
        ]
    case vars of
      [] -> pure appE
      _ ->
        if isBuiltinE f || isConstructorE f
          then do
            let result = unsafeApply id f args
            doTraceM "monomorphize" ("INPUT:\n" <> prettyAsStr appE <> "\n\nINPUT TY:\n" <> prettyAsStr (expTy id appE) <> "\n\nOUTPUT:\n" <> prettyAsStr result <> "\n\nOUTPUT TY:\n" <> prettyAsStr (expTy id result))
            pure result
          else do
            let result = monomorphizeWithBoundTyVars vars f args
            doTraceM "monomorphize" ("INPUT:\n" <> prettyAsStr appE <> "\n\nINPUT TY:\n" <> prettyAsStr (expTy id appE) <> "\n\nOUTPUT:\n" <> prettyAsStr result <> "\n\nOUTPUT TY:\n" <> prettyAsStr (expTy id result))
            pure result
  other -> pure other

monomorphizeWithBoundTyVars ::
  [(Int, Ident, PurusType)] ->
  Exp WithObjects PurusType (Vars PurusType) ->
  [Exp WithObjects PurusType (Vars PurusType)] ->
  Exp WithObjects PurusType (Vars PurusType)
monomorphizeWithBoundTyVars [] f args = doTrace "monomorphizeWithBoundTyVars" msg $ unsafeApply id f args
  where
    msg = prettify ["UNCHANGED", "Fun:\n" <> prettyAsStr f, "Args:\n" <> prettyAsStr args]
monomorphizeWithBoundTyVars bvars _f args = doTrace "monomorphizeWithBoundTyVars" msg result
  where
    f = snd $ stripTypeAbstractions _f
    msg=
      prettify
        [ "CHANGED"
        , "Fun (raw):\n" <> prettyAsStr _f
        , "Fun (TyAbs stripped):\n" <> prettyAsStr f
        , "Args:\n" <> prettyAsStr args
        , "Bound Idents:\n" <> prettyAsStr idents
        , "Fun Ty (stripped):\n" <> prettyAsStr fT
        , "Arg Types:\n" <> prettyAsStr argTs
        , "Instantiations:\n" <> prettyAsStr (M.toList instantiations)
        , "TyInst args:\n" <> prettyAsStr toInst
        , "Monomorphized Function:\n" <> prettyAsStr monoF
        , "Monomorphized Fun ty:\n" <> prettyAsStr (expTy id monoF)
        , "Result:\n" <> prettyAsStr result
        , "Result ty:\n" <> prettyAsStr (expTy id result)
        ]

    result = unsafeApply id monoF args

    (toInst, monoF') = rebuildMonomorphizedFunction instantiations f bvars

    monoF = foldl' (flip TyInstE) monoF' toInst

    idents = (\(_, b, _) -> runIdent b) <$> bvars

    fT = snd . stripQuantifiers . expTy id $ f
    fTs = splitFunTyParts fT
    argTs = expTy id <$> args

    instantiations = getInstantiations idents fTs argTs

    rebuildMonomorphizedFunction ::
      M.Map Text PurusType ->
      Exp WithObjects PurusType (Vars PurusType) ->
      [(Int, Ident, PurusType)] ->
      ([PurusType], Exp WithObjects PurusType (Vars PurusType))
    rebuildMonomorphizedFunction varMap fE [] = ([], fE)
    rebuildMonomorphizedFunction varMap fE ((indx, nm, k) : rest) = case M.lookup (runIdent nm) varMap of
      Nothing -> rebuildMonomorphizedFunction varMap fE rest
      Just t -> first (t :) $ rebuildMonomorphizedFunction varMap fE rest

getInstantiations ::
  [Text] ->
  [PurusType] ->
  [PurusType] ->
  M.Map Text PurusType
getInstantiations [] _ _ = M.empty
getInstantiations _ [] _ = M.empty
getInstantiations _ _ [] = M.empty
getInstantiations (var : vars) fs@(fE : fEs) as@(aE : aEs) = case instantiates var aE fE of
  Nothing ->
    getInstantiations [var] fEs aEs
      <> getInstantiations vars fs as
  Just t -> M.insert var t $ getInstantiations vars fs as

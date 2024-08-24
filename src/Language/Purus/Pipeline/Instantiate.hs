{- After inlining, we have an AST which contains several different kinds of expression which
   will require - yet lack at this stage - type instantiations (TyInstE in our AST) in order to
   typecheck in PIR (and also to simplify some of our own subsequent compiler passes).
-}

module Language.Purus.Pipeline.Instantiate where


import Prelude
import Data.Map qualified as M
import Language.Purus.IR (
  Exp (..), expTy,
 )
import Language.Purus.IR.Utils
import Language.PureScript.CoreFn.FromJSON ()

import Data.Text (Text)
import Language.PureScript.CoreFn.TypeLike (instantiates, TypeLike (..))
import Data.Foldable (foldl')
import Data.Map (Map)

import Language.PureScript.CoreFn.Expr (PurusType)

import Control.Lens (view,_2)

import Language.Purus.Pipeline.Lift.Types
import Language.Purus.Debug
import Language.Purus.Pretty.Common (prettyStr)
import Language.Purus.IR (analyzeApp)


instantiateTypes :: MonoExp -> MonoExp
instantiateTypes = \case
  V v -> V v
  LitE t lit -> LitE t $ instantiateTypes <$> lit
  LamE bv scope -> LamE bv $ viaExp instantiateTypes scope
  AppE f a ->
    let a' = instantiateTypes a
        f' = instantiateTypes f
    in instantiateApp $ AppE f' a'
  CaseE t scrut alts -> CaseE t (instantiateTypes scrut) (mapAlt (viaExp instantiateTypes) <$> alts)
  LetE decls body -> LetE (mapBind (const (viaExp instantiateTypes)) <$> decls) (viaExp instantiateTypes body)
  AccessorE x t lbl obj -> AccessorE x t lbl (instantiateTypes obj)
  ObjectUpdateE x t e copy fs -> ObjectUpdateE x t (instantiateTypes e) copy (fmap instantiateTypes <$> fs)
  -- I'm not sure what to do for TyInstE or TyAbs TODO: Ask Koz
  TyAbs t inner -> TyAbs t (instantiateTypes inner)
  other -> other

instantiateApp :: MonoExp -> MonoExp
instantiateApp e = case analyzeApp e of
  Nothing -> e
  Just (f,args)  ->
    let (fTyVars,fInner) = stripQuantifiers (expTy id f)
        fTypes           = splitFunTyParts fInner
        argTypes         = expTy id <$> args
        quantifiedTyVars = view _2 <$> fTyVars
        instantiations   = getInstantiations quantifiedTyVars fTypes argTypes
        f'               = go instantiations quantifiedTyVars f
        msg              = prettify [ "Function:\n" <> prettyStr f
                                    , "Arguments:\n" <> prettyStr args
                                    , "Split Fun Types:\n" <> prettyStr fTypes
                                    , "Split Arg Types:\n" <> prettyStr argTypes
                                    , "Quantified TyVars:\n" <> prettyStr quantifiedTyVars
                                    , "Instantiations:\n" <> prettyStr (M.toList instantiations)
                                    , "New Function:\n" <> prettyStr f'
                                    , "New Function Type:\n" <> prettyStr (expTy id f')
                                    ]
    in doTrace "instantiateTypes" msg $ foldl' AppE f' args
 where
   go :: Map Text PurusType -> [Text] -> MonoExp -> MonoExp
   go _ [] ex = ex
   go dict (v:vs) ex = case M.lookup v dict of
     Nothing -> ex
     Just t -> go dict vs (TyInstE t ex)



{- Takes a list of variables, the split function types, and split arguments types,
   and returns a Map of type variable substitutions.
-}
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

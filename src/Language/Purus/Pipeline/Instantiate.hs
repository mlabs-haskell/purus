{- After inlining, we have an AST which contains several different kinds of expression which
   will require - yet lack at this stage - type instantiations (TyInstE in our AST) in order to
   typecheck in PIR (and also to simplify some of our own subsequent compiler passes).
-}

module Language.Purus.Pipeline.Instantiate (instantiateTypes, applyPolyRowArgs) where

import Prelude

import Data.Map (Map)
import Data.Map qualified as M

import Data.Foldable (foldl')

import Data.Text (Text)

import Language.PureScript.Constants.Prim qualified as C
import Language.PureScript.CoreFn.Expr (PurusType)
import Language.PureScript.CoreFn.TypeLike (TypeLike (..), instantiates)
import Language.PureScript.Names (Ident (Ident))
import Language.PureScript.Types (Type (..))

import Language.Purus.IR (BVar (..), Exp (..), analyzeApp, expTy)
import Language.Purus.IR.Utils (
  Vars,
  WithObjects,
  mapAlt,
  mapBind,
  transformTypesInExp,
  viaExp,
 )

import Control.Lens (transform, view, _2)
import Prettyprinter (Pretty)

{- After inlining and instantiating, we're left abstracted type variables and instantiated types which
   may be of kind `Row Type`. That's bad! We need to "apply" the instantiations to the abstractions so
   that we have concrete rows (or as concrete as they can possibly be at any rate) before we
   do object desugaring.
-}
applyPolyRowArgs ::
  Exp WithObjects PurusType (Vars PurusType) ->
  Exp WithObjects PurusType (Vars PurusType)
applyPolyRowArgs = transform $ \case
  instE@(TyInstE t (TyAbs (BVar _ kvTy (Ident kvNm)) innerE)) -> case kvTy of
    TypeApp _ (TypeConstructor _ C.Row) _ -> transformTypesInExp (replaceAllTypeVars [(kvNm, t)]) innerE
    _ -> instE
  other -> other

{- Instantiates every type abstraction wherever it is possible to deduce the instantiation.
-}
instantiateTypes :: forall x (t :: *). (TypeLike t, Pretty t, Pretty (KindOf t)) => Exp x t (Vars t) -> Exp x t (Vars t)
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
  TyAbs t inner -> TyAbs t (instantiateTypes inner)
  TyInstE t inner -> TyInstE t (instantiateTypes inner)

instantiateApp :: forall x (t :: *). (Pretty t, TypeLike t) => Exp x t (Vars t) -> Exp x t (Vars t)
instantiateApp e = case analyzeApp e of
  Nothing -> e
  Just (f, args) ->
    let (fTyVars, fInner) = stripQuantifiers (expTy id f)
        fTypes = splitFunTyParts fInner
        argTypes = expTy id <$> args
        quantifiedTyVars = view _2 <$> fTyVars
        instantiations = getInstantiations quantifiedTyVars fTypes argTypes
        f' = go instantiations quantifiedTyVars f
     in foldl' AppE f' args
  where
    go :: Map Text t -> [Text] -> Exp x t (Vars t) -> Exp x t (Vars t)
    go _ [] ex = ex
    go dict (v : vs) ex = case M.lookup v dict of
      Nothing -> ex
      Just t -> go dict vs (TyInstE t ex)

{- Takes a list of variables, the split function types, and split arguments types,
   and returns a Map of type variable substitutions.
-}
getInstantiations ::
  (TypeLike t) =>
  [Text] ->
  [t] ->
  [t] ->
  M.Map Text t
getInstantiations [] _ _ = M.empty
getInstantiations _ [] _ = M.empty
getInstantiations _ _ [] = M.empty
getInstantiations (var : vars) fs@(fE : fEs) as@(aE : aEs) = case instantiates var aE fE of
  Nothing ->
    getInstantiations [var] fEs aEs
      <> getInstantiations vars fs as
  Just t -> M.insert var t $ getInstantiations vars fs as

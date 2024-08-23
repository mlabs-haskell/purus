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
import Language.Purus.Pipeline.Lift
import Language.PureScript.CoreFn.Expr (PurusType)
import Language.Purus.Debug
import Language.Purus.Pretty.Common (prettyAsStr)
import Control.Lens (view,_2)
import Language.Purus.IR (analyzeApp)


instantiateTypes :: MonoExp -> MonoExp
instantiateTypes = \case
  V v -> V v
  LitE t lit -> LitE t $ monoMorph <$> lit
  LamE bv scope -> LamE bv $ viaExp monoMorph scope
  appE@(AppE f a) ->
    let a' = monoMorph a
        f' = monoMorph f
    in instantiateApp $ AppE f' a'
  CaseE t scrut alts -> CaseE t (monoMorph scrut) (mapAlt (viaExp monoMorph) <$> alts)
  LetE _REMOVE decls body -> LetE _REMOVE (mapBind (const (viaExp monoMorph)) <$> decls) (viaExp monoMorph body)
  AccessorE x t lbl obj -> AccessorE x t lbl (monoMorph obj)
  ObjectUpdateE x t e copy fs -> ObjectUpdateE x t (monoMorph e) copy (fmap monoMorph <$> fs)
  -- I'm not sure what to do for TyInstE or TyAbs TODO: Ask Koz
  TyAbs t inner -> TyAbs t (monoMorph inner)
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
        msg              = prettify [ "Function:\n" <> prettyAsStr f
                                    , "Arguments:\n" <> prettyAsStr args
                                    , "Split Fun Types:\n" <> prettyAsStr fTypes
                                    , "Split Arg Types:\n" <> prettyAsStr argTypes
                                    , "Quantified TyVars:\n" <> prettyAsStr quantifiedTyVars
                                    , "Instantiations:\n" <> prettyAsStr (M.toList instantiations)
                                    , "New Function:\n" <> prettyAsStr f'
                                    , "New Function Type:\n" <> prettyAsStr (expTy id f')
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


{- NOTE: Nullnary constructors

   The instantiation functions above *should* work (or can at least be fixed in theory to work) on every
   polymorphic *function* or *function-like-construct* (i.e. non-nullary data constructors).

   It will not work for polymorphic nullary constructors, such as (from Haskell) Proxy or Nothing, which
   require type instantiations to typecheck in PIR, but do not accept any value-level arguments from which the
   types might be deduced.



-}

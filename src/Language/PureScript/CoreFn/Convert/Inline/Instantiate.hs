{- After inlining, we have an AST which contains several different kinds of expression which
   will require - yet lack at this stage - type instantiations (TyInstE in our AST) in order to
   typecheck in PIR (and also to simplify some of our own subsequent compiler passes).
-}

module Language.PureScript.CoreFn.Convert.Inline.Instantiate where


import Prelude
import Bound.Scope (fromScope, abstract)
import Data.Map qualified as M
import Language.PureScript.CoreFn.Convert.IR (
  BindE (..),
  Exp (..), expTy, unsafeAnalyzeApp, BVar (..), expTy',
 )
import Language.PureScript.CoreFn.Convert.Monomorphize.Utils
import Language.PureScript.CoreFn.Convert.IR.Utils
import Language.PureScript.CoreFn.FromJSON ()

import Data.Text (Text)
import Language.PureScript.CoreFn.TypeLike (instantiates, TypeLike (..))
import Data.Foldable (find, maximumBy, foldl')
import Data.Map (Map)
import Control.Monad
import Data.Set (Set)
import Data.Set qualified as S
import Language.PureScript.CoreFn.Convert.Inline.Lift
import Algebra.Graph.AdjacencyMap
    ( gmap, stars, vertexList, AdjacencyMap(..), vertexSet, edgeList, edges )
import Algebra.Graph.AdjacencyMap.Algorithm (scc, topSort, Cycle)
import Algebra.Graph.NonEmpty.AdjacencyMap (fromNonEmpty)
import Control.Lens.Combinators (cosmos, transformM, ix)
import Control.Lens.Operators ((^..), (.=) )
import Language.PureScript.Types
import Language.PureScript.CoreFn.Expr (PurusType)
import Language.PureScript.Constants.Prim qualified as C
import Language.PureScript.Environment (pattern RecordT)
import Bound (Var(..))
import Data.Text qualified as T
import Language.PureScript.CoreFn.Convert.Debug
import Language.PureScript.CoreFn.Pretty.Common (prettyAsStr)
import Control.Monad.State.Strict
import Prettyprinter
import Language.PureScript.Names
import Data.Maybe (mapMaybe)
import Control.Lens (view,_2)
import Language.PureScript.CoreFn.Convert.IR (analyzeApp)
import Control.Lens.Plated (transform)



monoMorph :: MonoExp -> MonoExp
monoMorph = \case
  V v -> V v
  LitE t lit -> LitE t $ monoMorph <$> lit
  LamE bv scope -> LamE bv $ viaExp monoMorph scope
  appE@(AppE f a) ->  instantiateTypes $ AppE (monoMorph f) (monoMorph a)
  CaseE t scrut alts -> CaseE t (monoMorph scrut) (mapAlt (viaExp monoMorph) <$> alts)
  LetE _REMOVE decls body -> LetE _REMOVE (mapBind (const (viaExp monoMorph)) <$> decls) (viaExp monoMorph body)
  AccessorE x t lbl obj -> AccessorE x t lbl (monoMorph obj)
  ObjectUpdateE x t e copy fs -> ObjectUpdateE x t (monoMorph e) copy (fmap monoMorph <$> fs)
  -- I'm not sure what to do for TyInstE or TyAbs TODO: Ask Koz
  TyAbs t inner -> TyAbs t (monoMorph inner)
  other -> other 

instantiateTypes :: MonoExp -> MonoExp
instantiateTypes e = case fmap (map monoMorph) <$> analyzeApp e of
  Nothing -> e
  Just (f,args) ->
    let (fTyVars,fInner) = stripQuantifiers (expTy id f)
        fTypes           = splitFunTyParts fInner
        argTypes         = expTy id <$> args
        quantifiedTyVars = view _2 <$> fTyVars
        instantiations   = getInstantiations quantifiedTyVars fTypes argTypes
        f'               = go instantiations quantifiedTyVars f
        msg              = prettify [ "Function:\n" <> prettyAsStr f
                                    , "Arguments:\n" <> prettyAsStr  args
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
   go dict (v:vs) ex = go dict vs (TyInstE (dict M.! v) ex)



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

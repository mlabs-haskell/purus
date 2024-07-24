module Language.PureScript.CoreFn.TypeLike where

import Language.PureScript.Types (TypeVarVisibility (..))
import Data.Text (Text)
import Prelude

import Language.PureScript.Types qualified as T
import Language.PureScript.Environment qualified as E
import Data.Bifunctor (Bifunctor(..))

import Data.Kind qualified as GHC
import Control.Lens.Operators ((<&>))
import Data.Maybe (catMaybes)
import Debug.Trace (trace)
import Prettyprinter (Pretty)
import Language.PureScript.CoreFn.Pretty.Common
import Language.PureScript.CoreFn.Convert.Debug (doTrace)
import Language.PureScript.Environment (pattern (:->))
import Language.PureScript.AST.SourcePos (pattern NullSourceAnn)

class TypeLike t where
  type KindOf t :: GHC.Type
  {- | Usually this will be the Type Application constructor -}
  applyType :: t -> t -> t

  {- | Given a typelike, remove all of the leading quantifiers while
       keeping track of the quantifier metadata (visibility, name, kind)
       for each removed quantifier.
  -}
  stripQuantifiers :: t -> ([(TypeVarVisibility, Text, KindOf t)], t)

  {- Given TypeLikes `a` and `b`, construct a function type `a -> b`
  -}
  funTy :: t -> t -> t

  {- | (a -> b -> c) -> [a,b]

       NOTE: Unsafe/partial. Check that you don't want `splitFunTyParts` instead
  -}
  funArgTypes :: t -> [t]
  funArgTypes = init . splitFunTyParts

  {- | Replaces all the type variables with the corresponding types -}
  replaceAllTypeVars :: [(Text,t)] -> t -> t

  {- | (a -> b -> c) -> [a,b,c]

     NOTE: Check that you don't want `funArgTypes` instead
  -}
  splitFunTyParts :: t -> [t]

  {- | Quantify over all free type variables in the argument type.
  -}
  quantify :: t -> t

  {- | `instantiates var mono poly` tries to find
       the type in `mono` that instantiates the TyVar in
       `poly`.

       E.g. `instantiates "x" (Maybe Int) (Maybe x) == Just Int`
  -}
  instantiates :: Text -- name of TyVar we're checking
               -> t -- Monomorphic/"more monomorphic" type
               -> t -- Polymorphic/"more polymorphic" type
               -> Maybe t

  {- | Collect the used type variables in a type -}
  usedTypeVariables :: t -> [(Text,KindOf t)]

  {- | Collect the free type variables in a type -}
  freeTypeVariables :: t -> [(Text,KindOf t)]

  {- | Get the (final) return type of a function. Returns the argument
       type if the argument is not a function.
  -}
  resultTy :: t -> t

  {- | Given a typelike t, return Just (Text, KindOf t) if it is a type variable.
       (Needed to implement some functions generically over the typelike)
  -}
  unTyVar :: t -> Maybe (Text, KindOf t)

  {- | Instantiate the first quantified variable in the second argument with the type in the first argument
  -}
  instTy :: t -> t -> t

  unFunction :: t -> Maybe (t,t)

  {- We need this to correctly reconstruct the type of expressions in the presence of TyAbs
  -}
  quantify1 :: Text -> KindOf t -> t -> t

    -- TODO: Just define it this way in the instances -_-
safeFunArgTypes :: forall t. TypeLike t => t -> [t]
safeFunArgTypes t = case splitFunTyParts t of
  [] -> []
  _  -> funArgTypes t

getInstantiations :: forall t. TypeLike t => t -> t -> [(Text,t)]
getInstantiations mono poly = catMaybes mInstantiations
  where
    freeInPoly = fst <$> usedTypeVariables poly
    mInstantiations = freeInPoly <&> \nm -> (nm,) <$> instantiates nm mono poly

instantiateWithArgs :: forall t. (TypeLike t, Pretty t) => t  -> [t] -> t
instantiateWithArgs f args = doTrace "instantiateWithArgs" msg result
  where
    msg = "instantiateWithArgs:\n  fun: " <> prettyAsStr f
           <> "\n  args: " <> prettyAsStr args
           <> "\n  instantiations: " <> prettyAsStr instantiations
           <> "\n  result: " <> prettyAsStr result
    result = quantify $ replaceAllTypeVars instantiations (unQuantify f)
    instantiations = getAllInstantiations f args

getAllInstantiations :: forall t
                      . (TypeLike t, Pretty t)
                    => t
                    -> [t]
                    -> [(Text,t)]
getAllInstantiations fun args@(_:_) = doTrace "getAllInstantiations" (prettyAsStr result) result
  where
    result = catMaybes $ zipWith go funArgs args

    funArgs = funArgTypes . unQuantify $ fun

    go t x = case unTyVar t of
      Just (v,_) -> Just (v,x)
      Nothing -> Nothing

getAllInstantiations _ _ = []


unQuantify :: forall t. TypeLike t => t -> t
unQuantify = snd . stripQuantifiers


instance TypeLike T.SourceType where
  type KindOf T.SourceType = T.SourceType

  applyType = T.srcTypeApp

  stripQuantifiers = \case
    T.ForAll _ vis var mk inner _ -> first ((vis,var,mk):) $ stripQuantifiers inner
    other -> ([],other)

  funTy = E.function

  funArgTypes = init . splitFunTyParts

  replaceAllTypeVars = T.replaceAllTypeVars

  splitFunTyParts = \case
    (a E.:-> b) -> a : splitFunTyParts b
    t           -> [t]

  quantify = T.quantify

  instantiates var x (T.TypeVar _ y _) | y == var = Just x
  instantiates var (T.TypeApp _ t1 t2) (T.TypeApp _ t1' t2') = case instantiates var t1 t1' of
    Just x -> Just x
    Nothing -> instantiates var t2 t2'
  instantiates _ _ _ = Nothing

  freeTypeVariables = T.freeTypeVariables

  usedTypeVariables = T.usedTypeVariables

  resultTy t = case snd $  stripQuantifiers t of
    (_ E.:-> b) -> resultTy b
    other -> other

  instTy t = \case
    T.ForAll _ _ var _ inner _ -> replaceAllTypeVars [(var,t)] inner
    other -> other

  unTyVar = \case
    T.TypeVar _ v k -> Just (v,k)
    _ -> Nothing

  unFunction = \case
    a :-> b -> Just (a,b)
    _ -> Nothing

  quantify1 nm k inner = T.ForAll NullSourceAnn TypeVarVisible nm k inner Nothing

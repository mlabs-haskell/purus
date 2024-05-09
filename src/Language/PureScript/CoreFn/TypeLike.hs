module Language.PureScript.CoreFn.TypeLike where

import Language.PureScript.Types (TypeVarVisibility)
import Data.Text (Text)
import Prelude

import Language.PureScript.Types qualified as T
import Language.PureScript.Environment qualified as E
import Data.Bifunctor (Bifunctor(..))

class TypeLike t where
  {- | Usually this will be the Type Application constructor -}
  applyType :: t -> t -> t

  {- | Given a typelike, remove all of the leading quantifiers while
       keeping track of the quantifier metadata (visibility, name, kind)
       for each removed quantifier.
  -}
  stripQuantifiers :: t -> ([(TypeVarVisibility, Text, Maybe t)], t)

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
  usedTypeVariables :: t -> [Text]

  {- | Collect the free type variables in a type -}
  freeTypeVariables :: t -> [Text]

  {- | Get the (final) return type of a function. Returns the argument
       type if the argument is not a function.
  -}
  resultTy :: t -> t

instance TypeLike T.SourceType where
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

  instantiates var x (T.TypeVar _ y) | y == var = Just x
  instantiates var (T.TypeApp _ t1 t2) (T.TypeApp _ t1' t2') = case instantiates var t1 t1' of
    Just x -> Just x
    Nothing -> instantiates var t2 t2'
  instantiates _ _ _ = Nothing

  freeTypeVariables = T.freeTypeVariables

  usedTypeVariables = T.usedTypeVariables

  resultTy t = case snd $  stripQuantifiers t of
    (_ E.:-> b) -> resultTy b
    other -> other

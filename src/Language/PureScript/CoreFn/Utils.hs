{-# OPTIONS_GHC -Wno-orphans #-}

module Language.PureScript.CoreFn.Utils where

import Data.Bifunctor (Bifunctor (first))
import Data.Bitraversable (Bitraversable (bitraverse))
import Data.Map (Map)
import Data.Map qualified as M
import Data.Text (Text)
import Language.PureScript.CoreFn.Desugar.Utils (traverseLit)
import Language.PureScript.CoreFn.Expr (
  Bind (..),
  CaseAlternative (CaseAlternative),
  Expr (..),
  PurusType,
 )
import Language.PureScript.Types
import Prelude hiding (error)
import Prelude qualified

import Control.Lens.Plated
import Language.PureScript.Environment (function, pattern (:->))
import Language.PureScript.Pretty.Types (prettyPrintType)

foldl1x :: (Foldable t) => String -> (a -> a -> a) -> t a -> a
foldl1x msg f xs
  | null xs = Prelude.error msg
  | otherwise = foldl1 f xs


{- TODO: REMOVE EVERYTHING BELOW (it should all be subsumed by the TypeLike class)

-}

-- TODO: Explain what this is / how it works
-- TODO: Type Constructors
instantiates ::
  Text -> -- Name of the TyVar we're checking
  SourceType -> -- Monomorphic type (or "more monomorphic" type)
  SourceType -> -- Polymorphic type (or "more polymoprhic" type)
  Maybe SourceType
instantiates var x (TypeVar _ y _) | y == var = Just x
instantiates var (TypeApp _ t1 t2) (TypeApp _ t1' t2') = case instantiates var t1 t1' of
  Just x -> Just x
  Nothing -> instantiates var t2 t2'
instantiates _ _ _ = Nothing

appFunArgs :: Expr a -> Expr a -> (Expr a, [Expr a])
appFunArgs f args = (appFun f, appArgs f args)
  where
    appArgs :: Expr a -> Expr a -> [Expr a]
    appArgs (App _ t1 t2) t3 = appArgs t1 t2 <> [t3]
    appArgs _ t3 = [t3]

    appFun :: Expr a -> Expr a
    appFun (App _ t1 _) = appFun t1
    appFun res = res

appType :: (Show a) => Expr a -> Expr a -> SourceType
appType fe ae = case stripQuantifiers funTy of
  ([], ft) ->
    let numArgs = length argTypes
     in foldl1x "appType first branch (CoreFn.Utils)" function . drop numArgs . splitFunTyParts $ ft
  (xs, ft) ->
    let funArgs = splitFunTyParts ft -- funArgTypes ft
        dict = mkInstanceMap M.empty xs argTypes funArgs
        numArgs = length argTypes
        msg =
          "FUNTY: "
            <> prettyPrintType 100 funTy
            <> "\nARGTY: "
            <> prettyPrintType 100 (exprType ae)
            <> "\nNUM ARGS:"
            <> show numArgs
     in quantify
          . foldl1x msg function
          . drop numArgs
          . splitFunTyParts
          . replaceAllTypeVars (M.toList dict)
          $ ft
  where
    (f, args) = appFunArgs fe ae
    funTy = exprType f
    argTypes = exprType <$> args

    mkInstanceMap :: Map Text SourceType -> [Text] -> [SourceType] -> [SourceType] -> Map Text SourceType
    mkInstanceMap acc [] _ _ = acc
    mkInstanceMap acc _ [] _ = acc
    mkInstanceMap acc _ _ [] = acc
    mkInstanceMap acc (var : vars) (mt : mts) (pt : pts) = case instantiates var mt pt of
      Nothing ->
        mkInstanceMap acc [var] mts pts
          <> mkInstanceMap M.empty vars (mt : mts) (pt : pts)
      Just t -> mkInstanceMap (M.insert var t acc) vars (mt : mts) (pt : pts)

stripQuantifiers :: SourceType -> ([Text], SourceType)
stripQuantifiers = first reverse . go []
  where
    go :: [Text] -> SourceType -> ([Text], SourceType)
    go acc (ForAll _ _ var _ inner _) = go (var : acc) inner
    go acc other = (acc, other)

-- | (a -> b -> c) -> [a,b,c]
splitFunTyParts :: Type a -> [Type a]
splitFunTyParts = \case
  (a :-> b) -> a : splitFunTyParts b
  t -> [t]

{- | (a -> b -> c) -> [a,b]

  NOTE: Unsafe/partial
-}
funArgTypes :: Type a -> [Type a]
funArgTypes = init . splitFunTyParts

exprType :: (Show a) => Expr a -> PurusType
exprType = \case
  Literal _ ty _ -> ty
  Accessor _ ty _ _ -> ty
  ObjectUpdate _ ty _ _ _ -> ty
  Abs _ ty _ _ -> ty
  App _ t1 t2 -> appType t1 t2
  Var _ ty __ -> ty
  Case _ ty _ _ -> ty
  Let _ _ e -> exprType e

instance Plated (Expr a) where
  plate f = \case
    Literal a t lit -> Literal a t <$> traverseLit f lit
    Accessor a t s e -> Accessor a t s <$> f e
    ObjectUpdate a t e cf fs ->
      (\e' fs' -> ObjectUpdate a t e' cf fs')
        <$> f e
        <*> traverse (traverse f) fs
    Abs a t bv e -> Abs a t bv <$> f e
    App a e1 e2 -> App a <$> f e1 <*> f e2
    Var a t qi -> pure $ Var a t qi
    Case a t scruts alts ->
      Case a t
        <$> traverse f scruts
        <*> traverse goAlt alts
    Let a decls body ->
      Let a <$> traverse goDecl decls <*> f body
    where
      goAlt (CaseAlternative caBinders caResult) =
        CaseAlternative caBinders
          <$> bitraverse (traverse (traverse f)) f caResult

      goDecl = \case
        NonRec a nm body -> NonRec a nm <$> f body
        Rec xs -> Rec <$> traverse (traverse f) xs

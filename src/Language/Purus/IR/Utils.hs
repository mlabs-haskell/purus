{-# LANGUAGE StarIsType #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use camelCase" #-}
module Language.Purus.IR.Utils (
  WithObjects,
  WithoutObjects,
  IR_Decl,
  Vars,
  asExp,
  viaExp,
  viaExpM,
  toExp,
  fromExp,
  deepMapMaybeBound,
  transformTypesInExp,
  containsBVar,
  traverseBind,
  mapBind,
  foldBinds,
  foldMBinds,
  flatBinds,
  allDeclIdentifiers,
  traverseAlt,
  mapAlt,
  isBuiltinE,
  isBuiltin,
  isConstructorE,
  isConstructor,
  mkBVar,
  unBVar,
  allBoundVars,
  stripSkolems,
  stripSkolemsFromExpr,
) where

import Prelude

import Bound (Scope, Var (..), abstract, fromScope)
import Control.Monad (join, foldM)
import Data.Void (Void)
import Language.PureScript.CoreFn.Expr (PurusType)
import Language.PureScript.CoreFn.TypeLike (TypeLike (KindOf))
import Language.PureScript.Names (
  Ident (Ident),
  ModuleName (ModuleName),
  Qualified (..),
  QualifiedBy (ByModuleName),
 )
import Language.Purus.IR (
  Alt (..),
  BVar (..),
  BindE (..),
  Exp (..),
  FVar (..),
  Lit (CharL, IntL, ObjectL, StringL),
  Pat (ConP, LitP, VarP),
  XAccessor,
  XObjectLiteral,
  XObjectUpdate,
 )
import Prettyprinter (Pretty)

import Data.Set (Set)
import Data.Set qualified as S

import Data.Map (Map)
import Data.Map qualified as M

import Data.Text qualified as T

import Data.Maybe (fromMaybe, mapMaybe)

import Data.Char (isUpper)

import Data.Functor.Identity (Identity (..))

import Control.Lens.Operators ((^..))
import Control.Lens.Plated (Plated (..), cosmos, transform)
import Data.Foldable (foldl')
import Language.PureScript.AST.SourcePos (SourceAnn)
import Language.PureScript.Types (Constraint (..), SourceType, Type (..))

{- Convenience/Utility Type Stuff -}

data WithObjects

type instance XAccessor WithObjects = ()
type instance XObjectUpdate WithObjects = ()
type instance XObjectLiteral WithObjects = ()

data WithoutObjects

type instance XAccessor WithoutObjects = Void
type instance XObjectUpdate WithoutObjects = Void
type instance XObjectLiteral WithoutObjects = Void

type IR_Decl = BindE PurusType (Exp WithObjects PurusType) (Vars PurusType)

type Vars t = Var (BVar t) (FVar t)

{-
  *********************
    Exp/Scope Utilities
  *********************
-}

-- Scope helpers
asExp ::
  forall x t r.
  Scope (BVar t) (Exp x t) (Vars t) ->
  (Exp x t (Vars t) -> r) ->
  r
asExp e f = f . fmap join . fromScope $ e

viaExp ::
  forall x t.
  (Exp x t (Vars t) -> Exp x t (Vars t)) ->
  Scope (BVar t) (Exp x t) (Vars t) ->
  Scope (BVar t) (Exp x t) (Vars t)
viaExp f scoped =
  abstract (\case B bv -> Just bv; _ -> Nothing)
    . f
    . fmap join
    . fromScope
    $ scoped

viaExpM ::
  forall x t m.
  (Monad m) =>
  (Exp x t (Vars t) -> m (Exp x t (Vars t))) ->
  Scope (BVar t) (Exp x t) (Vars t) ->
  m (Scope (BVar t) (Exp x t) (Vars t))
viaExpM f scoped = do
  let unscoped = toExp scoped
  transformed <- f unscoped
  pure $ abstract (\case B bv -> Just bv; _ -> Nothing) transformed

-- stupid, but `\e -> toExp e`` is probably the most duplicated code phrase
-- in this whole project
toExp :: Scope (BVar t) (Exp x t) (Vars t) -> Exp x t (Vars t)
toExp = fmap join . fromScope

fromExp :: Exp x t (Vars t) -> Scope (BVar t) (Exp x t) (Vars t)
fromExp = abstract $ \case B bv -> Just bv; _ -> Nothing

{- | Does not touch Var *binders*.

     This is primarily useful for updating identifiers or indices of BVar *expressions*
     when "moving expressions around" the AST, e.g. in lifting.

     A typical use case is: You've extracted an expression from a context where some variables in
     the expression are bound in that context, but free in the new context, and you need to bind them
     in the new context. To maintain uniqueness (of names or indices), it is necessary to rename/reindex
     (or conceivably retype, typically by renaming tyvars) the previously bound vars *wherever*
     they occur in the extracted expression.

     Note that this passes over the `BVar (KindOf t)` that lives inside a `TyAbs`, i.e., this is for transforming
     *term-level* bound vars
-}
deepMapMaybeBound ::
  forall x t.
  (TypeLike t, Pretty t, Pretty (KindOf t), Show (Exp x t (Vars t))) =>
  (BVar t -> Maybe (BVar t)) ->
  Exp x t (Vars t) ->
  Exp x t (Vars t)
deepMapMaybeBound _f = \case
  V (B bv) -> V . B . f $ bv
  V (F fv) -> V . F $ fv
  AppE e1 e2 -> AppE (go e1) (go e2)
  LamE bv body -> LamE bv $ goScope body
  LetE bs e ->
    let bs' = mapBind (const goScope) <$> bs
        e' = goScope e
     in LetE bs' e'
  LitE t lit -> LitE t $ go <$> lit
  AccessorE x t str e -> AccessorE x t str $ go e
  ObjectUpdateE x t e copy fs ->
    let e' = go e
        fs' = fmap go <$> fs
     in ObjectUpdateE x t e' copy fs'
  TyInstE t e -> TyInstE t (go e)
  TyAbs tv e -> TyAbs tv $ go e
  CaseE t scrut alts ->
    let scrut' = go scrut
        alts' = mapAlt goScope <$> alts
     in CaseE t scrut' alts'
  where
    goScope =
      abstract (\case B bv -> Just $ f bv; _ -> Nothing)
        . go
        . fmap join
        . fromScope
    go = deepMapMaybeBound _f
    f bv = fromMaybe bv (_f bv)

-- touches everything, *except* the kind annotations in TyAbs (can't write a generic traversal b/c (KindOf t ~ t) is
-- only true for SourceType and not Ty)
transformTypesInExp ::
  forall x t.
  (t -> t) ->
  Exp x t (Vars t) ->
  Exp x t (Vars t)
transformTypesInExp f = \case
  V (B bv) -> V . B . goBV $ bv
  V (F (FVar fvTy fvNm)) -> V . F $ FVar (f fvTy) fvNm
  AppE e1 e2 -> AppE (go e1) (go e2)
  LitE t lit -> LitE (f t) (go <$> lit)
  LamE bv body ->
    let bv' = goBV bv
        body' = goScope body
     in LamE bv' body'
  LetE bs body ->
    let bs' = mapBind (const goScope) <$> bs
        body' = goScope body
     in LetE bs' body'
  AccessorE x t str e ->
    let t' = f t
        e' = go e
     in AccessorE x t' str e'
  ObjectUpdateE x t e copy fs ->
    let t' = f t
        e' = go e
        fs' = fmap go <$> fs
     in ObjectUpdateE x t' e' copy fs'
  CaseE t scrut alts ->
    let t' = f t
        scrut' = go scrut
        alts' = goAlt <$> alts
     in CaseE t' scrut' alts'
  TyInstE t e -> TyInstE (f t) (go e)
  TyAbs btv e -> TyAbs btv (go e)
  where
    -- can't use mapAlt b/c we need to update types in patterns
    -- (We still have object lit pats before object desugaring)
    goAlt :: Alt x t (Exp x t) (Vars t) -> Alt x t (Exp x t) (Vars t)
    goAlt (UnguardedAlt pat body) =
      let pat' = goPat pat
          body' = goScope body
       in UnguardedAlt pat' body'

    goPat :: Pat x t (Exp x t) (Vars t) -> Pat x t (Exp x t) (Vars t)
    goPat = \case
      VarP nm indx t -> VarP nm indx (f t)
      LitP (ObjectL x fs) -> LitP . ObjectL x $ fmap goPat <$> fs
      ConP tn cn ps -> ConP tn cn $ goPat <$> ps
      other -> other

    goBV :: BVar t -> BVar t
    goBV (BVar bvIx bvTy bvNm) = BVar bvIx (f bvTy) bvNm

    goScope :: Scope (BVar t) (Exp x t) (Vars t) -> Scope (BVar t) (Exp x t) (Vars t)
    goScope = viaExp go

    go = transformTypesInExp f

containsBVar :: Ident -> Int -> Scope (BVar t) (Exp x t) (Vars t) -> Bool
containsBVar idnt indx expr =
  any
    ( \case
        V (B (BVar bvix _ bvident)) -> bvix == indx && idnt == bvident
        _ -> False
    )
    subExpressions
  where
    subExpressions = toExp expr ^.. cosmos

{-
  *********************
    BindE Utilities
  *********************
-}

traverseBind ::
  forall (f :: * -> *) x t.
  (Applicative f) =>
  ( (Ident, Int) ->
    Scope (BVar t) (Exp x t) (Vars t) ->
    f (Scope (BVar t) (Exp x t) (Vars t))
  ) ->
  BindE t (Exp x t) (Vars t) ->
  f (BindE t (Exp x t) (Vars t))
traverseBind f = \case
  NonRecursive nm i b -> curry goNonRec nm i b
  Recursive xs -> Recursive <$> traverse (\(nm, body) -> (nm,) <$> f nm body) xs
  where
    goNonRec i@(nm, indx) body = NonRecursive nm indx <$> f i body

mapBind ::
  forall x t.
  ( (Ident, Int) ->
    Scope (BVar t) (Exp x t) (Vars t) ->
    Scope (BVar t) (Exp x t) (Vars t)
  ) ->
  BindE t (Exp x t) (Vars t) ->
  BindE t (Exp x t) (Vars t)
mapBind f = runIdentity . traverseBind (\a b -> pure $ f a b)

-- it's a foldl' if that ever matters
foldBinds ::
  forall x t r.
  ( r ->
    (Ident, Int) ->
    Scope (BVar t) (Exp x t) (Vars t) ->
    r
  ) ->
  r ->
  [BindE t (Exp x t) (Vars t)] ->
  r
foldBinds _ e [] = e
foldBinds f e (x : xs) = case x of
  NonRecursive nm i b -> foldBinds f (f e (nm, i) b) xs
  Recursive recBinds ->
    let e' = foldl' (\acc (nm, b) -> f acc nm b) e recBinds
     in foldBinds f e' xs

foldMBinds ::
  forall x t m r.
  Monad m =>
  ( r ->
    (Ident, Int) ->
    Scope (BVar t) (Exp x t) (Vars t) ->
    m r
  ) ->
  r ->
  [BindE t (Exp x t) (Vars t)] ->
  m r
foldMBinds f e [] = pure e
foldMBinds f e (x : xs) = case x of
  NonRecursive nm i b -> f e (nm,i) b >>= \e' -> foldMBinds f e' xs
  Recursive recBinds -> do
    e' <- foldM (\acc (nm,b) -> f acc nm b) e recBinds
    foldMBinds f e' xs
    
flatBinds :: [BindE t (Exp x t) (Vars t)] -> Map (Ident, Int) (Scope (BVar t) (Exp x t) (Vars t))
flatBinds = foldBinds (\acc nm scoped -> M.insert nm scoped acc) M.empty

-- N.B. we're using Set instead of [] mainly to ensure that everything has the same order
allDeclIdentifiers :: forall x t. (Ord t) => [BindE t (Exp x t) (Vars t)] -> Set (Ident, Int)
allDeclIdentifiers [] = S.empty
allDeclIdentifiers (b : rest) = case b of
  NonRecursive nm indx _ -> S.insert (nm, indx) $ allDeclIdentifiers rest
  Recursive xs ->
    let rest' = allDeclIdentifiers rest
     in foldl' (\acc ((nm, indx), _) -> S.insert (nm, indx) acc) rest' xs

{-
  *********************
    Alt Utilities
  *********************
-}

traverseAlt ::
  forall (f :: * -> *) x t.
  (Functor f) =>
  (Scope (BVar t) (Exp x t) (Vars t) -> f (Scope (BVar t) (Exp x t) (Vars t))) ->
  Alt x t (Exp x t) (Vars t) ->
  f (Alt x t (Exp x t) (Vars t))
traverseAlt f (UnguardedAlt pat body) = UnguardedAlt pat <$> f body

mapAlt ::
  forall x t.
  (Scope (BVar t) (Exp x t) (Vars t) -> Scope (BVar t) (Exp x t) (Vars t)) ->
  Alt x t (Exp x t) (Vars t) ->
  Alt x t (Exp x t) (Vars t)
mapAlt f alt = runIdentity . traverseAlt (pure . f) $ alt

{-
  *********************
    Misc Utilities (Predicates/Var manipulation/etc)
  *********************
-}

isBuiltinE :: Exp x ty1 (Var b (FVar ty2)) -> Bool
isBuiltinE = \case
  V (F (FVar _ qi)) -> isBuiltin qi
  TyInstE _ e -> isBuiltinE e
  _ -> False

isBuiltin :: Qualified a -> Bool
isBuiltin (Qualified (ByModuleName (ModuleName "Builtin")) _) = True
isBuiltin _ = False

isConstructorE :: Exp x ty1 (Var b (FVar ty2)) -> Bool
isConstructorE = \case
  V (F (FVar _ qi)) -> isConstructor qi
  TyInstE _ e -> isConstructorE e
  _ -> False

-- After the recent changes, constructors *can't* be inlined, i.e., they must remain
-- free until the final PIR compilation stage
isConstructor :: Qualified Ident -> Bool
isConstructor (Qualified _ (Ident nm)) = isUpper (T.head nm)
isConstructor _ = False

mkBVar :: Ident -> Int -> t -> BVar t
mkBVar idnt indx ty = BVar indx ty idnt

unBVar :: BVar t -> (Ident, Int)
unBVar (BVar indx _ idnt) = (idnt, indx)

allBoundVars :: forall x t. (Ord t) => Exp x t (Vars t) -> [BVar t]
allBoundVars e = S.toList . S.fromList $ flip mapMaybe everything $ \case
  V (B bv) -> Just bv
  _ -> Nothing
  where
    everything = e ^.. cosmos

{- Remove skolems from *types* by stripping their index and turning the remaining ident into a TyVar
-}
stripSkolems :: PurusType -> PurusType
stripSkolems = transform $ \case
  Skolem a nm ki _ _ -> TypeVar a nm ki
  other -> other

{- Strip skolems from *all type annotations in an expression*
-}
stripSkolemsFromExpr :: Exp x PurusType (Vars PurusType) -> Exp x PurusType (Vars PurusType)
stripSkolemsFromExpr = transformTypesInExp stripSkolems

{-
  *********************
  Orphan instances
  *********************
-}


{- Useful for transform/rewrite/cosmos/etc -}
instance Plated (Exp x t (Vars t)) where
  plate = go
    where
      go ::
        forall f.
        (Applicative f) =>
        (Exp x t (Vars t) -> f (Exp x t (Vars t))) ->
        Exp x t (Vars t) ->
        f (Exp x t (Vars t))
      go tfun = \case
        LamE bv e -> LamE bv <$> scopeHelper e
        CaseE t es alts ->
          let goAlt :: Alt x t (Exp x t) (Vars t) -> f (Alt x t (Exp x t) (Vars t))
              goAlt (UnguardedAlt pats scoped) =
                UnguardedAlt pats <$> scopeHelper scoped
           in CaseE t <$> tfun es <*> traverse goAlt alts
        LetE decls scoped ->
          let goDecls :: BindE t (Exp x t) (Vars t) -> f (BindE t (Exp x t) (Vars t))
              goDecls = \case
                NonRecursive ident bvix expr ->
                  NonRecursive ident bvix <$> scopeHelper expr
                Recursive xs ->
                  Recursive <$> traverse (\(i, x) -> (i,) <$> scopeHelper x) xs
           in LetE <$> traverse goDecls decls <*> scopeHelper scoped
        AppE e1 e2 -> AppE <$> tfun e1 <*> tfun e2
        AccessorE x t pss e -> AccessorE x t pss <$> tfun e
        ObjectUpdateE x t e cf fs ->
          (\e' fs' -> ObjectUpdateE x t e' cf fs')
            <$> tfun e
            <*> traverse (\(nm, expr) -> (nm,) <$> tfun expr) fs
        LitE t lit -> LitE t <$> traverseLit lit
        V a -> pure (V a)
        TyAbs bv e -> TyAbs bv <$> tfun e
        TyInstE t e -> TyInstE t <$> tfun e
        where
          scopeHelper ::
            Scope (BVar t) (Exp x t) (Vars t) ->
            f (Scope (BVar t) (Exp x t) (Vars t))
          scopeHelper scoped =
            let unscoped = toExp scoped
                effed = tfun unscoped
                abstr = abstract $ \case
                  B bv -> Just bv
                  _ -> Nothing
             in abstr <$> effed

          traverseLit = \case
            IntL i -> pure $ IntL i
            StringL str -> pure $ StringL str
            CharL char -> pure $ CharL char
            ObjectL x fs -> ObjectL x <$> traverse (\(str, e) -> (str,) <$> tfun e) fs

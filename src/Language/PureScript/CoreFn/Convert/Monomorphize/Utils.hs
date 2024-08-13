{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- has to be here (more or less)
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use camelCase" #-}

module Language.PureScript.CoreFn.Convert.Monomorphize.Utils where

import Prelude

import Bound (abstract)
import Bound.Scope (Scope (..), fromScope, mapBound, toScope)
import Bound.Var (Var (..))
import Control.Lens ((<&>), (^..), (^?))
import Control.Lens.Plated (Plated (..), cosmos, transform)
import Control.Monad (join)
import Control.Monad.Except (throwError)
import Control.Monad.RWS (RWST (..))
import Control.Monad.RWS.Class (MonadReader (..), gets, modify')
import Data.Aeson qualified as Aeson
import Data.Bifunctor (first)
import Data.Char (isUpper)
import Data.Functor.Identity (runIdentity)
import Data.List (find)
import Data.Map (Map)
import Data.Map qualified as M
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import GHC.IO (throwIO)
import Language.PureScript.AST.SourcePos (SourceAnn)
import Language.PureScript.CoreFn.Ann (Ann)
import Language.PureScript.CoreFn.Convert.Debug (doTrace, prettify)
import Language.PureScript.CoreFn.Convert.DesugarCore (
  IR_Decl,
  Vars,
  WithObjects,
 )
import Language.PureScript.CoreFn.Convert.IR (Alt (..), BVar (..), BindE (..), Exp (..), FVar (..), Pat (..), expTy, expTy', ppExp, _V, Lit (..))
import Language.PureScript.CoreFn.Expr (Bind, PurusType)
import Language.PureScript.CoreFn.FromJSON ()
import Language.PureScript.CoreFn.Module (Module (..))
import Language.PureScript.CoreFn.Pretty (prettyAsStr)
import Language.PureScript.CoreFn.TypeLike (TypeLike (..), unQuantify)
import Language.PureScript.Label (Label (..))
import Language.PureScript.Names (Ident (..), ModuleName (..), Qualified (..), QualifiedBy (..), runIdent, pattern ByNullSourcePos)
import Language.PureScript.PSString (PSString)
import Language.PureScript.Types (
  Constraint (..),
  RowListItem (..),
  SourceType,
  Type (..),
  TypeVarVisibility,
  rowToList,
 )
import Prettyprinter (Pretty)
import Data.Set qualified as S
import Data.Set (Set)
import Data.Foldable (foldl')

{- Monomorphizer monad & related utilities -}

-- TODO: better error messages
newtype MonoError
  = MonoError String
  deriving (Show)

-- Just a newtype over `Int`
newtype MonoState = MonoState
  { unique :: Int
  }

-- Reads (ModuleName,ModuleDecls), writes nothing (...yet), State is a newtype over Int for fresh names & etc
type Monomorphizer a = RWST (ModuleName, [BindE PurusType (Exp WithObjects PurusType) (Vars PurusType)]) () MonoState (Either MonoError) a

getModName :: Monomorphizer ModuleName
getModName = ask <&> fst

getModBinds :: Monomorphizer [BindE PurusType (Exp WithObjects PurusType) (Vars PurusType)]
getModBinds = ask <&> snd

note :: String -> Maybe b -> Monomorphizer b
note err = \case
  Nothing -> throwError $ MonoError err
  Just x -> pure x

freshUnique :: Monomorphizer Int
freshUnique = do
  u <- gets unique
  modify' $ \(MonoState _) -> MonoState (u + 1)
  pure u

freshen :: Ident -> Monomorphizer Ident
freshen ident = do
  u <- gets unique
  modify' $ \(MonoState _) -> MonoState (u + 1)
  let uTxt = T.pack (show u)
  case ident of
    Ident t -> pure $ Ident $ t <> "_$$" <> uTxt
    GenIdent (Just t) i -> pure $ GenIdent (Just $ t <> "_$$" <> uTxt) i -- we only care about a unique ord property for the maps
    GenIdent Nothing i -> pure $ GenIdent (Just $ "var_$$" <> uTxt) i
    -- other two shouldn't exist at this stage
    other -> pure other

freshBVar :: t -> Monomorphizer (BVar t)
freshBVar t = do
  u <- gets unique
  modify' $ \(MonoState _) -> MonoState (u + 1)
  let gIdent = Ident $ T.pack ("x_$$" <> show u)
  pure $ BVar u t gIdent

{-
   Misc utils for constructing/analyzing expressions
-}

qualifyNull :: a -> Qualified a
qualifyNull = Qualified ByNullSourcePos

-- REVIEW: IDK if this is right? Do we need to abstract here?
-- Construct a Let expression from a list of BindEs and a scoped body
gLet ::
  [BindE PurusType (Exp WithObjects PurusType) (Vars PurusType)] ->
  Scope (BVar PurusType) (Exp WithObjects PurusType) (Vars PurusType) ->
  Exp WithObjects PurusType (Vars PurusType)
gLet binds e = LetE M.empty binds e

-- Tools for updating variable types/names

-- REVIEW: Is this right? Do we really want to update bound & free var types at the same time like this?
updateVarTyS ::
  forall x.
  BVar SourceType ->
  SourceType ->
  Scope (BVar SourceType) (Exp x SourceType) (Var (BVar SourceType) (FVar SourceType)) ->
  Scope (BVar SourceType) (Exp x SourceType) (Var (BVar SourceType) (FVar SourceType))
updateVarTyS (BVar bvIx _ bvIdent) ty scoped = abstr unscoped
  where
    abstr = abstract $ \case
      B (BVar bvIx' _ bvIdent')
        | bvIx == bvIx' && bvIdent == bvIdent' -> Just $ BVar bvIx ty bvIdent'
      B bv -> Just bv
      _ -> Nothing

    scoped' = mapBound goBound scoped
    unscoped = join <$> fromScope scoped'
    goBound :: BVar SourceType -> BVar SourceType
    goBound bv@(BVar bvIx' _ bvIdent')
      | bvIx == bvIx' && bvIdent == bvIdent' = BVar bvIx ty bvIdent
      | otherwise = bv

-- doesn't change types!
renameBoundVar ::
  Ident ->
  Ident ->
  Scope (BVar t) (Exp WithObjects t) (FVar t) ->
  Scope (BVar t) (Exp WithObjects t) (FVar t)
renameBoundVar old new = mapBound $ \case
  BVar bvIx bvTy bvIdent | bvIdent == old -> BVar bvIx bvTy new
  other -> other

{- Given a function and a list of arguments that hopefully
   match the type & number of the args in the functions signature,
   apply the function to all of the arguments.

   TODO: Eventually we shouldn't need this but it's useful to throw errors
         while debugging if we get something that's not a function
-}
unsafeApply ::
  forall a x t.
  (TypeLike t, Pretty t, Pretty (KindOf t)) =>
  (a -> Vars t) ->
  Exp x t a ->
  [Exp x t a] ->
  Exp x t a
unsafeApply f e (arg : args) = case unFunction . snd . stripQuantifiers . expTy f $ e of
  Just _ -> unsafeApply f (AppE e arg) args
  other ->
    Prelude.error $
      "Unexpected argument to unsafeApply:\n  "
        <> "Fun Expression: "
        <> ppExp (f <$> e)
        <> "\n  Arg: "
        <> ppExp (f <$> arg)
        <> "\n  FunType: "
        <> prettyAsStr other
        <> "\n  ArgType: "
        <> prettyAsStr (expTy f arg)
unsafeApply _ e [] = e

{- Find the declaration *group* to which a given identifier belongs.
-}
findInlineDeclGroup ::
  Ident ->
  [BindE ty (Exp x ty) a] ->
  Maybe (BindE ty (Exp x ty) a)
findInlineDeclGroup _ [] = Nothing
findInlineDeclGroup ident (NonRecursive ident' bvix expr : rest)
  | ident == ident' = Just $ NonRecursive ident' bvix expr
  | otherwise = findInlineDeclGroup ident rest
findInlineDeclGroup ident (Recursive xs : rest) = case find (\x -> fst (fst x) == ident) xs of
  Nothing -> findInlineDeclGroup ident rest
  Just _ -> Just (Recursive xs)

letBindRecursive ::
  (TypeLike t, Pretty t, Pretty (KindOf t)) =>
  (a -> Vars t) ->
  Ident ->
  Int ->
  Scope (BVar t) (Exp x t) a ->
  Scope (BVar t) (Exp x t) (Vars t)
letBindRecursive f idnt indx _scoped
  | containsBVar idnt indx scoped =
      let result = abstr $ LetE M.empty [NonRecursive idnt indx scoped] (toScope (V . B $ BVar indx (expTy' id scoped) idnt))
          msg =
            "IDENT:\n"
              <> prettyAsStr idnt
              <> "#"
              <> prettyAsStr indx
              <> "\n\nINPUT EXPR:\n"
              <> prettyAsStr (fromScope scoped)
              <> "\n\nINPUT TY:\n"
              <> prettyAsStr (expTy' id scoped)
              <> "\n\nRESULT:\n"
              <> prettyAsStr (fromScope result)
              <> "\n\nRESULT TY:\n"
              <> prettyAsStr (expTy' id result)
       in doTrace "letBindRecursive" msg result
  | otherwise = scoped
  where
    scoped = f <$> _scoped
    abstr = abstract $ \case
      B bv -> Just bv
      _ -> Nothing

{- Find the body of a declaration with the given name in the given module.

-}
findDeclBody ::
  forall k.
  Text ->
  Module IR_Decl k PurusType Ann ->
  Maybe (Scope (BVar PurusType) (Exp WithObjects PurusType) (Vars PurusType))
findDeclBody nm Module {..} = doTrace "findDeclBody" ("NAME: " <> T.unpack nm) $ findDeclBody' (Ident nm) moduleDecls

findDeclBody' ::
  forall x ty.
  (TypeLike ty, Pretty ty, Pretty (KindOf ty)) =>
  Ident ->
  [BindE ty (Exp x ty) (Vars ty)] ->
  Maybe (Scope (BVar ty) (Exp x ty) (Vars ty))
findDeclBody' ident binds = case findInlineDeclGroup ident binds of
  Nothing -> Nothing
  Just decl -> case decl of
    NonRecursive nrid nrix e -> Just $ letBindRecursive id nrid nrix e
    Recursive xs -> case find (\x -> fst (fst x) == ident) xs of
      Nothing -> Nothing
      Just ((idnt, indx), e) -> Just $ letBindRecursive id idnt indx e

{- Turns a Row Type into a Map of field names to Row item data.

   NOTE: Be sure to unwrap the enclosing record if you're working w/ a
         record type.
-}
mkFieldMap :: SourceType -> M.Map PSString (RowListItem SourceAnn)
mkFieldMap fs = M.fromList $ (\x -> (runLabel (rowListLabel x), x)) <$> (fst . rowToList $ fs)

-- TODO: Doesn't have much of a purpose after the IR rework
extractAndFlattenAlts :: Alt x t (Exp x t) a -> [Scope (BVar t) (Exp x t) a]
extractAndFlattenAlts (UnguardedAlt _ _ res) = [res]

{- Updates the identifier and type of free variables using the provided Map

   Note that this erases original source position information, since it is meant to be
   used during inlining (and ergo the original source position may no longer be
   accurate or meaningful, e.g. in generated code)
-}
updateFreeVars ::
  Map Ident (Ident, SourceType) ->
  Exp WithObjects PurusType (Vars PurusType) ->
  Exp WithObjects PurusType (Vars PurusType)
updateFreeVars dict = transform updateFreeVar
  where
    updateFreeVar ::
      Exp WithObjects PurusType (Vars PurusType) ->
      Exp WithObjects PurusType (Vars PurusType)
    updateFreeVar expr = case expr ^? _V of
      Just (F (FVar _ (Qualified (ByModuleName _) varId))) -> case M.lookup varId dict of
        Nothing -> expr
        Just (newId, newType) -> V $ F (FVar newType (Qualified ByNullSourcePos newId))
      _ -> expr

{- IO utility. Reads a CoreFn module from a source file. Probably this should be somewhere else?

-}
decodeModuleIO :: FilePath -> IO (Module (Bind Ann) PurusType PurusType Ann)
decodeModuleIO path =
  Aeson.eitherDecodeFileStrict' path >>= \case
    Left err -> throwIO $ userError err
    Right modx -> pure modx

{- Mashup of `foldM` and `transverseScope`.
-}
foldMScopeViaExp ::
  (Monad f) =>
  b ->
  (b -> Exp x t (Var (BVar t) a) -> f b) ->
  [Scope (BVar t) (Exp x t) (Var (BVar t) a)] ->
  f b
foldMScopeViaExp e _ [] = pure e
foldMScopeViaExp e f (x : xs) = do
  let unscopedX = join <$> fromScope x
  this <- f e unscopedX
  foldMScopeViaExp this f xs

-- Exp x t (Var l a) -> Var l (Exp x t a)

distributeExp :: Var l (Exp x t a) -> Exp x t (Var l a)
distributeExp = \case
  B bv -> pure (B bv)
  F fv -> F <$> fv

-- something is wrong with my attempts to write a plated instance and I dunno how to fix it,
-- but specific traverals seem to work, so this should work?
transformExp ::
  forall x t f.
  (Monad f) =>
  (Exp x t (Vars t) -> f (Exp x t (Vars t))) ->
  Exp x t (Vars t) ->
  f (Exp x t (Vars t))
transformExp f = \case
  LamE bv e -> LamE bv <$> transformScope e
  CaseE t e alts ->
    let goAlt :: Alt x t (Exp x t) (Vars t) -> f (Alt x t (Exp x t) (Vars t))
        goAlt (UnguardedAlt bs pats scoped) = UnguardedAlt bs pats <$> transformScope scoped
     in CaseE t <$> runTransform e <*> traverse goAlt alts
  LetE binds decls scoped ->
    let goDecls :: BindE t (Exp x t) (Vars t) -> f (BindE t (Exp x t) (Vars t))
        goDecls = \case
          NonRecursive ident bvix expr ->
            NonRecursive ident bvix <$> transformScope expr
          Recursive xs -> Recursive <$> traverse (\(i, x) -> (i,) <$> transformScope x) xs
     in LetE binds <$> traverse goDecls decls <*> transformScope scoped
  AppE e1 e2 -> AppE <$> runTransform e1 <*> runTransform e2
  AccessorE x t pss e -> AccessorE x t pss <$> runTransform e
  ObjectUpdateE x t e cf fs ->
    (\e' fs' -> ObjectUpdateE x t e' cf fs')
      <$> runTransform e
      <*> traverse (\(nm, expr) -> (nm,) <$> runTransform expr) fs
  LitE t lit -> LitE t <$> traverse runTransform lit
  V a -> pure (V a)
  TyAbs bv e -> TyAbs bv <$> runTransform e
  TyInstE t e -> TyInstE t <$> runTransform e
  where
    runTransform :: Exp x t (Vars t) -> f (Exp x t (Vars t))
    runTransform x = transformExp f x >>= f

    transformScope ::
      Scope (BVar t) (Exp x t) (Vars t) ->
      f (Scope (BVar t) (Exp x t) (Vars t))
    transformScope scoped = do
      let unscoped = join <$> fromScope scoped
      transformed <- runTransform unscoped
      pure $ toScope (F <$> transformed)

stripTypeAbstractions :: Exp x t a -> ([(Int, Ident, KindOf t)], Exp x t a)
stripTypeAbstractions = \case
  TyAbs (BVar i k nm) inner -> first ((i, nm, k) :) $ stripTypeAbstractions inner
  other -> ([], other)

isSelfRecursiveNR :: BindE t (Exp x t) (Vars t) -> Bool
isSelfRecursiveNR (NonRecursive ident indx body) = containsBVar ident indx body
isSelfRecursiveNR _ = False

containsBVar :: Ident -> Int -> Scope (BVar t) (Exp x t) (Vars t) -> Bool
containsBVar idnt indx expr =
  any
    ( \case
        V (B (BVar bvix _ bvident)) -> bvix == indx && idnt == bvident
        _ -> False
    )
    subExpressions
  where
    subExpressions = (join <$> fromScope expr) ^.. cosmos

-- Builtins shouldn't be inlined because they can't be.
-- TODO/REVIEW: Figure out whether it's necessary to *monomorphize*
--              polymorphic builtins. (Trivial to implement if needed)
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

inlineable :: Qualified Ident -> Bool
inlineable nm = not (isConstructor nm || isBuiltin nm)

mkBVar :: Ident -> Int -> t -> BVar t
mkBVar idnt indx ty = BVar indx ty idnt

unBVar :: BVar t -> (Ident, Int)
unBVar (BVar indx _ idnt) = (idnt, indx)


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
foldBinds :: forall x t r
           . (     r
                -> (Ident,Int)
                -> Scope (BVar t) (Exp x t) (Vars t)
                -> r
             ) -> r
               -> [BindE t (Exp x t) (Vars t)]
               -> r
foldBinds _ e [] = e
foldBinds f e (x:xs) = case x of
  NonRecursive nm i b -> foldBinds f (f e (nm,i) b) xs
  Recursive recBinds ->
    let e' = foldl' (\acc (nm,b) -> f acc nm b) e recBinds
    in foldBinds f e' xs


foldMBindsWith :: forall (m :: * -> *) x t r
               . Monad m
              => (r -> Ident -> Int -> Scope (BVar t) (Exp x t) (Vars t) -> m r)
              -> (r -> [((Ident,Int),Scope (BVar t) (Exp x t) (Vars t))] -> m r)
              -> r
              -> [BindE t (Exp x t) (Vars t)]
              -> m r
foldMBindsWith _ _  e [] = pure e
foldMBindsWith fNonRec fRec e (x:xs) = case x of
  NonRecursive nm i b -> fNonRec e nm i b >>= \e' -> foldMBindsWith fNonRec fRec e' xs
  Recursive recBinds  -> fRec e recBinds >>= \e' -> foldMBindsWith fNonRec fRec e' xs


traverseAlt ::
  forall (f :: * -> *) x t.
  (Functor f) =>
  (Scope (BVar t) (Exp x t) (Vars t) -> f (Scope (BVar t) (Exp x t) (Vars t))) ->
  Alt x t (Exp x t) (Vars t) ->
  f (Alt x t (Exp x t) (Vars t))
traverseAlt f (UnguardedAlt _REMOVE pat body) = UnguardedAlt _REMOVE pat <$> f body

mapAlt ::
  forall x t.
  (Scope (BVar t) (Exp x t) (Vars t) -> (Scope (BVar t) (Exp x t) (Vars t))) ->
  Alt x t (Exp x t) (Vars t) ->
  Alt x t (Exp x t) (Vars t)
mapAlt f alt = runIdentity . traverseAlt (pure . f) $ alt

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
  Monad m =>
  (Exp x t (Vars t) -> m (Exp x t (Vars t))) ->
  Scope (BVar t) (Exp x t) (Vars t) ->
  m (Scope (BVar t) (Exp x t) (Vars t))
viaExpM f scoped = do
  let unscoped = join <$> fromScope scoped
  transformed <- f unscoped
  pure $ abstract (\case B bv -> Just bv;_ -> Nothing) transformed 

allBoundVars :: forall x t. Ord t => Exp x t (Vars t) -> [BVar t]
allBoundVars e = S.toList . S.fromList $ flip mapMaybe everything $ \case
  V (B bv) -> Just bv
  _ -> Nothing
  where
    everything = e ^.. cosmos

-- stupid, but `\e -> join <$> fromScope e`` is probably the most duplicated code phrase
-- in this whole project
toExp :: Scope (BVar t) (Exp x t) (Vars t) -> Exp x t (Vars t)
toExp = fmap join . fromScope

fromExp :: Exp x t (Vars t) -> Scope (BVar t) (Exp x t) (Vars t)
fromExp = abstract $ \case B bv -> Just bv; _ -> Nothing
-- N.B. we're using Set instead of [] mainly to ensure that everything has the same order

allDeclIdentifiers :: forall x t. Ord t => [BindE t (Exp x t) (Vars t)] -> Set (Ident, Int)
allDeclIdentifiers [] = S.empty
allDeclIdentifiers (b : rest) = case b of
  NonRecursive nm indx _ -> S.insert (nm, indx) $ allDeclIdentifiers rest
  Recursive xs ->
    let rest' = allDeclIdentifiers rest
     in foldl' (\acc ((nm, indx), _) -> S.insert (nm, indx) acc) rest' xs

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
  LetE _REMOVE bs e ->
    let bs' = mapBind (const goScope) <$> bs
        e' = goScope e
     in LetE _REMOVE bs' e'
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
  forall x t (f :: * -> *).
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
  LetE _REMOVE bs body ->
    let bs' = mapBind (const goScope) <$> bs
        body' = goScope body
    in LetE _REMOVE bs' body'
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
   goAlt (UnguardedAlt _REMOVE pat body) =
     let pat' = goPat pat
         body' = goScope body
     in UnguardedAlt _REMOVE pat' body'

   goPat :: Pat x t (Exp x t) (Vars t) -> Pat x t (Exp x t) (Vars t)
   goPat = \case
     VarP nm indx t -> VarP nm indx (f t)
     LitP (ObjectL x fs) -> LitP . ObjectL x $ fmap goPat  <$> fs
     ConP tn cn ps -> ConP tn cn $ goPat <$> ps
     other -> other


   goBV :: BVar t -> BVar t
   goBV (BVar bvIx bvTy bvNm) = BVar bvIx (f bvTy) bvNm

   goScope :: Scope (BVar t) (Exp x t) (Vars t) -> Scope (BVar t) (Exp x t) (Vars t)
   goScope = viaExp go

   go = transformTypesInExp f


updateTypes ::
  forall x t.
  (TypeLike t, Pretty t, Pretty (KindOf t), Show (Exp x t (Vars t))) =>
  [(Text, t)] ->
  Exp x t (Vars t) ->
  Exp x t (Vars t)
updateTypes vars e = doTrace "updateTypes" msg result
  where
    msg =
      prettify
        [ "UPDATE:\n" <> prettyAsStr vars
        , "INPUT EXPR:\n" <> prettyAsStr e
        , "RESULT EXPR:\n " <> prettyAsStr result
        , "RESULT TY:\n" <> prettyAsStr (expTy id result)
        , "RESULT EXPR (RAW):\n" <> show result
        ]
    result = updateTypes' vars e

updateTypes' ::
  forall x t.
  (TypeLike t, Pretty t, Pretty (KindOf t), Show (Exp x t (Vars t))) =>
  [(Text, t)] ->
  Exp x t (Vars t) ->
  Exp x t (Vars t)
updateTypes' vars = \case
  V x -> case x of
    B (BVar bvix bvty bvnm) ->
      V . B $ BVar bvix (f vars bvty) bvnm
    F (FVar ty ident) -> V . F $ FVar (f vars ty) ident
  AppE e1 e2 -> AppE (updateTypes vars e1) (updateTypes vars e2)
  LamE (BVar bvix bvty bvnm) body ->
    let bv = BVar bvix (f vars bvty) bvnm
        body' = goScope vars body
     in LamE bv body'
  -- TODO: Remove the "bindings" arg in LetE. We aggresively ignore it everywhere
  LetE n bs e ->
    let bs' = goBind vars <$> bs
        e' = goScope vars e
     in LetE n bs' e'
  LitE t lit ->
    let t' = f vars t
        lit' = updateTypes vars <$> lit
     in LitE t' lit'
  AccessorE x t str e ->
    let t' = f vars t
        e' = updateTypes vars e
     in AccessorE x t' str e'
  ObjectUpdateE x t e copy fs ->
    let t' = f vars t
        e' = updateTypes vars e
        fs' = fmap (updateTypes vars) <$> fs
     in ObjectUpdateE x t' e' copy fs'
  TyInstE t e ->
    let t' = f vars t
        e' = updateTypes vars e
     in TyInstE t' e'
  -- If we cross a TyAbs binder, we have tyvar shadowing and should remove the bound tyvar from our set
  TyAbs bv@(BVar _ _ bvid) e ->
    let vars' = filter (\x -> fst x /= runIdent bvid) vars
     in TyAbs bv $ updateTypes vars' e
  CaseE ty scrut alts ->
    let ty' = f vars ty
        scrut' = updateTypes vars scrut
        alts' = goAlt vars <$> alts
     in CaseE ty' scrut' alts'
  where
    f :: [(Text, t)] -> t -> t
    f vs ty =
      quantify
        . replaceAllTypeVars vs
        $ unQuantify ty

    goScope :: [(Text, t)] -> Scope (BVar t) (Exp x t) (Vars t) -> Scope (BVar t) (Exp x t) (Vars t)
    goScope vs scoped =
      let unscoped = join <$> fromScope scoped
          effed = updateTypes vs unscoped
       in F <$> toScope effed

    goBind :: [(Text, t)] -> BindE t (Exp x t) (Vars t) -> BindE t (Exp x t) (Vars t)
    goBind vs = \case
      NonRecursive nrid nrix scoped -> NonRecursive nrid nrix $ goScope vs scoped
      Recursive xs -> Recursive $ fmap (goScope vs) <$> xs

    goAlt :: [(Text, t)] -> Alt x t (Exp x t) (Vars t) -> Alt x t (Exp x t) (Vars t)
    goAlt vs (UnguardedAlt _bs pat scoped) =
      let pat' = goPat vs pat
       in UnguardedAlt _bs pat' (goScope vs scoped)

    goPat :: [(Text, t)] -> Pat x t (Exp x t) (Vars t) -> Pat x t (Exp x t) (Vars t)
    goPat vs = \case
      VarP vid vix vty -> VarP vid vix $ f vs vty
      WildP -> WildP
      LitP lit -> LitP $ goPat vs <$> lit
      ConP tn cn ps -> ConP tn cn $ goPat vs <$> ps

tyAbstractExpr ::
  [(TypeVarVisibility, Text, KindOf PurusType)] ->
  Exp WithObjects PurusType (Vars PurusType) ->
  Monomorphizer (Exp WithObjects PurusType (Vars PurusType))
tyAbstractExpr [] e = pure e
tyAbstractExpr ((_, var, kind) : rest) e = do
  bvix <- freshUnique
  let bv = BVar bvix kind (Ident var)
  e' <- tyAbstractExpr rest e
  pure $ TyAbs bv e'

-- put this somewhere else

instance Plated SourceType where
  plate f = \case
    tu@(TUnknown _ _) -> pure tu
    tv@(TypeVar _ _ _) -> pure tv
    tstr@(TypeLevelString _ _) -> pure tstr
    tint@(TypeLevelInt _ _) -> pure tint
    twild@(TypeWildcard _ _) -> pure twild
    tcon@(TypeConstructor _ _) -> pure tcon
    top@(TypeOp _ _) -> pure top
    TypeApp a t1 t2 -> TypeApp a <$> f t1 <*> f t2
    KindApp a t1 t2 -> KindApp a <$> f t1 <*> f t2
    ForAll a vis var mk innerTy scop ->
      (\mk' innerTy' -> ForAll a vis var mk' innerTy' scop)
        <$> f mk
        <*> f innerTy
    ConstrainedType a constraint t -> ConstrainedType a <$> goConstraint f constraint <*> f t
    Skolem a txt mk i scop -> (\mk' -> Skolem a txt mk' i scop) <$> f mk
    REmpty a -> pure $ REmpty a
    RCons a l x xs -> RCons a l <$> f x <*> f xs
    KindedType a t1 t2 -> KindedType a <$> f t1 <*> f t2
    BinaryNoParensType a t1 t2 t3 -> BinaryNoParensType a <$> f t1 <*> f t2 <*> f t3
    ParensInType a t -> ParensInType a <$> f t
    where
      goConstraint ::
        forall f.
        (Applicative f) =>
        (SourceType -> f SourceType) ->
        Constraint SourceAnn ->
        f (Constraint SourceAnn)
      goConstraint g (Constraint a cn kargs args cdata) =
        (\kargs' args' -> Constraint a cn kargs' args' cdata)
          <$> traverse g kargs
          <*> traverse g args

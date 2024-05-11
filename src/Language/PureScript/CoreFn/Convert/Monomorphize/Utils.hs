{-# OPTIONS_GHC -Wno-orphans #-} -- has to be here (more or less)
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiWayIf #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use if" #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TypeApplications #-}
module Language.PureScript.CoreFn.Convert.Monomorphize.Utils  where

import Prelude

import Language.PureScript.CoreFn.Expr (PurusType)
import Language.PureScript.CoreFn.Convert.IR (_V, Exp(..), FVar(..), BindE(..), Ty (..), pattern (:~>), BVar (..), flattenBind, expTy', abstractMany, mkBindings, Alt (..), Lit (..), expTy)
import Language.PureScript.Names (Ident(..), ModuleName (..), QualifiedBy (..), Qualified (..), pattern ByNullSourcePos)
import Language.PureScript.Types
    ( SourceType, TypeVarVisibility (TypeVarInvisible), genPureName )
import Language.PureScript.CoreFn.FromJSON ()
import Data.Text qualified as T
import Data.Map (Map)
import Data.Map qualified as M
import Control.Lens
    ( _2, view, Indexable (..), (<&>) )
import Control.Monad.RWS.Class (gets, modify', MonadReader (..))
import Control.Monad.RWS (RWST(..), MonadTrans (..))
import Control.Monad.Except (throwError)
import Language.PureScript.CoreFn.Utils (Context)
import Data.Text (Text)
import Language.PureScript.CoreFn.Convert.DesugarCore (WithObjects)
import Bound.Var (Var(..))
import Bound.Scope (instantiateEither, Scope (..), abstract, abstractEither, splat, toScope, fromScope, mapScope)
import Data.Bifunctor (first, Bifunctor (..))
import Data.Maybe (fromMaybe)
import Data.List (sortOn, find)
import Protolude.List (ordNub)
import Language.PureScript.CoreFn.TypeLike (TypeLike(..))
import Control.Lens ((^?))
import Control.Lens.IndexedPlated
import Bound (Scope(unscope), Bound (..))
import Control.Lens.Plated
import Bound (instantiate)
import Control.Monad (join)
import Control.Lens.Type (IndexedTraversal')
import Data.Functor.Identity (Identity(..))
import Language.PureScript.Environment (pattern (:->))
import Language.PureScript.CoreFn.Pretty (prettyTypeStr)


transverseScopeAndVariables ::
  (Monad exp, Traversable exp, Applicative f) =>
  (exp a -> f fvar1) ->
  (Var bvar1 fvar1 -> exp (Var bvar2 fvar2)) ->
  Scope bvar1 exp a ->
  f (Scope bvar2 exp fvar2)
transverseScopeAndVariables f g expr = toScope . (g =<<) <$> traverse (traverse f) (unscope expr)

transverseScopeViaExp :: Applicative f
                     => (Exp x t a -> f (Exp x t a))
                     -> Scope (BVar t) (Exp x t) a
                     -> f (Scope (BVar t) (Exp x t) a)
transverseScopeViaExp f scope
  = toScope  . (sequence =<<) <$> traverse (traverse f) (unscope scope)

instance Plated (Exp x t a) where
  plate = go
   where
     go :: forall f
         . ( Applicative f)
        => (Exp x t a -> f  (Exp x t a))
        -> Exp x t a
        -> f (Exp x t  a)
     go  tfun = \case
      LamE t bv e ->  LamE t bv <$> helper e
      CaseE t es alts ->
        let goAlt ::  Alt x t (Exp x t) a -> f (Alt x t (Exp x t) a)
            goAlt (UnguardedAlt bs pats scoped) = UnguardedAlt bs pats <$> helper scoped
        in CaseE t <$> traverse tfun es <*>  traverse goAlt alts
      LetE binds decls scoped ->
        let goDecls :: BindE t (Exp x t) a -> f (BindE t (Exp x t) a)
            goDecls = \case
              NonRecursive ident scopd ->
                NonRecursive ident <$> helper scopd
              Recursive xs ->
                Recursive <$> traverse (\(i,x) -> (i,) <$> helper x) xs
        in LetE binds <$> traverse goDecls decls <*> helper scoped
      AppE e1 e2 -> AppE <$> tfun e1 <*> tfun e2
      AccessorE x t pss e -> AccessorE x t pss <$> tfun e
      ObjectUpdateE x t e cf fs -> (\e' fs' -> ObjectUpdateE x t e' cf fs')
                                   <$> tfun  e
                                   <*> traverse (\(nm,expr) -> (nm,) <$> tfun expr) fs
      LitE t lit -> LitE t <$> traverseLit lit
      other -> tfun other
      where
        traverseLit :: Lit x (Exp x t a)
                    -> f (Lit x (Exp x t a))
        traverseLit  = \case
          IntL i -> pure $ IntL i
          NumL d -> pure $ NumL d
          StringL str -> pure $ StringL str
          CharL char -> pure $ CharL char
          BoolL b -> pure $ BoolL b
          ArrayL xs -> ArrayL <$> traverse tfun  xs
          ConstArrayL xs -> ConstArrayL <$> pure xs
          ObjectL x fs -> ObjectL x <$> traverse (\(str,e) -> (str,) <$> tfun e) fs

        helper ::  Scope (BVar t) (Exp x t) a -> f (Scope (BVar t) (Exp x t) a)
        helper expr = toScope  . (sequence =<<) <$> traverse (traverse tfun) (unscope expr)



-- TODO: better error messages
data MonoError
 = MonoError String deriving (Show)

-- ok we need monads
data MonoState = MonoState {
  {- Original Identifier -> Type -> (Fresh Ident, Expr)
  -}
  visited :: Map Ident (Map SourceType (Ident, Exp WithObjects PurusType (FVar PurusType))),
  unique :: Int
}

type Monomorphizer a = RWST (ModuleName, [BindE PurusType (Exp WithObjects PurusType) (FVar PurusType)]) () MonoState (Either MonoError)  a

getModName :: Monomorphizer ModuleName
getModName = ask <&> fst

getModBinds :: Monomorphizer [BindE PurusType (Exp WithObjects PurusType) (FVar PurusType)]
getModBinds = ask <&> snd

note ::  String -> Maybe b -> Monomorphizer b
note  err = \case
  Nothing -> throwError $ MonoError err
  Just x -> pure x

type IR_Decl = BindE PurusType (Exp WithObjects PurusType) (FVar PurusType)

defInstantiate :: Scope b (Exp x ty) a -> Exp x ty (Var b a)
defInstantiate scoped = instantiateEither  (either (V . B) (V . F)) scoped

freshen :: Ident -> Monomorphizer Ident
freshen ident = do
  u <- gets unique
  modify' $ \(MonoState v _) -> MonoState v (u + 1)
  let uTxt = T.pack (show u)
  case ident of
    Ident t -> pure $ Ident $ t <> "_$$" <> uTxt
    GenIdent (Just t) i -> pure $ GenIdent (Just $ t <> "_$$" <> uTxt) i -- we only care about a unique ord property for the maps
    GenIdent Nothing i  -> pure $ GenIdent (Just $ "var_$$" <> uTxt) i
    -- other two shouldn't exist at this stage
    other -> pure other

qualifyNull :: Ident -> Qualified Ident
qualifyNull = Qualified ByNullSourcePos

gLet ::
  [BindE PurusType (Exp WithObjects PurusType) (FVar PurusType)] ->
  Exp WithObjects PurusType (FVar PurusType) ->
  Exp WithObjects PurusType (FVar PurusType)
gLet binds e = LetE bindings binds (abstract (abstractMany allBoundIdents) e)
  where
    bindings = mkBindings allBoundIdents
    allBoundIdents = uncurry (flip FVar . qualifyNull) <$> second (expTy' F) <$> concatMap flattenBind binds

type WithObjs t f = Exp WithObjects t (f t)

type Vars t = (Var (BVar t) (FVar t))


-- I *think* all CTors should be translated to functions at this point?
-- TODO: We can make sure the variables are well-scoped too
updateVarTyE ::  Ident
            -> PurusType
            -> Exp WithObjects PurusType (FVar PurusType)
            -> Exp WithObjects PurusType (FVar PurusType)
updateVarTyE  ident ty e = transform  goVar e
  where
    goVar :: Exp WithObjects PurusType (FVar PurusType)  -> Exp WithObjects PurusType (FVar PurusType)
    goVar  expr = case expr ^? _V of
      Just (FVar _ (Qualified q@(BySourcePos _) varId)) | varId == ident -> V  $ FVar ty (Qualified q ident)
      _ -> expr

updateVarTyS :: forall x t. Int -> Ident -> t -> Scope (BVar t) (Exp x t) (FVar t) -> Scope (BVar t) (Exp x t) (FVar t)
updateVarTyS ix ident ty scoped = mapScope goBound goFree scoped
  where
    goBound :: BVar t -> BVar t
    goBound bv@(BVar bvIx _ bvIdent)
      | bvIx == ix && bvIdent == ident = BVar bvIx ty ident
      | otherwise = bv

    goFree :: FVar t -> FVar t
    goFree fv@(FVar _ (Qualified q@(BySourcePos _) varId))
      | varId == ident = FVar ty (Qualified q varId)
      | otherwise = fv
    goFree other = other

-- TODO: Eventually we shouldn't need this but it's useful to throw errors
--       while debugging if we get something that's not a function
unsafeApply ::
  Exp WithObjects PurusType (FVar PurusType) ->
  [Exp WithObjects PurusType (FVar PurusType)] ->
  Exp WithObjects PurusType (FVar PurusType)
unsafeApply e (arg:args)= case expTy F e of
  (_ :-> _) -> unsafeApply (AppE e arg) args
  other -> Prelude.error $ "Unexpected argument to unsafeApply:" <> prettyTypeStr other
unsafeApply e [] = e


findInlineDeclGroup ::
  Ident ->
  [BindE PurusType (Exp x ty) a] ->
  Maybe (BindE PurusType (Exp x ty) a)
findInlineDeclGroup _ [] = Nothing
findInlineDeclGroup ident (NonRecursive ident' expr:rest)
  | ident == ident' = Just $ NonRecursive ident' expr
  | otherwise = findInlineDeclGroup ident rest
findInlineDeclGroup ident (Recursive xs:rest) = case  find (\x -> snd (fst x) == ident) xs of
  Nothing -> findInlineDeclGroup ident rest
         -- idk if we need to specialize the whole group?
  Just _ -> Just (Recursive xs)

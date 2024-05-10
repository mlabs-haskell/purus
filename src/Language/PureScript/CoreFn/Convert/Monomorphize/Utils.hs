{-# OPTIONS_GHC -Wno-orphans #-} -- has to be here (more or less)
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiWayIf #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use if" #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE PartialTypeSignatures #-}
module Language.PureScript.CoreFn.Convert.Monomorphize.Utils  where

import Prelude

import Language.PureScript.CoreFn.Expr (PurusType)
import Language.PureScript.CoreFn.Convert.IR (_V, Exp(..), FVar(..), BindE(..), Ty (..), pattern (:~>), BVar (..), flattenBind, expTy', abstractMany, mkBindings, Alt (..), Lit (..))
import Language.PureScript.Names (Ident(..), ModuleName (..), QualifiedBy (..), Qualified (..), pattern ByNullSourcePos)
import Language.PureScript.Types
    ( SourceType, TypeVarVisibility (TypeVarInvisible), genPureName )
import Language.PureScript.CoreFn.FromJSON ()
import Data.Text qualified as T
import Data.Map (Map)
import Data.Map qualified as M
import Control.Lens
    ( _2, view, Indexable (..) )
import Control.Monad.RWS.Class (gets, modify')
import Control.Monad.RWS (RWST(..))
import Control.Monad.Except (throwError)
import Language.PureScript.CoreFn.Utils (Context)
import Data.Text (Text)
import Language.PureScript.CoreFn.Convert.DesugarCore (WithObjects)
import Bound.Var (Var(..))
import Bound.Scope (instantiateEither, Scope (..), abstract, abstractEither, splat, toScope)
import Data.Bifunctor (first, Bifunctor (..))
import Data.Maybe (fromMaybe)
import Data.List (sortOn)
import Protolude.List (ordNub)
import Language.PureScript.CoreFn.TypeLike (TypeLike(..))
import Control.Lens ((^?))
import Control.Lens.IndexedPlated
import Bound (Scope(unscope), Bound (..))
import Control.Lens.Plated
import Bound (instantiate)
import Control.Monad (join)
import Control.Lens.Type (IndexedTraversal')

{- This instance is morally the same thing as:

traverseExp :: forall x t a f
             .  Applicative f
             => (a -> Var (BVar t) a)
             -> (Exp x t (Var (BVar t) a) -> f (Exp x t (Var (BVar t) a)))
             -> Exp x t (Var (BVar t) a)
             -> f (Exp x t (Var (BVar t) a))
traverseExp ifun tfun = \case
  LamE t bv e ->  LamE t bv  <$> helper e
  CaseE t es alts ->
    let goAlt :: Alt x t (Exp x t) (Var (BVar t) a) -> f (Alt x t (Exp x t) (Var (BVar t) a))
        goAlt (UnguardedAlt bs pats scoped) = UnguardedAlt bs pats <$> helper scoped
    in CaseE t <$> traverse tfun es <*>  traverse goAlt alts
  LetE binds decls scoped ->
    let mm = join <$> instantiateEither (either (V . B) (V . F)) scoped
        bop = fmap (fmap ifun) <$> tfun mm

        goDecls :: BindE t (Exp x t) (Var (BVar t) a) -> f (BindE t (Exp x t) (Var (BVar t) a))
        goDecls = \case
          NonRecursive ident scopd -> NonRecursive ident <$> helper scopd
          Recursive xs -> Recursive <$> traverse (\(i,x) -> (i,) <$> helper x) xs
    in LetE binds <$> traverse goDecls decls <*> (toScope <$> bop)
  AppE e1 e2 -> AppE <$> tfun e1 <*> tfun e2
  AccessorE x t pss e -> AccessorE x t pss <$> tfun e
  ObjectUpdateE x t e cf fs -> (\e' fs' -> ObjectUpdateE x t e' cf fs')
                               <$> tfun e
                               <*> traverse (\(nm,expr) -> (nm,) <$> tfun expr) fs
  LitE t lit -> LitE t <$> traverseLit lit
  other -> tfun other
 where
   traverseLit :: Lit x (Exp x t (Var (BVar t) a)) -> f (Lit x (Exp x t (Var (BVar t) a)))
   traverseLit = \case
     IntL i -> pure $ IntL i
     NumL d -> pure $ NumL d
     StringL str -> pure $ StringL str
     CharL char -> pure $ CharL char
     BoolL b -> pure $ BoolL b
     ArrayL xs -> ArrayL <$> traverse tfun xs
     ConstArrayL xs -> ConstArrayL <$> pure xs
     ObjectL x fs -> ObjectL x <$> traverse (\(str,e) -> (str,) <$> tfun e) fs

   helper :: Scope (BVar t) (Exp x t) (Var (BVar t) a) -> f (Scope (BVar t) (Exp x t) (Var (BVar t) a))
   helper expr = let mm = join <$> instantiateEither (either (V . B) (V . F)) expr
                     bop = fmap (fmap ifun) <$> tfun mm
                 in toScope <$> bop

-}

instance Plated (Exp x t (Var (BVar t) a)) where

  plate = go
   where
     -- yes it really needs that grotesque type signature lol
     go :: forall f
         . ( Applicative f)
        =>  (Exp x t (Var (BVar t) a) -> f  (Exp x t (Var (BVar t) a)))
        -> Exp x t (Var (BVar t) a)
        -> f (Exp x t (Var (BVar t) a))
     go  tfun = \case
      LamE t bv e ->  LamE t bv <$> helper e
      CaseE t es alts ->
        let goAlt ::  Alt x t (Exp x t) (Var (BVar t) a) -> f (Alt x t (Exp x t) (Var (BVar t) a))
            goAlt (UnguardedAlt bs pats scoped) = UnguardedAlt bs pats <$> helper scoped
        in CaseE t <$> traverse tfun es <*>  traverse goAlt alts
      LetE binds decls scoped ->
        let goDecls :: BindE t (Exp x t) (Var (BVar t) a) -> f (BindE t (Exp x t) (Var (BVar t) a))
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
        traverseLit :: Lit x (Exp x t (Var (BVar t) a))
                    -> f (Lit x (Exp x t (Var (BVar t) a)))
        traverseLit  = \case
          IntL i -> pure $ IntL i
          NumL d -> pure $ NumL d
          StringL str -> pure $ StringL str
          CharL char -> pure $ CharL char
          BoolL b -> pure $ BoolL b
          ArrayL xs -> ArrayL <$> traverse tfun  xs
          ConstArrayL xs -> ConstArrayL <$> pure xs
          ObjectL x fs -> ObjectL x <$> traverse (\(str,e) -> (str,) <$> tfun e) fs

        helper ::  Scope (BVar t) (Exp x t) (Var (BVar t) a) -> f (Scope (BVar t) (Exp x t) (Var (BVar t) a))
        helper expr = let mm = join <$> splat (V . F) (V . B) expr
                          pow :: Var (BVar t) a -> Var (BVar t) (Var (BVar t) a)
                          pow = \case
                            B bv -> B bv
                            F fv -> F (F fv)
                      in toScope . fmap pow <$> tfun mm

-- TODO: better error messages
data MonoError
 = MonoError Context String deriving (Show)

-- ok we need monads
data MonoState = MonoState {
  {- Original Identifier -> Type -> (Fresh Ident, Expr)
  -}
  visited :: Map Ident (Map SourceType (Ident, Exp WithObjects PurusType (FVar PurusType))),
  unique :: Int
}

type Monomorphizer a = RWST (ModuleName, [BindE PurusType (Exp WithObjects PurusType) (FVar PurusType)]) () MonoState (Either MonoError)  a

note :: Context  -> String -> Maybe b -> Monomorphizer b
note d err = \case
  Nothing -> throwError $ MonoError d err
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

type WithObjs t a = Exp WithObjects t a

type Vars t = (Var (BVar t) (FVar t))


-- I *think* all CTors should be translated to functions at this point?
-- TODO: We can make sure the variables are well-scoped too
updateVarTy ::  Ident -> PurusType -> WithObjs PurusType (Vars PurusType) -> WithObjs PurusType (Vars PurusType)
updateVarTy  ident ty e = transform  goVar e
  where
    goVar ::   WithObjs PurusType (Vars PurusType) -> WithObjs PurusType (Vars PurusType)
    goVar  expr = case expr ^? _V of
      Just (F (FVar _ (Qualified q@(BySourcePos _) varId))) | varId == ident -> V . F $ FVar ty (Qualified q ident)
      Just (B (BVar i _ ident')) | ident == ident'  -> V . B $ BVar i ty ident
      _ -> expr

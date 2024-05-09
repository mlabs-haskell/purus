{-# OPTIONS_GHC -Wno-orphans #-} -- has to be here (more or less)
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiWayIf #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use if" #-}
module Language.PureScript.CoreFn.Convert.Monomorphize.Utils  where

import Prelude

import Language.PureScript.CoreFn.Expr (PurusType)
import Language.PureScript.CoreFn.Convert.IR (_V, Exp(..), FVar(..), BindE(..), Ty (..), pattern (:~>), BVar (..), flattenBind, expTy', abstractMany, mkBindings, Alt (..))
import Language.PureScript.Names (Ident(..), ModuleName (..), QualifiedBy (..), Qualified (..), pattern ByNullSourcePos)
import Language.PureScript.Types
    ( SourceType, TypeVarVisibility (TypeVarInvisible), genPureName )
import Language.PureScript.CoreFn.FromJSON ()
import Data.Text qualified as T
import Data.Map (Map)
import Data.Map qualified as M
import Control.Lens
    ( _2, view, Indexable )
import Control.Monad.RWS.Class (gets, modify')
import Control.Monad.RWS (RWST(..))
import Control.Monad.Except (throwError)
import Language.PureScript.CoreFn.Utils (Context)
import Data.Text (Text)
import Language.PureScript.CoreFn.Convert.DesugarCore (WithObjects)
import Bound.Var (Var(..))
import Bound.Scope (instantiateEither, Scope, abstract)
import Data.Bifunctor (first, Bifunctor (..))
import Data.Maybe (fromMaybe)
import Data.List (sortOn)
import Protolude.List (ordNub)
import Language.PureScript.CoreFn.TypeLike (TypeLike(..))
import Control.Lens ((^?))
import Control.Lens.IndexedPlated

-- probably should be in the IR module but that is already incomprehensibly verbose

type InstFun a t = (a -> V (BVar t) (FVar t))

instance IndexedPlated (InstFun a t) (Exp x t a) where
  iplate inst f = \case
    LitE ann ty lit -> Literal ann ty <$> traverseLit (indexed f d) lit
    AccessorE ann ty field e -> Accessor ann ty field <$>  indexed f d e
    ObjectUpdateE ann ty orig copyFields updateFields ->
      (\orig' updateFields' -> ObjectUpdate ann ty orig' copyFields updateFields')
      <$> indexed f d orig
      <*> traverse (sequenceA . second (indexed f d)) updateFields
    LamE ann ty ident body -> Abs ann ty ident <$> indexed f (M.insert ident (arg ty) d) body
    AppE ann fE argE -> App ann <$> indexed f d fE <*> indexed f d argE
    CaseE a ty scrutinees alternatives ->
      Case a ty <$> traverse (indexed f d) scrutinees <*> traverseAltE d f alternatives
    LetE a binds e ->
      LetE a <$> traverseBinds d f  binds <*> indexed f d e
    other -> pure other -- ctors and vars don't contain any sub-expressions
   where
     arg = undefined {-
       \case
       ForAll _ _ _ _ inner _ -> arg inner
       a :-> _ -> a
       other -> other
     -}
     traverseBinds :: forall p f
                    . (Indexable (InstFun a t) p, Applicative f)
                   => InstFun a t
                   -> p (Exp x t a) (f (Exp x t a))
                   -> [BindE t (Exp x t) a] -> f [BindE t (Exp x t) a]
     traverseBinds instF g binds = traverse (go cxt) binds
       where
         go :: InstFun a t -> BindE t (Exp x t) a -> f (BindE t (Exp x t) a)
         go gCxt = \case
           NonRecursive ann ident e ->
             let cxt' = M.insert ident (exprType e) gCxt
             in NonRec ann ident <$> indexed g cxt' e
           Recursive es -> Rec <$> goRecursive gCxt es
         goRecursive :: InstFun a t -> [((a, Ident), Exp x t a)] -> f [((a, Ident), Exp x t a)]
         goRecursive _ [] = pure []
         goRecursive gCxt (((ann,nm),e):rest) =
           let gCxt' = M.insert nm (exprType e) gCxt
           in (\x xs -> ((ann,nm),x):xs) <$> indexed g gCxt' e <*> goRecursive gCxt' rest
     traverseAltE :: forall p f
                   . (Indexable (InstFun a t) p, Applicative f)
                  => InstFun a t
                  -> p (Exp x t a) (f (Exp x t a))
                  -> [Alt x t (Exp x t) a]
                  -> f [Alt x t (Exp x t) a]
     traverseAltE instF g alts = traverse (go cxt) alts
       where
         go :: InstFun a t -> Alt x t (Exp x t) a -> f (Alt x t (Exp x t) a)
         go instG (UnguardedAlt binders result) =
           UnguardedAlt  binders
           <$> helper instG gogogo

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
    allBoundIdents = uncurry (flip FVar . qualifyNull) <$>   second (expTy' F) <$> concatMap flattenBind binds

-- I *think* all CTors should be translated to functions at this point?
-- TODO: We can make sure the variables are well-scoped too
updateVarTy ::  Ident -> PurusType -> Exp WithObjects PurusType (FVar PurusType) -> Exp WithObjects PurusType (FVar PurusType)
updateVarTy  ident ty = transform goVar
  where
    goVar ::  Exp WithObjects PurusType (FVar PurusType) -> Exp WithObjects PurusType (FVar PurusType)
    goVar  expr = case expr ^? _V of
      Just (FVar _ (Qualified q@(BySourcePos _) varId)) | varId == ident -> V $ FVar ty (Qualified q ident)
      _ -> expr

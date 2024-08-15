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
import Language.PureScript.CoreFn.Convert.IR.Utils

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
type Monomorphizer  = RWST (ModuleName, [BindE PurusType (Exp WithObjects PurusType) (Vars PurusType)]) () MonoState (Either MonoError)

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


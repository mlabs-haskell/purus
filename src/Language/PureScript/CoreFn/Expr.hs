{-# LANGUAGE TemplateHaskell #-}
module Language.PureScript.CoreFn.Expr where
import Prelude

import Control.Arrow ((***))

import GHC.Generics
import Data.Aeson (FromJSON, ToJSON)


import Language.PureScript.AST.Literals (Literal)
import Language.PureScript.CoreFn.Binders (Binder)
import Language.PureScript.Names (Ident, ProperName, ProperNameType(..), Qualified)
import Language.PureScript.PSString (PSString)
import Language.PureScript.Types (Type, SourceType)

import Control.Lens.TH (makePrisms)
import Control.Lens (Traversal', Lens')

type PurusType = SourceType -- Type ()

-- |
-- Data type for expressions and terms
--
data Expr a
  -- |
  -- A literal value
  --
  = Literal a PurusType (Literal (Expr a))
  -- |
  -- A data constructor (type name, constructor name, field names)
  --
  | Constructor a PurusType (ProperName 'TypeName) (ProperName 'ConstructorName) [Ident]
  -- |
  -- A record property accessor
  --
  | Accessor a PurusType PSString (Expr a)
  -- |
  -- Partial record update (original value, fields to copy (if known), fields to update)
  --
  | ObjectUpdate a PurusType (Expr a) (Maybe [PSString]) [(PSString, Expr a)]
  -- |
  -- Function introduction
  --
  | Abs a PurusType Ident (Expr a)
  -- |
  -- Function application
  --
  | App a PurusType (Expr a) (Expr a)
  -- |
  -- Variable
  --
  | Var a PurusType (Qualified Ident)
  -- |
  -- A case expression
  --
  | Case a PurusType [Expr a] [CaseAlternative a]
  -- |
  -- A let binding
  --
  | Let a PurusType [Bind a] (Expr a)
  deriving (Eq, Ord, Show, Functor, Generic)

eType :: Lens' (Expr a) PurusType
eType f = \case
  Literal ann ty lit -> (\t -> Literal ann t lit) <$> f ty
  Constructor a ty tNm cNm fs -> (\t -> Constructor a t tNm cNm fs) <$> f ty
  Accessor ann ty str e -> (\t -> Accessor ann t str e) <$> f ty
  ObjectUpdate ann ty e keep upd -> (\t -> ObjectUpdate ann t e keep upd) <$> f ty
  Abs a ty nm e -> (\t -> Abs a t nm e) <$> f ty
  App a ty e1 e2 -> (\t -> App a t e1 e2) <$> f ty
  Var a ty nm -> (\t -> Var a t nm) <$> f ty
  Case a ty es alts -> (\t -> Case a t es alts) <$> f ty
  Let a ty bs e -> (\t -> Let a t bs e) <$> f ty

instance FromJSON a => FromJSON (Expr a)
instance ToJSON a => ToJSON (Expr a)

exprType :: Expr a -> PurusType
exprType = \case
  Literal _ ty _ -> ty
  Constructor _ ty _ _ _ -> ty
  Accessor _ ty _ _ -> ty
  ObjectUpdate _ ty _ _ _ -> ty
  Abs _ ty _ _ -> ty
  App _ ty _ _ -> ty
  Var _ ty __ -> ty
  Case _ ty _ _ -> ty
  Let _ ty _ _ -> ty

-- |
-- A let or module binding.
--
data Bind a
  -- |
  -- Non-recursive binding for a single value
  --
  = NonRec a Ident (Expr a)
  -- |
  -- Mutually recursive binding group for several values
  --
  | Rec [((a, Ident), Expr a)] deriving (Eq, Ord, Show, Functor, Generic)

instance FromJSON a => FromJSON (Bind a)
instance ToJSON a => ToJSON (Bind a)

-- |
-- A guard is just a boolean-valued expression that appears alongside a set of binders
--
type Guard a = Expr a

-- |
-- An alternative in a case statement
--
data CaseAlternative a = CaseAlternative
  { -- |
    -- A collection of binders with which to match the inputs
    --
    caseAlternativeBinders :: [Binder a]
    -- |
    -- The result expression or a collect of guarded expressions
    --
  , caseAlternativeResult :: Either [(Guard a, Expr a)] (Expr a)
  } deriving (Eq, Ord, Show, Generic)

instance FromJSON a => FromJSON (CaseAlternative a)
instance ToJSON a => ToJSON (CaseAlternative a)

instance Functor CaseAlternative where

  fmap f (CaseAlternative cabs car) = CaseAlternative
    (fmap (fmap f) cabs)
    (either (Left . fmap (fmap f *** fmap f)) (Right . fmap f) car)

-- |
-- Extract the annotation from a term
--
extractAnn :: Expr a -> a
extractAnn (Literal a _ _) = a
extractAnn (Constructor a _ _   _ _) = a
extractAnn (Accessor a _ _ _) = a
extractAnn (ObjectUpdate a _ _ _ _) = a
extractAnn (Abs a _ _ _) = a
extractAnn (App a _ _ _) = a
extractAnn (Var a _ _) = a
extractAnn (Case a _ _ _) = a
extractAnn (Let a _ _ _) = a


-- |
-- Modify the annotation on a term
--
modifyAnn :: (a -> a) -> Expr a -> Expr a
modifyAnn f (Literal a b c)          = Literal (f a) b c
modifyAnn f (Constructor a b c d e)  = Constructor (f a) b c d e
modifyAnn f (Accessor a b c d)       = Accessor (f a) b c d
modifyAnn f (ObjectUpdate a b c d e) = ObjectUpdate (f a) b c d e
modifyAnn f (Abs a b c d)            = Abs (f a) b c d
modifyAnn f (App a b c d)            = App (f a) b c d
modifyAnn f (Var a b c)              = Var (f a) b c
modifyAnn f (Case a b c d)           = Case (f a) b c d
modifyAnn f (Let a b c d)            = Let (f a) b c d

makePrisms ''Expr
makePrisms ''Bind

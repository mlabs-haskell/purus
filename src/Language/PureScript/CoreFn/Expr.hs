{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE InstanceSigs #-}

module Language.PureScript.CoreFn.Expr where

import Prelude

import Control.Arrow ((***))

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics

import Language.PureScript.AST.Literals (Literal)
import Language.PureScript.CoreFn.Binders (Binder)
import Language.PureScript.Names (Ident, Qualified)
import Language.PureScript.PSString (PSString)
import Language.PureScript.Types (SourceType)

import Control.Lens.TH (makePrisms)
import Control.Lens.Combinators (Plated (plate))

type PurusType = SourceType -- Type ()

{- |
Data type for expressions and terms
-}
data Expr a
  = -- |
    -- A literal value
    Literal a PurusType (Literal (Expr a))
  | -- A record property accessor
    --
    Accessor a PurusType PSString (Expr a)
  | -- |
    -- Partial record update (original value, fields to copy (if known), fields to update)
    ObjectUpdate a PurusType (Expr a) (Maybe [PSString]) [(PSString, Expr a)]
  | -- |
    -- Function introduction
    Abs a PurusType Ident (Expr a)
  | -- |
    -- Function application
    App a (Expr a) (Expr a)
  | -- |
    -- Variable
    Var a PurusType (Qualified Ident)
  | -- |
    -- A case expression
    Case a PurusType [Expr a] [CaseAlternative a]
  | -- |
    -- A let binding
    Let a [Bind a] (Expr a)
  deriving (Eq, Ord, Show, Functor, Generic)

instance (FromJSON a) => FromJSON (Expr a)
instance (ToJSON a) => ToJSON (Expr a)

instance Plated (Expr a) where
  {-# INLINEABLE plate #-}
  plate :: forall f . Applicative f => 
    (Expr a -> f (Expr a)) -> Expr a -> f (Expr a)
  plate f = \case
    Literal a t lit -> Literal a t <$> traverse f lit
    Accessor a t s e -> Accessor a t s <$> f e
    ObjectUpdate a t e cf fs ->
      ObjectUpdate a t <$> f e <*> pure cf <*> traverse (traverse f) fs
    Abs a t bv e -> Abs a t bv <$> f e
    App a e1 e2 -> App a <$> f e1 <*> f e2
    x@(Var _ _ _) -> pure x
    Case a t scruts alts ->
      Case a t <$> traverse f scruts <*> traverse goAlt alts
    Let a decls body ->
      Let a <$> traverse goDecl decls <*> f body
    where
      goAlt :: CaseAlternative a -> f (CaseAlternative a)
      goAlt (CaseAlternative binds res) = 
        CaseAlternative binds <$> traverse f res
      goDecl :: Bind a -> f (Bind a)
      goDecl = \case
        NonRec a nm body -> NonRec a nm <$> f body
        Rec xs -> Rec <$> traverse (traverse f) xs

{- |
A let or module binding.
-}
data Bind a
  = -- |
    -- Non-recursive binding for a single value
    NonRec a Ident (Expr a)
  | -- |
    -- Mutually recursive binding group for several values
    Rec [((a, Ident), Expr a)]
  deriving (Eq, Ord, Show, Functor, Generic)

instance (FromJSON a) => FromJSON (Bind a)
instance (ToJSON a) => ToJSON (Bind a)

{- |
A guard is just a boolean-valued expression that appears alongside a set of binders
-}
type Guard a = Expr a

{- |
An alternative in a case statement
-}
data CaseAlternative a = CaseAlternative
  { caseAlternativeBinders :: [Binder a]
  -- ^
  -- A collection of binders with which to match the inputs
  , caseAlternativeResult :: Either [(Guard a, Expr a)] (Expr a)
  -- ^
  -- The result expression or a collect of guarded expressions
  }
  deriving (Eq, Ord, Show, Generic)

instance (FromJSON a) => FromJSON (CaseAlternative a)
instance (ToJSON a) => ToJSON (CaseAlternative a)

instance Functor CaseAlternative where
  fmap f (CaseAlternative cabs car) =
    CaseAlternative
      (fmap (fmap f) cabs)
      (either (Left . fmap (fmap f *** fmap f)) (Right . fmap f) car)

{- |
Extract the annotation from a term
-}
extractAnn :: Expr a -> a
extractAnn (Literal a _ _) = a
extractAnn (Accessor a _ _ _) = a
extractAnn (ObjectUpdate a _ _ _ _) = a
extractAnn (Abs a _ _ _) = a
extractAnn (App a _ _) = a
extractAnn (Var a _ _) = a
extractAnn (Case a _ _ _) = a
extractAnn (Let a _ _) = a

{- |
Modify the annotation on a term
-}
modifyAnn :: (a -> a) -> Expr a -> Expr a
modifyAnn f (Literal a b c) = Literal (f a) b c
modifyAnn f (Accessor a b c d) = Accessor (f a) b c d
modifyAnn f (ObjectUpdate a b c d e) = ObjectUpdate (f a) b c d e
modifyAnn f (Abs a b c d) = Abs (f a) b c d
modifyAnn f (App a b c) = App (f a) b c
modifyAnn f (Var a b c) = Var (f a) b c
modifyAnn f (Case a b c d) = Case (f a) b c d
modifyAnn f (Let a b c) = Let (f a) b c

makePrisms ''Expr
makePrisms ''Bind

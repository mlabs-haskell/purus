{- |
The core functional representation for binders
-}
module Language.PureScript.CoreFn.Binders where

import Prelude

import Language.PureScript.AST.Literals (Literal)
import Language.PureScript.Names (Ident, ProperName, ProperNameType (..), Qualified)
import Language.PureScript.Types

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics
import Control.Lens.Combinators (Plated (plate))

{- |
Data type for binders
-}
data Binder a
  = -- |
    -- Wildcard binder
    NullBinder a
  | -- |
    -- A binder which matches a literal value
    LiteralBinder a (Literal (Binder a))
  | -- |
    -- A binder which binds an identifier
    VarBinder a Ident SourceType
  | -- |
    -- A binder which matches a data constructor
    ConstructorBinder a (Qualified (ProperName 'TypeName)) (Qualified (ProperName 'ConstructorName)) [Binder a]
  | -- |
    -- A binder which binds its input to an identifier
    NamedBinder a Ident (Binder a)
  deriving (Eq, Ord, Show, Functor, Generic)

instance (FromJSON a) => FromJSON (Binder a)
instance (ToJSON a) => ToJSON (Binder a)

instance Plated (Binder a) where
  {-# INLINEABLE plate #-}
  plate f = \case
    x@(NullBinder _) -> pure x 
    LiteralBinder x lit -> LiteralBinder x <$> traverse f lit
    x@(VarBinder _ _ _) -> pure x
    ConstructorBinder x tyName conName binds -> ConstructorBinder x tyName conName <$> traverse f binds
    NamedBinder x ident bind -> NamedBinder x ident <$> f bind

extractBinderAnn :: Binder a -> a
extractBinderAnn (NullBinder a) = a
extractBinderAnn (LiteralBinder a _) = a
extractBinderAnn (VarBinder a _ _) = a
extractBinderAnn (ConstructorBinder a _ _ _) = a
extractBinderAnn (NamedBinder a _ _) = a

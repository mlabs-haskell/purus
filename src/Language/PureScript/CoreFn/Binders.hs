-- |
-- The core functional representation for binders
--
module Language.PureScript.CoreFn.Binders where

import Prelude

import Language.PureScript.AST.Literals (Literal)
import Language.PureScript.Names (Ident, ProperName, ProperNameType(..), Qualified)
import Language.PureScript.Types

import GHC.Generics
import Data.Aeson (FromJSON, ToJSON)

-- |
-- Data type for binders
--
data Binder a
  -- |
  -- Wildcard binder
  --
  = NullBinder a 
  -- |
  -- A binder which matches a literal value
  --
  | LiteralBinder a (Literal (Binder a))
  -- |
  -- A binder which binds an identifier
  --
  | VarBinder a Ident SourceType
  -- |
  -- A binder which matches a data constructor
  --
  | ConstructorBinder a (Qualified (ProperName 'TypeName)) (Qualified (ProperName 'ConstructorName)) [Binder a]
  -- |
  -- A binder which binds its input to an identifier
  --
  | NamedBinder a Ident (Binder a) deriving (Eq, Ord, Show, Functor, Generic)

instance FromJSON a => FromJSON (Binder a)
instance ToJSON a => ToJSON (Binder a)

extractBinderAnn :: Binder a -> a
extractBinderAnn (NullBinder a) = a
extractBinderAnn (LiteralBinder a _) = a
extractBinderAnn (VarBinder a _ _) = a
extractBinderAnn (ConstructorBinder a _ _ _) = a
extractBinderAnn (NamedBinder a _ _) = a

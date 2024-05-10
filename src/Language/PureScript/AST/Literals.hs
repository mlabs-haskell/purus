-- |
-- The core functional representation for literal values.
--
module Language.PureScript.AST.Literals where

import Prelude
import Language.PureScript.PSString (PSString)

-- For serializing/deserializing Typed CoreFn
import GHC.Generics ( Generic )
import Data.Aeson (FromJSON, ToJSON)
-- |
-- Data type for literal values. Parameterised so it can be used for Exprs and
-- Binders.
--
data Literal a -- a ~ Expr Ann
  -- |
  -- A numeric literal
  --
  = NumericLiteral (Either Integer Double)
  -- |
  -- A string literal
  --
  | StringLiteral PSString
  -- |
  -- A character literal
  --
  | CharLiteral Char
  -- |
  -- A boolean literal
  --
  | BooleanLiteral Bool
  -- |
  -- An array literal
  --
  | ArrayLiteral [a]
  -- | ArrayLiteral [Literal Void]
  -- |
  -- An object literal
  --
  | ObjectLiteral [(PSString, a)]
  deriving (Eq, Ord, Show, Functor, Generic)

instance FromJSON a => FromJSON (Literal a)
instance ToJSON a => ToJSON (Literal a)

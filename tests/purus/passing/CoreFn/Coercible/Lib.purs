module Coercible.Lib
  ( module Coercible.Lib2
  , NTLib1 (..)
  , NTLib3 (..)
  ) where

import Coercible.Lib2
import Prim (Type)

newtype NTLib1 (a :: Type) = NTLib1 a

newtype NTLib3 (a :: Type) (b :: Type) = NTLib3 a
type role NTLib3 representational representational

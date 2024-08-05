module Language.PureScript.Label (Label (..)) where

import Codec.Serialise (Serialise)
import Control.DeepSeq (NFData)
import Data.Aeson qualified as A
import Data.Monoid ()
import Data.String (IsString (..))
import GHC.Generics (Generic)
import Prelude

import Language.PureScript.PSString (PSString)

{- |
Labels are used as record keys and row entry names. Labels newtype PSString
because records are indexable by PureScript strings at runtime.
-}
newtype Label = Label {runLabel :: PSString}
  deriving (Show, Eq, Ord, IsString, Semigroup, Monoid, A.ToJSON, A.FromJSON, Generic)

instance NFData Label
instance Serialise Label

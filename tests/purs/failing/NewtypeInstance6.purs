-- @shouldFailWith KindsDoNotUnify
module Main where

import Prelude

newtype X a b = X (List b)

derive newtype instance functorX :: Functor X

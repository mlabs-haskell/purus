-- @shouldFailWith PartiallyAppliedSynonym
module Main where

import Prelude

type T (a :: (~>) List) = Int

-- @shouldFailWith PartiallyAppliedSynonym
module Main where

import Prelude

data Proxy :: forall k. k -> Type
data Proxy a = Proxy

f :: forall (a :: (~>) List). Proxy a -> Proxy a
f x = x

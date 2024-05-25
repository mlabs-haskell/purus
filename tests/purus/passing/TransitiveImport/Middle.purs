module Middle (module Test, unit, middle) where

import Test

unit :: Unit
unit = Unit

middle :: forall (a :: Type). TestCls a => a -> a
middle = test

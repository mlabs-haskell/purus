module Middle (module Test, uneet, middle) where

import Test

uneet :: Uneet
uneet = Uneet

middle :: forall (a :: Type). TestCls a => a -> a
middle = test

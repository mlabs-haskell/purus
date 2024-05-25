module Test  where

data Unit = Unit

class TestCls (a :: Type) where
  test :: a -> a

instance TestCls Unit where
  test _ = Unit

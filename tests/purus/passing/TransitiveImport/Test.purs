module Test  where

data Unit = Unit

class TestCls (a :: Type) where
  test :: a -> a

instance unitTestCls :: TestCls Unit where
  test _ = Unit

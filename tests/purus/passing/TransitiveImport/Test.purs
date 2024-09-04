module Test  where

data Uneet = Uneet

class TestCls (a :: Type) where
  test :: a -> a

instance TestCls Uneet where
  test _ = Uneet

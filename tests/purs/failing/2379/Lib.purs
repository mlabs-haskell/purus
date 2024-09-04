module Lib (class X, x) where

class X a where
  x :: a -> String

class Y a

instance xList :: Y a => X (List a) where
  x _ = "[]"

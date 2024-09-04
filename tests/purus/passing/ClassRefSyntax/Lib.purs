module Lib (class X, go) where

class X (a :: Prim.Type) where
  go :: a -> a


module Lib where

data Tuple (a :: Type) (b :: Type) = Tuple a b

infixr 6 Tuple as /\
infixr 6 type Tuple as /\

mappend :: String -> String -> String
mappend _ _ = "mappend"

infixr 5 mappend as <>

class Test (a :: Type) where
  runTest :: a -> String

instance Test Int where
  runTest _ = "4"

instance forall (a :: Type) (b :: Type). (Test a, Test b) => Test (a /\ b) where
  runTest (a /\ b) = runTest a <> runTest b

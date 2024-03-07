module Lib where

data Tuple a b = Tuple a b

infixr 6 Tuple as /\
infixr 6 type Tuple as /\

mappend :: String -> String -> String
mappend _ _ = "mappend"

infixr 5 mappend as <>

class Test a where
  runTest :: a -> String

instance Test Int where
  runTest _ = "4"

instance (Test a, Test b) => Test (a /\ b) where
  runTest (a /\ b) = runTest a <> runTest b

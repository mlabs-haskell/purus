module Lib where

import Prim (Type, String, Int)

data Tuple (a :: Prim.Type) (b :: Prim.Type) = Tuple a b

infixr 6 Tuple as /\
infixr 6 type Tuple as /\

mappend :: String -> String -> String
mappend _ _ = "mappend"

infixr 5 mappend as <>

class Test (a :: Prim.Type) where
  runTest :: a -> String

instance Test Int where
  runTest _ = "4"

{- TODO/FIXME: Disabled while I figure out what to do here
instance (Test a, Test b) => Test (a /\ b) where
  runTest (a /\ b) = runTest a <> runTest b
-}

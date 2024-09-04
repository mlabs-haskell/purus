module Main where

import Prelude

import Data.Foldable (class Foldable)
import Data.Traversable (class Traversable)
import Data.Tuple (Tuple)
import Effect.Console (log)

data Test a
  = Test1 (Tuple a Int)
  | Test2 (Tuple (List a) a)
  | Test3 { x :: Tuple { a :: a } Int, y :: Tuple { a :: List a } { a :: a } }
derive instance Functor Test
derive instance Foldable Test
derive instance Traversable Test

main = log "Done"

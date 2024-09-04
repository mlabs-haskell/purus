module Main where

import Prelude

import Data.Functor.Contravariant (class Contravariant)
import Data.Predicate (Predicate)
import Data.Tuple (Tuple)
import Effect.Console (log)

data Test f a
  = Test0
  | Test1 (Predicate a)
  | Test2 (Predicate (Predicate (Predicate a)))
  | Test3 Int (forall a. List a -> List a)
  | Test4 Int (f a)
  | Test5 (List (a -> Int)) (Tuple (Predicate a) Int)
  | Test6 { nested :: List { x :: f { a :: a } } }
derive instance Contravariant f => Contravariant (Test f)

main = log "Done"

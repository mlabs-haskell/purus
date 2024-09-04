module Main where

import Prelude

import Data.Predicate (Predicate)
import Data.Profunctor (class Profunctor)
import Data.Tuple (Tuple)
import Effect.Console (log)

data Test f a b
  = Test0
  | Test1 (Predicate a) b
  | Test2 Int (forall a. List a -> List a)
  | Test3 Int (f a b) (f a Int) (f Int b)
  | Test4 (List (a -> Int)) (Tuple b Int)
  | Test5 { nested :: List { x :: f { a :: a } { b :: b } } }
derive instance Profunctor f => Profunctor (Test f)

main = log "Done"

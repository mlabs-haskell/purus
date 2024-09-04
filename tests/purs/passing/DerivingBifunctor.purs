module Main where

import Prelude

import Data.Bifoldable (class Bifoldable)
import Data.Bifunctor (class Bifunctor)
import Data.Bitraversable (class Bitraversable)
import Data.Predicate (Predicate)
import Data.Tuple (Tuple)
import Effect.Console (log)

data Test f a b
  = Test0
  | Test1 (List a) b
  | Test2 Int (forall a. List a -> List a)
  | Test3 Int (f a b) (f a Int) (f Int b)
  | Test4 (List (Tuple a Int)) (Tuple b Int)
  | Test5 { nested :: List { x :: f { a :: a } { b :: b } } }
derive instance Bifunctor f => Bifunctor (Test f)
derive instance Bifoldable f => Bifoldable (Test f)
derive instance Bitraversable f => Bitraversable (Test f)

data FromProAndContra a b = FromProAndContra (Predicate (a -> Int)) (Predicate b -> Int)
derive instance Bifunctor FromProAndContra

main = log "Done"

-- @shouldFailWith NoInstanceFound
module Main where

import Prelude

import Data.List as List
import Data.Maybe (Maybe(..))

data Sound = Moo | Quack | Bark

type Animal = { sound :: Sound }

animalFunc :: List Animal -> Unit
animalFunc animals
  | Just { sound } <- animals # List.find \{ sound: Moo } -> true = unit
  | otherwise = unit

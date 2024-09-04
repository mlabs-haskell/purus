-- @shouldFailWith CycleInTypeSynonym
module Main where

import Prelude

type T1 = List T2

type T2 = T1

-- @shouldFailWith InfiniteType
module Main where

foo :: List Int -> Int
foo xs = go xs where
  go :: List _ -> Int
  go [] = 0
  go xs = go [xs]

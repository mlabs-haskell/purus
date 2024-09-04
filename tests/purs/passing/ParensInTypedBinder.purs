module Main where

import Prelude
import Effect (Effect)
import Effect.Console (log)

foo :: List Int
foo = do
  xss :: List (List Int) <- [[[1,2,3], [4, 5]], [[6]]]
  xs :: List Int <- xss
  xs

main :: Effect Unit
main = log "Done"

module Main where

import Prelude
import Effect.Console (log)

length :: forall a. List a -> Int
length _ = 0

type Foo a = List a

foo _ = length ([] :: Foo Number)

main = log "Done"

module Main where

import Prelude
import Effect.Console (log)

type A a = List a

data Foo a = Foo (A a) | Bar

foo (Foo []) = Bar

main = log "Done"

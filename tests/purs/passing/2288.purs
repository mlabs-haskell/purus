module Main where

import Prelude
import Effect
import Effect.Console
import Data.List
import Data.List.Partial as P
import Partial.Unsafe

length :: forall a. List a -> Int
length = go 0 where
  go acc arr =
    if null arr
    then acc
    else go (acc + 1) (unsafePartial P.tail arr)

main = do
  logShow (length (1 .. 10000))
  log "Done"

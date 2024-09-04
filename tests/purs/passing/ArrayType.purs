module Main where

import Prelude
import Effect.Console (log)

class Pointed p where
  point :: forall a. a -> p a

instance pointedList :: Pointed List where
  point a = [a]

main = log "Done"

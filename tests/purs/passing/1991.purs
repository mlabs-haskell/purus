module Main where

import Prelude
import Effect.Console (log)

singleton :: forall a. a -> List a
singleton x = [x]

empty :: forall a. List a
empty = []

foldMap :: forall a m. Semigroup m => (a -> m) -> List a -> m
foldMap f [a, b, c, d, e] = f a <> f b <> f c <> f d <> f e
foldMap f xs = foldMap f xs -- spin, not used

regression :: List Int
regression =
  let as = [1,2,3,4,5]
      as' = foldMap (\x -> if 1 < x && x < 4 then singleton x else empty) as
  in as'

main = log "Done"

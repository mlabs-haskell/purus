-- @shouldFailWith NoInstanceFound

module Main where

import Prelude
import Effect.Console (logShow)

class Su a where
  su :: a -> a

class Su (List a) <= Cl a where
  cl :: a -> a -> a

instance suNumber :: Su Number where
  su n = n + 1.0

instance suList :: Su a => Su (List a) where
  su [x] = [su x]

instance clNumber :: Cl Number where
  cl n m = n + m

test :: forall a. Cl a => a -> List a
test x = su [cl x x]

main = logShow $ test 10.0

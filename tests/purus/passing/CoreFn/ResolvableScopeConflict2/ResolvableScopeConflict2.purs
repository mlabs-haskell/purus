module Main where

import A

thing :: Int
thing = 1

-- Not an error as although we have `thing` in scope from both Main and A,
-- as the local declaration takes precedence over the implicit import
what :: Boolean -> Int
what true = thing
what false = zing

main = "Done"

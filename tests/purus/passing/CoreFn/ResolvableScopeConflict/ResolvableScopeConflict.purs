module Main where

import A (thing)
import B

-- Not an error as although we have `thing` in scope from both A and B, it is
-- imported explicitly from A, giving it a resolvable solution.
what :: Boolean -> Int
what true = thing
what false = zing

main = "Done"

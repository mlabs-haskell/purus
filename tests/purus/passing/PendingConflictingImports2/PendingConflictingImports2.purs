module Main where

import A

-- No error as we never force `thing` to be resolved in `Main`
thing :: Int
thing = 2

main =  "Done"

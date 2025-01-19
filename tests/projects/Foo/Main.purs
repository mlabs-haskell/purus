module Main where

import Prim

testRecord :: {b :: Int, a :: Int, c :: Int}
testRecord = {b: 2, c: 3, a: 1}

main = testRecord.a

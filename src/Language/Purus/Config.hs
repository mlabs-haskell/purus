{- Contains constants that affect compiler behavior.

   At the moment, just contains the max tuple size. In the future, might contain something else!

-}

module Language.Purus.Config (maxTupleSize) where

import Prelude (Int)

-- TODO/FIXME: Raise this before release
maxTupleSize :: Int
maxTupleSize = 6

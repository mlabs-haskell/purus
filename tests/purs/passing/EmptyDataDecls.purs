module Main where

import Prelude
import Test.Assert
import Effect.Console (log)

data Z
data S n

data ListBox n a = ListBox (List a)

nil :: forall a. ListBox Z a
nil = ListBox []

cons' :: forall a n. a -> ListBox n a -> ListBox (S n) a
cons' x (ListBox xs) = ListBox $ append [x] xs

main = case cons' 1 $ cons' 2 $ cons' 3 nil of
         ListBox [1, 2, 3] -> log "Done"
         _ -> assert' "Failed" false

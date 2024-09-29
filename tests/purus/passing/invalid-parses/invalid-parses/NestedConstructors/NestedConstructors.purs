module NestedConstructors where

data Nat = S Nat
         | Z

atLeastTwo :: Nat -> Boolean
atLeastTwo (S (S _)) = True
atLeastTwo _ = False

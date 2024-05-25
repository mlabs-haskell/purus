module M1 where

id :: forall (a :: Type). a -> a
id = \x -> x

foo = id

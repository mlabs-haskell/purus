module Prelude where


data Bool
  = True
  | False

and :: Bool -> Bool -> Bool
and True True = True
and _ _ = False

or :: Bool -> Bool -> Bool
or True _ = True
or _ True = True
or _ _ = False

not :: Bool -> Bool
not True = False
not False = True

data List (a :: Type) = Nil | Cons a (List a)

appendList :: forall (a :: Type). List a -> List a -> List a
appendList Nil ys = ys
appendList (Cons x xs) ys = Cons x (appendList xs ys)

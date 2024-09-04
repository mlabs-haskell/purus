module List where

data List (a :: Type) = Cons a (List a) | Nil

infixr 6 Cons as :

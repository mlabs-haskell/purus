module Demo where

{- Builtins -}
minus :: Int -> Int -> Int
minus = Builtin.subtractInteger
infixr 5 minus as -

plus :: Int -> Int -> Int
plus  = Builtin.addInteger
infixr 5 plus as +

multiply :: Int -> Int -> Int
multiply = Builtin.multiplyInteger
infixr 5 multiply as *

divide :: Int -> Int -> Int
divide  = Builtin.divideInteger
infixr 5 divide as /


testMinus :: Int
testMinus = 10 - 6

testPlus :: Int
testPlus = 2 + 2

testMultiply :: Int
testMultiply = 2 * 2

testDivide :: Int
testDivide = 4 / 2

{- Data types -}
data Identitee (a :: Type) = Identitee a

unIdentitee :: Identitee Int -> Int
unIdentitee = case _ of
  Identitee x -> x

testIdentitee = unIdentitee (Identitee 101)

{- Validator -}
testValidator :: forall (a :: Type) (b :: Type) (c :: Type)
              . a -> b -> c -> Boolean
testValidator datum redeemer context =  True

testValidatorApplied :: Boolean
testValidatorApplied = testValidator "datum" "redeemer" "context"

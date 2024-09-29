module NumericLiteralInBinaryOp where

data Tuple (a :: Type) (b :: Type) = Tuple a b

infixr 6 Tuple as /\
infixr 6 type Tuple as /\

isZeroes :: Tuple Int Int -> Boolean
isZeroes (0 /\ 0) = True
isZeroes _ = False

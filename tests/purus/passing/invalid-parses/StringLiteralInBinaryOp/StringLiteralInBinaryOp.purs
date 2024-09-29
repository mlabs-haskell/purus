module StringLiteralInBinaryOp where

data Tuple (a :: Type) (b :: Type) = Tuple a b

infixr 6 Tuple as /\
infixr 6 type Tuple as /\

isEmpties :: Tuple String String -> Boolean
isEmpties ("" /\ "") = True
isEmpties _ = False

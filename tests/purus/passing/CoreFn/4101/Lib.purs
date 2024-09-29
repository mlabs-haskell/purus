module Lib where

-- newtype Const :: forall (j :: Type) (k :: j). Type -> k -> Type
newtype Const (a :: Type) (b :: Type) = Const a

data Unit = Unit

type CONST = Const
type UNIT = CONST Unit

module Lib where

data T :: Type -> Type
data T (msg :: Type) = E

type TAlias :: Type -> Type
type TAlias (msg :: Type) = T msg

module Lib where

type Template (col :: Type -> Type) = { bio :: col String }
type Identity (a :: Type) = a
type Patch = Template Identity

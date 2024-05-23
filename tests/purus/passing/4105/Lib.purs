module Lib where

type Template (col :: Prim.Type -> Prim.Type) = { bio :: col String }
type Identity (a :: Prim.Type) = a
type Patch = Template Identity

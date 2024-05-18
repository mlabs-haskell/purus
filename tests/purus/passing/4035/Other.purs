module Other where

type Id :: forall (k :: Type). (k :: Type) -> (k :: Type)
type Id a = (a :: Type)

module Other where


type Id :: Type -> Type
type Id (a :: Prim.Type) = a

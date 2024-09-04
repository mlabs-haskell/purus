module M1 where

applyFn :: forall (a :: Type) (b :: Type). (forall (c :: Type) (d :: Type). c -> d) -> a -> b
applyFn f a = f a

infixr 1000 applyFn as $

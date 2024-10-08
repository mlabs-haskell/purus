module InitializationError where

class Alpha a where
  alpha :: a Int -> a Int
class Alpha a <= Bravo a
class Bravo a <= Charlie a

charlieAlpha :: forall a. Charlie a => a Int -> a Int
charlieAlpha = alpha

instance alphaList :: Alpha List where
  alpha = charlieAlpha
instance Bravo List
instance Charlie List

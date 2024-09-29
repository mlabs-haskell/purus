module NumericLiteralInConstructor where

data V2 = V2 Int Int

isZeroV2 :: V2 -> Bool
isZeroV2 (V2 0 0) = True
isZeroV2 _        = False

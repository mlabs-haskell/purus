module StringLiteralInConstructor where

data V2 = V2 String String

isEmptyV2 :: V2 -> Bool
isEmptyV2 (V2 "" "") = True
isEmptyV2 _        = False

module RowSyntax where

class IsARow (r :: Row Type)
instance IsARow [hello :: String]

class IsARow' (r :: Row Type)
instance IsARow' (r :: Row Type)

data RowProxy (r :: Row Type) = RowProxy

aRowProxy :: RowProxy [field :: Int]
aRowProxy = RowProxy

moreFields :: RowProxy [field1 :: Int, field2 :: String, field3 :: Boolean]
moreFields = RowProxy

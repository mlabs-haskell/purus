module RowSyntax where

type OneRow :: Row Type
type OneRow = [one :: Int]

type SomeRow :: Row Type
type SomeRow = [inn'it :: Int, stirring :: String]


class IsARow (r :: Row Type)
instance IsARow [hello :: String]

class IsARow' (r :: Row Type)
instance IsARow' (r :: Row Type)

data RowProxy (r :: Row Type) = RowProxy

aRowProxy :: RowProxy [field :: Int]
aRowProxy = RowProxy

moreFields :: RowProxy [field1 :: Int, field2 :: String, field3 :: Boolean]
moreFields = RowProxy

type TestRecord1 = {foob :: String, ar :: Int}

type TestRecord2 = Record [foob :: String, ar :: Int]

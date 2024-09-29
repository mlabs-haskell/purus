module NestedBinaryOps where

arrayLengthAtLeastTwo :: Array a -> Boolean
arrayLengthAtLeastTwo (_:_:_) = True
arrayLengthAtLeastTwo _ = False

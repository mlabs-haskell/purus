module Prelude where

serializeScriptHash :: ScriptHash -> Builtin.BuiltinData
serializeScriptHash (ScriptHash dat) = serializeByteString dat

deserializeScriptHash :: Builtin.BuiltinData -> ScriptHash
deserializeScriptHash dat = ScriptHash (deserializeByteString dat)

serializeRedeemer :: Redeemer -> Builtin.BuiltinData
serializeRedeemer (Redeemer dat) = dat

deserializeRedeemer :: Builtin.BuiltinData -> Redeemer
deserializeRedeemer = Redeemer

serializeRedeemerHash :: RedeemerHash -> Builtin.BuiltinData
serializeRedeemerHash (RedeemerHash bs) = serializeByteString bs

deserializeRedeemerHash :: Builtin.BuiltinData -> RedeemerHash
deserializeRedeemerHash dat = RedeemerHash (deserializeByteString dat)

serializeDatum :: Datum -> Builtin.BuiltinData
serializeDatum (Datum dat) = dat

deserializeDatum :: Builtin.BuiltinData -> Datum
deserializeDatum = Datum

serializeDatumHash :: DatumHash -> Builtin.BuiltinData
serializeDatumHash (DatumHash bs) = serializeByteString bs

deserializeDatumHash :: Builtin.BuiltinData -> DatumHash
deserializeDatumHash dat = DatumHash (deserializeByteString dat)

serializeInt :: Int -> Builtin.BuiltinData
serializeInt = Builtin.iData

deserializeInt :: Builtin.BuiltinData -> Int
deserializeInt = Builtin.unIData

serializeByteString :: Builtin.BuiltinByteString -> Builtin.BuiltinData
serializeByteString = Builtin.bData

deserializeByteString :: Builtin.BuiltinData -> Builtin.BuiltinByteString
deserializeByteString = Builtin.unBData

serializeList :: 
  forall (a :: Type) . (a -> Builtin.BuiltinData) -> List a -> Builtin.BuiltinData
serializeList f arr = Builtin.listData (go arr)
  where
    go :: List a -> Builtin.BuiltinList Builtin.BuiltinData
    go = case _ of 
             Nil -> Builtin.mkNilData unit
             Cons x xs -> Builtin.mkCons (f x) (go xs)
  
deserializeList :: 
  forall (a :: Type) . (Builtin.BuiltinData -> a) -> Builtin.BuiltinData -> List a
deserializeList f dat = 
  let unlisted = Builtin.unListData dat
   in go unlisted
   where
     go :: Builtin.BuiltinList Builtin.BuiltinData -> List a
     go ell = if Builtin.nullList ell
              then Nil
              else let h = Builtin.headList ell
                       t = Builtin.tailList ell
                     in Cons (f h) (go t)

serializeTuple2 :: 
  forall (a :: Type) (b :: Type) . 
  (a -> Builtin.BuiltinData) -> 
  (b -> Builtin.BuiltinData) -> 
  Tuple2 a b -> 
  Builtin.BuiltinData
serializeTuple2 fA fB (Tuple2 x y) = 
  Builtin.constrData 0 (Builtin.mkCons (fA x) (Builtin.mkCons (fB y) (Builtin.mkNilData unit)))

deserializeTuple2 :: 
  forall (a :: Type) (b :: Type) .
  (Builtin.BuiltinData -> a) ->
  (Builtin.BuiltinData -> b) -> 
  Builtin.BuiltinData -> 
  Tuple2 a b
deserializeTuple2 fA fB dat = 
    let unlisted = Builtin.sndPair (Builtin.unConstrData dat)
        x = fA (Builtin.headList unlisted)
        unlisted' = Builtin.tailList unlisted
        y = fB (Builtin.headList unlisted') 
      in Tuple2 x y

serializeMaybe :: 
  forall (a :: Type) . 
  (a -> Builtin.BuiltinData) -> 
  Maybe a -> 
  Builtin.BuiltinData
serializeMaybe f m = case m of 
  Nothing -> Builtin.constrData 0 (Builtin.mkNilData unit)
  Just x -> Builtin.constrData 1 (Builtin.mkCons (f x) (Builtin.mkNilData unit))

deserializeMaybe :: 
  forall (a :: Type) . 
  (Builtin.BuiltinData -> a) -> 
  Builtin.BuiltinData -> 
  Maybe a
deserializeMaybe f dat = 
  let p = Builtin.unConstrData dat
      tag = Builtin.fstPair p
    in if Builtin.equalsInteger tag 0
       then Nothing
       else let unlisted = Builtin.sndPair p
              in Just (f (Builtin.headList unlisted))

maybe :: 
  forall (a :: Type) (b :: Type) . 
  b -> 
  (a -> b) -> 
  Maybe a -> 
  b
maybe whenNothing whenJust m = case m of 
  Nothing -> whenNothing
  Just x -> whenJust x

fromMaybe :: 
  forall (a :: Type) . 
  a -> 
  Maybe a -> 
  a
fromMaybe whenNothing m = case m of 
  Nothing -> whenNothing
  Just x -> x

identity :: forall (a :: Type) . a -> a
identity x = x

const :: forall (a :: Type) (b :: Type) . a -> b -> b
const _ x = x

and :: Boolean -> Boolean -> Boolean
and x y = Builtin.ifThenElse x y False

or :: Boolean -> Boolean -> Boolean
or x y = Builtin.ifThenElse x True y

not :: Boolean -> Boolean
not x = Builtin.ifThenElse x False True

xor :: Boolean -> Boolean -> Boolean
xor x y = Builtin.ifThenElse x (not y) y

mapList :: 
  forall (a :: Type) (b :: Type) . 
  (a -> b) -> 
  List a ->
  List b
mapList f ell = case ell of 
  Nil -> Nil
  Cons x xs -> Cons (f x) (mapList f xs)

filterList :: 
  forall (a :: Type) . 
  (a -> Boolean) -> 
  List a -> 
  List a
filterList f ell = case ell of 
  Nil -> Nil
  Cons x xs -> if f x
               then Cons x (filterList f xs)
               else filterList f xs

takeList :: 
  forall (a :: Type) . 
  Int -> 
  List a -> 
  List a
takeList count ell = 
  if Builtin.lessThanEqualsInteger count 0
  then Nil
  else case ell of 
          Nil -> Nil
          Cons x xs -> Cons x (takeList (Builtin.subtractInteger count 1) xs)

dropList :: 
  forall (a :: Type) . 
  Int -> 
  List a -> 
  List a
dropList count ell = 
  if Builtin.lessThanEqualsInteger count 0
  then ell
  else case ell of 
           Nil -> Nil
           Cons _ xs -> dropList (Builtin.subtractInteger count 1) xs

zipWithList :: 
  forall (a :: Type) (b :: Type) (c :: Type) . 
  (a -> b -> c) -> 
  List a -> 
  List b -> 
  List c
zipWithList f ell1 ell2 = case ell1 of 
  Nil -> Nil
  Cons x xs -> case ell2 of 
                   Nil -> Nil
                   Cons y ys -> Cons (f x y) (zipWithList f xs ys)

appendList :: 
  forall (a :: Type) . 
  List a -> 
  List a -> 
  List a
appendList ell1 ell2 = case ell1 of 
  Nil -> ell2
  Cons x xs -> Cons x (appendList xs ell2)

lengthList :: 
  forall (a :: Type) . 
  List a -> 
  Int
lengthList ell = case ell of 
  Nil -> 0
  Cons _ xs -> Builtin.addInteger 1 (lengthList xs)

anyList :: 
  forall (a :: Type) . 
  (a -> Boolean) -> 
  List a -> 
  Boolean
anyList p ell = case ell of 
  Nil -> False
  Cons x xs -> if p x
               then True
               else anyList p xs

allList ::
  forall (a :: Type) . 
  (a -> Boolean) -> 
  List a -> 
  Boolean
allList p ell = case ell of 
  Nil -> True
  Cons x xs -> if p x
               then allList p xs
               else False

sumList :: List Int -> Int
sumList ell = case ell of 
  Nil -> 0
  Cons x xs -> Builtin.addInteger x (sumList xs)

productList :: List Int -> Int
productList ell = case ell of 
  Nil -> 1
  Cons x xs -> Builtin.multiplyInteger x (productList xs)

apply :: 
  forall (a :: Type) (b :: Type) . 
  (a -> b) -> 
  a -> 
  b
apply f x = f x

infixr 0 apply as $

flip :: 
  forall (a :: Type) (b :: Type) (c :: Type) . 
  (a -> b -> c) -> 
  b ->
  a -> 
  c
flip f x y = f y x

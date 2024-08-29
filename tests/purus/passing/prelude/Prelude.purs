module Prelude where

serializeScriptContext :: ScriptContext -> BuiltinData
serializeScriptContext (ScriptContext rec) = 
  constrData 0 (mkCons (serializeTxInfo rec.txInfo)
                  (mkCons (serializeScriptPurpose rec.purpose)
                     (mkNilData unit)))

deserializeScriptContext :: BuiltinData -> ScriptContext
deserializeScriptContext dat = 
  let dat' = sndPair (unConstrData dat)
      unlisted = unListData dat'
      txInfo = deserializeTxInfo (headList unlisted)
      unlisted' = tailList unlisted'
      purpose = deserializeScriptPurpose (headList unlisted')
    in ScriptContext { txInfo: txInfo, purpose: purpose }

serializeScriptPurpose :: ScriptPurpose -> BuiltinData
serializeScriptPurpose sp = case sp of 
  Minting cs -> 
    constrData 0 (mkCons (serializeCurrencySymbol cs) (mkNilData unit))
  Spending tor -> 
    constrData 1 (mkCons (serializeTxOutRef tor) (mkNilData unit))
  Rewarding sc -> 
    constrData 2 (mkCons (serializeStakingCredential sc) (mkNilData unit))
  Certifying d -> 
    constrData 3 (mkCons (serializeDCert d) (mkNilData unit))

deserializeScriptPurpose :: BuiltinData -> ScriptPurpose
deserializeScriptPurpose dat = 
  let p = unConstrData dat
      tag = fstPair p
      x = headList (unListData (sndPair p))
    in if equalsInteger 0 tag
       then Minting (deserializeCurrencySymbol x)
       else if equalsInteger 1 tag
            then Spending (deserializeTxOutRef x)
            else if equalsInteger 2 tag
                 then Rewarding (deserializeStakingCredential x)
                 else Cerifying (deserializeDCert x)

serializeDCert :: DCert -> BuiltinData
serializeDCert d = case d of 
  DCertDelegRegKey sc -> 
    constrData 0 
      (mkCons (serializeStakingCredential sc)
         (mkNilData unit))
  DCertDelegDeRegKey sc -> 
    constrData 1 
      (mkCons (serializeStakingCredential sc)
         (mkNilData unit))
  DCertDelegDelegatee sc pkh -> 
    constrData 2 
      (mkCons (serializeStakingCredential sc)
         (mkCons (serializePubKeyHash pkh)
            (mkNilData unit)))
  DCertPoolRegister pkh1 pkh2 -> 
    constrData 3 
      (mkCons (serializePubKeyHash pkh1)
         (mkCons (serializePubKeyHash pkh2)
            (mkNilData unit)))
  DCertPoolRetire pkh i -> 
    constrData 4 
      (mkCons (serializePubKeyHash pkh)
         (mkCons (serializeInt i)
            (mkNilData unit)))
  DCertGenesis -> 
    constrData 5 (mkNilData unit)
  DCertMir -> 
    constrData 6 (mkNilData unit)

deserializeDCert :: BuiltinData -> DCert
deserializeDCert dat = 
  let p = unConstrData dat
      tag = fstPair p
      unlisted = unListdata (sndPair p)
    in if equalsInteger tag 0
       then DCertDelegRegKey (deserializeStakingCredential (headList unlisted))
       else if equalsInteger tag 1
            then DCertDelegDeRegKey (deserializeStakingCredential (headList unlisted))
            else if equalsInteger tag 2
                 then let sc = deserializeStakingCredential (headList unlisted)
                          unlisted' = tailList unlisted
                          pkh = deserializePubKeyHash (headList unlisted')
                        in DCertDelegDelegatee sc pkh
                 else if equalsInteger tag 3
                      then let pkh1 = deserializePubKeyHash (headList unlisted)
                               unlisted' = tailList unlisted
                               pkh2 = deserializePubKeyHash (headList unlisted')
                            in DCertPoolRegister pkh1 pkh2
                      else if equalsInteger tag 4
                           then let pkh = deserializePubKeyHash (headList unlisted)
                                    unlisted' = tailList unlisted
                                    i = deserializeInt (headList unlisted')
                                  in DCertPoolRetire pkh i
                           else if equalsInteger tag 5
                                then DCertGenesis
                                else DCertMir

serializeStakingCredential :: StakingCredential -> BuiltinData
serializeStakingCredential sc = case sc of 
  StakingHash c -> 
    constrData 0 (mkCons (serializeCredential c) (mkNilData unit))
  StakingPtr i1 i2 i3 -> 
    constrData 1 (mkCons (serializeInt i1)
                    (mkCons (serializeInt i2)
                       (mkCons (serializeInt i3)
                          (mkNilData unit))))

deserializeStakingCredential :: BuiltinData -> StakingCredential
deserializeStakingCredential dat = 
  let p = unConstrData dat
      tag = fstPair p
      unlisted = unListData (sndPair p)
    in if equalsInteger tag 0
       then StakingHash (deserializeCredential (headList unlisted))
       else let i1 = deserializeInt (headList unlisted)
                unlisted' = tailList unlisted
                i2 = deserializeInt (headList unlisted')
                unlisted'' = tailList (unlisted')
                i3 = deserializeInt (headList unlisted'')
              in StakingPtr i1 i2 i3

serializeCredential :: Credential -> BuiltinData
serializeCredential c = case c of 
  PubKeyCredential pkh -> constrData 0 (mkCons (serializePubKeyHash pkh) (mkNilData unit))
  ScriptCredential sh -> constrData 1 (mkCons (serializeScriptHash sh) (mkNilData unit))

deserializeCredential :: BuiltinData -> Credential
deserializeCredential dat = 
  let p = unConstrData dat
      tag = fstPair p
      unlisted = unListData (sndPair p)
    in if equalsInteger tag 0
       then PubKeyCredential (deserializePubKeyHash (headList unlisted))
       else ScriptCredential (deserializeScriptHash (headList unlisted))

serializeValue :: Value -> BuiltinData
serializeValue (Value v) = 
  serializeAssocMap serializeCurrencySymbol (serializeAssocMap serializeTokenName serializeInt) v

deserializeValue :: BuiltinData -> Value
deserializeValue dat = 
  Value (deserializeAssocMap deserializeCurrencySymbol (deserializeAssocMap
        deserializeTokenName deserializeInt) dat)

serializeCurrencySymbol :: CurrencySymbol -> BuiltinData
serializeCurrencySymbol (CurrencySymbol bs) = serializeByteString bs

deserializeCurrencySymbol :: BuiltinData -> CurrencySymbol
deserializeCurrencySymbol dat = CurrencySymbol (deserializeByteString dat)

serializeTokenName :: TokenName -> BuiltinData
serializeTokenName (TokenName bs) = serializeByteString bs

deserializeTokenName :: BuiltinData -> TokenName
deserializeTokenName dat = TokenName (deserializeByteString dat)

serializeLovelace :: Lovelace -> BuiltinData
serializeLovelace (Lovelace i) = serializeInt i

deserializeLovelace :: BuiltinData -> Lovelace
deserializeLovelace dat = Lovelace (deserializeInt dat)

serializePOSIXTime :: POSIXTime -> BuiltinData
serializePOSIXTIme (POSIXTime t) = serializeInt t

deserializePOSIXTime :: BuiltinData -> POSIXTime
deserializePOSIXTIme dat = POSIXTime (deserializeInt dat)

serializeAddress :: Address -> BuiltinData
serializeAddress (Address rec) = 
  constrData 0 (mkCons (serializeCredential rec.credential) 
                  (mkCons (serializeMaybe serializeStakingCredential rec.stakingCredential) 
                     (mkNilData unit)))

deserializeAddress :: BuiltinData -> Address
deserializeAddress dat = 
  let dat' = sndPair (unConstrData dat)
      unlisted = unListData dat'
      credential = deserializeCredential (headList unlisted)
      unlisted' = tailList unlisted
      stakingCredential = deserializeMaybe deserializeStakingCredential unlisted'
    in Address { credential: credential, stakingCredential: stakingCredential }

serializePubKeyHash :: PubKeyHash -> BuiltinData
serializePubKeyHash (PubKeyHash bs) = 
  serializeByteString bs

deserializePubKeyHash :: BuiltinData -> PubKeyHash
deserializePubKeyHash dat = 
  PubKeyHash (deserializeByteString dat)

serializeTxId :: TxId -> BuiltinData
serializeTxId (TxId bs) = 
  constrData 0 (mkCons (serializeByteString bs) (mkNilData unit))

deserializeTxId :: BuiltinData -> TxId
deserializeTxId dat = 
  let dat' = sndPair (unConstrData dat)
      unlisted = unListData dat'
    in TxId (deserializeByteString (headList unlisted))

serializeTxInfo :: TxInfo -> BuiltinData
serializeTxInfo (TxInfo rec) = 
  constrData 0 (mkCons (serializeList serializeTxInInfo rec.inputs) 
                  (mkCons (serializeList serializeTxInInfo rec.referenceInputs)
                     (mkCons (serializeList serializeTxOut rec.outputs)
                        (mkCons (serializeValue rec.fee)
                           (mkCons (serializeValue rec.mint)
                              (mkCons (serializeList serializeDCert rec.dCert)
                                 (mkCons (serializeAssocMap serializeStakingCredential serializeInt rec.wdrl)
                                    (mkCons (serializeInterval serializePOSIXTime rec.validTimeRange)
                                       (mkCons (serializeList serializePubKeyHash rec.signatories)
                                          (mkCons (serializeAssocMap serializeScriptPurpose serializeRedeemer rec.redeemers)
                                             (mkCons (serializeAssocMap serializeDatumHash serializeDatum rec.data)
                                                (mkCons (serializeTxId rec.id)
                                                   (mkNilData unit)))))))))))))

deserializeTxInfo :: BuiltinData -> TxInfo
deserializeTxInfo dat = 
  let unlisted = unListData (sndPair (unConstrData dat))
      inputs = deserializeList deserializeTxInInfo (headList unlisted)
      unlisted1 = tailList unlisted
      referenceInputs = deserializeList deserializeTxInInfo (headList unlisted1)
      unlisted2 = tailList unlisted1
      outputs = deserializeList deserializeTxOut (headList unlisted2)
      unlisted3 = tailList unlisted2
      fee = deserializeValue (headList unlisted3)
      unlisted4 = tailList unlisted3
      mint = deserializeValue (headList unlisted4)
      unlisted5 = tailList unlisted4
      dCert = deserializeList deserializeDCert (headList unlisted5)
      unlisted6 = tailList unlisted5
      wdrl = deserializeAssocMap deserializeStakingCredential deserializeInt (headList unlisted6)
      unlisted7 = tailList unlisted6
      validTimeRange = deserializeInterval deserializePOSIXTime (headList unlisted7)
      unlisted8 = tailList unlisted7
      signatories = deserializeList deserializePubKeyHash (headList unlisted8)
      unlisted9 = tailList unlisted8
      redeemers = deserializeAssocMap deserializeScriptPurpose deserializeRedeemer (headList unlisted9)
      unlisted10 = tailList unlisted9
      data1 = deserializeAssocMap deserializeDatumHash deserializeDatum (headList unlisted10)
      unlisted11 = tailList unlisted10
      id1 = deserializeTxId (headList unlisted11)
    in TxInfo { inputs: inputs,
                referenceInputs: referenceInputs,
                outputs: outputs,
                fee: fee,
                mint: mint,
                dCert: dCert,
                wdrl: wdrl,
                validTimeRange: validTimeRange,
                signatories: signatories,
                redeemers: redeemers,
                data: data1,
                id: id1
              }

serializeTxOut :: TxOut -> BuiltinData
serializeTxOut (TxOut rec) = 
  constrData 0 (mkCons (serializeAddress rec.address)
                  (mkCons (serializeValue rec.value)
                     (mkCons (serializeOutputDatum rec.outputDatum)
                        (mkCons (serializeMaybe serializeScriptHash rec.referenceScript)
                           (mkNilData unit)))))

deserializeTxOut :: BuiltinData -> TxOut
deserializeTxOut dat = 
  let unlisted = unListData (sndPair (unConstrData dat))
      address = deserializeAddress (headList unlisted)
      unlisted1 = tailList unlisted
      value = deserializeValue (headList unlisted1)
      unlisted2 = tailList unlisted1
      outputDatum = deserializeOutputDatum (headList unlisted2)
      unlisted3 = tailList unlisted2
      referenceScript = deserializeMaybe deserializeScriptHash (headList unlisted3)
    in TxOut { address: address,
               value: value,
               outputDatum: outputDatum,
               referenceScript: referenceScript
             }

serializeTxOutRef :: TxOutRef -> BuiltinData
serializeTxOutRef (TxOutRef rec) = 
  constrData 0 (mkCons (serializeTxId rec.id)
                  (mkCons (serializeInt rec.idx)
                     (mkNilData unit)))

deserializeTxOutRef :: BuiltinData -> TxOutRef
deserializeTxOutRef dat = 
  let unlisted = unListData (sndPair (unConstrData dat))
      id1 = deserializeTxId (headList unlisted)
      unlisted' = tailList unlisted
      idx = deserializeInt (headList unlisted')
    in TxOutRef { id: id1, idx: idx }

serializeTxInInfo :: TxInInfo -> BuiltinData
serializeTxInInfo (TxInInfo rec) = 
  constrData 0 (mkCons (serializeTxOutRef rec.outRef)
                  (mkCons (serializeTxOut rec.resolved)
                     (mkNilData unit)))

deserializeTxInInfo :: BuiltinData -> TxInInfo
deserializeTxInInfo dat = 
  let unlisted = unListData (sndPair (unConstrData dat))
      outRef = deserializeTxOutRef (headList dat)
      unlisted' = tailList unlisted
      resolved = deserializeTxOut (headList unlisted')
    in TxInInfo { outRef: outRef, resolved: resolved }

serializeOutputDatum :: OutputDatum -> BuiltinData
serializeOutputDatum od = case od of 
  NoOutputDatum -> constrData 0 (mkNilData unit)
  OutputDatumHash h -> constrData 1 (mkCons (serializeDatumHash h) (mkNilData unit))
  OutputDatum d -> constrData 2 (mkCons (serializeDatum d) (mkNilData unit))

deserializeOutputDatum :: BuiltinData -> OutputDatum
deserializeOutputDatum dat = 
  let p = unConstrData dat
      tag = fstPair p
    in if equalsInteger tag 0
       then NoOutputDatum
       else let unlisted = unListData (sndPair p)
                x = headList unlisted
              in if equalsInteger tag 1
                 then OutputDatumHash (deserializeDatumHash x)
                 else OutputDatum (deserializeDatum x)

serializeInterval :: 
  forall (a :: Type) . (a -> BuiltinData) -> Interval a -> BuiltinData
serializeInterval f (Interval rec) = 
  constrData 0 (mkCons (serializeLowerBound f rec.from) 
                  (mkCons (serializeUpperBound f rec.to)
                     (mkNilData unit)))

deserializeInterval :: 
  forall (a :: Type) . (BuiltinData -> a) -> BuiltinData -> Interval a
deserializeInterval f dat = 
  let dat' = sndPair (unConstrData dat)
      unlisted = unListData dat'
      from = deserializeLowerBound f (headList unlisted)
      unlisted' = tailList unlisted
      to = deserializeUpperBound f (headList unlisted')
    in Interval {from: from, to: to}

serializeExtended :: 
  forall (a :: Type) . (a -> BuiltinData) -> Extended a -> BuiltinData
serializeExtended f e = case e of 
  NegInf -> constrData 0 (mkNilData unit)
  Finite x -> constrData 1 (mkCons (f x) (mkNilData unit))
  PosInf -> constrData 2 (mkNilData unit)

deserializeExtended :: 
  forall (a :: Type) . (BuiltinData -> a) -> BuiltinData -> Extended a
deserializeExtended f dat = 
  let p = unConstrData dat
      tag = fstPair p
    in if equalsInteger tag 0
       then NegInfo
       else if equalsInteger tag 1
            then let unlisted = unListData (sndPair p)
                  in Finite (f (headList unlisted))
            else PosInf

serializeLowerBound :: 
  forall (a :: Type) . (a -> BuiltinData) -> LowerBound a -> BuiltinData
serializeLowerBound f (LowerBound e) = constrData 0 (serializeExtended f e)

deserializeLowerBound :: 
  forall (a :: Type) . (BuiltinData -> a) -> BuiltinData -> LowerBound a
deserializeLowerBound f dat = 
  let dat' = sndPair (unConstrData dat)
      unlisted = unListData dat'
      e = deserializeExtended f (headList unlisted)
    in LowerBound e

serializeUpperBound :: 
  forall (a :: Type) . (a -> BuiltinData) -> UpperBound a -> BuiltinData
serializeUpperBound f (UpperBound e) = constrData 0 (serializeExtended f e)

deserializeUpperBound :: 
  forall (a :: Type) . (BuiltinData -> a) -> BuiltinData -> UpperBound a
deserializeUpperBound f dat = 
  let dat' = sndPair (unConstrData dat)
      unlisted = unListData dat'
      e = deserializeExtended f (headList unlisted)
    in UpperBound e

serializeAssocMap :: 
  forall (k :: Type) (v :: Type) . 
  (k -> BuiltinData) -> 
  (v -> BuiltinData) -> 
  AssocMap k v -> 
  BuiltinData
serializeAssocMap fK fV (AssocMap ell) = mapData (go ell)
  where
    go :: Array (Tuple2 k v) -> Array BuiltinData
    go arr = case arr of 
      Nil -> Nil
      Cons x xs -> Cons (serializeTuple2 fK fV x) (go xs)

deserializeAssocMap :: 
  forall (k :: Type) (v :: Type) . 
  (BuiltinData -> k) -> 
  (BuiltinData -> v) -> 
  BuiltinData -> 
  AssocMap k v
deserializeAssocMap fK fV dat = AssocMap (go (unMapData dat))
  where
    go :: Array BuiltinData -> Array (Tuple2 k v)
    go arr = case arr of 
      Nil -> Nil
      Cons d ds -> Cons (deserializeTuple2 fK fV d) (go ds)

serializeScriptHash :: ScriptHash -> BuiltinData
serializeScriptHash (ScriptHash dat) = serializeByteString dat

deserializeScriptHash :: BuiltinData -> ScriptHash
deserializeScriptHash dat = ScriptHash (deserializeByteString dat)

serializeRedeemer :: Redeemer -> BuiltinData
serializeRedeemer (Redeemer dat) = dat

deserializeRedeemer :: BuiltinData -> Redeemer
deserializeRedeemer = Redeemer

serializeRedeemerHash :: RedeemerHash -> BuiltinData
serializeRedeemerHash (RedeemerHash bs) = serializeByteString bs

deserializeRedeemerHash :: BuiltinData -> RedeemerHash
deserializeRedeemerHash dat = RedeemerHash (deserializeByteString dat)

serializeDatum :: Datum -> BuiltinData
serializeDatum (Datum dat) = dat

deserializeDatum :: BuiltinData -> Datum
deserializeDatum = Datum

serializeDatumHash :: DatumHash -> BuiltinData
serializeDatumHash (DatumHash bs) = serializeByteString bs

deserializeDatumHash :: BuiltinData -> DatumHash
deserializeDatumHash dat = DatumHash (deserializeByteString dat)

serializeInt :: Int -> BuiltinData
serializeInt = iData

deserializeInt :: BuiltinData -> Int
deserializeInt = unIData

serializeByteString :: BuiltinByteString -> BuiltinData
serializeByteString = bData

deserializeByteString :: BuiltinData -> BuiltinByteString
deserializeByteString = unBData

serializeArray :: 
  forall (a :: Type) . (a -> BuiltinData) -> Array a -> BuiltinData
serializeArray f arr = case arr of 
  Nil -> mkNilData unit
  Cons x xs -> mkCons (f x) (serializeArray f xs)

deserializeArray :: 
  forall (a :: Type) . (BuiltinData -> a) -> BuiltinData -> Array a
deserializeArray f dat = go (unListData dat)
  where
    go :: Array BuiltinData -> Array a
    go arr = case arr of 
      Nil -> Nil
      Cons d ds -> Cons (f d) (go ds)

serializeTuple2 :: 
  forall (a :: Type) (b :: Type) . 
  (a -> BuiltinData) -> 
  (b -> BuiltinData) -> 
  Tuple2 a b -> 
  BuiltinData
serializeTuple2 fA fB (Tuple2 x y) = 
  constrData 0 (mkCons (fA x) (mkCons (fB y) (mkNilData unit))

deserializeTuple2 :: 
  forall (a :: Type) (b :: Type) .
  (BuiltinData -> a) ->
  (BuiltinData -> b) -> 
  BuiltinData -> 
  Tuple2 a b
deserializeTuple fA fB dat = 
    let dat' = sndPair (unConstrData dat)
        unlisted = unListData dat'
        x = fA (headList unlisted)
        unlisted' = tailList unlisted
        y = fB (headList unlisted') 
      in Tuple2 x y

data Maybe (a :: Type) = Nothing | Just a

serializeMaybe :: 
  forall (a :: Type) . 
  (a -> BuiltinData) -> 
  Maybe a -> 
  BuiltinData
serializeMaybe f m = case m of 
  Nothing -> constrData 0 (mkNilData unit)
  Just x -> constrData 1 (mkCons (f x) (mkNilData unit))

deserializeMaybe :: 
  forall (a :: Type) . 
  (BuiltinData -> a) -> 
  BuiltinData -> 
  Maybe a
deserializeMaybe f dat = 
  let p = unConstrData dat
      tag = fstPair p
    in if equalsInteger tag 0
       then Nothing
       else let unlisted = unListData (sndPair p)
              in Just (f (headList unlisted))

maybe :: 
  forall (a :: Type) (b :: Type) . 
  b -> 
  (a -> b) -> 
  Maybe a -> 
  b
maybe whenNothing whenJust m = case m of 
  Nothing -> whenNothing
  Just x -> whenJust x

fromJust :: 
  forall (a :: Type) .
  Maybe a -> 
  a
fromJust m = case m of 
  Nothing -> error "fromJust: found Nothing"
  Just x -> x

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
and x y = ifThenElse x y False

or :: Boolean -> Boolean -> Boolean
or x y = ifThenElse x True y

not :: Boolean -> Boolean
not x = ifThenElse x False True

xor :: Boolean -> Boolean -> Boolean
xor x y = ifThenElse x (not y) y

mapList :: 
  forall (a :: Type) (b :: Type) . 
  (a -> b) -> 
  Array a ->
  Array b
mapList f ell = case ell of 
  Nil -> Nil
  Cons x xs -> Cons (f x) (mapList f xs)

filterList :: 
  forall (a :: Type) . 
  (a -> Boolean) -> 
  Array a -> 
  Array a
filterList f ell = case ell of 
  Nil -> Nil
  Cons x xs -> if f x
               then Cons x (filterList f xs)
               else filterList f xs

takeList :: 
  forall (a :: Type) . 
  Int -> 
  Array a -> 
  Array a
takeList count ell = 
  if lessThanEqualsInteger count 0
  then Nil
  else case ell of 
          Nil -> Nil
          Cons x xs -> Cons x (takeList (subtractInteger 1) xs)

dropList :: 
  forall (a :: Type) . 
  Int -> 
  Array a -> 
  Array a
dropList count ell = 
  if lessThanEqualsInteger count 0
  then ell
  else case ell of 
           Nil -> Nil
           Cons _ xs -> dropList (subtractInteger 1) xs

zipWithList :: 
  forall (a :: Type) (b :: Type) (c :: Type) . 
  (a -> b -> c) -> 
  Array a -> 
  Array b -> 
  Array c
zipWithList f ell1 ell2 = case ell1 of 
  Nil -> Nil
  Cons x xs -> case ell2 of 
                   Nil -> Nil
                   Cons y ys -> Cons (f x y) (zipWithList f xs ys)

appendList :: 
  forall (a :: Type) . 
  Array a -> 
  Array a -> 
  Array a
appendList ell1 ell2 = case ell1 of 
  Nil -> ell2
  Cons x xs -> Cons x (appendList xs ell2)

lengthList :: 
  forall (a :: Type) . 
  Array a -> 
  Int
lengthList ell = case ell of 
  Nil -> 0
  Cons _ xs -> addInteger 1 (lengthList xs)

anyList :: 
  forall (a :: Type) . 
  (a -> Boolean) -> 
  Array a -> 
  Boolean
anyList p ell = case ell of 
  Nil -> False
  Cons x xs -> if p x
               then True
               else anyList p xs

allList ::
  forall (a :: Type) . 
  (a -> Boolean) -> 
  Array a -> 
  Boolean
allList p ell = case ell of 
  Nil -> True
  Cons x xs -> if p x
               then allList p xs
               else False

sumList :: Array Int -> Int
sumList ell = case ell of 
  Nil -> 0
  Cons x xs -> addInteger x (sumList xs)

productList :: Array Int -> Int
productList ell = case ell of 
  Nil -> 1
  Cons x xs -> multiplyInteger x (productList xs)

compose :: 
  forall (a :: Type) (b :: Type) (c :: Type) .
  (a -> b) -> 
  (c -> a) -> 
  c ->
  a
compose f g x = f (g x)

infixr 9 compose as .

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

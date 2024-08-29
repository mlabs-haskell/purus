module Prelude where

serializeScriptContext :: ScriptContext -> Builtin.BuiltinData
serializeScriptContext (ScriptContext rec) = 
  Builtin.constrData 0 (Builtin.mkCons (serializeTxInfo rec.txInfo)
                  (Builtin.mkCons (serializeScriptPurpose rec.purpose)
                     (Builtin.mkNilData unit)))

deserializeScriptContext :: Builtin.BuiltinData -> ScriptContext
deserializeScriptContext dat = 
  let dat' = Builtin.sndPair (Builtin.unConstrData dat)
      unlisted = Builtin.unListData dat'
      txInfo = deserializeTxInfo (Builtin.headList unlisted)
      unlisted' = Builtin.tailList unlisted
      purpose = deserializeScriptPurpose (Builtin.headList unlisted')
    in ScriptContext { txInfo: txInfo, purpose: purpose }

serializeScriptPurpose :: ScriptPurpose -> Builtin.BuiltinData
serializeScriptPurpose sp = case sp of 
  Minting cs -> 
    Builtin.constrData 0 (Builtin.mkCons (serializeCurrencySymbol cs) (Builtin.mkNilData unit))
  Spending tor -> 
    Builtin.constrData 1 (Builtin.mkCons (serializeTxOutRef tor) (Builtin.mkNilData unit))
  Rewarding sc -> 
    Builtin.constrData 2 (Builtin.mkCons (serializeStakingCredential sc) (Builtin.mkNilData unit))
  Certifying d -> 
    Builtin.constrData 3 (Builtin.mkCons (serializeDCert d) (Builtin.mkNilData unit))

deserializeScriptPurpose :: Builtin.BuiltinData -> ScriptPurpose
deserializeScriptPurpose dat = 
  let p = Builtin.unConstrData dat
      tag = Builtin.fstPair p
      x = Builtin.headList (Builtin.unListData (Builtin.sndPair p))
    in if Builtin.equalsInteger 0 tag
       then Minting (deserializeCurrencySymbol x)
       else if Builtin.equalsInteger 1 tag
            then Spending (deserializeTxOutRef x)
            else if Builtin.equalsInteger 2 tag
                 then Rewarding (deserializeStakingCredential x)
                 else Certifying (deserializeDCert x)

serializeDCert :: DCert -> Builtin.BuiltinData
serializeDCert d = case d of 
  DCertDelegRegKey sc -> 
    Builtin.constrData 0 
      (Builtin.mkCons (serializeStakingCredential sc)
         (Builtin.mkNilData unit))
  DCertDelegDeRegKey sc -> 
    Builtin.constrData 1 
      (Builtin.mkCons (serializeStakingCredential sc)
         (Builtin.mkNilData unit))
  DCertDelegDelegate sc pkh -> 
    Builtin.constrData 2 
      (Builtin.mkCons (serializeStakingCredential sc)
         (Builtin.mkCons (serializePubKeyHash pkh)
            (Builtin.mkNilData unit)))
  DCertPoolRegister pkh1 pkh2 -> 
    Builtin.constrData 3 
      (Builtin.mkCons (serializePubKeyHash pkh1)
         (Builtin.mkCons (serializePubKeyHash pkh2)
            (Builtin.mkNilData unit)))
  DCertPoolRetire pkh i -> 
    Builtin.constrData 4 
      (Builtin.mkCons (serializePubKeyHash pkh)
         (Builtin.mkCons (serializeInt i)
            (Builtin.mkNilData unit)))
  DCertGenesis -> 
    Builtin.constrData 5 (Builtin.mkNilData unit)
  DCertMir -> 
    Builtin.constrData 6 (Builtin.mkNilData unit)

deserializeDCert :: Builtin.BuiltinData -> DCert
deserializeDCert dat = 
  let p = Builtin.unConstrData dat
      tag = Builtin.fstPair p
      unlisted = Builtin.unListData (Builtin.sndPair p)
    in if Builtin.equalsInteger tag 0
       then DCertDelegRegKey (deserializeStakingCredential (Builtin.headList unlisted))
       else if Builtin.equalsInteger tag 1
            then DCertDelegDeRegKey (deserializeStakingCredential (Builtin.headList unlisted))
            else if Builtin.equalsInteger tag 2
                 then let sc = deserializeStakingCredential (Builtin.headList unlisted)
                          unlisted' = Builtin.tailList unlisted
                          pkh = deserializePubKeyHash (Builtin.headList unlisted')
                        in DCertDelegDelegate sc pkh
                 else if Builtin.equalsInteger tag 3
                      then let pkh1 = deserializePubKeyHash (Builtin.headList unlisted)
                               unlisted' = Builtin.tailList unlisted
                               pkh2 = deserializePubKeyHash (Builtin.headList unlisted')
                            in DCertPoolRegister pkh1 pkh2
                      else if Builtin.equalsInteger tag 4
                           then let pkh = deserializePubKeyHash (Builtin.headList unlisted)
                                    unlisted' = Builtin.tailList unlisted
                                    i = deserializeInt (Builtin.headList unlisted')
                                  in DCertPoolRetire pkh i
                           else if Builtin.equalsInteger tag 5
                                then DCertGenesis
                                else DCertMir

serializeStakingCredential :: StakingCredential -> Builtin.BuiltinData
serializeStakingCredential sc = case sc of 
  StakingHash c -> 
    Builtin.constrData 0 (Builtin.mkCons (serializeCredential c) (Builtin.mkNilData unit))
  StakingPtr i1 i2 i3 -> 
    Builtin.constrData 1 (Builtin.mkCons (serializeInt i1)
                    (Builtin.mkCons (serializeInt i2)
                       (Builtin.mkCons (serializeInt i3)
                          (Builtin.mkNilData unit))))

deserializeStakingCredential :: Builtin.BuiltinData -> StakingCredential
deserializeStakingCredential dat = 
  let p = Builtin.unConstrData dat
      tag = Builtin.fstPair p
      unlisted = Builtin.unListData (Builtin.sndPair p)
    in if Builtin.equalsInteger tag 0
       then StakingHash (deserializeCredential (Builtin.headList unlisted))
       else let i1 = deserializeInt (Builtin.headList unlisted)
                unlisted' = Builtin.tailList unlisted
                i2 = deserializeInt (Builtin.headList unlisted')
                unlisted'' = Builtin.tailList (unlisted')
                i3 = deserializeInt (Builtin.headList unlisted'')
              in StakingPtr i1 i2 i3

serializeCredential :: Credential -> Builtin.BuiltinData
serializeCredential c = case c of 
  PubKeyCredential pkh -> Builtin.constrData 0 (Builtin.mkCons (serializePubKeyHash pkh) (Builtin.mkNilData unit))
  ScriptCredential sh -> Builtin.constrData 1 (Builtin.mkCons (serializeScriptHash sh) (Builtin.mkNilData unit))

deserializeCredential :: Builtin.BuiltinData -> Credential
deserializeCredential dat = 
  let p = Builtin.unConstrData dat
      tag = Builtin.fstPair p
      unlisted = Builtin.unListData (Builtin.sndPair p)
    in if Builtin.equalsInteger tag 0
       then PubKeyCredential (deserializePubKeyHash (Builtin.headList unlisted))
       else ScriptCredential (deserializeScriptHash (Builtin.headList unlisted))

serializeValue :: Value -> Builtin.BuiltinData
serializeValue (Value v) = 
  serializeAssocMap serializeCurrencySymbol (serializeAssocMap serializeTokenName serializeInt) v

deserializeValue :: Builtin.BuiltinData -> Value
deserializeValue dat = 
  Value (deserializeAssocMap deserializeCurrencySymbol (deserializeAssocMap
        deserializeTokenName deserializeInt) dat)

serializeCurrencySymbol :: CurrencySymbol -> Builtin.BuiltinData
serializeCurrencySymbol (CurrencySymbol bs) = serializeByteString bs

deserializeCurrencySymbol :: Builtin.BuiltinData -> CurrencySymbol
deserializeCurrencySymbol dat = CurrencySymbol (deserializeByteString dat)

serializeTokenName :: TokenName -> Builtin.BuiltinData
serializeTokenName (TokenName bs) = serializeByteString bs

deserializeTokenName :: Builtin.BuiltinData -> TokenName
deserializeTokenName dat = TokenName (deserializeByteString dat)

serializeLovelace :: Lovelace -> Builtin.BuiltinData
serializeLovelace (Lovelace i) = serializeInt i

deserializeLovelace :: Builtin.BuiltinData -> Lovelace
deserializeLovelace dat = Lovelace (deserializeInt dat)

serializePOSIXTime :: POSIXTime -> Builtin.BuiltinData
serializePOSIXTIme (POSIXTime t) = serializeInt t

deserializePOSIXTime :: Builtin.BuiltinData -> POSIXTime
deserializePOSIXTIme dat = POSIXTime (deserializeInt dat)

serializeAddress :: Address -> Builtin.BuiltinData
serializeAddress (Address rec) = 
  Builtin.constrData 0 (Builtin.mkCons (serializeCredential rec.credential) 
                  (Builtin.mkCons (serializeMaybe serializeStakingCredential rec.stakingCredential) 
                     (Builtin.mkNilData unit)))

deserializeAddress :: Builtin.BuiltinData -> Address
deserializeAddress dat = 
  let dat' = Builtin.sndPair (Builtin.unConstrData dat)
      unlisted = Builtin.unListData dat'
      credential = deserializeCredential (Builtin.headList unlisted)
      unlisted' = Builtin.tailList unlisted
      stakingCredential = deserializeMaybe deserializeStakingCredential unlisted'
    in Address { credential: credential, stakingCredential: stakingCredential }

serializePubKeyHash :: PubKeyHash -> Builtin.BuiltinData
serializePubKeyHash (PubKeyHash bs) = 
  serializeByteString bs

deserializePubKeyHash :: Builtin.BuiltinData -> PubKeyHash
deserializePubKeyHash dat = 
  PubKeyHash (deserializeByteString dat)

serializeTxId :: TxId -> Builtin.BuiltinData
serializeTxId (TxId bs) = 
  Builtin.constrData 0 (Builtin.mkCons (serializeByteString bs) (Builtin.mkNilData unit))

deserializeTxId :: Builtin.BuiltinData -> TxId
deserializeTxId dat = 
  let dat' = Builtin.sndPair (Builtin.unConstrData dat)
      unlisted = Builtin.unListData dat'
    in TxId (deserializeByteString (Builtin.headList unlisted))

serializeTxInfo :: TxInfo -> Builtin.BuiltinData
serializeTxInfo (TxInfo rec) = 
  Builtin.constrData 0 (Builtin.mkCons (serializeList serializeTxInInfo rec.inputs) 
                  (Builtin.mkCons (serializeList serializeTxInInfo rec.referenceInputs)
                     (Builtin.mkCons (serializeList serializeTxOut rec.outputs)
                        (Builtin.mkCons (serializeValue rec.fee)
                           (Builtin.mkCons (serializeValue rec.mint)
                              (Builtin.mkCons (serializeList serializeDCert rec.dCert)
                                 (Builtin.mkCons (serializeAssocMap serializeStakingCredential serializeInt rec.wdrl)
                                    (Builtin.mkCons (serializeInterval go rec.validTimeRange)
                                       (Builtin.mkCons (serializeList serializePubKeyHash rec.signatories)
                                          (Builtin.mkCons (serializeAssocMap serializeScriptPurpose serializeRedeemer rec.redeemers)
                                             (Builtin.mkCons (serializeAssocMap serializeDatumHash serializeDatum rec.data)
                                                (Builtin.mkCons (serializeTxId rec.id)
                                                   (Builtin.mkNilData unit)))))))))))))
  where
    go :: POSIXTime -> Builtin.BuiltinData
    go (POSIXTime t) = serializeInt t

deserializeTxInfo :: Builtin.BuiltinData -> TxInfo
deserializeTxInfo dat = 
  let unlisted = Builtin.unListData (Builtin.sndPair (Builtin.unConstrData dat))
      inputs =  deserializeTxInInfo (Builtin.headList unlisted)
      unlisted1 = Builtin.tailList unlisted
      referenceInputs =  deserializeTxInInfo (Builtin.headList unlisted1)
      unlisted2 = Builtin.tailList unlisted1
      outputs =  deserializeTxOut (Builtin.headList unlisted2)
      unlisted3 = Builtin.tailList unlisted2
      fee = deserializeValue (Builtin.headList unlisted3)
      unlisted4 = Builtin.tailList unlisted3
      mint = deserializeValue (Builtin.headList unlisted4)
      unlisted5 = Builtin.tailList unlisted4
      dCert =  deserializeDCert (Builtin.headList unlisted5)
      unlisted6 = Builtin.tailList unlisted5
      wdrl = deserializeAssocMap deserializeStakingCredential deserializeInt (Builtin.headList unlisted6)
      unlisted7 = Builtin.tailList unlisted6
      validTimeRange = deserializeInterval go (Builtin.headList unlisted7)
      unlisted8 = Builtin.tailList unlisted7
      signatories =  deserializePubKeyHash (Builtin.headList unlisted8)
      unlisted9 = Builtin.tailList unlisted8
      redeemers = deserializeAssocMap deserializeScriptPurpose deserializeRedeemer (Builtin.headList unlisted9)
      unlisted10 = Builtin.tailList unlisted9
      data1 = deserializeAssocMap deserializeDatumHash deserializeDatum (Builtin.headList unlisted10)
      unlisted11 = Builtin.tailList unlisted10
      id1 = deserializeTxId (Builtin.headList unlisted11)
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
  where
    go :: Builtin.BuiltinData -> POSIXTime
    go t = POSIXTime (deserializeInt t)

serializeTxOut :: TxOut -> Builtin.BuiltinData
serializeTxOut (TxOut rec) = 
  Builtin.constrData 0 (Builtin.mkCons (serializeAddress rec.address)
                  (Builtin.mkCons (serializeValue rec.value)
                     (Builtin.mkCons (serializeOutputDatum rec.outputDatum)
                        (Builtin.mkCons (serializeMaybe serializeScriptHash rec.referenceScript)
                           (Builtin.mkNilData unit)))))

deserializeTxOut :: Builtin.BuiltinData -> TxOut
deserializeTxOut dat = 
  let unlisted = Builtin.unListData (Builtin.sndPair (Builtin.unConstrData dat))
      address = deserializeAddress (Builtin.headList unlisted)
      unlisted1 = Builtin.tailList unlisted
      value = deserializeValue (Builtin.headList unlisted1)
      unlisted2 = Builtin.tailList unlisted1
      outputDatum = deserializeOutputDatum (Builtin.headList unlisted2)
      unlisted3 = Builtin.tailList unlisted2
      referenceScript = deserializeMaybe deserializeScriptHash (Builtin.headList unlisted3)
    in TxOut { address: address,
               value: value,
               outputDatum: outputDatum,
               referenceScript: referenceScript
             }

serializeTxOutRef :: TxOutRef -> Builtin.BuiltinData
serializeTxOutRef (TxOutRef rec) = 
  Builtin.constrData 0 (Builtin.mkCons (serializeTxId rec.id)
                  (Builtin.mkCons (serializeInt rec.idx)
                     (Builtin.mkNilData unit)))

deserializeTxOutRef :: Builtin.BuiltinData -> TxOutRef
deserializeTxOutRef dat = 
  let unlisted = Builtin.unListData (Builtin.sndPair (Builtin.unConstrData dat))
      id1 = deserializeTxId (Builtin.headList unlisted)
      unlisted' = Builtin.tailList unlisted
      idx = deserializeInt (Builtin.headList unlisted')
    in TxOutRef { id: id1, idx: idx }

serializeTxInInfo :: TxInInfo -> Builtin.BuiltinData
serializeTxInInfo (TxInInfo rec) = 
  Builtin.constrData 0 (Builtin.mkCons (serializeTxOutRef rec.outRef)
                  (Builtin.mkCons (serializeTxOut rec.resolved)
                     (Builtin.mkNilData unit)))

deserializeTxInInfo :: Builtin.BuiltinData -> TxInInfo
deserializeTxInInfo dat = 
  let unlisted = Builtin.unListData (Builtin.sndPair (Builtin.unConstrData dat))
      outRef = deserializeTxOutRef (Builtin.headList dat)
      unlisted' = Builtin.tailList unlisted
      resolved = deserializeTxOut (Builtin.headList unlisted')
    in TxInInfo { outRef: outRef, resolved: resolved }

serializeOutputDatum :: OutputDatum -> Builtin.BuiltinData
serializeOutputDatum od = case od of 
  NoOutputDatum -> Builtin.constrData 0 (Builtin.mkNilData unit)
  OutputDatumHash h -> Builtin.constrData 1 (Builtin.mkCons (serializeDatumHash h) (Builtin.mkNilData unit))
  OutputDatum d -> Builtin.constrData 2 (Builtin.mkCons (serializeDatum d) (Builtin.mkNilData unit))

deserializeOutputDatum :: Builtin.BuiltinData -> OutputDatum
deserializeOutputDatum dat = 
  let p = Builtin.unConstrData dat
      tag = Builtin.fstPair p
    in if Builtin.equalsInteger tag 0
       then NoOutputDatum
       else let unlisted = Builtin.unListData (Builtin.sndPair p)
                x = Builtin.headList unlisted
              in if Builtin.equalsInteger tag 1
                 then OutputDatumHash (deserializeDatumHash x)
                 else OutputDatum (deserializeDatum x)

serializeInterval :: 
  forall (a :: Type) . (a -> Builtin.BuiltinData) -> Interval a -> Builtin.BuiltinData
serializeInterval f (Interval rec) = 
  Builtin.constrData 0 (Builtin.mkCons (serializeLowerBound f rec.from) 
                  (Builtin.mkCons (serializeUpperBound f rec.to)
                     (Builtin.mkNilData unit)))

deserializeInterval :: 
  forall (a :: Type) . (Builtin.BuiltinData -> a) -> Builtin.BuiltinData -> Interval a
deserializeInterval f dat = 
  let dat' = Builtin.sndPair (Builtin.unConstrData dat)
      unlisted = Builtin.unListData dat'
      from = deserializeLowerBound f (Builtin.headList unlisted)
      unlisted' = Builtin.tailList unlisted
      to = deserializeUpperBound f (Builtin.headList unlisted')
    in Interval {from: from, to: to}

serializeExtended :: 
  forall (a :: Type) . (a -> Builtin.BuiltinData) -> Extended a -> Builtin.BuiltinData
serializeExtended f e = case e of 
  NegInf -> Builtin.constrData 0 (Builtin.mkNilData unit)
  Finite x -> Builtin.constrData 1 (Builtin.mkCons (f x) (Builtin.mkNilData unit))
  PosInf -> Builtin.constrData 2 (Builtin.mkNilData unit)

deserializeExtended :: 
  forall (a :: Type) . (Builtin.BuiltinData -> a) -> Builtin.BuiltinData -> Extended a
deserializeExtended f dat = 
  let p = Builtin.unConstrData dat
      tag = Builtin.fstPair p
    in if Builtin.equalsInteger tag 0
       then NegInf
       else if Builtin.equalsInteger tag 1
            then let unlisted = Builtin.unListData (Builtin.sndPair p)
                  in Finite (f (Builtin.headList unlisted))
            else PosInf

serializeLowerBound :: 
  forall (a :: Type) . (a -> Builtin.BuiltinData) -> LowerBound a -> Builtin.BuiltinData
serializeLowerBound f (LowerBound e) = Builtin.constrData 0 (serializeExtended f e)

deserializeLowerBound :: 
  forall (a :: Type) . (Builtin.BuiltinData -> a) -> Builtin.BuiltinData -> LowerBound a
deserializeLowerBound f dat = 
  let dat' = Builtin.sndPair (Builtin.unConstrData dat)
      unlisted = Builtin.unListData dat'
      e = deserializeExtended f (Builtin.headList unlisted)
    in LowerBound e

serializeUpperBound :: 
  forall (a :: Type) . (a -> Builtin.BuiltinData) -> UpperBound a -> Builtin.BuiltinData
serializeUpperBound f (UpperBound e) = Builtin.constrData 0 (serializeExtended f e)

deserializeUpperBound :: 
  forall (a :: Type) . (Builtin.BuiltinData -> a) -> Builtin.BuiltinData -> UpperBound a
deserializeUpperBound f dat = 
  let dat' = Builtin.sndPair (Builtin.unConstrData dat)
      unlisted = Builtin.unListData dat'
      e = deserializeExtended f (Builtin.headList unlisted)
    in UpperBound e

serializeAssocMap :: 
  forall (k :: Type) (v :: Type) . 
  (k -> Builtin.BuiltinData) -> 
  (v -> Builtin.BuiltinData) -> 
  AssocMap k v -> 
  Builtin.BuiltinData
serializeAssocMap fK fV (AssocMap ell) = Builtin.mapData (go ell)
  where
    go :: Array (Tuple2 k v) -> Array Builtin.BuiltinData
    go arr = case arr of 
      Nil -> Nil
      Cons x xs -> Cons (serializeTuple2 fK fV x) (go xs)

deserializeAssocMap :: 
  forall (k :: Type) (v :: Type) . 
  (Builtin.BuiltinData -> k) -> 
  (Builtin.BuiltinData -> v) -> 
  Builtin.BuiltinData -> 
  AssocMap k v
deserializeAssocMap fK fV dat = AssocMap (go (Builtin.unMapData dat))
  where
    go :: Array Builtin.BuiltinData -> Array (Tuple2 k v)
    go arr = case arr of 
      Nil -> Nil
      Cons d ds -> Cons (deserializeTuple2 fK fV d) (go ds)

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
  forall (a :: Type) . (a -> Builtin.BuiltinData) -> Array a -> Builtin.BuiltinData
serializeList f arr = case arr of 
  Nil -> Builtin.mkNilData unit
  Cons x xs -> Builtin.mkCons (f x) (serializeList f xs)

deserializeList :: 
  forall (a :: Type) . (Builtin.BuiltinData -> a) -> Builtin.BuiltinData -> Array a
deserializeList f dat = go (Builtin.unListData dat)
  where
    go :: Array Builtin.BuiltinData -> Array a
    go arr = case arr of 
      Nil -> Nil
      Cons d ds -> Cons (f d) (go ds)

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
    let dat' = Builtin.sndPair (Builtin.unConstrData dat)
        unlisted = Builtin.unListData dat'
        x = fA (Builtin.headList unlisted)
        unlisted' = Builtin.tailList unlisted
        y = fB (Builtin.headList unlisted') 
      in Tuple2 x y

data Maybe (a :: Type) = Nothing | Just a

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
       else let unlisted = Builtin.unListData (Builtin.sndPair p)
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
  if Builtin.lessThanEqualsInteger count 0
  then Nil
  else case ell of 
          Nil -> Nil
          Cons x xs -> Cons x (takeList (Builtin.subtractInteger count 1) xs)

dropList :: 
  forall (a :: Type) . 
  Int -> 
  Array a -> 
  Array a
dropList count ell = 
  if Builtin.lessThanEqualsInteger count 0
  then ell
  else case ell of 
           Nil -> Nil
           Cons _ xs -> dropList (Builtin.subtractInteger count 1) xs

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
  Cons _ xs -> Builtin.addInteger 1 (lengthList xs)

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
  Cons x xs -> Builtin.addInteger x (sumList xs)

productList :: Array Int -> Int
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

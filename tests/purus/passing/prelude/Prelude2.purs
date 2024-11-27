module Prelude2 where

import Prelude 

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
      unlisted = Builtin.sndPair p
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
      unlisted = Builtin.sndPair p
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
serializePOSIXTime (POSIXTime t) = serializeInt t

deserializePOSIXTime :: Builtin.BuiltinData -> POSIXTime
deserializePOSIXTime dat = POSIXTime (deserializeInt dat)

serializeAddress :: Address -> Builtin.BuiltinData
serializeAddress (Address rec) =
  Builtin.constrData 0 (Builtin.mkCons (serializeCredential rec.credential)
                  (Builtin.mkCons (serializeMaybe serializeStakingCredential rec.stakingCredential)
                     (Builtin.mkNilData unit)))

deserializeAddress :: Builtin.BuiltinData -> Address
deserializeAddress dat =
  let unlisted = Builtin.sndPair (Builtin.unConstrData dat)
      credential = deserializeCredential (Builtin.headList unlisted)
      unlisted' = Builtin.tailList unlisted
      stakingCredential = deserializeMaybe deserializeStakingCredential (Builtin.headList unlisted')
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
  let unlisted = Builtin.sndPair (Builtin.unConstrData dat)
    in TxId (deserializeByteString (Builtin.headList unlisted))

serializeTxOut :: TxOut -> Builtin.BuiltinData
serializeTxOut (TxOut rec) =
  Builtin.constrData 0 (Builtin.mkCons (serializeAddress rec.address)
                  (Builtin.mkCons (serializeValue rec.value)
                     (Builtin.mkCons (serializeOutputDatum rec.datum)
                        (Builtin.mkCons (serializeMaybe serializeScriptHash rec.referenceScript)
                           (Builtin.mkNilData unit)))))

deserializeTxOut :: Builtin.BuiltinData -> TxOut
deserializeTxOut dat =
  let unlisted = Builtin.sndPair (Builtin.unConstrData dat)
      address = deserializeAddress (Builtin.headList unlisted)
      unlisted1 = Builtin.tailList unlisted
      value = deserializeValue (Builtin.headList unlisted1)
      unlisted2 = Builtin.tailList unlisted1
      outputDatum = deserializeOutputDatum (Builtin.headList unlisted2)
      unlisted3 = Builtin.tailList unlisted2
      referenceScript = deserializeMaybe deserializeScriptHash (Builtin.headList unlisted3)
    in TxOut { address: address,
               value: value,
               datum: outputDatum,
               referenceScript: referenceScript
             }

serializeTxOutRef :: TxOutRef -> Builtin.BuiltinData
serializeTxOutRef (TxOutRef rec) =
  Builtin.constrData 0 (Builtin.mkCons (serializeTxId rec.id)
                  (Builtin.mkCons (serializeInt rec.idx)
                     (Builtin.mkNilData unit)))

deserializeTxOutRef :: Builtin.BuiltinData -> TxOutRef
deserializeTxOutRef dat =
  let unlisted = Builtin.sndPair (Builtin.unConstrData dat)
      id1 = deserializeTxId (Builtin.headList unlisted)
      unlisted' = Builtin.tailList unlisted
      idx = deserializeInt (Builtin.headList unlisted')
    in TxOutRef { id: id1, idx: idx }

serializeTxInInfo :: TxInInfo -> Builtin.BuiltinData
serializeTxInInfo (TxInInfo rec) =
  Builtin.constrData 0 (Builtin.mkCons (serializeTxOutRef rec.txOutRef)
                  (Builtin.mkCons (serializeTxOut rec.resolved)
                     (Builtin.mkNilData unit)))

deserializeTxInInfo :: Builtin.BuiltinData -> TxInInfo
deserializeTxInInfo dat =
  let unlisted = Builtin.sndPair (Builtin.unConstrData dat)
      outRef = deserializeTxOutRef (Builtin.headList unlisted)
      unlisted' = Builtin.tailList unlisted
      resolved = deserializeTxOut (Builtin.headList unlisted')
    in TxInInfo { txOutRef: outRef, resolved: resolved }

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
       else let unlisted = Builtin.sndPair p
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
  let unlisted = Builtin.sndPair (Builtin.unConstrData dat)
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
            then let unlisted = Builtin.sndPair p
                  in Finite (f (Builtin.headList unlisted))
            else PosInf

serializeLowerBound ::
  forall (a :: Type) . (a -> Builtin.BuiltinData) -> LowerBound a -> Builtin.BuiltinData
serializeLowerBound f (LowerBound e) =
  Builtin.constrData 0 (Builtin.mkCons (serializeExtended f e) (Builtin.mkNilData unit))

deserializeLowerBound ::
  forall (a :: Type) . (Builtin.BuiltinData -> a) -> Builtin.BuiltinData -> LowerBound a
deserializeLowerBound f dat =
  let unlisted = Builtin.sndPair (Builtin.unConstrData dat)
      e = deserializeExtended f (Builtin.headList unlisted)
    in LowerBound e

serializeUpperBound ::
  forall (a :: Type) . (a -> Builtin.BuiltinData) -> UpperBound a -> Builtin.BuiltinData
serializeUpperBound f (UpperBound e) =
  Builtin.constrData 0 (Builtin.mkCons (serializeExtended f e) (Builtin.mkNilData unit))

deserializeUpperBound ::
  forall (a :: Type) . (Builtin.BuiltinData -> a) -> Builtin.BuiltinData -> UpperBound a
deserializeUpperBound f dat =
  let unlisted = Builtin.sndPair (Builtin.unConstrData dat)
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
    go ::
      List (Tuple2 k v) ->
      Builtin.BuiltinList (Builtin.BuiltinPair Builtin.BuiltinData Builtin.BuiltinData)
    go = case _ of
             Cons p xs ->
               let Tuple2 x y = p
                  in Builtin.mkCons (Builtin.mkPairData (fK x) (fV y)) (go xs)
             Nil -> Builtin.mkNilPairData unit

deserializeAssocMap ::
  forall (k :: Type) (v :: Type) .
  (Builtin.BuiltinData -> k) ->
  (Builtin.BuiltinData -> v) ->
  Builtin.BuiltinData ->
  AssocMap k v
deserializeAssocMap fK fV dat = AssocMap (go (Builtin.unMapData dat))
  where
    go ::
      Builtin.BuiltinList (Builtin.BuiltinPair Builtin.BuiltinData Builtin.BuiltinData) ->
      List (Tuple2 k v)
    go ell = if Builtin.nullList ell
             then Nil
             else let p = Builtin.headList ell
                      t = Builtin.tailList ell
                      x = Builtin.fstPair p
                      y = Builtin.sndPair p
                    in Cons (Tuple2 (fK x) (fV y)) (go t)

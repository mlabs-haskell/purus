module Prelude3 where

import Prelude
import Prelude2

serializeScriptContext :: ScriptContext -> Builtin.BuiltinData
serializeScriptContext (ScriptContext rec) =
  Builtin.constrData 0 (Builtin.mkCons (serializeTxInfo rec.txInfo)
                  (Builtin.mkCons (serializeScriptPurpose rec.purpose)
                     (Builtin.mkNilData unit)))

deserializeScriptContext :: Builtin.BuiltinData -> ScriptContext
deserializeScriptContext dat =
  let unlisted = Builtin.sndPair (Builtin.unConstrData dat)
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
      x = Builtin.headList (Builtin.sndPair p)
    in if Builtin.equalsInteger 0 tag
       then Minting (deserializeCurrencySymbol x)
       else if Builtin.equalsInteger 1 tag
            then Spending (deserializeTxOutRef x)
            else if Builtin.equalsInteger 2 tag
                 then Rewarding (deserializeStakingCredential x)
                 else Certifying (deserializeDCert x)

serializeTxInfo :: TxInfo -> Builtin.BuiltinData
serializeTxInfo (TxInfo rec) =
  Builtin.constrData 0 (Builtin.mkCons (serializeList serializeTxInInfo rec.inputs)
                  (Builtin.mkCons (serializeList serializeTxInInfo rec.referenceInputs)
                     (Builtin.mkCons (serializeList serializeTxOut rec.outputs)
                        (Builtin.mkCons (serializeValue rec.fee)
                           (Builtin.mkCons (serializeValue rec.mint)
                              (Builtin.mkCons (serializeList serializeDCert rec.dCert)
                                 (Builtin.mkCons (serializeAssocMap serializeStakingCredential serializeInt rec.wdrl)
                                    (Builtin.mkCons (serializeInterval serializePOSIXTime rec.validRange)
                                       (Builtin.mkCons (serializeList serializePubKeyHash rec.signatories)
                                          (Builtin.mkCons (serializeAssocMap serializeScriptPurpose serializeRedeemer rec.redeemers)
                                             (Builtin.mkCons (serializeAssocMap serializeDatumHash serializeDatum rec.data)
                                                (Builtin.mkCons (serializeTxId rec.id)
                                                   (Builtin.mkNilData unit)))))))))))))

deserializeTxInfo :: Builtin.BuiltinData -> TxInfo
deserializeTxInfo dat =
  let unlisted = Builtin.sndPair (Builtin.unConstrData dat)
      inputs =  deserializeList deserializeTxInInfo (Builtin.headList unlisted)
      unlisted1 = Builtin.tailList unlisted
      referenceInputs = deserializeList deserializeTxInInfo (Builtin.headList unlisted1)
      unlisted2 = Builtin.tailList unlisted1
      outputs =  deserializeList deserializeTxOut (Builtin.headList unlisted2)
      unlisted3 = Builtin.tailList unlisted2
      fee = deserializeValue (Builtin.headList unlisted3)
      unlisted4 = Builtin.tailList unlisted3
      mint = deserializeValue (Builtin.headList unlisted4)
      unlisted5 = Builtin.tailList unlisted4
      dCert = deserializeList deserializeDCert (Builtin.headList unlisted5)
      unlisted6 = Builtin.tailList unlisted5
      wdrl = deserializeAssocMap deserializeStakingCredential deserializeInt (Builtin.headList unlisted6)
      unlisted7 = Builtin.tailList unlisted6
      validTimeRange = deserializeInterval deserializePOSIXTime (Builtin.headList unlisted7)
      unlisted8 = Builtin.tailList unlisted7
      signatories = deserializeList deserializePubKeyHash (Builtin.headList unlisted8)
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
                validRange: validTimeRange,
                signatories: signatories,
                redeemers: redeemers,
                data: data1,
                id: id1
              }

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
      unlisted = Builtin.sndPair p
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

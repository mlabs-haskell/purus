module Validator where


equalsPKH :: PubKeyHash -> PubKeyHash -> Boolean
equalsPKH (PubKeyHash bs1) (PubKeyHash bs2) = Builtin.equalsByteString bs1 bs2

validateIfSignedBy :: forall (a :: Type) (b :: Type). PubKeyHash -> a -> b -> Builtin.BuiltinData -> Boolean
validateIfSignedBy pkh _ _ cxtData = anyList (equalsPKH pkh) signatories 
  where
    ScriptContext cxt = deserializeScriptContext cxtData
    TxInfo info = cxt.txInfo
    signatories = info.signatories

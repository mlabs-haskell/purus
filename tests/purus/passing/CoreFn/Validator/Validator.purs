module Validator where

data C (a :: Type) (b :: Type) (c :: Type) = C a b c

and :: Boolean -> Boolean -> Boolean
and True True = True
and _ _ = False

-- iffbool I guess
eqBool :: Boolean -> Boolean -> Boolean
eqBool True True = True
eqBool False False = True
eqBool _ _ = False


equalsC :: C Int String (Maybe Boolean) -> C Int String (Maybe Boolean) -> Boolean
equalsC (C i1 s1 (Just b1)) (C i2 s2 (Just b2)) = and (Builtin.equalsInteger i1 i2)
                                                  (and (Builtin.equalsString s1 s2)
                                                       (eqBool b1 b2)
                                                  )
equalsC (C i1 s1 Nothing) (C i2 s2 Nothing) = and (Builtin.equalsInteger i1 i2) (Builtin.equalsString s1 s2)
equalsC _ _ = False 

{-
equalsPKH :: PubKeyHash -> PubKeyHash -> Boolean
equalsPKH (PubKeyHash bs1) (PubKeyHash bs2) = Builtin.equalsByteString bs1 bs2

validateIfSignedBy :: forall (a :: Type) (b :: Type). PubKeyHash -> a -> b -> Builtin.BuiltinData -> Boolean
validateIfSignedBy pkh _ _ cxtData = anyList (equalsPKH pkh) signatories 
  where
    scxt = deserializeScriptContext cxtData
    ScriptContext cxt = scxt
    TxInfo info = cxt.txInfo
    signatories = info.signatories
-}

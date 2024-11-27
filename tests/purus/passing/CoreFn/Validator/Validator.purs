module Validator where

import Prim


--not :: Boolean -> Boolean
--not True = False
--not False = True

--foo :: forall (a :: Type) . (Builtin.BuiltinData -> a) -> Builtin.BuiltinList Builtin.BuiltinData -> List a
--foo f = go f

--go :: forall (a :: Type). (Builtin.BuiltinData -> a) -> Builtin.BuiltinList Builtin.BuiltinData -> List a
--go f xs = if not (Builtin.nullList xs)
--              then Cons (f  (Builtin.headList xs)) (go f (Builtin.tailList xs))
--              else Nil

{-
aTest :: forall (k :: Type) (v :: Type)
      . (Builtin.BuiltinData -> k)
      -> (Builtin.BuiltinData -> v)
      -> Builtin.BuiltinData
      -> AssocMap k v
aTest fK' fV' dat = AssocMap (go fK' fV' (Builtin.unMapData dat))

go :: forall (k :: Type) (v :: Type)
      . (Builtin.BuiltinData -> k)
      -> (Builtin.BuiltinData -> v)
      -> Builtin.BuiltinList (Builtin.BuiltinPair Builtin.BuiltinData Builtin.BuiltinData)
      ->List (Tuple2 k v)
go fK fV ell = if not (Builtin.nullList ell)
             then let p = Builtin.headList ell
                      t = Builtin.tailList ell
                      x = Builtin.fstPair p
                      y = Builtin.sndPair p
                  in Cons (Tuple2 (fK x) (fV y)) (go fK fV t)
             else Nil 
-}
equalsPKH :: PubKeyHash -> PubKeyHash -> Boolean
equalsPKH (PubKeyHash bs1) (PubKeyHash bs2) = Builtin.equalsByteString bs1 bs2

validateIfSignedBy :: forall (a :: Type) (b :: Type). PubKeyHash -> a -> b -> Builtin.BuiltinData -> Boolean
validateIfSignedBy pkh _ _ cxtData = anyList (equalsPKH pkh) signatories 
  where
    scxt = Prim.deserializeScriptContext cxtData
    ScriptContext cxt = scxt
    TxInfo info = cxt.txInfo
    signatories = info.signatories

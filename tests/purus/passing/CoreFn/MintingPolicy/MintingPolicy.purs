module MintingPolicy where

guard :: Boolean -> Int
guard b = case b of
  True -> 1
  False -> Builtin.divideInteger 1 0

oneAtATime :: Builtin.BuiltinData -> Builtin.BuiltinData -> Builtin.BuiltinData -> Int
oneAtATime _ _ cxtData = case mintList of
    Cons valMap xs -> case xs of
      Nil -> guard False
      _ -> guard True
    _ -> guard False
  where
    scxt = Prim.deserializeScriptContext cxtData
    ScriptContext cxt = scxt 
    TxInfo info = cxt.txInfo
    Value mintMap = info.mint
    AssocMap mintList = mintMap 

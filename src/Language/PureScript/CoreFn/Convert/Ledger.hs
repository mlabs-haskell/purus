module Language.PureScript.CoreFn.Convert.Ledger (
  -- * Whole lot
  ledgerTypes,
  -- * Individual types
  -- ** Context types
  scriptContextType,
  scriptPurposeType,
  -- ** Bytes
  ledgerBytesType,
  -- ** Certificates
  dcertType,
  -- ** Credentials
  stakingCredentialType,
  credentialType,
  -- ** Value
  valueType,
  currencySymbolType,
  tokenNameType,
  lovelaceType,
  -- ** Time
  posixTimeType,
  -- ** Types for representing transactions
  addressType,
  pubKeyHashType,
  txIdType,
  txInfoType,
  txOutType,
  txOutRefType,
  txInInfoType,
  outputDatumType,
  -- ** Intervals
  intervalType,
  extendedType,
  upperBoundType,
  lowerBoundType,
  -- ** Association maps
  assocMapType,
  -- ** Newtypes and hash types
  scriptHashType,
  redeemerType,
  redeemerHashType,
  datumType,
  datumHashType,
  -- ** Data
  dataType
  ) where

import Language.PureScript.Roles (Role (Nominal))
import Language.PureScript.Label (Label)
import Language.PureScript.Constants.Prim qualified as C
import Prelude
import Data.Text (Text)
import Language.PureScript.Names (
  Qualified (Qualified),
  ProperNameType (TypeName, ConstructorName),
  ProperName (ProperName),
  QualifiedBy (ByModuleName),
  ModuleName (ModuleName)
  )
import Language.PureScript.Types (
  Type (TypeConstructor, TypeApp, REmpty, RCons, TypeVar)
  )
import Language.PureScript.Environment (
  TypeKind (DataType), 
  kindType, (-:>), 
  DataDeclType (Newtype, Data)
  )
import Language.PureScript.AST.SourcePos (nullSourceAnn, SourceAnn)

-- | Ledger API (V2) types, as per https://github.com/IntersectMBO/plutus/blob/master/plutus-ledger-api/src/PlutusLedgerApi/V2.hs
ledgerTypes :: [(Qualified (ProperName 'TypeName), (Type SourceAnn, TypeKind))]
ledgerTypes = [
  -- Context types
  scriptContextType,
  scriptPurposeType,
  -- Bytes
  ledgerBytesType,
  -- Certificates
  dcertType,
  -- Credentials
  stakingCredentialType,
  credentialType,
  -- Value
  valueType,
  currencySymbolType,
  tokenNameType,
  lovelaceType,
  -- Time
  posixTimeType,
  -- No POSIXTimeRange, people can use the actual type instead of a synonym
  -- Types for representing transactions
  addressType,
  pubKeyHashType,
  txIdType,
  txInfoType,
  txOutType,
  txOutRefType,
  txInInfoType,
  outputDatumType,
  -- Intervals
  intervalType,
  extendedType,
  upperBoundType,
  lowerBoundType,
  -- Association maps
  assocMapType,
  -- Newtypes and hash types
  scriptHashType,
  redeemerType,
  redeemerHashType,
  datumType,
  datumHashType,
  -- Data
  dataType
  ]

scriptContextType :: (Qualified (ProperName 'TypeName), (Type SourceAnn, TypeKind))
scriptContextType = 
  monoType "ScriptContext" . 
  recordType "ScriptContext" $ 
  [("txInfo", tyCon "TxInfo"),
   ("purpose", tyCon "ScriptPurpose")]

scriptPurposeType :: (Qualified (ProperName 'TypeName), (Type SourceAnn, TypeKind))
scriptPurposeType = 
  monoType "ScriptPurpose" . 
  sumType $ 
  [("Minting", [tyCon "CurrencySymbol"]),
   ("Spending", [tyCon "TxOutRef"]),
   ("Rewarding", [tyCon "StakingCredential"]),
   ("Certifying", [tyCon "DCert"])]

ledgerBytesType :: (Qualified (ProperName 'TypeName), (Type SourceAnn, TypeKind))
ledgerBytesType = newtypeOf "LedgerBytes" (tyCon "BuiltinByteString")

dcertType :: (Qualified (ProperName 'TypeName), (Type SourceAnn, TypeKind))
dcertType = 
  monoType "DCert" . 
  sumType $ 
  [("DCertDelegRegKey", [tyCon "StakingCredential"]),
   ("DCertDelegDeRegKey", [tyCon "StakingCredential"]),
   ("DCertDelegDelegate", [tyCon "StakingCredential", tyCon "PubKeyHash"]),
   ("DCertPoolRegister", [tyCon "PubKeyHash", tyCon "PubKeyHash"]),
   ("DCertPoolRetire", [tyCon "PubKeyHash", tyCon "Int"]),
   ("DCertGenesis", []),
   ("DCertMir", [])]

stakingCredentialType :: (Qualified (ProperName 'TypeName), (Type SourceAnn, TypeKind))
stakingCredentialType = 
  monoType "StakingCredential" . 
  sumType $ 
  [("StakingHash", [tyCon "Credential"]),
   ("StakingPtr", [tyCon "Int", tyCon "Int", tyCon "Int"])]

credentialType :: (Qualified (ProperName 'TypeName), (Type SourceAnn, TypeKind))
credentialType = 
  monoType "Credential" . 
  sumType $ 
  [("PubKeyCredential", [tyCon "PubKeyHash"]),
   ("ScriptCredential", [tyCon "ScriptHash"])]

valueType :: (Qualified (ProperName 'TypeName), (Type SourceAnn, TypeKind))
valueType = newtypeOf "Value" (mapOf (tyCon "CurrencySymbol") (mapOf (tyCon "TokenName") (tyCon "Int")))

currencySymbolType :: (Qualified (ProperName 'TypeName), (Type SourceAnn, TypeKind))
currencySymbolType = newtypeOf "CurrencySymbol" (tyCon "BuiltinByteString")

tokenNameType :: (Qualified (ProperName 'TypeName), (Type SourceAnn, TypeKind))
tokenNameType = newtypeOf "TokenName" (tyCon "BuiltinByteString")

lovelaceType :: (Qualified (ProperName 'TypeName), (Type SourceAnn, TypeKind))
lovelaceType = newtypeOf "Lovelace" (tyCon "Int")

posixTimeType :: (Qualified (ProperName 'TypeName), (Type SourceAnn, TypeKind))
posixTimeType = newtypeOf "POSIXTime" (tyCon "Int")

addressType :: (Qualified (ProperName 'TypeName), (Type SourceAnn, TypeKind))
addressType = 
  monoType "Address" . 
  recordType "Address" $ 
  [("credential", tyCon "Credential"),
   ("stakingCredential", maybeOf (tyCon "StakingCredential"))]

pubKeyHashType :: (Qualified (ProperName 'TypeName), (Type SourceAnn, TypeKind))
pubKeyHashType = newtypeOf "PubKeyHash" (tyCon "BuiltinByteString")

txIdType :: (Qualified (ProperName 'TypeName), (Type SourceAnn, TypeKind))
txIdType = newtypeOf "TxId" (tyCon "BuiltinByteString")

txInfoType :: (Qualified (ProperName 'TypeName), (Type SourceAnn, TypeKind))
txInfoType = 
  monoType "TxInfo" . 
  recordType "TxInfo" $ 
  [("inputs", listOf (tyCon "TxInInfo")),
   ("referenceInputs", listOf (tyCon "TxInInfo")),
   ("outputs", listOf (tyCon "TxOut")),
   ("fee", tyCon "Value"),
   ("mint", tyCon "Value"),
   ("dCert", listOf (tyCon "DCert")),
   ("wdrl", mapOf (tyCon "StakingCredential") (tyCon "Int")),
   ("validRange", tyApp (tyCon "Interval") (tyCon "POSIXTime")),
   ("signatories", listOf (tyCon "PubKeyHash")),
   ("redeemers", mapOf (tyCon "ScriptPurpose") (tyCon "Redeemer")),
   ("data", mapOf (tyCon "DatumHash") (tyCon "Datum")),
   ("id", tyCon "TxId")]

txOutType :: (Qualified (ProperName 'TypeName), (Type SourceAnn, TypeKind))
txOutType = 
  monoType "TxOut" . 
  recordType "TxOut" $ 
  [("address", tyCon "Address"),
   ("value", tyCon "Value"),
   ("datum", tyCon "OutputDatum"),
   ("referenceScript", maybeOf (tyCon "ScriptHash"))]

txOutRefType :: (Qualified (ProperName 'TypeName), (Type SourceAnn, TypeKind))
txOutRefType = 
  monoType "TxOutRef" . 
  recordType "TxOut" $ 
  [("id", tyCon "TxId"),
   ("idx", tyCon "Int")]

txInInfoType :: (Qualified (ProperName 'TypeName), (Type SourceAnn, TypeKind))
txInInfoType = 
  monoType "TxInInfo" . 
  recordType "TxInInfo" $ 
  [("txOutRef", tyCon "TxOutRef"),
   ("resolved", tyCon "TxOut")]

outputDatumType :: (Qualified (ProperName 'TypeName), (Type SourceAnn, TypeKind))
outputDatumType = 
  monoType "OutputDatum" . 
  sumType $ 
  [("NoOutputDatum", []),
   ("OutputDatumHash", [tyCon "DatumHash"]),
   ("OutputDatum", [tyCon "Datum"])]

intervalType :: (Qualified (ProperName 'TypeName), (Type SourceAnn, TypeKind))
intervalType = 
  polyType "Interval" ["a"] . 
  polyRecordType "Interval" ["a"] $ 
  [("from", tyApp (tyCon "LowerBound") (tyVar "a")),
   ("to", tyApp (tyCon "UpperBound") (tyVar "a"))]

extendedType :: (Qualified (ProperName 'TypeName), (Type SourceAnn, TypeKind))
extendedType = 
  polyType "Extended" ["a"] .
  polySumType ["a"] $ 
  [("NegInf", []),
   ("Finite", [tyVar "a"]),
   ("PosInf", [])]

upperBoundType :: (Qualified (ProperName 'TypeName), (Type SourceAnn, TypeKind))
upperBoundType = polyNewtypeOf "UpperBound" ["a"] (tyApp (tyCon "Extended") (tyVar "a"))

lowerBoundType :: (Qualified (ProperName 'TypeName), (Type SourceAnn, TypeKind))
lowerBoundType = polyNewtypeOf "LowerBound" ["a"] (tyApp (tyCon "Extended") (tyVar "a"))

assocMapType :: (Qualified (ProperName 'TypeName), (Type SourceAnn, TypeKind))
assocMapType = (
  primName "AssocMap", 
  (kindType -:> kindType -:> kindType,
   DataType Newtype
            [nominalVar "k", nominalVar "v"]
            [(ProperName "AssocMap", [listOf (tuple2Of (tyVar "k") (tyVar "v"))])])
  )

scriptHashType :: (Qualified (ProperName 'TypeName), (Type SourceAnn, TypeKind))
scriptHashType = newtypeOf "ScriptHash" (tyCon "BuiltinByteString")

redeemerType :: (Qualified (ProperName 'TypeName), (Type SourceAnn, TypeKind))
redeemerType = newtypeOf "Redeemer" (tyCon "BuiltinData")

redeemerHashType :: (Qualified (ProperName 'TypeName), (Type SourceAnn, TypeKind))
redeemerHashType = newtypeOf "RedeemerHash" (tyCon "BuiltinByteString")

datumType :: (Qualified (ProperName 'TypeName), (Type SourceAnn, TypeKind))
datumType = newtypeOf "Datum" (tyCon "BuiltinData")

datumHashType :: (Qualified (ProperName 'TypeName), (Type SourceAnn, TypeKind))
datumHashType = newtypeOf "DatumHash" (tyCon "BuiltinByteString")

dataType :: (Qualified (ProperName 'TypeName), (Type SourceAnn, TypeKind))
dataType = 
  monoType "Data" . 
  sumType $ 
  [("Constr", [tyCon "Int", listOf (tyCon "Data")]),
   ("Map", [listOf (tuple2Of (tyCon "Data") (tyCon "Data"))]),
   ("List", [listOf (tyCon "Data")]),
   ("I", [tyCon "Int"]),
   ("B", [tyCon "BuiltinByteString"])]

-- Helpers

monoType :: Text -> TypeKind -> (Qualified (ProperName 'TypeName), (Type SourceAnn, TypeKind))
monoType tyName tyDef = (primName tyName, (kindType, tyDef)) 

polyType :: Text -> [Text] -> TypeKind -> (Qualified (ProperName 'TypeName), (Type SourceAnn, TypeKind))
polyType tyName vars tyDef = (primName tyName, (go vars, tyDef))
  where
    go :: [Text] -> Type SourceAnn
    go = \case
      [] -> kindType
      (_ : vs) -> kindType -:> go vs

primName :: Text -> Qualified (ProperName 'TypeName)
primName tyName = Qualified (ByModuleName (ModuleName "Prim")) (ProperName tyName)

recordType :: Text -> [(Label, Type SourceAnn)] -> TypeKind
recordType conName fields = DataType Newtype [] [(ProperName conName, [mkRecordT . go $ fields])]
  where
    go :: [(Label, Type SourceAnn)] -> Type SourceAnn
    go = \case
      [] -> REmpty nullSourceAnn
      ((fName, fType) : xs) -> RCons nullSourceAnn fName fType $ go xs

polyRecordType :: Text -> [Text] -> [(Label, Type SourceAnn)] -> TypeKind
polyRecordType conName vars fields = 
  DataType Newtype (fmap nominalVar vars) [(ProperName conName, [mkRecordT . go $ fields])]
  where  
    go :: [(Label, Type SourceAnn)] -> Type SourceAnn
    go = \case
      [] -> REmpty nullSourceAnn
      ((fName, fType) : xs) -> RCons nullSourceAnn fName fType $ go xs

tyCon :: Text -> Type SourceAnn
tyCon = TypeConstructor nullSourceAnn . primName

mkRecordT :: Type SourceAnn -> Type SourceAnn
mkRecordT = TypeApp nullSourceAnn (TypeConstructor nullSourceAnn C.Record)

sumType :: [(Text, [Type SourceAnn])] -> TypeKind
sumType = DataType Data [] . fmap go
  where
    go :: (Text, [Type SourceAnn]) -> (ProperName 'ConstructorName, [Type SourceAnn])
    go (varName, varArgs) = (ProperName varName, varArgs)

newtypeOf :: Text -> Type SourceAnn -> (Qualified (ProperName 'TypeName), (Type SourceAnn, TypeKind))
newtypeOf tyName def = 
  (primName tyName, (kindType, DataType Newtype [] [(ProperName tyName, [def])]))

polyNewtypeOf :: Text -> [Text] -> Type SourceAnn -> (Qualified (ProperName 'TypeName), (Type SourceAnn, TypeKind))
polyNewtypeOf tyName vars def = 
  (primName tyName, (go vars, DataType Newtype (fmap nominalVar vars) [(ProperName tyName, [def])]))
  where
    go :: [Text] -> Type SourceAnn
    go = \case
      [] -> kindType
      (_ : vs) -> kindType -:> go vs

mapOf :: Type SourceAnn -> Type SourceAnn -> Type SourceAnn
mapOf keyT = tyApp (tyApp (tyCon "AssocMap") keyT)

tyApp :: Type SourceAnn -> Type SourceAnn -> Type SourceAnn
tyApp f = TypeApp nullSourceAnn f

maybeOf :: Type SourceAnn -> Type SourceAnn
maybeOf = tyApp (tyCon "Maybe")

listOf :: Type SourceAnn -> Type SourceAnn
listOf = tyApp (tyCon "Array")

tuple2Of :: Type SourceAnn -> Type SourceAnn -> Type SourceAnn
tuple2Of x = tyApp (tyApp (tyCon "Tuple2") x)

nominalVar :: Text -> (Text, Type SourceAnn, Role)
nominalVar varName = (varName, kindType, Nominal)

tyVar :: Text -> Type SourceAnn
tyVar varName = TypeVar nullSourceAnn varName kindType

polySumType :: [Text] -> [(Text, [Type SourceAnn])] -> TypeKind
polySumType vars = DataType Data (fmap nominalVar vars)  . fmap go
  where
    go :: (Text, [Type SourceAnn]) -> (ProperName 'ConstructorName, [Type SourceAnn])
    go (varName, varArgs) = (ProperName varName, varArgs)

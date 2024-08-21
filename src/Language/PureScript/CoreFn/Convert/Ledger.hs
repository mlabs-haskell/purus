module Language.PureScript.CoreFn.Convert.Ledger (
  -- * Whole lot
  ledgerTypes,
  ledgerDecls,
  -- * Individual types
  -- ** Context types
  -- *** ScriptContext
  scriptContextType,
  scriptContextDecl,
  -- *** ScriptPurpose
  scriptPurposeType,
  scriptPurposeDecl,
  -- ** Bytes
  -- *** LedgerBytes
  ledgerBytesType,
  ledgerBytesDecl,
  -- ** Certificates
  dcertType,
  dcertDecl,
  -- ** Credentials
  stakingCredentialType,
  stakingCredentialDecl,
  credentialType,
  credentialDecl,
  -- ** Value
  valueType,
  valueDecl,
  currencySymbolType,
  currencySymbolDecl,
  tokenNameType,
  tokenNameDecl,
  lovelaceType,
  lovelaceDecl,
  -- ** Time
  posixTimeType,
  posixTimeDecl,
  -- ** Types for representing transactions
  addressType,
  addressDecl,
  pubKeyHashType,
  pubKeyHashDecl,
  txIdType,
  txIdDecl,
  txInfoType,
  txInfoDecl,
  txOutType,
  txOutDecl,
  txOutRefType,
  txOutRefDecl,
  txInInfoType,
  txInInfoDecl,
  outputDatumType,
  outputDatumDecl,
  -- ** Intervals
  intervalType,
  intervalDecl,
  extendedType,
  extendedDecl,
  upperBoundType,
  upperBoundDecl,
  lowerBoundType,
  lowerBoundDecl,
  -- ** Association maps
  assocMapType,
  assocMapDecl,
  -- ** Newtypes and hash types
  scriptHashType,
  scriptHashDecl,
  redeemerType,
  redeemerDecl,
  redeemerHashType,
  redeemerHashDecl,
  datumType,
  datumDecl,
  datumHashType,
  datumHashDecl,
  -- ** Data
  dataType,
  dataDecl
  ) where

import Language.PureScript.CoreFn.Desugar.Utils (properToIdent)
import Language.PureScript.CoreFn.Convert.IR (Kind (KindType), Ty (TyCon, TyApp, TyVar))
import Language.PureScript.CoreFn.Module (
  DataDecl (DataDecl), 
  CtorDecl (CtorDecl)
  )
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
  ModuleName (ModuleName),
  Ident (Ident, UnusedIdent)
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

-- | Ledger API constructor declarations, as per https://github.com/IntersectMBO/plutus/blob/master/plutus-ledger-api/src/PlutusLedgerApi/V2.hs
ledgerDecls :: [(Qualified (ProperName 'TypeName), DataDecl Kind Ty)]
ledgerDecls = [
  -- Context types
  scriptContextDecl,
  scriptPurposeDecl,
  -- Bytes
  ledgerBytesDecl,
  -- Certificates
  dcertDecl,
  -- Credentials
  stakingCredentialDecl,
  credentialDecl,
  -- Value
  valueDecl,
  currencySymbolDecl,
  tokenNameDecl,
  lovelaceDecl,
  -- Time
  posixTimeDecl,
  -- No POSIXTimeRange, people can use the actual type instead of a synonym
  -- Types for representing transactions
  addressDecl,
  pubKeyHashDecl,
  txIdDecl,
  txInfoDecl,
  txOutDecl,
  txOutRefDecl,
  txInInfoDecl,
  outputDatumDecl,
  -- Intervals
  intervalDecl,
  extendedDecl,
  upperBoundDecl,
  lowerBoundDecl,
  -- Association maps
  assocMapDecl,
  -- Newtypes and hash types
  scriptHashDecl,
  redeemerDecl,
  redeemerHashDecl,
  datumDecl,
  datumHashDecl,
  -- Data
  dataDecl
  ]

scriptContextType :: (Qualified (ProperName 'TypeName), (Type SourceAnn, TypeKind))
scriptContextType = 
  monoType "ScriptContext" . 
  recordType "ScriptContext" $ 
  [("txInfo", tyCon "TxInfo"),
   ("purpose", tyCon "ScriptPurpose")]

scriptContextDecl :: (Qualified (ProperName 'TypeName), DataDecl Kind Ty)
scriptContextDecl = recordDecl "ScriptContext" [("txInfo", primTyCon "TxInfo"),
                                                ("purpose", primTyCon "ScriptPurpose")]

scriptPurposeType :: (Qualified (ProperName 'TypeName), (Type SourceAnn, TypeKind))
scriptPurposeType = 
  monoType "ScriptPurpose" . 
  sumType $ 
  [("Minting", [tyCon "CurrencySymbol"]),
   ("Spending", [tyCon "TxOutRef"]),
   ("Rewarding", [tyCon "StakingCredential"]),
   ("Certifying", [tyCon "DCert"])]

scriptPurposeDecl :: (Qualified (ProperName 'TypeName), DataDecl Kind Ty)
scriptPurposeDecl =
  sumDecl "ScriptPurpose" [("Minting", [primTyCon "CurrencySymbol"]),
                           ("Spending", [primTyCon "TxOutRef"]),
                           ("Rewarding", [primTyCon "StakingCredential"]),
                           ("Certifying", [primTyCon "DCert"])]

ledgerBytesType :: (Qualified (ProperName 'TypeName), (Type SourceAnn, TypeKind))
ledgerBytesType = newtypeOf "LedgerBytes" (tyCon "BuiltinByteString")

ledgerBytesDecl :: (Qualified (ProperName 'TypeName), DataDecl Kind Ty)
ledgerBytesDecl = newtypeDecl "LedgerBytes" . primTyCon $ "BuiltinByteString"

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

dcertDecl :: (Qualified (ProperName 'TypeName), DataDecl Kind Ty)
dcertDecl = 
  sumDecl "DCert" [("DCertDelegRegKey", [primTyCon "StakingCredential"]),
                   ("DCertDelegDeRegKey", [primTyCon "StakingCredential"]),
                   ("DCertDelegDelegate", [primTyCon "StakingCredential", primTyCon "PubKeyHash"]),
                   ("DCertPoolRegister", [primTyCon "PubKeyHash", primTyCon "PubKeyHash"]),
                   ("DCertPoolRetire", [primTyCon "PubKeyHash", primTyCon "Int"]),
                   ("DCertGenesis", []),
                   ("DCertMir", [])]

stakingCredentialType :: (Qualified (ProperName 'TypeName), (Type SourceAnn, TypeKind))
stakingCredentialType = 
  monoType "StakingCredential" . 
  sumType $ 
  [("StakingHash", [tyCon "Credential"]),
   ("StakingPtr", [tyCon "Int", tyCon "Int", tyCon "Int"])]

stakingCredentialDecl :: (Qualified (ProperName 'TypeName), DataDecl Kind Ty)
stakingCredentialDecl = 
  sumDecl "StakingCredential" [("StakingHash", [primTyCon "Credential"]),
                               ("StakingPtr", [primTyCon "Int", primTyCon "Int", primTyCon "Int"])]

credentialType :: (Qualified (ProperName 'TypeName), (Type SourceAnn, TypeKind))
credentialType = 
  monoType "Credential" . 
  sumType $ 
  [("PubKeyCredential", [tyCon "PubKeyHash"]),
   ("ScriptCredential", [tyCon "ScriptHash"])]

credentialDecl :: (Qualified (ProperName 'TypeName), DataDecl Kind Ty)
credentialDecl = 
  sumDecl "Credential" [("PubKeyCredential", [primTyCon "PubKeyHash"]),
                        ("ScriptCredential", [primTyCon "ScriptHash"])]

valueType :: (Qualified (ProperName 'TypeName), (Type SourceAnn, TypeKind))
valueType = newtypeOf "Value" (mapOf (tyCon "CurrencySymbol") (mapOf (tyCon "TokenName") (tyCon "Int")))

valueDecl :: (Qualified (ProperName 'TypeName), DataDecl Kind Ty)
valueDecl = 
  newtypeDecl "Value" . 
  mapTy (primTyCon "CurrencySymbol") . 
  mapTy (primTyCon "TokenName") . 
  primTyCon $ "Int"

currencySymbolType :: (Qualified (ProperName 'TypeName), (Type SourceAnn, TypeKind))
currencySymbolType = newtypeOf "CurrencySymbol" (tyCon "BuiltinByteString")

currencySymbolDecl :: (Qualified (ProperName 'TypeName), DataDecl Kind Ty)
currencySymbolDecl = newtypeDecl "CurrencySymbol" . primTyCon $ "BuiltinByteString"

tokenNameType :: (Qualified (ProperName 'TypeName), (Type SourceAnn, TypeKind))
tokenNameType = newtypeOf "TokenName" (tyCon "BuiltinByteString")

tokenNameDecl :: (Qualified (ProperName 'TypeName), DataDecl Kind Ty)
tokenNameDecl = newtypeDecl "TokenName" . primTyCon $ "BuiltinByteString"

lovelaceType :: (Qualified (ProperName 'TypeName), (Type SourceAnn, TypeKind))
lovelaceType = newtypeOf "Lovelace" (tyCon "Int")

lovelaceDecl :: (Qualified (ProperName 'TypeName), DataDecl Kind Ty)
lovelaceDecl = newtypeDecl "Lovelace" . primTyCon $ "Int"

posixTimeType :: (Qualified (ProperName 'TypeName), (Type SourceAnn, TypeKind))
posixTimeType = newtypeOf "POSIXTime" (tyCon "Int")

posixTimeDecl :: (Qualified (ProperName 'TypeName), DataDecl Kind Ty)
posixTimeDecl = newtypeDecl "POSIXTime" . primTyCon $ "Int"

addressType :: (Qualified (ProperName 'TypeName), (Type SourceAnn, TypeKind))
addressType = 
  monoType "Address" . 
  recordType "Address" $ 
  [("credential", tyCon "Credential"),
   ("stakingCredential", maybeOf (tyCon "StakingCredential"))]

addressDecl :: (Qualified (ProperName 'TypeName), DataDecl Kind Ty)
addressDecl = recordDecl "Address" [("credential", primTyCon "Credential"),
                                    ("stakingCredential", maybeTy . primTyCon $ "StakingCredential")]

pubKeyHashType :: (Qualified (ProperName 'TypeName), (Type SourceAnn, TypeKind))
pubKeyHashType = newtypeOf "PubKeyHash" (tyCon "BuiltinByteString")

pubKeyHashDecl :: (Qualified (ProperName 'TypeName), DataDecl Kind Ty)
pubKeyHashDecl = newtypeDecl "PubKeyHash" . primTyCon $ "BuiltinByteString"

txIdType :: (Qualified (ProperName 'TypeName), (Type SourceAnn, TypeKind))
txIdType = newtypeOf "TxId" (tyCon "BuiltinByteString")

txIdDecl :: (Qualified (ProperName 'TypeName), DataDecl Kind Ty)
txIdDecl = newtypeDecl "TxId" . primTyCon $ "BuiltinByteString"

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

txInfoDecl :: (Qualified (ProperName 'TypeName), DataDecl Kind Ty)
txInfoDecl = recordDecl "TxInfo" [("inputs", listTy . primTyCon $ "TxInInfo"),
                                  ("referenceInputs", listTy . primTyCon $ "TxInInfo"),
                                  ("outputs", listTy . primTyCon $ "TxOut"),
                                  ("fee", primTyCon "Value"),
                                  ("mint", primTyCon "Value"),
                                  ("dCert", listTy . primTyCon $ "DCert"),
                                  ("wdrl", mapTy (primTyCon "StakingCredential") (primTyCon "Int")),
                                  ("validRange", TyApp (primTyCon "Interval") (primTyCon "POSIXTime")),
                                  ("signatories", listTy . primTyCon $ "PubKeyHash"),
                                  ("redeemers", mapTy (primTyCon "ScriptPurpose") (primTyCon "Redeemer")),
                                  ("data", mapTy (primTyCon "DatumHash") (primTyCon "Datum")),
                                  ("id", primTyCon "TxId")]

txOutType :: (Qualified (ProperName 'TypeName), (Type SourceAnn, TypeKind))
txOutType = 
  monoType "TxOut" . 
  recordType "TxOut" $ 
  [("address", tyCon "Address"),
   ("value", tyCon "Value"),
   ("datum", tyCon "OutputDatum"),
   ("referenceScript", maybeOf (tyCon "ScriptHash"))]

txOutDecl :: (Qualified (ProperName 'TypeName), DataDecl Kind Ty)
txOutDecl = recordDecl "TxOut" [("address", primTyCon "Address"),
                                ("value", primTyCon "Value"),
                                ("datum", primTyCon "OutputDatum"),
                                ("referenceScript", maybeTy . primTyCon $ "ScriptHash")]

txOutRefType :: (Qualified (ProperName 'TypeName), (Type SourceAnn, TypeKind))
txOutRefType = 
  monoType "TxOutRef" . 
  recordType "TxOut" $ 
  [("id", tyCon "TxId"),
   ("idx", tyCon "Int")]

txOutRefDecl :: (Qualified (ProperName 'TypeName), DataDecl Kind Ty)
txOutRefDecl = recordDecl "TxOutRef" [("id", primTyCon "TxId"),
                                      ("idx", primTyCon "Int")]

txInInfoType :: (Qualified (ProperName 'TypeName), (Type SourceAnn, TypeKind))
txInInfoType = 
  monoType "TxInInfo" . 
  recordType "TxInInfo" $ 
  [("txOutRef", tyCon "TxOutRef"),
   ("resolved", tyCon "TxOut")]

txInInfoDecl :: (Qualified (ProperName 'TypeName), DataDecl Kind Ty)
txInInfoDecl = recordDecl "TxInInfo" [("txOutRef", primTyCon "TxOutRef"),
                                      ("resolved", primTyCon "TxOut")]

outputDatumType :: (Qualified (ProperName 'TypeName), (Type SourceAnn, TypeKind))
outputDatumType = 
  monoType "OutputDatum" . 
  sumType $ 
  [("NoOutputDatum", []),
   ("OutputDatumHash", [tyCon "DatumHash"]),
   ("OutputDatum", [tyCon "Datum"])]

outputDatumDecl :: (Qualified (ProperName 'TypeName), DataDecl Kind Ty)
outputDatumDecl = 
  sumDecl "OutputDatum" [("NoOutputDatum", []),
                         ("OutputDatumHash", [primTyCon "DatumHash"]),
                         ("OutputDatum", [primTyCon "Datum"])]

intervalType :: (Qualified (ProperName 'TypeName), (Type SourceAnn, TypeKind))
intervalType = 
  polyType "Interval" ["a"] . 
  polyRecordType "Interval" ["a"] $ 
  [("from", tyApp (tyCon "LowerBound") (tyVar "a")),
   ("to", tyApp (tyCon "UpperBound") (tyVar "a"))]

intervalDecl :: (Qualified (ProperName 'TypeName), DataDecl Kind Ty)
intervalDecl = 
  polyRecordDecl "Interval" ["a"] [("from", TyApp (primTyCon "LowerBound") (TyVar "a" KindType)), 
                                   ("to", TyApp (primTyCon "UpperBound") (TyVar "a" KindType))]

extendedType :: (Qualified (ProperName 'TypeName), (Type SourceAnn, TypeKind))
extendedType = 
  polyType "Extended" ["a"] .
  polySumType ["a"] $ 
  [("NegInf", []),
   ("Finite", [tyVar "a"]),
   ("PosInf", [])]

extendedDecl :: (Qualified (ProperName 'TypeName), DataDecl Kind Ty)
extendedDecl = let name = primName "Extended" in 
  (name, 
   DataDecl Data 
            name 
            [("a", KindType)] 
            [CtorDecl (primIdent "NegInf") [],
             CtorDecl (primIdent "Finite") [(UnusedIdent, TyVar "a" KindType)],
             CtorDecl (primIdent "PosInf") []])

upperBoundType :: (Qualified (ProperName 'TypeName), (Type SourceAnn, TypeKind))
upperBoundType = polyNewtypeOf "UpperBound" ["a"] (tyApp (tyCon "Extended") (tyVar "a"))

upperBoundDecl :: (Qualified (ProperName 'TypeName), DataDecl Kind Ty)
upperBoundDecl = polyNewtypeDecl "UpperBound" ["a"] (TyApp (primTyCon "Extended") (TyVar "a" KindType))

lowerBoundType :: (Qualified (ProperName 'TypeName), (Type SourceAnn, TypeKind))
lowerBoundType = polyNewtypeOf "LowerBound" ["a"] (tyApp (tyCon "Extended") (tyVar "a"))

lowerBoundDecl :: (Qualified (ProperName 'TypeName), DataDecl Kind Ty)
lowerBoundDecl = polyNewtypeDecl "LowerBound" ["a"] (TyApp (primTyCon "Extended") (TyVar "a" KindType))

assocMapType :: (Qualified (ProperName 'TypeName), (Type SourceAnn, TypeKind))
assocMapType = (
  primName "AssocMap", 
  (kindType -:> kindType -:> kindType,
   DataType Newtype
            [nominalVar "k", nominalVar "v"]
            [(ProperName "AssocMap", [listOf (tuple2Of (tyVar "k") (tyVar "v"))])])
  )

assocMapDecl :: (Qualified (ProperName 'TypeName), DataDecl Kind Ty)
assocMapDecl = polyNewtypeDecl "AssocMap" ["k", "v"] (listTy (tuple2Ty (TyVar "k" KindType) (TyVar "v" KindType)))

scriptHashType :: (Qualified (ProperName 'TypeName), (Type SourceAnn, TypeKind))
scriptHashType = newtypeOf "ScriptHash" (tyCon "BuiltinByteString")

scriptHashDecl :: (Qualified (ProperName 'TypeName), DataDecl Kind Ty)
scriptHashDecl = newtypeDecl "ScriptHash" . primTyCon $ "BuiltinByteString"

redeemerType :: (Qualified (ProperName 'TypeName), (Type SourceAnn, TypeKind))
redeemerType = newtypeOf "Redeemer" (tyCon "BuiltinData")

redeemerDecl :: (Qualified (ProperName 'TypeName), DataDecl Kind Ty)
redeemerDecl = newtypeDecl "Redeemer" . primTyCon $ "BuiltinData"

redeemerHashType :: (Qualified (ProperName 'TypeName), (Type SourceAnn, TypeKind))
redeemerHashType = newtypeOf "RedeemerHash" (tyCon "BuiltinByteString")

redeemerHashDecl :: (Qualified (ProperName 'TypeName), DataDecl Kind Ty)
redeemerHashDecl = newtypeDecl "RedeemerHash" . primTyCon $ "BuiltinByteString"

datumType :: (Qualified (ProperName 'TypeName), (Type SourceAnn, TypeKind))
datumType = newtypeOf "Datum" (tyCon "BuiltinData")

datumDecl :: (Qualified (ProperName 'TypeName), DataDecl Kind Ty)
datumDecl = newtypeDecl "Datum" . primTyCon $ "BuiltinData"

datumHashType :: (Qualified (ProperName 'TypeName), (Type SourceAnn, TypeKind))
datumHashType = newtypeOf "DatumHash" (tyCon "BuiltinByteString")

datumHashDecl :: (Qualified (ProperName 'TypeName), DataDecl Kind Ty)
datumHashDecl = newtypeDecl "Datumhash" . primTyCon $ "BuiltinByteString"

dataType :: (Qualified (ProperName 'TypeName), (Type SourceAnn, TypeKind))
dataType = 
  monoType "Data" . 
  sumType $ 
  [("Constr", [tyCon "Int", listOf (tyCon "Data")]),
   ("Map", [listOf (tuple2Of (tyCon "Data") (tyCon "Data"))]),
   ("List", [listOf (tyCon "Data")]),
   ("I", [tyCon "Int"]),
   ("B", [tyCon "BuiltinByteString"])]

dataDecl :: (Qualified (ProperName 'TypeName), DataDecl Kind Ty)
dataDecl = 
  sumDecl "Data" [("Constr", [primTyCon "Int", listTy (primTyCon "Data")]),
                  ("Map", [listTy (tuple2Ty (primTyCon "Data") (primTyCon "Data"))]),
                  ("List", [listTy (primTyCon "Data")]),
                  ("I", [primTyCon "Int"]),
                  ("B", [primTyCon "BuiltinByteString"])]

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

sumDecl :: Text -> [(Text, [Ty])] -> (Qualified (ProperName 'TypeName), DataDecl Kind Ty)
sumDecl tyName arms = let tyName' = primName tyName in 
  (tyName', DataDecl Data tyName' [] . fmap go $ arms)
  where
    go :: (Text, [Ty]) -> CtorDecl Ty
    go (conName, tys) = CtorDecl (primIdent conName) (fmap (UnusedIdent,) tys)

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

recordDecl :: Text -> [(Text, Ty)] -> (Qualified (ProperName 'TypeName), DataDecl Kind Ty)
recordDecl tyName fields = let tyName' = primName tyName in 
  (tyName', DataDecl Newtype tyName' [] [CtorDecl (properToIdent <$> tyName')
                                                  (fmap go fields)])
  where
    go :: (Text, Ty) -> (Ident, Ty)
    go (fieldName, fieldTy) = (Ident fieldName, fieldTy)

polyRecordDecl :: Text -> [Text] -> [(Text, Ty)] -> 
  (Qualified (ProperName 'TypeName), DataDecl Kind Ty)
polyRecordDecl tyName vars fields = let tyName' = primName tyName in
  (tyName', DataDecl Newtype tyName' (fmap (, KindType) vars) [CtorDecl (properToIdent <$> tyName')
                                                   (fmap go fields)])
  where
    go :: (Text, Ty) -> (Ident, Ty)
    go (fieldName, fieldTy) = (Ident fieldName, fieldTy)

newtypeDecl :: Text -> Ty -> (Qualified (ProperName 'TypeName), DataDecl Kind Ty)
newtypeDecl tyName def = let tyName' = primName tyName in
  (tyName', DataDecl Newtype tyName' [] [CtorDecl (properToIdent <$> tyName')
                                                  [(UnusedIdent, def)]])

polyNewtypeDecl :: Text -> [Text] -> Ty -> (Qualified (ProperName 'TypeName), DataDecl Kind Ty)
polyNewtypeDecl tyName vars def = let tyName' = primName tyName in
  (tyName', DataDecl Newtype tyName' (fmap (, KindType) vars) [CtorDecl (properToIdent <$> tyName')
                                                                        [(UnusedIdent, def)]])

mapTy :: Ty -> Ty -> Ty
mapTy k = TyApp (TyApp (TyCon (primName "AssocMap")) k)

primTyCon :: Text -> Ty
primTyCon = TyCon . primName

maybeTy :: Ty -> Ty
maybeTy = TyApp (TyCon . primName $ "Maybe")

listTy :: Ty -> Ty
listTy = TyApp (TyCon . primName $ "Array")

tuple2Ty :: Ty -> Ty -> Ty
tuple2Ty x = TyApp (TyApp (primTyCon "Tuple2") x)

primIdent :: Text -> Qualified Ident
primIdent name = Qualified (ByModuleName (ModuleName "Prim")) (Ident name)

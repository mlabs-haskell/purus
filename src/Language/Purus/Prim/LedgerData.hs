module Language.Purus.Prim.LedgerData where

import Language.PureScript.CoreFn.Module (
  DataDecl (DataDecl),
  CtorDecl (CtorDecl)
  )
import Prelude
import Language.PureScript.Names (
  Qualified (Qualified),
  ProperNameType (TypeName, ConstructorName),
  ProperName (ProperName),
  QualifiedBy (ByModuleName),
  ModuleName (ModuleName),
  Ident (Ident, UnusedIdent)
  )
import Language.PureScript.Types (
  Type (TypeApp, TypeVar), SourceType
  )
import Language.PureScript.Environment (
  TypeKind (DataType),
  kindType, (-:>),
  DataDeclType (Newtype, Data)
  )
import Language.PureScript.AST.SourcePos (nullSourceAnn)

import Language.Purus.Prim.Utils
    ( primName,
      sumDecl,
      recordDecl,
      polyRecordDecl,
      newtypeDecl,
      polyNewtypeDecl,
      mapTy,
      primTyCon,
      maybeTy,
      listTy,
      tuple2Ty,
      primIdent )

-- | Ledger API data declarations, as per https://github.com/IntersectMBO/plutus/blob/master/plutus-ledger-api/src/PlutusLedgerApi/V2.hs
ledgerDecls :: [(Qualified (ProperName 'TypeName), DataDecl SourceType SourceType)]
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
  datumHashDecl
  ]

scriptContextDecl :: (Qualified (ProperName 'TypeName), DataDecl SourceType SourceType)
scriptContextDecl = recordDecl "ScriptContext" [("txInfo", primTyCon "TxInfo"),
                                                ("purpose", primTyCon "ScriptPurpose")]

scriptPurposeDecl :: (Qualified (ProperName 'TypeName), DataDecl SourceType SourceType)
scriptPurposeDecl =
  sumDecl "ScriptPurpose" [("Minting", [primTyCon "CurrencySymbol"]),
                           ("Spending", [primTyCon "TxOutRef"]),
                           ("Rewarding", [primTyCon "StakingCredential"]),
                           ("Certifying", [primTyCon "DCert"])]

ledgerBytesDecl :: (Qualified (ProperName 'TypeName), DataDecl SourceType SourceType)
ledgerBytesDecl = newtypeDecl "LedgerBytes" . primTyCon $ "BuiltinByteString"

dcertDecl :: (Qualified (ProperName 'TypeName), DataDecl SourceType SourceType)
dcertDecl =
  sumDecl "DCert" [("DCertDelegRegKey", [primTyCon "StakingCredential"]),
                   ("DCertDelegDeRegKey", [primTyCon "StakingCredential"]),
                   ("DCertDelegDelegate", [primTyCon "StakingCredential", primTyCon "PubKeyHash"]),
                   ("DCertPoolRegister", [primTyCon "PubKeyHash", primTyCon "PubKeyHash"]),
                   ("DCertPoolRetire", [primTyCon "PubKeyHash", primTyCon "Int"]),
                   ("DCertGenesis", []),
                   ("DCertMir", [])]

stakingCredentialDecl :: (Qualified (ProperName 'TypeName), DataDecl SourceType SourceType)
stakingCredentialDecl =
  sumDecl "StakingCredential" [("StakingHash", [primTyCon "Credential"]),
                               ("StakingPtr", [primTyCon "Int", primTyCon "Int", primTyCon "Int"])]


credentialDecl :: (Qualified (ProperName 'TypeName), DataDecl SourceType SourceType)
credentialDecl =
  sumDecl "Credential" [("PubKeyCredential", [primTyCon "PubKeyHash"]),
                        ("ScriptCredential", [primTyCon "ScriptHash"])]

valueDecl :: (Qualified (ProperName 'TypeName), DataDecl SourceType SourceType)
valueDecl =
  newtypeDecl "Value" .
  mapTy (primTyCon "CurrencySymbol") .
  mapTy (primTyCon "TokenName") .
  primTyCon $ "Int"

currencySymbolDecl :: (Qualified (ProperName 'TypeName), DataDecl SourceType SourceType)
currencySymbolDecl = newtypeDecl "CurrencySymbol" . primTyCon $ "BuiltinByteString"

tokenNameDecl :: (Qualified (ProperName 'TypeName), DataDecl SourceType SourceType)
tokenNameDecl = newtypeDecl "TokenName" . primTyCon $ "BuiltinByteString"

lovelaceDecl :: (Qualified (ProperName 'TypeName), DataDecl SourceType SourceType)
lovelaceDecl = newtypeDecl "Lovelace" . primTyCon $ "Int"

posixTimeDecl :: (Qualified (ProperName 'TypeName), DataDecl SourceType SourceType)
posixTimeDecl = newtypeDecl "POSIXTime" . primTyCon $ "Int"

addressDecl :: (Qualified (ProperName 'TypeName), DataDecl SourceType SourceType)
addressDecl = recordDecl "Address" [("credential", primTyCon "Credential"),
                                    ("stakingCredential", maybeTy . primTyCon $ "StakingCredential")]

pubKeyHashDecl :: (Qualified (ProperName 'TypeName), DataDecl SourceType SourceType)
pubKeyHashDecl = newtypeDecl "PubKeyHash" . primTyCon $ "BuiltinByteString"

txIdDecl :: (Qualified (ProperName 'TypeName), DataDecl SourceType SourceType)
txIdDecl = newtypeDecl "TxId" . primTyCon $ "BuiltinByteString"

txInfoDecl :: (Qualified (ProperName 'TypeName), DataDecl SourceType SourceType)
txInfoDecl = recordDecl "TxInfo" [("inputs", listTy . primTyCon $ "TxInInfo"),
                                  ("referenceInputs", listTy . primTyCon $ "TxInInfo"),
                                  ("outputs", listTy . primTyCon $ "TxOut"),
                                  ("fee", primTyCon "Value"),
                                  ("mint", primTyCon "Value"),
                                  ("dCert", listTy . primTyCon $ "DCert"),
                                  ("wdrl", mapTy (primTyCon "StakingCredential") (primTyCon "Int")),
                                  ("validRange", TypeApp nullSourceAnn (primTyCon "Interval") (primTyCon "POSIXTime")),
                                  ("signatories", listTy . primTyCon $ "PubKeyHash"),
                                  ("redeemers", mapTy (primTyCon "ScriptPurpose") (primTyCon "Redeemer")),
                                  ("data", mapTy (primTyCon "DatumHash") (primTyCon "Datum")),
                                  ("id", primTyCon "TxId")]

txOutDecl :: (Qualified (ProperName 'TypeName), DataDecl SourceType SourceType)
txOutDecl = recordDecl "TxOut" [("address", primTyCon "Address"),
                                ("value", primTyCon "Value"),
                                ("datum", primTyCon "OutputDatum"),
                                ("referenceScript", maybeTy . primTyCon $ "ScriptHash")]

txOutRefDecl :: (Qualified (ProperName 'TypeName), DataDecl SourceType SourceType)
txOutRefDecl = recordDecl "TxOutRef" [("id", primTyCon "TxId"),
                                      ("idx", primTyCon "Int")]

txInInfoDecl :: (Qualified (ProperName 'TypeName), DataDecl SourceType SourceType)
txInInfoDecl = recordDecl "TxInInfo" [("txOutRef", primTyCon "TxOutRef"),
                                      ("resolved", primTyCon "TxOut")]

outputDatumDecl :: (Qualified (ProperName 'TypeName), DataDecl SourceType SourceType)
outputDatumDecl =
  sumDecl "OutputDatum" [("NoOutputDatum", []),
                         ("OutputDatumHash", [primTyCon "DatumHash"]),
                         ("OutputDatum", [primTyCon "Datum"])]


intervalDecl :: (Qualified (ProperName 'TypeName), DataDecl SourceType SourceType)
intervalDecl =
  polyRecordDecl "Interval" ["a"] [("from", TypeApp nullSourceAnn (primTyCon "LowerBound") (TypeVar nullSourceAnn "a" kindType)),
                                   ("to", TypeApp nullSourceAnn (primTyCon "UpperBound") (TypeVar nullSourceAnn "a" kindType))]

extendedDecl :: (Qualified (ProperName 'TypeName), DataDecl SourceType SourceType)
extendedDecl = let name = primName "Extended" in
  (name,
   DataDecl Data
            name
            [("a", kindType)]
            [CtorDecl (primIdent "NegInf") [],
             CtorDecl (primIdent "Finite") [(UnusedIdent, TypeVar nullSourceAnn "a" kindType)],
             CtorDecl (primIdent "PosInf") []])


upperBoundDecl :: (Qualified (ProperName 'TypeName), DataDecl SourceType SourceType)
upperBoundDecl = polyNewtypeDecl "UpperBound" ["a"] (TypeApp nullSourceAnn (primTyCon "Extended") (TypeVar nullSourceAnn "a" kindType))


lowerBoundDecl :: (Qualified (ProperName 'TypeName), DataDecl SourceType SourceType)
lowerBoundDecl = polyNewtypeDecl "LowerBound" ["a"] (TypeApp nullSourceAnn (primTyCon "Extended") (TypeVar nullSourceAnn "a" kindType))


assocMapDecl :: (Qualified (ProperName 'TypeName), DataDecl SourceType SourceType)
assocMapDecl = polyNewtypeDecl "AssocMap" ["k", "v"] (listTy (tuple2Ty (TypeVar nullSourceAnn "k" kindType) (TypeVar nullSourceAnn "v" kindType)))


scriptHashDecl :: (Qualified (ProperName 'TypeName), DataDecl SourceType SourceType)
scriptHashDecl = newtypeDecl "ScriptHash" . primTyCon $ "BuiltinByteString"


redeemerDecl :: (Qualified (ProperName 'TypeName), DataDecl SourceType SourceType)
redeemerDecl = newtypeDecl "Redeemer" . primTyCon $ "BuiltinData"


redeemerHashDecl :: (Qualified (ProperName 'TypeName), DataDecl SourceType SourceType)
redeemerHashDecl = newtypeDecl "RedeemerHash" . primTyCon $ "BuiltinByteString"

datumDecl :: (Qualified (ProperName 'TypeName), DataDecl SourceType SourceType)
datumDecl = newtypeDecl "Datum" . primTyCon $ "BuiltinData"

datumHashDecl :: (Qualified (ProperName 'TypeName), DataDecl SourceType SourceType)
datumHashDecl = newtypeDecl "Datumhash" . primTyCon $ "BuiltinByteString"



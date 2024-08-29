{-# LANGUAGE TypeApplications #-}

module Language.Purus.Prim.Ledger (
  ledgerTypes,
  ledgerCons,
  ledgerConstructorsEnv,
  ledgerConstructorsEnvReadable
 ) where

import Data.Map qualified as M
 
import Data.Bifunctor (first)
import Data.List (foldl')
import Language.PureScript.AST.SourcePos (SourceAnn, nullSourceAnn)
import Language.PureScript.CoreFn.Module (
  CtorDecl (CtorDecl),
  DataDecl (DataDecl),
 )
import Language.PureScript.CoreFn.TypeLike (TypeLike (applyType, funTy), quantify)
import Language.PureScript.Environment (
  DataDeclType (Newtype),
  TypeKind (DataType),
  kindType,
  (-:>),
 )
import Language.PureScript.Names (
  Ident,
  ProperName (ProperName, runProperName),
  ProperNameType (ConstructorName, TypeName),
  Qualified,
  coerceProperName,
  disqualify,
  runIdent,
 )
import Language.PureScript.Types (
  SourceType,
  Type (TypeConstructor, TypeVar),
 )
import Language.Purus.IR ()
import Language.Purus.Prim.LedgerData ( ledgerDecls )
import Language.Purus.Prim.Utils
    ( arm,
      listOf,
      mapOf,
      maybeOf,
      monoType,
      mononym,
      newtypeOf,
      nominalVar,
      polyNewtypeOf,
      polyRecordType,
      polySumType,
      polyType,
      primName,
      recordType,
      sumType,
      tuple2Of,
      tyApp,
      tyCon,
      tyVar )
import Prelude

import Language.Purus.Pretty.Common (docString)
import Prettyprinter
    ( Pretty(pretty), (<+>), hardline, punctuate, vcat )

-- | Ledger API (V2) types, as per https://github.com/IntersectMBO/plutus/blob/master/plutus-ledger-api/src/PlutusLedgerApi/V2.hs
ledgerTypes :: [(Qualified (ProperName 'TypeName), (Type SourceAnn, TypeKind))]
ledgerTypes =
  [ -- Context types
    scriptContextType
  , scriptPurposeType
  , -- Bytes
    ledgerBytesType
  , -- Certificates
    dcertType
  , -- Credentials
    stakingCredentialType
  , credentialType
  , -- Value
    valueType
  , currencySymbolType
  , tokenNameType
  , lovelaceType
  , -- Time
    posixTimeType
  , -- No POSIXTimeRange, people can use the actual type instead of a synonym
    -- Types for representing transactions
    addressType
  , pubKeyHashType
  , txIdType
  , txInfoType
  , txOutType
  , txOutRefType
  , txInInfoType
  , outputDatumType
  , -- Intervals
    intervalType
  , extendedType
  , upperBoundType
  , lowerBoundType
  , -- Association maps
    assocMapType
  , -- Newtypes and hash types
    scriptHashType
  , redeemerType
  , redeemerHashType
  , datumType
  , datumHashType
  ]

-- | Ledger API (V2) constructors, as per https://github.com/IntersectMBO/plutus/blob/master/plutus-ledger-api/src/PlutusLedgerApi/V2.hs
ledgerCons :: [(Qualified Ident, Qualified (ProperName 'TypeName))]
ledgerCons =
  [ -- Context types
    scriptContextCon
  , mintingCon
  , spendingCon
  , rewardingCon
  , certifyingCon
  , -- Bytes
    ledgerBytesCon
  , -- Certificates
    dcertDelegRegKeyCon
  , dcertDelegDeRegKeyCon
  , dcertDelegDelegateCon
  , dcertPoolRegisterCon
  , dcertPoolRetireCon
  , dcertGenesisCon
  , dcertMirCon
  , -- Credentials
    stakingHashCon
  , stakingPtrCon
  , pubKeyCredentialCon
  , scriptCredentialCon
  , -- Value
    valueCon
  , currencySymbolCon
  , tokenNameCon
  , lovelaceCon
  , -- Time
    posixTimeCon
  , -- Types for representing transactions
    addressCon
  , pubKeyHashCon
  , txIdCon
  , txInfoCon
  , txOutCon
  , txOutRefCon
  , txInInfoCon
  , noOutputDatumCon
  , outputDatumHashCon
  , outputDatumCon
  , -- Intervals
    intervalCon
  , negInfCon
  , finiteCon
  , posInfCon
  , upperBoundCon
  , lowerBoundCon
  , -- Association maps
    assocMapCon
  , -- Newtypes and hash types
    scriptHashCon
  , redeemerCon
  , redeemerHashCon
  , datumCon
  , datumHashCon
  ]

scriptContextType :: (Qualified (ProperName 'TypeName), (Type SourceAnn, TypeKind))
scriptContextType =
  monoType "ScriptContext"
    . recordType "ScriptContext"
    $ [ ("txInfo", tyCon "TxInfo")
      , ("purpose", tyCon "ScriptPurpose")
      ]

scriptContextCon :: (Qualified Ident, Qualified (ProperName 'TypeName))
scriptContextCon = mononym "ScriptContext"

scriptPurposeType :: (Qualified (ProperName 'TypeName), (Type SourceAnn, TypeKind))
scriptPurposeType =
  monoType "ScriptPurpose"
    . sumType
    $ [ ("Minting", [tyCon "CurrencySymbol"])
      , ("Spending", [tyCon "TxOutRef"])
      , ("Rewarding", [tyCon "StakingCredential"])
      , ("Certifying", [tyCon "DCert"])
      ]

mintingCon :: (Qualified Ident, Qualified (ProperName 'TypeName))
mintingCon = arm "Minting" "ScriptPurpose"

spendingCon :: (Qualified Ident, Qualified (ProperName 'TypeName))
spendingCon = arm "Spending" "ScriptPurpose"

rewardingCon :: (Qualified Ident, Qualified (ProperName 'TypeName))
rewardingCon = arm "Rewarding" "ScriptPurpose"

certifyingCon :: (Qualified Ident, Qualified (ProperName 'TypeName))
certifyingCon = arm "Certifying" "ScriptPurpose"

ledgerBytesType :: (Qualified (ProperName 'TypeName), (Type SourceAnn, TypeKind))
ledgerBytesType = newtypeOf "LedgerBytes" (tyCon "BuiltinByteString")

ledgerBytesCon :: (Qualified Ident, Qualified (ProperName 'TypeName))
ledgerBytesCon = mononym "LedgerBytes"

dcertType :: (Qualified (ProperName 'TypeName), (Type SourceAnn, TypeKind))
dcertType =
  monoType "DCert"
    . sumType
    $ [ ("DCertDelegRegKey", [tyCon "StakingCredential"])
      , ("DCertDelegDeRegKey", [tyCon "StakingCredential"])
      , ("DCertDelegDelegate", [tyCon "StakingCredential", tyCon "PubKeyHash"])
      , ("DCertPoolRegister", [tyCon "PubKeyHash", tyCon "PubKeyHash"])
      , ("DCertPoolRetire", [tyCon "PubKeyHash", tyCon "Int"])
      , ("DCertGenesis", [])
      , ("DCertMir", [])
      ]

dcertDelegRegKeyCon :: (Qualified Ident, Qualified (ProperName 'TypeName))
dcertDelegRegKeyCon = arm "DCertDelegRegKey" "DCert"

dcertDelegDeRegKeyCon :: (Qualified Ident, Qualified (ProperName 'TypeName))
dcertDelegDeRegKeyCon = arm "DCertDelegDeRegKey" "DCert"

dcertDelegDelegateCon :: (Qualified Ident, Qualified (ProperName 'TypeName))
dcertDelegDelegateCon = arm "DCertDelegDelegate" "DCert"

dcertPoolRegisterCon :: (Qualified Ident, Qualified (ProperName 'TypeName))
dcertPoolRegisterCon = arm "DCertPoolRegister" "DCert"

dcertPoolRetireCon :: (Qualified Ident, Qualified (ProperName 'TypeName))
dcertPoolRetireCon = arm "DCertPoolRetire" "DCert"

dcertGenesisCon :: (Qualified Ident, Qualified (ProperName 'TypeName))
dcertGenesisCon = arm "DCertGenesis" "DCert"

dcertMirCon :: (Qualified Ident, Qualified (ProperName 'TypeName))
dcertMirCon = arm "DCertMir" "DCert"

stakingCredentialType :: (Qualified (ProperName 'TypeName), (Type SourceAnn, TypeKind))
stakingCredentialType =
  monoType "StakingCredential"
    . sumType
    $ [ ("StakingHash", [tyCon "Credential"])
      , ("StakingPtr", [tyCon "Int", tyCon "Int", tyCon "Int"])
      ]

stakingHashCon :: (Qualified Ident, Qualified (ProperName 'TypeName))
stakingHashCon = arm "StakingHash" "StakingCredential"

stakingPtrCon :: (Qualified Ident, Qualified (ProperName 'TypeName))
stakingPtrCon = arm "StakingPtr" "StakingCredential"

credentialType :: (Qualified (ProperName 'TypeName), (Type SourceAnn, TypeKind))
credentialType =
  monoType "Credential"
    . sumType
    $ [ ("PubKeyCredential", [tyCon "PubKeyHash"])
      , ("ScriptCredential", [tyCon "ScriptHash"])
      ]

pubKeyCredentialCon :: (Qualified Ident, Qualified (ProperName 'TypeName))
pubKeyCredentialCon = arm "PubKeyCredential" "Credential"

scriptCredentialCon :: (Qualified Ident, Qualified (ProperName 'TypeName))
scriptCredentialCon = arm "ScriptCredential" "Credential"

valueType :: (Qualified (ProperName 'TypeName), (Type SourceAnn, TypeKind))
valueType = newtypeOf "Value" (mapOf (tyCon "CurrencySymbol") (mapOf (tyCon "TokenName") (tyCon "Int")))

valueCon :: (Qualified Ident, Qualified (ProperName 'TypeName))
valueCon = mononym "Value"

currencySymbolType :: (Qualified (ProperName 'TypeName), (Type SourceAnn, TypeKind))
currencySymbolType = newtypeOf "CurrencySymbol" (tyCon "BuiltinByteString")

currencySymbolCon :: (Qualified Ident, Qualified (ProperName 'TypeName))
currencySymbolCon = mononym "CurrencySymbol"

tokenNameType :: (Qualified (ProperName 'TypeName), (Type SourceAnn, TypeKind))
tokenNameType = newtypeOf "TokenName" (tyCon "BuiltinByteString")

tokenNameCon :: (Qualified Ident, Qualified (ProperName 'TypeName))
tokenNameCon = mononym "TokenName"

lovelaceType :: (Qualified (ProperName 'TypeName), (Type SourceAnn, TypeKind))
lovelaceType = newtypeOf "Lovelace" (tyCon "Int")

lovelaceCon :: (Qualified Ident, Qualified (ProperName 'TypeName))
lovelaceCon = mononym "Lovelace"

posixTimeType :: (Qualified (ProperName 'TypeName), (Type SourceAnn, TypeKind))
posixTimeType = newtypeOf "POSIXTime" (tyCon "Int")

posixTimeCon :: (Qualified Ident, Qualified (ProperName 'TypeName))
posixTimeCon = mononym "POSIXTime"

addressType :: (Qualified (ProperName 'TypeName), (Type SourceAnn, TypeKind))
addressType =
  monoType "Address"
    . recordType "Address"
    $ [ ("credential", tyCon "Credential")
      , ("stakingCredential", maybeOf (tyCon "StakingCredential"))
      ]

addressCon :: (Qualified Ident, Qualified (ProperName 'TypeName))
addressCon = mononym "Address"

pubKeyHashType :: (Qualified (ProperName 'TypeName), (Type SourceAnn, TypeKind))
pubKeyHashType = newtypeOf "PubKeyHash" (tyCon "BuiltinByteString")

pubKeyHashCon :: (Qualified Ident, Qualified (ProperName 'TypeName))
pubKeyHashCon = mononym "PubKeyHash"

txIdType :: (Qualified (ProperName 'TypeName), (Type SourceAnn, TypeKind))
txIdType = newtypeOf "TxId" (tyCon "BuiltinByteString")

txIdCon :: (Qualified Ident, Qualified (ProperName 'TypeName))
txIdCon = mononym "TxId"

txInfoType :: (Qualified (ProperName 'TypeName), (Type SourceAnn, TypeKind))
txInfoType =
  monoType "TxInfo"
    . recordType "TxInfo"
    $ [ ("inputs", listOf (tyCon "TxInInfo"))
      , ("referenceInputs", listOf (tyCon "TxInInfo"))
      , ("outputs", listOf (tyCon "TxOut"))
      , ("fee", tyCon "Value")
      , ("mint", tyCon "Value")
      , ("dCert", listOf (tyCon "DCert"))
      , ("wdrl", mapOf (tyCon "StakingCredential") (tyCon "Int"))
      , ("validRange", tyApp (tyCon "Interval") (tyCon "POSIXTime"))
      , ("signatories", listOf (tyCon "PubKeyHash"))
      , ("redeemers", mapOf (tyCon "ScriptPurpose") (tyCon "Redeemer"))
      , ("data", mapOf (tyCon "DatumHash") (tyCon "Datum"))
      , ("id", tyCon "TxId")
      ]

txInfoCon :: (Qualified Ident, Qualified (ProperName 'TypeName))
txInfoCon = mononym "TxInfo"

txOutType :: (Qualified (ProperName 'TypeName), (Type SourceAnn, TypeKind))
txOutType =
  monoType "TxOut"
    . recordType "TxOut"
    $ [ ("address", tyCon "Address")
      , ("value", tyCon "Value")
      , ("datum", tyCon "OutputDatum")
      , ("referenceScript", maybeOf (tyCon "ScriptHash"))
      ]

txOutCon :: (Qualified Ident, Qualified (ProperName 'TypeName))
txOutCon = mononym "TxOut"

txOutRefType :: (Qualified (ProperName 'TypeName), (Type SourceAnn, TypeKind))
txOutRefType =
  monoType "TxOutRef"
    . recordType "TxOut"
    $ [ ("id", tyCon "TxId")
      , ("idx", tyCon "Int")
      ]

txOutRefCon :: (Qualified Ident, Qualified (ProperName 'TypeName))
txOutRefCon = mononym "TxOutRef"

txInInfoType :: (Qualified (ProperName 'TypeName), (Type SourceAnn, TypeKind))
txInInfoType =
  monoType "TxInInfo"
    . recordType "TxInInfo"
    $ [ ("txOutRef", tyCon "TxOutRef")
      , ("resolved", tyCon "TxOut")
      ]

txInInfoCon :: (Qualified Ident, Qualified (ProperName 'TypeName))
txInInfoCon = mononym "TxInInfo"

outputDatumType :: (Qualified (ProperName 'TypeName), (Type SourceAnn, TypeKind))
outputDatumType =
  monoType "OutputDatum"
    . sumType
    $ [ ("NoOutputDatum", [])
      , ("OutputDatumHash", [tyCon "DatumHash"])
      , ("OutputDatum", [tyCon "Datum"])
      ]

noOutputDatumCon :: (Qualified Ident, Qualified (ProperName 'TypeName))
noOutputDatumCon = arm "NoOutputDatum" "OutputDatum"

outputDatumHashCon :: (Qualified Ident, Qualified (ProperName 'TypeName))
outputDatumHashCon = arm "OutputDatumHash" "OutputDatum"

outputDatumCon :: (Qualified Ident, Qualified (ProperName 'TypeName))
outputDatumCon = mononym "OutputDatum"

intervalType :: (Qualified (ProperName 'TypeName), (Type SourceAnn, TypeKind))
intervalType =
  polyType "Interval" ["a"]
    . polyRecordType "Interval" ["a"]
    $ [ ("from", tyApp (tyCon "LowerBound") (tyVar "a"))
      , ("to", tyApp (tyCon "UpperBound") (tyVar "a"))
      ]

intervalCon :: (Qualified Ident, Qualified (ProperName 'TypeName))
intervalCon = mononym "Interval"

extendedType :: (Qualified (ProperName 'TypeName), (Type SourceAnn, TypeKind))
extendedType =
  polyType "Extended" ["a"]
    . polySumType ["a"]
    $ [ ("NegInf", [])
      , ("Finite", [tyVar "a"])
      , ("PosInf", [])
      ]

negInfCon :: (Qualified Ident, Qualified (ProperName 'TypeName))
negInfCon = arm "NegInf" "Extended"

finiteCon :: (Qualified Ident, Qualified (ProperName 'TypeName))
finiteCon = arm "Finite" "Extended"

posInfCon :: (Qualified Ident, Qualified (ProperName 'TypeName))
posInfCon = arm "PosInf" "Extended"

upperBoundType :: (Qualified (ProperName 'TypeName), (Type SourceAnn, TypeKind))
upperBoundType = polyNewtypeOf "UpperBound" ["a"] (tyApp (tyCon "Extended") (tyVar "a"))

upperBoundCon :: (Qualified Ident, Qualified (ProperName 'TypeName))
upperBoundCon = mononym "UpperBound"

lowerBoundType :: (Qualified (ProperName 'TypeName), (Type SourceAnn, TypeKind))
lowerBoundType = polyNewtypeOf "LowerBound" ["a"] (tyApp (tyCon "Extended") (tyVar "a"))

lowerBoundCon :: (Qualified Ident, Qualified (ProperName 'TypeName))
lowerBoundCon = mononym "LowerBound"

assocMapType :: (Qualified (ProperName 'TypeName), (Type SourceAnn, TypeKind))
assocMapType =
  ( primName "AssocMap"
  ,
    ( kindType -:> kindType -:> kindType
    , DataType
        Newtype
        [nominalVar "k", nominalVar "v"]
        [(ProperName "AssocMap", [listOf (tuple2Of (tyVar "k") (tyVar "v"))])]
    )
  )

assocMapCon :: (Qualified Ident, Qualified (ProperName 'TypeName))
assocMapCon = mononym "AssocMap"

scriptHashType :: (Qualified (ProperName 'TypeName), (Type SourceAnn, TypeKind))
scriptHashType = newtypeOf "ScriptHash" (tyCon "BuiltinByteString")

scriptHashCon :: (Qualified Ident, Qualified (ProperName 'TypeName))
scriptHashCon = mononym "ScriptHash"

redeemerType :: (Qualified (ProperName 'TypeName), (Type SourceAnn, TypeKind))
redeemerType = newtypeOf "Redeemer" (tyCon "BuiltinData")

redeemerCon :: (Qualified Ident, Qualified (ProperName 'TypeName))
redeemerCon = mononym "Redeemer"

redeemerHashType :: (Qualified (ProperName 'TypeName), (Type SourceAnn, TypeKind))
redeemerHashType = newtypeOf "RedeemerHash" (tyCon "BuiltinByteString")

redeemerHashCon :: (Qualified Ident, Qualified (ProperName 'TypeName))
redeemerHashCon = mononym "RedeemerHash"

datumType :: (Qualified (ProperName 'TypeName), (Type SourceAnn, TypeKind))
datumType = newtypeOf "Datum" (tyCon "BuiltinData")

datumCon :: (Qualified Ident, Qualified (ProperName 'TypeName))
datumCon = mononym "Datum"

datumHashType :: (Qualified (ProperName 'TypeName), (Type SourceAnn, TypeKind))
datumHashType = newtypeOf "DatumHash" (tyCon "BuiltinByteString")

datumHashCon :: (Qualified Ident, Qualified (ProperName 'TypeName))
datumHashCon = mononym "DatumHash"

ledgerConstructorsEnv ::
  M.Map
    (Qualified (ProperName 'ConstructorName))
    (DataDeclType, ProperName 'TypeName, SourceType, [Ident])
ledgerConstructorsEnv = M.fromList $ concatMap go ledgerDecls
  where
    go ::
      (Qualified (ProperName 'TypeName), DataDecl SourceType SourceType) ->
      [ ( Qualified (ProperName 'ConstructorName)
        , (DataDeclType, ProperName 'TypeName, SourceType, [Ident])
        )
      ]
    go (tn, DataDecl declType _tn dArgs dCtors) = goCtor <$> dCtors
      where
        goCtor ::
          CtorDecl SourceType ->
          (Qualified (ProperName 'ConstructorName), (DataDeclType, ProperName 'TypeName, SourceType, [Ident]))
        goCtor (CtorDecl nm fields) =
          let qCtorNm = coerceProperName @_ @'ConstructorName . ProperName . runIdent <$> nm
              (fIds, fTys) = unzip fields
              tCon = TypeConstructor nullSourceAnn tn
              tcTyVars = uncurry (TypeVar nullSourceAnn) <$> dArgs
              tyConAppliedToArgs = foldl' applyType tCon tcTyVars
              constructorFnType = quantify $ foldr funTy tyConAppliedToArgs fTys
           in (qCtorNm, (declType, disqualify tn, constructorFnType, fIds))

-- stupid utility to let us manually verify correctness by giving us a readable representation
ledgerConstructorsEnvReadable :: String
ledgerConstructorsEnvReadable =
  docString
    . (<> hardline)
    . vcat
    . punctuate hardline
    . fmap
      ( (\(a, b) -> pretty a <+> "::" <+> pretty b)
          . first (runProperName . disqualify)
      )
    . M.toList
    $ (\(_, _, t, _) -> t)
      <$> ledgerConstructorsEnv

module Language.PureScript.LedgerApi (
  DataDeclType (..),
  showDataDeclType,
  TypeKind(..),
  plutusTxTypes,
  ledgerTypes,
  kindType,
  (-:>),
  mkRecordT,
  ) where

import Language.PureScript.Constants.Prim qualified as C
import Data.Text (Text)
import Data.Text qualified as Text
import Language.PureScript.Names (
  Qualified (Qualified),
  ProperName (ProperName),
  ProperNameType (TypeName, ConstructorName),
  QualifiedBy (ByModuleName),
  ModuleName (ModuleName)
  )
import Language.PureScript.Types (
  SourceType, srcTypeConstructor,
  Type (TypeApp, RCons, REmpty, TypeVar, TypeConstructor)
  )
import Language.PureScript.AST.SourcePos (nullSourceAnn, 
  SourceAnn)
import Language.PureScript.Roles (Role (Nominal))
import Prelude
import GHC.Generics (Generic)
import Control.DeepSeq (NFData)
import Codec.Serialise (Serialise)
import Data.Aeson (ToJSON (toJSON, toEncoding),
  FromJSON (parseJSON), withText)

-- | The type ('data' or 'newtype') of a data type declaration
data DataDeclType
  = -- | A standard data constructor
    Data
  | -- | A newtype constructor
    Newtype
  deriving (Show, Eq, Ord, Generic)

instance NFData DataDeclType

instance Serialise DataDeclType

showDataDeclType :: DataDeclType -> Text
showDataDeclType = \case
  Data -> "data"
  Newtype -> "newtype"

instance ToJSON DataDeclType where
  toJSON = toJSON . showDataDeclType
  toEncoding = toEncoding . showDataDeclType

instance FromJSON DataDeclType where
  parseJSON = withText "DataDeclType" $ \case
    "data" -> pure Data
    "newtype" -> pure Newtype
    other -> fail $ "invalid type: '" ++ Text.unpack other ++ "'"

-- | The kinds of a type
data TypeKind
  = -- | Data type
    DataType DataDeclType [(Text, SourceType, Role)] [(ProperName 'ConstructorName, [SourceType])]
  | -- | Type synonym
    TypeSynonym
  | -- | Foreign data
    ExternData [Role]
  | -- | A local type variable
    LocalTypeVariable
  | -- | A scoped type variable
    ScopedTypeVar
  deriving (Show, Eq, Generic)

instance NFData TypeKind
instance Serialise TypeKind

-- | Kind of ground types
kindType :: SourceType
kindType = srcTypeConstructor C.Type

-- | Kind arrow
(-:>) :: SourceType -> SourceType -> SourceType
(-:>) = function
  where
    function :: SourceType -> SourceType -> SourceType
    function = TypeApp nullSourceAnn . TypeApp nullSourceAnn tyFunction
    tyFunction :: SourceType
    tyFunction = srcTypeConstructor C.Function
infixr 4 -:>

-- | PlutusTx base types
plutusTxTypes :: [(Qualified (ProperName 'TypeName), (SourceType, TypeKind))]
plutusTxTypes = [
  ledgerMonoType "BuiltinData",
  ledgerMonoType "BuiltinByteString"
  ]

-- | Ledger API (V2) types, as per https://github.com/IntersectMBO/plutus/blob/master/plutus-ledger-api/src/PlutusLedgerApi/V2.hs
ledgerTypes :: [(Qualified (ProperName 'TypeName), (SourceType, TypeKind))]
ledgerTypes = [
  -- Context types
  (primName "ScriptContext", (kindType, DataType Newtype
                                                 []
                                                 [(ProperName "ScriptContext", [
                                                    mkRecordT (RCons nullSourceAnn "txInfo" (tyCon "TxInfo") . 
                                                               RCons nullSourceAnn "purpose" (tyCon "ScriptPurpose") $ 
                                                               REmpty nullSourceAnn)
                                                  ])])),
  (primName "ScriptPurpose", (kindType, DataType Data 
                                                 []
                                                 [(ProperName "Minting", [tyCon "CurrencySymbol"]),
                                                  (ProperName "Spending", [tyCon "TxOutRef"]),
                                                  (ProperName "Rewarding", [tyCon "StakingCredential"]),
                                                  (ProperName "Certifying", [tyCon "DCert"])
                                                  ])),
  -- Bytes
  aNewtype "LedgerBytes" "BuiltinByteString",
  -- Certificates
  (primName "DCert", (kindType, DataType Data
                                         []
                                         [(ProperName "DCertDelegRegKey", [tyCon "StakingCredential"]),
                                          (ProperName "DCertDelegDeRegKey", [tyCon "StakingCredential"]),
                                          (ProperName "DCertDelegDelegate", [tyCon "StakingCredential", tyCon "PubKeyHash"]),
                                          (ProperName "DCertPoolRegister", [tyCon "PubKeyHash", tyCon "PubKeyHash"]),
                                          (ProperName "DCertPoolRetire", [tyCon "PubKeyHash", tyCon "Int"]),
                                          (ProperName "DCertGenesis", []),
                                          (ProperName "DCertMir", [])])),
  -- Credentials
  (primName "StakingCredential", (kindType, DataType Data
                                                     []
                                                     [(ProperName "StakingHash", [tyCon "Credential"]),
                                                      (ProperName "StakingPtr", [tyCon "Int", tyCon "Int", tyCon "Int"])])),
  (primName "Credential", (kindType, DataType Data 
                                              [] 
                                              [(ProperName "PubKeyCredential", [tyCon "PubKeyHash"]), 
                                               (ProperName "ScriptCredential", [tyCon "ScriptHash"])])),
  -- Value
  (primName "Value", (kindType, DataType Newtype
                                         []
                                         [(ProperName "Value", [mapOf (tyCon "CurrencySymbol") (mapOf (tyCon "TokenName") (tyCon "Int"))])])), 
  aNewtype "CurrencySymbol" "BuiltinByteString",
  aNewtype "TokenName" "BuiltinByteString",
  aNewtype "Lovelace" "BuiltinByteString",
  -- Time
  aNewtype "POSIXTime" "Int",
  -- No POSIXTimeRange, people can use the actual type instead of a synonym
  -- Types for representing transactions
  (primName "Address", (kindType, DataType Newtype 
                                           []
                                           [(ProperName "Address", [
                                              mkRecordT (RCons nullSourceAnn "credential" (tyCon "Address") .
                                                         RCons nullSourceAnn "stakingCredential" (maybeOf (tyCon "StakingCredential")) $
                                                         REmpty nullSourceAnn)
                                            ]
                                           )])),
  aNewtype "PubKeyHash" "BuiltinByteString",
  aNewtype "TxId" "BuiltinByteString",
  (primName "TxInfo", (kindType, DataType Newtype
                                          []
                                          [(ProperName "TxInfo", [
                                              mkRecordT (RCons nullSourceAnn "inputs" (listOf (tyCon "TxInInfo")) .
                                                         RCons nullSourceAnn "referenceInputs" (listOf (tyCon "TxInInfo")) .
                                                         RCons nullSourceAnn "outputs" (listOf (tyCon "TxOut")) .
                                                         RCons nullSourceAnn "fee" (tyCon "Value") . 
                                                         RCons nullSourceAnn "mint" (tyCon "Value") .
                                                         RCons nullSourceAnn "dCert" (listOf (tyCon "DCert")) .
                                                         RCons nullSourceAnn "wdrl" (mapOf (tyCon "StakingCredential") (tyCon "Int")) .
                                                         RCons nullSourceAnn "validRange" (tyApp (tyCon "Interval") (tyCon "POSIXTime")) .
                                                         RCons nullSourceAnn "signatories" (listOf (tyCon "PubKeyHash")) .
                                                         RCons nullSourceAnn "redeemers" (mapOf (tyCon "ScriptPurpose") (tyCon "Redeemer")) . 
                                                         RCons nullSourceAnn "data" (mapOf (tyCon "DatumHash") (tyCon "Datum")) .
                                                         RCons nullSourceAnn "id" (tyCon "TxId") $ 
                                                         REmpty nullSourceAnn)
                                              ]
                                              )])),
  (primName "TxOut", (kindType, DataType Newtype 
                                         []
                                         [(ProperName "TxOut", [
                                            mkRecordT (RCons nullSourceAnn "address" (tyCon "Address") . 
                                                       RCons nullSourceAnn "value" (tyCon "Value") .
                                                       RCons nullSourceAnn "datum" (tyCon "OutputDatum") .
                                                       RCons nullSourceAnn "referenceScript" (maybeOf (tyCon "ScriptHash")) $ 
                                                       REmpty nullSourceAnn)
                                            ])])),
  (primName "TxOutRef", (kindType, DataType Newtype
                                            []
                                            [(ProperName "TxOutRef", [
                                                mkRecordT (RCons nullSourceAnn "id" (tyCon "TxId") .
                                                           RCons nullSourceAnn "idx" (tyCon "Int") $
                                                           REmpty nullSourceAnn)
                                                ])])),
  (primName "TxInInfo", (kindType, DataType Newtype 
                                            []
                                            [(ProperName "TxInInfo", [
                                                mkRecordT (RCons nullSourceAnn "txOutRef" (tyCon "TxOutRef") . 
                                                           RCons nullSourceAnn "resolved" (tyCon "TxOut") $
                                                           REmpty nullSourceAnn)
                                              ])])),
  (primName "OutputDatum", (kindType, DataType Data
                                               []
                                               [(ProperName "NoOutputDatum", []),
                                                (ProperName "OutputDatumHash", [tyCon "DatumHash"]),
                                                (ProperName "OutputDatum", [tyCon "Datum"])])),
  -- Intervals
  (primName "Interval", (kindType -:> kindType, 
                         DataType Newtype 
                                  [nominalVar "a"] 
                                  [(ProperName "Interval", [
                                                  mkRecordT (RCons nullSourceAnn "from" (tyApp (tyCon "LowerBound") (tyVar "a")) . 
                                                             RCons nullSourceAnn "to" (tyApp (tyCon "UpperBound") (tyVar "a")) $
                                                             REmpty nullSourceAnn)
                                                  ])])),
  (primName "Extended", (kindType -:> kindType, 
                         DataType Data 
                                  [nominalVar "a"] 
                                  [(ProperName "NegInf", []),
                                   (ProperName "Finite", [tyVar "a"]),
                                   (ProperName "PosInf", [])])),
  -- No Closure, just use the type it's a synonym of
  (primName "UpperBound", (kindType -:> kindType, 
                           DataType Newtype 
                                    [nominalVar "a"] 
                                    [(ProperName "UpperBound", [tyApp (tyCon "Extended") (tyVar "a")])])),
  (primName "LowerBound", (kindType -:> kindType, 
                           DataType Newtype 
                                    [nominalVar "a"] 
                                    [(ProperName "LowerBound", [tyApp (tyCon "Extended") (tyVar "a")])])),
  -- AssociationMaps
  (primName "AssocMap", (kindType -:> kindType -:> kindType, 
                         DataType Newtype 
                                  [nominalVar "k", nominalVar "v"] 
                                  [(ProperName "AssocMap", [listOf (tyApp (tyApp (tyCon "Tuple2") (tyVar "k")) (tyVar "v"))])])),
  -- Newtypes and hash types
  aNewtype "ScriptHash" "BuiltinByteString",
  aNewtype "Redeemer" "BuiltinData",
  aNewtype "RedeemerHash" "BuiltinByteString",
  aNewtype "Datum" "BuiltinData",
  aNewtype "DatumHash" "BuiltinByteString",
  -- Data
  (primName "Data", (kindType, DataType Data [] [(ProperName "I", [tyCon "Int"]),
                                                 (ProperName "B", [tyCon "BuiltinByteString"]),
                                                 (ProperName "Constr", [tyCon "Int", listOf (tyCon "Data")]),
                                                 (ProperName "List", [listOf (tyCon "Data")]),
                                                 (ProperName "Map", [listOf (tyApp (tyApp (tyCon "Tuple2") (tyCon "Data")) (tyCon "Data"))])]))
  ]

mkRecordT :: SourceType -> SourceType
mkRecordT = TypeApp nullSourceAnn (TypeConstructor nullSourceAnn C.Record)

-- Helpers

aNewtype :: Text -> Text -> (Qualified (ProperName 'TypeName), (SourceType, TypeKind))
aNewtype tyName wrappedName = (primName tyName, (kindType, DataType Newtype [] [(ProperName tyName, [tyCon wrappedName])]))

tyApp :: Type SourceAnn -> Type SourceAnn -> Type SourceAnn
tyApp f = TypeApp nullSourceAnn f

tyCon :: Text -> Type SourceAnn
tyCon = TypeConstructor nullSourceAnn . primName

listOf :: Type SourceAnn -> Type SourceAnn
listOf = tyApp (tyCon "Array")

mapOf :: Type SourceAnn -> Type SourceAnn -> Type SourceAnn
mapOf keyT = tyApp (tyApp (tyCon "AssocMap") keyT)

maybeOf :: Type SourceAnn -> Type SourceAnn
maybeOf = tyApp (tyCon "Maybe")

nominalVar :: Text -> (Text, SourceType, Role)
nominalVar varName = (varName, kindType, Nominal)

tyVar :: Text -> Type SourceAnn
tyVar varName = TypeVar nullSourceAnn varName kindType

ledgerMonoType :: Text -> (Qualified (ProperName 'TypeName), (SourceType, TypeKind))
ledgerMonoType typeName = (primName typeName, (kindType, ExternData []))

primName :: Text -> Qualified (ProperName 'TypeName)
primName typeName = Qualified (ByModuleName (ModuleName "Prim")) (ProperName typeName)

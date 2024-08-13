module Language.PureScript.LedgerApi (
  DataDeclType (..),
  showDataDeclType,
  TypeKind(..),
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
import Language.PureScript.AST.SourcePos (nullSourceAnn)
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

-- | Ledger API (V2) types, as per https://github.com/IntersectMBO/plutus/blob/master/plutus-ledger-api/src/PlutusLedgerApi/V2.hs
ledgerTypes :: [(Qualified (ProperName 'TypeName), (SourceType, TypeKind))]
ledgerTypes = [
  -- Context types
  ledgerMonoType "ScriptContext",
  ledgerMonoType "ScriptPurpose",
  -- Bytes
  ledgerMonoType "LedgerBytes",
  -- Certificates
  ledgerMonoType "DCert",
  -- Credentials
  ledgerMonoType "StakingCredential",
  ledgerMonoType "Credential",
  -- Value
  ledgerMonoType "Value",
  ledgerMonoType "CurrencySymbol",
  ledgerMonoType "TokenName",
  ledgerMonoType "Lovelace",
  -- Time
  ledgerMonoType "POSIXTime",
  -- No POSIXTimeRange, people can use the actual type instead of a synonym
  -- Types for representing transactions
  ledgerMonoType "Address",
  ledgerMonoType "PubKeyHash",
  ledgerMonoType "TxId",
  ledgerMonoType "TxInfo",
  ledgerMonoType "TxOut",
  ledgerMonoType "TxOutRef",
  ledgerMonoType "TxInInfo",
  ledgerMonoType "OutputDatum",
  -- Intervals
  (primName "Interval", (kindType -:> kindType, DataType 
                                                Newtype 
                                                [("a", kindType, Nominal)] 
                                                [(ProperName "Interval", [
                                                  mkRecordT (RCons nullSourceAnn "from" (TypeApp nullSourceAnn (TypeConstructor nullSourceAnn . primName $ "LowerBound") (TypeVar nullSourceAnn "a" kindType)) . 
                                                             RCons nullSourceAnn "to" (TypeApp nullSourceAnn (TypeConstructor nullSourceAnn . primName $ "UpperBound") (TypeVar nullSourceAnn "a" kindType)) $
                                                             REmpty nullSourceAnn)
                                                  ])])),
  (primName "Extended", (kindType -:> kindType, 
                         DataType Data 
                                  [("a", kindType, Nominal)] 
                                  [(ProperName "NegInf", []),
                                   (ProperName "Finite", [TypeVar nullSourceAnn "a" kindType]),
                                   (ProperName "PosInf", [])])),
  -- No Closure, just use the type it's a synonym of
  (primName "UpperBound", (kindType -:> kindType, 
                           DataType Newtype 
                                    [("a", kindType, Nominal)] 
                                    [(ProperName "UpperBound", [TypeApp nullSourceAnn (TypeConstructor nullSourceAnn . primName $ "Extended") (TypeVar nullSourceAnn "a" kindType)])])),
  (primName "LowerBound", (kindType -:> kindType, 
                           DataType Newtype 
                                    [("a", kindType, Nominal)] 
                                    [(ProperName "LowerBound", [TypeApp nullSourceAnn (TypeConstructor nullSourceAnn . primName $ "Extended") (TypeVar nullSourceAnn "a" kindType)])])),
  -- AssociationMaps
  (primName "AssocMap", (kindType -:> kindType -:> kindType, 
                         DataType Newtype 
                                  [("k", kindType, Nominal), ("v", kindType, Nominal)] 
                                  [(ProperName "AssocMap", [
                                    TypeApp nullSourceAnn (TypeConstructor nullSourceAnn . primName $ "Array") (TypeApp nullSourceAnn (TypeApp nullSourceAnn (TypeConstructor nullSourceAnn . primName $ "Tuple2") (TypeVar nullSourceAnn "k" kindType)) 
                                                                                                                                                                                                                  (TypeVar nullSourceAnn "v" kindType))
                                    ])])),
  -- Newtypes and hash types
  ledgerMonoType "ScriptHash",
  ledgerMonoType "Redeemer",
  ledgerMonoType "RedeemerHash",
  ledgerMonoType "Datum",
  ledgerMonoType "DatumHash",
  -- Data
  ledgerMonoType "Data",
  ledgerMonoType "BuiltinData"
  ]

mkRecordT :: SourceType -> SourceType
mkRecordT = TypeApp nullSourceAnn (TypeConstructor nullSourceAnn C.Record)

-- Helpers

ledgerMonoType :: Text -> (Qualified (ProperName 'TypeName), (SourceType, TypeKind))
ledgerMonoType typeName = (primName typeName, (kindType, ExternData []))

primName :: Text -> Qualified (ProperName 'TypeName)
primName typeName = Qualified (ByModuleName (ModuleName "Prim")) (ProperName typeName)

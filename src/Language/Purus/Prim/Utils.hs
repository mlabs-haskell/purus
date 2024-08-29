module Language.Purus.Prim.Utils where

-- Helpers

import Data.Text (Text)
import Language.PureScript.AST.SourcePos (SourceAnn, nullSourceAnn)
import Language.PureScript.Constants.Prim qualified as C
import Language.PureScript.CoreFn.Module (
  CtorDecl (CtorDecl),
  DataDecl (DataDecl),
 )
import Language.PureScript.Environment (
  DataDeclType (Data, Newtype),
  TypeKind (DataType),
  kindType,
  (-:>),
 )
import Language.PureScript.Label (Label)
import Language.PureScript.Names (
  Ident (Ident, UnusedIdent),
  ModuleName (ModuleName),
  ProperName (..),
  ProperNameType (ConstructorName, TypeName),
  Qualified (Qualified),
  QualifiedBy (ByModuleName),
 )
import Language.PureScript.Roles (Role (Nominal))
import Language.PureScript.Types (
  SourceType,
  Type (RCons, REmpty, TypeApp, TypeConstructor, TypeVar),
 )
import Prelude

-- | Converts a ProperName to an Ident. Duplicated here to break a module cycle.
properToIdent :: ProperName a -> Ident
properToIdent = Ident . runProperName

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

sumDecl :: Text -> [(Text, [SourceType])] -> (Qualified (ProperName 'TypeName), DataDecl SourceType SourceType)
sumDecl tyName arms =
  let tyName' = primName tyName
   in (tyName', DataDecl Data tyName' [] . fmap go $ arms)
  where
    go :: (Text, [SourceType]) -> CtorDecl SourceType
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

rowFromFields :: [(Label, SourceType)] -> SourceType
rowFromFields [] = REmpty nullSourceAnn
rowFromFields ((l, t) : rest) = RCons nullSourceAnn l t $ rowFromFields rest

recordFromFields :: [(Label, SourceType)] -> SourceType
recordFromFields = mkRecordT . rowFromFields

polySumType :: [Text] -> [(Text, [Type SourceAnn])] -> TypeKind
polySumType vars = DataType Data (fmap nominalVar vars) . fmap go
  where
    go :: (Text, [Type SourceAnn]) -> (ProperName 'ConstructorName, [Type SourceAnn])
    go (varName, varArgs) = (ProperName varName, varArgs)

recordDecl :: Text -> [(Label, SourceType)] -> (Qualified (ProperName 'TypeName), DataDecl SourceType SourceType)
recordDecl tyName fields =
  let tyName' = primName tyName
   in ( tyName'
      , DataDecl
          Newtype
          tyName'
          []
          [ CtorDecl
              (properToIdent <$> tyName')
              [(UnusedIdent, recordFromFields fields)]
          ]
      )

polyRecordDecl ::
  Text ->
  [Text] ->
  [(Label, SourceType)] ->
  (Qualified (ProperName 'TypeName), DataDecl SourceType SourceType)
polyRecordDecl tyName vars fields =
  let tyName' = primName tyName
   in ( tyName'
      , DataDecl
          Newtype
          tyName'
          (fmap (,kindType) vars)
          [ CtorDecl
              (properToIdent <$> tyName')
              [(UnusedIdent, recordFromFields fields)]
          ]
      )

newtypeDecl :: Text -> SourceType -> (Qualified (ProperName 'TypeName), DataDecl SourceType SourceType)
newtypeDecl tyName def =
  let tyName' = primName tyName
   in ( tyName'
      , DataDecl
          Newtype
          tyName'
          []
          [ CtorDecl
              (properToIdent <$> tyName')
              [(UnusedIdent, def)]
          ]
      )

polyNewtypeDecl :: Text -> [Text] -> SourceType -> (Qualified (ProperName 'TypeName), DataDecl SourceType SourceType)
polyNewtypeDecl tyName vars def =
  let tyName' = primName tyName
   in ( tyName'
      , DataDecl
          Newtype
          tyName'
          (fmap (,kindType) vars)
          [ CtorDecl
              (properToIdent <$> tyName')
              [(UnusedIdent, def)]
          ]
      )

mapTy :: SourceType -> SourceType -> SourceType
mapTy k = TypeApp nullSourceAnn (TypeApp nullSourceAnn (TypeConstructor nullSourceAnn (primName "AssocMap")) k)

primTyCon :: Text -> SourceType
primTyCon = TypeConstructor nullSourceAnn . primName

maybeTy :: SourceType -> SourceType
maybeTy = TypeApp nullSourceAnn (TypeConstructor nullSourceAnn . primName $ "Maybe")

listTy :: SourceType -> SourceType
listTy = TypeApp nullSourceAnn (TypeConstructor nullSourceAnn . primName $ "Array")

tuple2Ty :: SourceType -> SourceType -> SourceType
tuple2Ty x = TypeApp nullSourceAnn (TypeApp nullSourceAnn (primTyCon "Tuple2") x)

primIdent :: Text -> Qualified Ident
primIdent name = Qualified (ByModuleName (ModuleName "Prim")) (Ident name)

mononym :: Text -> (Qualified Ident, Qualified (ProperName 'TypeName))
mononym tyName = (primIdent tyName, primName tyName)

arm :: Text -> Text -> (Qualified Ident, Qualified (ProperName 'TypeName))
arm conName tyName = (primIdent conName, primName tyName)

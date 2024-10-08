{-# LANGUAGE NoOverloadedStrings #-}

{- |
Dump the core functional representation in JSON format for consumption
by third-party code generators
-}
module Language.PureScript.CoreFn.ToJSON (
  moduleToJSON,
  moduleToJSON',
  nullifyAnnModule
) where

import Prelude

import Control.Arrow ((***))
import Control.Lens (Plated(..), transform)
import Data.Functor.Identity (runIdentity)
import Data.Aeson (ToJSON (..), Value (..), object)
import Data.Aeson qualified
import Data.Aeson.Key qualified
import Data.Aeson.Types (Pair)
import Data.Either (isLeft)
import Data.Map.Strict qualified as M
import Data.Text (Text)
import Data.Text qualified as T
import Data.Version (Version, showVersion)

import Language.PureScript.AST.Literals (Literal (..))
import Language.PureScript.AST.SourcePos (SourceSpan (..), SourceAnn, pattern NullSourceAnn)
import Language.PureScript.CoreFn (Ann, Bind (..), Binder (..), CaseAlternative (..), ConstructorType (..), Expr (..), Meta (..), Module (..))
import Language.PureScript.CoreFn.Module
import Language.PureScript.CoreFn.Ann (nullAnn)
import Language.PureScript.Names (Ident, ModuleName (..), ProperName (..), Qualified (..), QualifiedBy (..), runIdent)
import Language.PureScript.PSString (PSString)
import Language.PureScript.Types (SourceType, Type(..))

constructorTypeToJSON :: ConstructorType -> Value
constructorTypeToJSON ProductType = toJSON "ProductType"
constructorTypeToJSON SumType = toJSON "SumType"

infixr 8 .=
(.=) :: (ToJSON a) => String -> a -> Pair
key .= value = Data.Aeson.Key.fromString key Data.Aeson..= value

metaToJSON :: Meta -> Value
metaToJSON (IsConstructor t is) =
  object
    [ "metaType" .= "IsConstructor"
    , "constructorType" .= constructorTypeToJSON t
    , "identifiers" .= identToJSON `map` is
    ]
metaToJSON IsNewtype = object ["metaType" .= "IsNewtype"]
metaToJSON IsTypeClassConstructor = object ["metaType" .= "IsTypeClassConstructor"]
metaToJSON IsForeign = object ["metaType" .= "IsForeign"]
metaToJSON IsWhere = object ["metaType" .= "IsWhere"]
metaToJSON IsSyntheticApp = object ["metaType" .= "IsSyntheticApp"]

sourceSpanToJSON :: SourceSpan -> Value
sourceSpanToJSON (SourceSpan _ spanStart spanEnd) =
  object
    [ "start" .= spanStart
    , "end" .= spanEnd
    ]

annToJSON :: Ann -> Value
annToJSON (ss, _, m) =
  object
    [ "sourceSpan" .= sourceSpanToJSON ss
    , "meta" .= maybe Null metaToJSON m
    ]

literalToJSON :: (a -> Value) -> Literal a -> Value
literalToJSON _ (NumericLiteral (Left n)) =
  object
    [ "literalType" .= "IntLiteral"
    , "value" .= n
    ]
literalToJSON _ (NumericLiteral (Right n)) =
  object
    [ "literalType" .= "NumberLiteral"
    , "value" .= n
    ]
literalToJSON _ (StringLiteral s) =
  object
    [ "literalType" .= "StringLiteral"
    , "value" .= s
    ]
literalToJSON _ (CharLiteral c) =
  object
    [ "literalType" .= "CharLiteral"
    , "value" .= c
    ]
literalToJSON _ (BooleanLiteral b) =
  object
    [ "literalType" .= "BooleanLiteral"
    , "value" .= b
    ]
literalToJSON t (ListLiteral xs) =
  object
    [ "literalType" .= "ListLiteral"
    , "value" .= map t xs
    ]
literalToJSON t (ObjectLiteral xs) =
  object
    [ "literalType" .= "ObjectLiteral"
    , "value" .= recordToJSON t xs
    ]

identToJSON :: Ident -> Value
identToJSON = toJSON . runIdent

properNameToJSON :: ProperName a -> Value
properNameToJSON = toJSON . runProperName

qualifiedToJSON :: (a -> Text) -> Qualified a -> Value
qualifiedToJSON f (Qualified qb a) =
  case qb of
    ByModuleName mn ->
      object
        [ "moduleName" .= moduleNameToJSON mn
        , "identifier" .= toJSON (f a)
        ]
    BySourcePos ss ->
      object
        [ "sourcePos" .= toJSON ss
        , "identifier" .= toJSON (f a)
        ]

moduleNameToJSON :: ModuleName -> Value
moduleNameToJSON (ModuleName name) = toJSON (T.splitOn (T.pack ".") name)

moduleToJSON :: Version -> Module (Bind Ann) SourceType SourceType Ann -> Value
moduleToJSON v m =
  object
    [ "sourceSpan" .= sourceSpanToJSON (moduleSourceSpan m)
    , "moduleName" .= moduleNameToJSON (moduleName m)
    , "modulePath" .= toJSON (modulePath m)
    , "imports" .= map importToJSON (moduleImports m)
    , "exports" .= map identToJSON (moduleExports m)
    , "reExports" .= reExportsToJSON (moduleReExports m)
    , "foreign" .= map identToJSON (moduleForeign m)
    , "decls" .= map bindToJSON (moduleDecls m)
    , "builtWith" .= toJSON (showVersion v)
    , "comments" .= map toJSON (moduleComments m)
    , "dataTypes" .= toJSON (moduleDataTypes m)
    ]
  where
    importToJSON (ann, mn) =
      object
        [ "annotation" .= annToJSON ann
        , "moduleName" .= moduleNameToJSON mn
        ]

    reExportsToJSON :: M.Map ModuleName [Ident] -> Value
    reExportsToJSON = toJSON . M.map (map runIdent)

moduleToJSON' :: Module (Bind Ann) SourceType SourceType Ann -> Value
moduleToJSON' m =
  object
    [ "sourceSpan" .= sourceSpanToJSON (moduleSourceSpan m)
    , "moduleName" .= moduleNameToJSON (moduleName m)
    , "modulePath" .= toJSON (modulePath m)
    , "imports" .= map importToJSON (moduleImports m)
    , "exports" .= map identToJSON (moduleExports m)
    , "reExports" .= reExportsToJSON (moduleReExports m)
    , "foreign" .= map identToJSON (moduleForeign m)
    , "decls" .= map bindToJSON (moduleDecls m)
    , "comments" .= map toJSON (moduleComments m)
    , "dataTypes" .= toJSON (moduleDataTypes m)
    ]
  where
    importToJSON (ann, mn) =
      object
        [ "annotation" .= annToJSON ann
        , "moduleName" .= moduleNameToJSON mn
        ]

    reExportsToJSON :: M.Map ModuleName [Ident] -> Value
    reExportsToJSON = toJSON . M.map (map runIdent)


bindToJSON :: Bind Ann -> Value
bindToJSON (NonRec ann n e) =
  object
    [ "bindType" .= "NonRec"
    , "annotation" .= annToJSON ann
    , "identifier" .= identToJSON n
    , "expression" .= exprToJSON e
    ]
bindToJSON (Rec bs) =
  object
    [ "bindType" .= "Rec"
    , "binds"
        .= map
          ( \((ann, n), e) ->
              object
                [ "identifier" .= identToJSON n
                , "annotation" .= annToJSON ann
                , "expression" .= exprToJSON e
                ]
          )
          bs
    ]

recordToJSON :: (a -> Value) -> [(PSString, a)] -> Value
recordToJSON f = toJSON . map (toJSON *** f)

exprToJSON :: Expr Ann -> Value
exprToJSON (Var ann ty i) =
  object
    [ "kind" .= toJSON "Var"
    , "type" .= toJSON ty
    , "annotation" .= annToJSON ann
    , "value" .= qualifiedToJSON runIdent i
    ]
exprToJSON (Literal ann ty l) =
  object
    [ "kind" .= "Literal"
    , "type" .= toJSON ty
    , "annotation" .= annToJSON ann
    , "value" .= literalToJSON exprToJSON l
    ]
exprToJSON (Accessor ann ty f r) =
  object
    [ "kind" .= "Accessor"
    , "type" .= toJSON ty
    , "annotation" .= annToJSON ann
    , "fieldName" .= f
    , "expression" .= exprToJSON r
    ]
exprToJSON (ObjectUpdate ann ty r copy fs) =
  object
    [ "kind" .= "ObjectUpdate"
    , "type" .= toJSON ty
    , "annotation" .= annToJSON ann
    , "expression" .= exprToJSON r
    , "copy" .= toJSON copy
    , "updates" .= recordToJSON exprToJSON fs
    ]
exprToJSON (Abs ann ty p b) =
  object
    [ "kind" .= "Abs"
    , "type" .= toJSON ty
    , "annotation" .= annToJSON ann
    , "argument" .= identToJSON p
    , "body" .= exprToJSON b
    ]
exprToJSON (App ann f x) =
  object
    [ "kind" .= "App"
    , "annotation" .= annToJSON ann
    , "abstraction" .= exprToJSON f
    , "argument" .= exprToJSON x
    ]
exprToJSON (Case ann ty ss cs) =
  object
    [ "kind" .= "Case"
    , "type" .= toJSON ty
    , "annotation" .= annToJSON ann
    , "caseExpressions"
        .= map exprToJSON ss
    , "caseAlternatives"
        .= map caseAlternativeToJSON cs
    ]
exprToJSON (Let ann bs e) =
  object
    [ "kind" .= "Let"
    , "annotation" .= annToJSON ann
    , "binds" .= map bindToJSON bs
    , "expression" .= exprToJSON e
    ]

caseAlternativeToJSON :: CaseAlternative Ann -> Value
caseAlternativeToJSON (CaseAlternative bs r') =
  let isGuarded = isLeft r'
   in object
        [ "binders" .= toJSON (map binderToJSON bs)
        , "isGuarded" .= toJSON isGuarded
        , (if isGuarded then "expressions" else "expression")
            .= case r' of
              Left rs -> toJSON $ map (\(g, e) -> object ["guard" .= exprToJSON g, "expression" .= exprToJSON e]) rs
              Right r -> exprToJSON r
        ]

binderToJSON :: Binder Ann -> Value
binderToJSON (VarBinder ann v t) =
  object
    [ "binderType" .= "VarBinder"
    , "annotation" .= annToJSON ann
    , "identifier" .= identToJSON v
    , "type" .= toJSON t
    ]
binderToJSON (NullBinder ann) =
  object
    [ "binderType" .= "NullBinder"
    , "annotation" .= annToJSON ann
    ]
binderToJSON (LiteralBinder ann l) =
  object
    [ "binderType" .= "LiteralBinder"
    , "annotation" .= annToJSON ann
    , "literal" .= literalToJSON binderToJSON l
    ]
binderToJSON (ConstructorBinder ann d c bs) =
  object
    [ "binderType" .= "ConstructorBinder"
    , "annotation" .= annToJSON ann
    , "typeName" .= qualifiedToJSON runProperName d
    , "constructorName"
        .= qualifiedToJSON runProperName c
    , "binders" .= map binderToJSON bs
    ]
binderToJSON (NamedBinder ann n b) =
  object
    [ "binderType" .= "NamedBinder"
    , "annotation" .= annToJSON ann
    , "identifier" .= identToJSON n
    , "binder" .= binderToJSON b
    ]

-- Misc helpers to erase annotations when serializing CoreFn (since we ignore the annotations anyway)
nullifyAnnDataTypes :: Datatypes (Type SourceAnn) (Type SourceAnn)
                    -> Datatypes (Type SourceAnn) (Type SourceAnn)
nullifyAnnDataTypes = runIdentity . bitraverseDatatypes (pure . nullifyAnnTypes) (pure . nullifyAnnTypes)

nullifyAnnTypes :: Type SourceAnn -> Type SourceAnn
nullifyAnnTypes = fmap (const NullSourceAnn)

nullifyAnnExpr :: Expr Ann -> Expr Ann
nullifyAnnExpr = fmap (const nullAnn)
               . transform (\case
                   Literal ann t lit -> Literal ann (nullifyAnnTypes t) lit
                   Accessor ann t f e -> Accessor ann (nullifyAnnTypes t) f e
                   ObjectUpdate ann t e cf fs ->
                     ObjectUpdate ann (nullifyAnnTypes t) e cf fs
                   Abs ann t ident e -> Abs ann (nullifyAnnTypes t) ident e
                   App ann t1 t2 -> App ann t1 t2
                   Var ann t qi -> Var ann (nullifyAnnTypes t) qi
                   Case ann t scruts alts -> Case ann (nullifyAnnTypes t) scruts alts
                   Let ann bs e -> Let ann bs e
                   )

nullifyAnnModule :: Module (Bind Ann) (Type SourceAnn) (Type SourceAnn) Ann
                 -> Module (Bind Ann) (Type SourceAnn) (Type SourceAnn) Ann
nullifyAnnModule Module{..} = Module {moduleDataTypes = cleanedDatatypes, moduleDecls = cleanedDecls,..}
  where
    cleanedDatatypes = nullifyAnnDataTypes moduleDataTypes
    cleanedDecls = go <$> moduleDecls
    go = \case
      NonRec _ i e -> NonRec nullAnn i (nullifyAnnExpr e)
      Rec xs -> Rec $ (\((ann,ident),expr) -> ((nullAnn,ident),nullifyAnnExpr expr)) <$> xs

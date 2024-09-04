{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE DeriveLift #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-implicit-lift #-}

module Language.Purus.TH (
  ctDecodeModule
  ) where

import Prelude
import Language.Haskell.TH (Exp, Q)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (eitherDecodeFileStrict')
import Language.PureScript.CoreFn.Module (
  Module (Module), 
  Datatypes (Datatypes),
  DataDecl (DataDecl),
  CtorDecl (CtorDecl)
  )
import Language.PureScript.Types (
  Type (TUnknown,
        TypeVar,
        TypeLevelString,
        TypeLevelInt,
        TypeWildcard,
        TypeConstructor,
        TypeOp,
        TypeApp,
        KindApp,
        ForAll,
        ConstrainedType,
        Skolem,
        REmpty,
        RCons,
        KindedType,
        BinaryNoParensType,
        ParensInType),
  Constraint (Constraint),
  ConstraintData (PartialConstraintData),
  WildcardData (HoleWildcard, UnnamedWildcard, IgnoredWildcard),
  SkolemScope (SkolemScope),
  TypeVarVisibility (TypeVarVisible, TypeVarInvisible)
  )
import Language.PureScript.CoreFn.Ann (Ann)
import Language.PureScript.AST.SourcePos (
  SourceAnn,
  SourcePos (SourcePos),
  SourceSpan (SourceSpan)
  )
import Language.PureScript.CoreFn.Expr (
  Bind (NonRec, Rec),
  Expr (Literal,
        Accessor,
        ObjectUpdate,
        Abs,
        App,
        Var,
        Case,
        Let),
  CaseAlternative (CaseAlternative)
  )
import Language.PureScript.CoreFn.FromJSON ()
import Language.Haskell.TH.Syntax (Lift)
import Language.PureScript.Names (
  Qualified (Qualified),
  Ident (Ident, GenIdent, UnusedIdent, InternalIdent),
  InternalIdentData (RuntimeLazyFactory, Lazy),
  QualifiedBy (BySourcePos, ByModuleName),
  ModuleName (ModuleName),
  ProperName (ProperName), 
  ProperNameType (TypeName, ClassName, ConstructorName),
  OpName (OpName), 
  OpNameType (TypeOpName),
  )
import Language.PureScript.Comments (
  Comment (LineComment, BlockComment)
  )
import Language.PureScript.Label (Label (Label))
import Language.PureScript.Environment (
  DataDeclType (Data, Newtype)
  )
import Language.PureScript.AST.Literals (
  Literal (NumericLiteral,
           StringLiteral,
           CharLiteral,
           BooleanLiteral,
           ArrayLiteral,
           ObjectLiteral
           )
  )
import Language.PureScript.CoreFn.Binders (
  Binder (NullBinder,
          LiteralBinder,
          VarBinder,
          ConstructorBinder,
          NamedBinder
          )
  )
import Language.PureScript.CoreFn.Meta (
  Meta (IsConstructor,
        IsNewtype,
        IsTypeClassConstructor,
        IsForeign,
        IsWhere,
        IsSyntheticApp
        ),
  ConstructorType (ProductType, SumType)
  )

deriving stock instance Lift InternalIdentData

deriving stock instance Lift Ident

deriving stock instance Lift SourcePos

deriving stock instance Lift ModuleName

deriving stock instance Lift QualifiedBy

deriving stock instance Lift a => Lift (Qualified a)

deriving stock instance Lift (ProperName 'TypeName)

deriving stock instance Lift (OpName 'TypeOpName)

deriving stock instance Lift (ProperName 'ClassName)

deriving stock instance Lift ConstraintData

deriving stock instance Lift SourceSpan

deriving stock instance Lift Comment

deriving stock instance Lift (Constraint SourceAnn)

deriving stock instance Lift WildcardData

deriving stock instance Lift SkolemScope

deriving stock instance Lift TypeVarVisibility

deriving stock instance Lift Label

deriving stock instance Lift (Type SourceAnn)

deriving stock instance Lift (CtorDecl (Type SourceAnn))

deriving stock instance Lift DataDeclType

deriving stock instance Lift (DataDecl (Type SourceAnn) (Type SourceAnn))

deriving stock instance Lift (Datatypes (Type SourceAnn) (Type SourceAnn))

deriving stock instance Lift a => Lift (Literal a)

deriving stock instance Lift (ProperName 'ConstructorName)

deriving stock instance Lift ConstructorType

deriving stock instance Lift Meta

deriving stock instance Lift (Binder Ann)

deriving stock instance Lift (CaseAlternative Ann)

deriving stock instance Lift (Expr Ann)

deriving stock instance Lift (Bind Ann)

deriving stock instance Lift (Module (Bind Ann) (Type SourceAnn) (Type SourceAnn) Ann)

ctDecodeModule :: FilePath -> Q Exp
ctDecodeModule fp = do
  decoded <- liftIO $ eitherDecodeFileStrict' @(Module (Bind Ann) (Type SourceAnn) (Type SourceAnn) Ann) fp
  case decoded of 
    Left err -> fail $ "Cannot construct a Module from " <> fp <> "\nReason: " <> err
    Right res -> [| res |]

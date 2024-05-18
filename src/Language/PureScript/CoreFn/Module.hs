{-# LANGUAGE StandaloneDeriving, ScopedTypeVariables, StrictData  #-}
{-# LANGUAGE StandaloneKindSignatures #-}
module Language.PureScript.CoreFn.Module where

import Prelude

import Data.Map.Strict (Map)
import Data.List (sort)

import Data.Text (Text)
import Language.PureScript.AST.SourcePos (SourceSpan)
import Language.PureScript.AST.Literals (Literal(..))
import Language.PureScript.Comments (Comment)
import Language.PureScript.CoreFn.Expr (Bind(..), Expr(..), CaseAlternative)
import Language.PureScript.CoreFn.Ann
import Language.PureScript.Names (Ident, ModuleName, ProperNameType (..), ProperName)
import Data.Bifunctor (second)
import Language.PureScript.AST.Declarations (DataConstructorDeclaration)
import Language.PureScript.Environment (DataDeclType)
import Language.PureScript.Types (SourceType)

import Data.Kind qualified as GHC

-- |
-- The CoreFn module representation
--
type Module :: GHC.Type -> GHC.Type -> GHC.Type
data Module decl a = Module
  { moduleSourceSpan :: SourceSpan
  , moduleComments :: [Comment]
  , moduleName :: ModuleName
  , modulePath :: FilePath
  , moduleImports :: [(a, ModuleName)]
  , moduleExports :: [Ident]
  , moduleReExports :: Map ModuleName [Ident]
  , moduleForeign :: [Ident]
  , moduleDecls :: [decl]
  , moduleDataTypes :: Map (ProperName 'TypeName) (DataDeclType,[(Text, SourceType)],[DataConstructorDeclaration])
  } deriving (Functor, Show)

deriving instance Eq a => Eq (Module (Bind a) a)


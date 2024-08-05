{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}

module Language.PureScript.CoreFn.Module where

import Prelude

import Data.Map.Strict (Map)

import Data.Text (Text)
import Language.PureScript.AST.SourcePos (SourceSpan)
import Language.PureScript.Comments (Comment)
import Language.PureScript.CoreFn.Expr (Bind (..))
import Language.PureScript.Environment (DataDeclType)
import Language.PureScript.Names (Ident, ModuleName, ProperName (..), ProperNameType (..), Qualified)

import Control.Lens (
  Ixed (ix),
  filtered,
  folded,
  makeLenses,
  view,
  (^.),
  (^?),
  (^?!),
 )

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)

import Data.Kind qualified as GHC
import Data.List (find, findIndex)
import Data.Map qualified as M
import Data.Maybe (fromJust)
import Language.PureScript.CoreFn.Desugar.Utils (properToIdent)

data DataDecl k t = DataDecl
  { _dDeclType :: !DataDeclType
  , _dDataTyName :: !(Qualified (ProperName 'TypeName))
  , _dDataArgs :: ![(Text, k)]
  , _dDataCtors :: ![CtorDecl t]
  }
  deriving (Show, Eq, Ord, Generic)
instance (ToJSON k, ToJSON t) => ToJSON (DataDecl k t)
instance (FromJSON k, FromJSON t) => FromJSON (DataDecl k t)

-- not worth writing all the necessary instances for a real Bitraversable instance.
-- monad instead of applicative solely for do notation and b/c it'll always be a monad anyway
bitraverseDataDecl ::
  forall f k k' t t'.
  (Monad f) =>
  (k -> f k') ->
  (t -> f t') ->
  DataDecl k t ->
  f (DataDecl k' t')
bitraverseDataDecl fk ft DataDecl {..} = do
  dataArgs' <- traverse (traverse fk) _dDataArgs
  dataCtors' <- traverse (traverseCtorDecl ft) _dDataCtors
  pure $ DataDecl _dDeclType _dDataTyName dataArgs' dataCtors'

data CtorDecl t = CtorDecl
  { _cdCtorName :: !(Qualified Ident)
  , _cdCtorFields :: ![(Ident, t)]
  }
  deriving (Show, Eq, Ord, Generic)
instance (ToJSON t) => ToJSON (CtorDecl t)
instance (FromJSON t) => FromJSON (CtorDecl t)

traverseCtorDecl :: forall f t t'. (Monad f) => (t -> f t') -> CtorDecl t -> f (CtorDecl t')
traverseCtorDecl f (CtorDecl nms fs) = CtorDecl nms <$> traverse (traverse f) fs

data Datatypes k t = Datatypes
  { _tyDict :: M.Map (Qualified (ProperName 'TypeName)) (DataDecl k t)
  , -- primarily for Olog(n) "is this a constructor?" which we'll need in the monomorphizer
    _ctorDict :: M.Map (Qualified Ident) (Qualified (ProperName 'TypeName))
  }
  deriving (Show, Eq, Ord, Generic)
instance (ToJSON k, ToJSON t) => ToJSON (Datatypes k t)
instance (FromJSON k, FromJSON t) => FromJSON (Datatypes k t)

makeLenses ''DataDecl
makeLenses ''CtorDecl
makeLenses ''Datatypes

lookupCtorType :: Qualified Ident -> Datatypes k t -> Maybe (Qualified (ProperName 'TypeName))
lookupCtorType qi (Datatypes _ ctors) = M.lookup qi ctors

-- | Unsafe (we only use this in contexts where failure is fatal) TODO: Throw a useful error
getConstructorIndexAndDecl ::
  Qualified (ProperName 'ConstructorName) ->
  Datatypes k t ->
  Either String (Int, CtorDecl t)
getConstructorIndexAndDecl qn dts = do
  let ctorIdent = properToIdent <$> qn
  ctorTyNm <- note ("No type found for constructor: " <> show ctorIdent) $ lookupCtorType ctorIdent dts
  dDecl <- note ("No data declaration found for type: " <> show ctorTyNm) $ lookupDataDecl ctorTyNm dts
  let allTheCtors = dDecl ^. dDataCtors
  decl <- note ("No constructor declaration found for: " <> show ctorIdent) $ find ((== ctorIdent) . view cdCtorName) allTheCtors
  indX <- note ("No constructor index found for: " <> show ctorIdent) $ findIndex ((== ctorIdent) . view cdCtorName) allTheCtors
  pure (indX, decl)
  where
    note :: forall x. String -> Maybe x -> Either String x
    note msg = \case
      Nothing -> Left msg
      Just res -> pure res

-- | Unsafe (we only use this in contexts where failure is fatal) TODO: Throw a useful error
getAllConstructorDecls ::
  Qualified (ProperName 'TypeName) ->
  Datatypes k t ->
  [CtorDecl t]
getAllConstructorDecls qn dts = dts ^?! tyDict . ix qn . dDataCtors

lookupDataDecl :: Qualified (ProperName 'TypeName) -> Datatypes k t -> Maybe (DataDecl k t)
lookupDataDecl qtn (Datatypes tys _) = M.lookup qtn tys

lookupCtorDecl :: Qualified Ident -> Datatypes k t -> Maybe (CtorDecl t)
lookupCtorDecl qi datatypes = do
  tyname <- datatypes ^? ctorDict . ix qi
  datatypes ^? tyDict . ix tyname . dDataCtors . folded . filtered ((== qi) . view cdCtorName)

instance Semigroup (Datatypes k t) where
  (Datatypes tyDict1 ctorDict1) <> (Datatypes tyDict2 ctorDict2) =
    Datatypes (tyDict1 <> tyDict2) (ctorDict1 <> ctorDict2)

instance Monoid (Datatypes k t) where
  mempty = Datatypes M.empty M.empty

bitraverseDatatypes ::
  forall f k k' t t'.
  (Monad f) =>
  (k -> f k') ->
  (t -> f t') ->
  Datatypes k t ->
  f (Datatypes k' t')
bitraverseDatatypes fk ft (Datatypes ddecls ctormap) = do
  ddecls' <- traverse (bitraverseDataDecl fk ft) ddecls
  pure $ Datatypes ddecls' ctormap

{- |
The CoreFn module representation.

`Module decl kind ty` contains a [decl] representing the module declarations
and a `DataTypes kind ty` representing the algebraic datatype declarations
-}
type Module :: GHC.Type -> GHC.Type -> GHC.Type -> GHC.Type -> GHC.Type
data Module decl kind ty ann = Module
  { moduleSourceSpan :: SourceSpan
  , moduleComments :: [Comment]
  , moduleName :: ModuleName
  , modulePath :: FilePath
  , moduleImports :: [(ann, ModuleName)]
  , moduleExports :: [Ident]
  , moduleReExports :: Map ModuleName [Ident]
  , moduleForeign :: [Ident]
  , moduleDecls :: [decl]
  , moduleDataTypes :: Datatypes kind ty
  }
  deriving (Functor, Show)

deriving instance (Eq a, Eq k, Eq t) => Eq (Module (Bind a) k t a)

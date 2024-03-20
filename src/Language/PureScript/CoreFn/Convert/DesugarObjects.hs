{-# OPTIONS_GHC -Wno-orphans #-} -- has to be here (more or less)
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Language.PureScript.CoreFn.Convert.DesugarObjects where

import Prelude
import Language.PureScript.CoreFn.Expr
    ( _Var,
      eType,
      exprType,
      Bind(..),
      CaseAlternative(CaseAlternative),
      Expr(..),
      PurusType )
import Language.PureScript.CoreFn.Module ( Module(..) )
import Language.PureScript.Names (Ident(..), Qualified (..), QualifiedBy (..), pattern ByNullSourcePos, ModuleName)
import Language.PureScript.Types
    ( rowToList, RowListItem(..), SourceType, Type(..) )
import Language.PureScript.CoreFn.Pretty.Common ( analyzeApp )
import Language.PureScript.CoreFn.Desugar.Utils ( showIdent' )
import Language.PureScript.Environment (pattern (:->), pattern ArrayT, pattern RecordT, function)
import Language.PureScript.CoreFn.Pretty
    ( prettyTypeStr, renderExprStr )
import Language.PureScript.CoreFn.Ann (Ann)
import Language.PureScript.CoreFn.FromJSON ()
import Data.Aeson qualified as Aeson
import Data.Text qualified as T
import Data.List (find)
import Debug.Trace ( trace, traceM )
import Language.PureScript.AST.Literals (Literal(..))
import Data.Map (Map)
import Data.Map qualified as M
import Language.PureScript.Label (Label(runLabel))
import Language.PureScript.PSString (PSString, prettyPrintString)
import Language.PureScript.AST.SourcePos
    ( SourceAnn, pattern NullSourceSpan )
import Data.Bitraversable (Bitraversable(bitraverse))
import Control.Lens.IndexedPlated
import Control.Lens ( (^..) )
import Control.Monad.RWS.Class ( MonadReader(ask), gets, modify' )
import Control.Monad.RWS
    ( RWST(..) )
import Control.Monad.Except (throwError)
import Language.PureScript.CoreFn.Convert.Plated ( Depth )
import Language.PureScript.CoreFn.Convert.Monomorphize
import GHC.Natural (Natural)

allTypes :: Expr Ann -> [SourceType]
allTypes e = e ^.. icosmos @Natural @(Expr Ann) 0 . eType


-- TODO: Rework these, everything fails if you have 'forall (...). RecordT'
isRecordType :: SourceType -> Bool
isRecordType (RecordT _) = True
isRecordType _ = False

isClosedRecord :: SourceType -> Bool
isClosedRecord (RecordT fields) = isClosedRow fields
isClosedRecord _ = False

isClosedRow :: SourceType -> Bool
isClosedRow = \case
  RCons _ _ _ rest -> isClosedRow rest
  REmpty _ -> True
  KindApp _ REmpty{} _  -> True -- Am not 100% sure if this is actually used in closed rows
  _ -> False

noOpenRows :: Expr Ann -> Bool
noOpenRows = all ((\x -> not (isRecordType x) || isClosedRecord x) . stripQuantifiers) . allTypes

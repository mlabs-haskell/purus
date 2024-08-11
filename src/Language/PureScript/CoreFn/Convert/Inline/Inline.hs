module Language.PureScript.CoreFn.Convert.Inline.Inline where

import Bound.Scope (Scope (..), abstract, fromScope)
import Bound.Var (Var (..))
import Data.Map qualified as M
import Language.PureScript.CoreFn.Convert.DesugarCore (
  Vars,
  WithObjects,
 )
import Language.PureScript.CoreFn.Convert.IR (
  Alt (..),
  BVar (..),
  BindE (..),
  Exp (..),
  FVar (..),
  Lit (..),
  Pat (..),
  expTy', expTy,
 )
import Language.PureScript.CoreFn.Convert.Monomorphize.Utils
    ( Monomorphizer,
      freshUnique,
      findInlineDeclGroup,
      isBuiltin,
      isConstructor,
      unBVar,
      mapBind,
      viaExp,
      allBoundVars,
      deepMapMaybeBound, traverseBind, viaExpM, foldBinds, transformTypesInExp, asExp )
import Language.PureScript.CoreFn.Expr (PurusType)
import Language.PureScript.CoreFn.FromJSON ()
import Language.PureScript.CoreFn.TypeLike (
  TypeLike (..),
 )
import Language.PureScript.Names (Ident (..), Qualified (..), QualifiedBy (..))
import Language.PureScript.PSString (PSString)

import Control.Applicative (Alternative ((<|>)))
import Control.Lens.Operators ((^..))
import Control.Lens.Plated (cosmos, transform)
import Control.Monad.Reader ( join, foldM, asks )
import Data.Bifunctor (Bifunctor (bimap, first))
import Data.Foldable (foldl', toList)
import Data.Map (Map)
import Data.Maybe ( catMaybes, mapMaybe )
import Data.Set (Set)
import Data.Set qualified as S
import Language.PureScript.CoreFn.Convert.Debug
    ( doTraceM, prettify )
import Language.PureScript.CoreFn.Pretty.Common (prettyAsStr, (<::>))
import Prettyprinter ( Pretty(pretty), align, vcat, hardline, indent, (<+>) )
import Control.Monad (void)
import Language.PureScript.Types (Type(..))

{-# OPTIONS_GHC -Wno-orphans #-} -- has to be here (more or less)
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DeriveTraversable, DeriveAnyClass  #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Language.PureScript.CoreFn.Convert.ToPIR where

import Prelude
import Language.PureScript.Names (Ident(..), Qualified (..), QualifiedBy (..), ProperNameType (..), ProperName(..), disqualify, runModuleName, showIdent, runIdent)
import Language.PureScript.Types
    ( SkolemScope, TypeVarVisibility )
import Language.PureScript.CoreFn.FromJSON ()
import Data.Text qualified as T
import Language.PureScript.PSString (PSString, prettyPrintString, decodeStringWithReplacement)
import Data.Text (Text)
import Bound
import Data.Kind qualified as GHC
import Control.Monad
import Data.Functor.Classes
import Data.Bifunctor (Bifunctor(bimap, first))
import Data.Maybe (fromJust)
import Text.Show.Deriving
import Prettyprinter
import Language.PureScript.Constants.Prim qualified as C
import Prettyprinter.Render.Text ( renderStrict )
import Data.Map (Map)
import Language.PureScript.CoreFn -- mainly for the module (we might need it for constructors? idk)
import Language.PureScript.CoreFn.Convert.IR
import PlutusIR
import PlutusIR.Core
import PlutusCore.Default
import Control.Monad.State
import PlutusCore (NamedDeBruijn)
import Language.PureScript.Constants.PLC
import Data.Map qualified as M
import Language.PureScript.CoreFn.Desugar.Utils (showIdent')
import PlutusIR.MkPir hiding (error)

data PIRConvertError = PIRConvertError String -- TODO: Refine later

type PIRTerm = Term TyName Name DefaultUni DefaultFun ()

-- TODO: Real monad stack w/ errors and shit
toPIR :: Exp FVar -> State Int PIRTerm
toPIR = \case
  V (FVar _ ident) -> case M.lookup (showIdent' ident) defaultFunMap of
    Just aBuiltin -> pure $ Builtin () aBuiltin
    Nothing -> error $ showIdent' ident <> " isn't a builtin, and it shouldn't be possible to have a free variable that's anything but a builtin"
  LitE _ lit -> litToPIR lit
  CtorE ty tn cn _ -> undefined


litToPIR :: Lit (Exp FVar) -> State Int PIRTerm
litToPIR = \case
    IntL i -> pure $ mkConstant () i
    NumL d -> error "TODO: Doubles"
    StringL str -> -- REVIEW/TODO/FIXME: This is *TOTALLY* wrong
      pure . mkConstant () $ prettyPrintString str
    CharL c -> -- REVIEW/TODO/FIXME: Is this what we want?
      pure
      . mkConstant ()
      . toInteger
      . fromEnum
      $ c
    BoolL b -> pure $ mkConstant () b
    ArrayL arr -> error $ "Too complicated for 5am, do tomorrow"
                          <> " Make sure to check for nested array lits (also check that everywhere else)"

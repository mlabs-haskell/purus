{-# LANGUAGE TemplateHaskell #-}

module Language.Purus.Types where

import Prelude

import Data.Map (Map)
import Data.Text (Text)

import Language.PureScript.Names

import PlutusIR qualified as PIR
import PlutusCore qualified as PLC

import Control.Lens.TH 

type PIRDatatype =
  PIR.Datatype
    PIR.TyName
    PIR.Name
    PLC.DefaultUni
    ()

type PIRType = PIR.Type PIR.TyName PLC.DefaultUni ()

type PIRTerm = PIR.Term PIR.TyName PIR.Name PLC.DefaultUni PLC.DefaultFun ()

data DatatypeDictionary = DatatypeDictionary
  { _pirDatatypes :: Map (Qualified (ProperName 'TypeName)) PIRDatatype
  -- ^ The datatype declarations & their corresponding PS type name
  , _constrNames :: Map (Qualified Ident) (PIR.Name, Int)
  -- ^ Map from PS Constructors (free variables) to PLC Names (w/ a unique) & constructor indices
  , _tyNames :: Map (Qualified (ProperName 'TypeName)) PIR.TyName
  -- ^ Map from PS Type names to PLC Type Names (w/ a unique)
  , _tyVars :: Map Text PIR.TyName
  -- ^ Locally bound type variables, to be used with `withLocalTyVars`
  , _vars :: Map Text PIR.Name
  -- ^ Locally bound variables. This is only used when we need to introduce new variables during conversion
  , _destructors :: Map (Qualified (ProperName 'TypeName)) PIR.Name
  -- ^ Map from a PS type name to the name of the case destructor
  }

-- jfc why didn't i makeLenses everywhere
makeLenses ''DatatypeDictionary

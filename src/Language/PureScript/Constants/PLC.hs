{-# LANGUAGE TemplateHaskell, QuasiQuotes, TemplateHaskellQuotes #-}
module Language.PureScript.Constants.PLC where

import Language.PureScript.Constants.Purus
import PlutusCore.Default
import Language.PureScript.Constants.PLC.TH

mkBuiltinMap ''DefaultFun

{-# LANGUAGE TemplateHaskell, QuasiQuotes, TemplateHaskellQuotes #-}
module Language.PureScript.Constants.PLC where

import PlutusCore.Default
import Language.PureScript.Constants.PLC.TH

mkBuiltinMap ''DefaultFun

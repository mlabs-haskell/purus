{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

module Language.PureScript.Constants.PLC where

import Language.PureScript.Constants.PLC.TH ( mkBuiltinMap )
import PlutusCore.Default ( DefaultFun )

mkBuiltinMap ''DefaultFun

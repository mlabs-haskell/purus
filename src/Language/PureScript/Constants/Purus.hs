{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TemplateHaskell #-}

module Language.PureScript.Constants.Purus where

import Data.Foldable (traverse_)
import Language.PureScript.Constants.PLC.TH
import Language.PureScript.Constants.TH qualified as TH
import PlutusCore.Default (DefaultFun)
import Prelude

$( ctorBaseNames ''DefaultFun >>= \builtins -> TH.declare $ do
    TH.mod "Builtin" do
      -- We can't really TH in the primitive types of DefaultUni
      -- (and anyway some of those types don't need representation
      -- here, b/c they correspond to Prim types), so we define them
      -- here.
      -- NOTE: Integer/ByteString/Text/Unit/Bool correspond to their Prim types, this is everything else
      TH.ty "BuiltinData" -- Opaque Plutus Data *Kind* (nullary)
      TH.ty "BuiltinPair"
      TH.ty "BuiltinList"
      TH.ty "BuiltinByteString"
      TH.ty "BuiltinUnit"
      TH.ty "BuiltinElementG1"
      TH.ty "BuiltinElementG2"
      TH.ty "BuiltinMlResult"

      -- We'll need this sooner or later
      TH.ty "AsData"

      -- Generates primitives from all the builtins
      TH.asIdent $ traverse_ TH.var builtins
 )

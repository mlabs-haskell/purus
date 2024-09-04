-- | The data type of compiler options
module Language.PureScript.Options where

import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set qualified as S
import Prelude

-- | The data type of compiler options
data Options = Options
  { optionsVerboseErrors :: Bool
  -- ^ Verbose error message
  , optionsNoComments :: Bool
  -- ^ Remove the comments from the generated js
  , optionsCodegenTargets :: S.Set CodegenTarget
  -- ^ Codegen targets (JS, CoreFn, etc.)
  }
  deriving (Show)

-- Default make options
defaultOptions :: Options
defaultOptions = Options False False (S.singleton CoreFn)

data CodegenTarget
  = Docs
  | CoreFn
  | {- N.B. We need a compilation mode that tests for changes from existing serialized CoreFn.
            This is the easiest way to implement that (though maybe we should do something else for the final version)
    -}
    CheckCoreFn
  deriving (Eq, Ord, Show)

codegenTargets :: Map String CodegenTarget
codegenTargets =
  Map.fromList
    [ ("coreFn", CoreFn)
    , ("checkCoreFn", CheckCoreFn)
    , -- , ("corefn", CoreFn)
      ("docs", Docs)
    ]

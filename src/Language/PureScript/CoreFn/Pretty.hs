module Language.PureScript.CoreFn.Pretty (
  module PRETTY,
  ppType,
  smartRender,
  writeModule,
  prettyModuleTxt,
  prettyModuleStr,
  renderExpr,
  renderExprStr,
  prettyTypeStr
) where

import Prelude hiding ((<>))

import Data.Text (Text)
import Data.Text qualified as T

import System.IO (Handle)

import Language.PureScript.CoreFn.Expr
    ( Expr(..), Bind )
import Language.PureScript.Types (Type (..))
import Language.PureScript.CoreFn.Module (Module)

import Language.PureScript.CoreFn.Pretty.Common as PRETTY
import Language.PureScript.CoreFn.Pretty.Expr as PRETTY
import Language.PureScript.CoreFn.Pretty.Types as PRETTY

import Prettyprinter
    ( Pretty (pretty),
      layoutSmart,
      defaultLayoutOptions,
      layoutPretty,
      Doc )
import Prettyprinter.Render.Text ( renderIO, renderStrict )


{- Rewritten prettyprinter that uses a modern printer library & is less convoluted.

   We primarily need this for writing the "prettified" CoreFn files for development purposes.
   The existing printer is extremely difficult to modify for our needs (e.g. there isn't a clear way to force
   an expression or type to print on one line). Because reading the CoreFn output is necessary
   to ensure correctness, it's important that we get get something legible.
-}


-- TODO: Remove
ppType :: Show a => Int -> Type a -> String
ppType _ t = prettyTypeStr t


-- TODO (maybe): It wouldn't be too hard to determine the terminal width and write a
--               display function that prints correctly-formatted-for-the-size
smartRender ::  Doc ann -> Text
smartRender = renderStrict . layoutPretty defaultLayoutOptions

writeModule :: Handle -> Module (Bind a) a -> IO ()
writeModule h m = renderIO h
                . layoutSmart defaultLayoutOptions
                $ prettyModule m

prettyModuleTxt :: Module (Bind a) a -> Text
prettyModuleTxt = renderStrict  . layoutPretty defaultLayoutOptions .  prettyModule

prettyModuleStr :: Module (Bind a) a -> String
prettyModuleStr = T.unpack . prettyModuleTxt

renderExpr :: Expr a -> Text
renderExpr = smartRender . asDynamic prettyValue

renderExprStr :: Expr a -> String
renderExprStr = T.unpack . renderExpr

prettyTypeStr :: forall a. Show a => Type a -> String
prettyTypeStr = T.unpack . smartRender . asOneLine prettyType


{- TYPES (move later) -}

-- TODO: Move
instance Show a => Pretty (Type a) where
  pretty t = pretty $ prettyTypeStr t

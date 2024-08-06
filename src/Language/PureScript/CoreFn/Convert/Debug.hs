module Language.PureScript.CoreFn.Convert.Debug where

import Control.Monad (when)
import Data.Set qualified as S
import Debug.Trace
import Prelude

{- Debug machinery for everything *after* CoreFn conversion.

-}

type TraceCfg = S.Set String

traceCfg :: TraceCfg
traceCfg = S.fromList ["getDeep", "updateAllBinds", "lift", "desugarCore", "addTypeAbstractions"]

doTrace :: forall x. String -> String -> x -> x
doTrace identifier msg x =
  if identifier `S.member` traceCfg
    then trace prefixed x
    else x
  where
    prefixed = mkMsg identifier msg

doTraceM ::
  forall f.
  (Applicative f) =>
  String ->
  String ->
  f ()
doTraceM identifier msg =
  Control.Monad.when (identifier `S.member` traceCfg) $ traceM prefixed
  where
    prefixed = mkMsg identifier msg

wrapTraceM :: forall m a. (Monad m) => String -> String -> m a -> m a
wrapTraceM identifier msg act = do
  when (identifier `S.member` traceCfg) $ traceM startMsg
  res <- act
  when (identifier `S.member` traceCfg) $ traceM endMsg
  pure res
  where
    padding = replicate 10 '='
    pad str = padding <> str <> padding
    startMsg = pad $ "BEGIN " <> msg
    endMsg = pad $ "END " <> msg

prettify :: [String] -> String
prettify = concatMap (\x -> x <> "\n\n")

mkMsg :: String -> String -> String
mkMsg header body =
  spacer
    <> header
    <> spacer
    <> "\n"
    <> body
    <> "\n"
    <> spacer
    <> spacer
    <> "\n\n"
  where
    spacer = replicate 20 '-'

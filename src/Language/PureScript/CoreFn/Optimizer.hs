module Language.PureScript.CoreFn.Optimizer (optimizeCoreFn) where

import Protolude hiding (Type, moduleName)

import Control.Monad.Supply (Supply)
import Language.PureScript.Constants.Libs qualified as C
import Language.PureScript.CoreFn.Ann (Ann)
import Language.PureScript.CoreFn.CSE (optimizeCommonSubexpressions)
import Language.PureScript.CoreFn.Expr (Bind, Expr (..))
import Language.PureScript.CoreFn.Module (Module (..))
import Language.PureScript.CoreFn.Traversals (everywhereOnValues)

{- |
CoreFn optimization pass.
-}
optimizeCoreFn :: Module (Bind Ann) k t Ann -> Supply (Module (Bind Ann) k t Ann)
optimizeCoreFn m = fmap (\md -> m {moduleDecls = md}) . optimizeCommonSubexpressions (moduleName m) . optimizeModuleDecls $ moduleDecls m

optimizeModuleDecls :: [Bind Ann] -> [Bind Ann]
optimizeModuleDecls = map transformBinds
  where
    (transformBinds, _, _) = everywhereOnValues identity transformExprs identity
    transformExprs =
      optimizeDataFunctionApply

optimizeDataFunctionApply :: Expr a -> Expr a
optimizeDataFunctionApply e = case e of
  (App a (App _ (Var _ t3 fn) x) y)
    | C.I_functionApply <- fn -> App a x y
    | C.I_functionApplyFlipped <- fn -> App a y x
  _ -> e

module Language.Purus.Docs (toDocDeclarations) where

import Data.Map (Map)
import Data.Map qualified as M
import Language.PureScript.Docs qualified as Docs
import Prelude
import Language.PureScript.Names (
  Ident, 
  Qualified (Qualified), 
  runIdent
  )
import Language.PureScript.CoreFn.Expr (PurusType)
import Data.Functor (void)

toDocDeclarations :: Map (Qualified Ident) PurusType -> [Docs.Declaration] 
toDocDeclarations = M.foldlWithKey' (\acc (Qualified _ ident) ty -> 
  let decInfo = Docs.ValueDeclaration (void ty) 
    in Docs.Declaration (runIdent ident) Nothing Nothing [] decInfo Nothing : acc) []

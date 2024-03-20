{-# OPTIONS_GHC -Wno-orphans #-} -- has to be here (more or less)
module Language.PureScript.CoreFn.Convert.Plated where

import Prelude hiding (error)
import Data.Bifunctor ( Bifunctor(second) )
import Language.PureScript.CoreFn.Expr
    ( Bind(..),
      CaseAlternative(CaseAlternative),
      Expr(..),
      Guard )
import Language.PureScript.CoreFn.Desugar.Utils ( traverseLit )
import GHC.Natural ( Natural )
import Data.Bitraversable (Bitraversable(bitraverse))
import Control.Lens.IndexedPlated ( IndexedPlated(..) )
import Control.Lens ( Indexable(indexed) )

type Depth = Natural

instance IndexedPlated Natural (Expr a) where
  iplate d f = \case
    Literal ann ty lit -> Literal ann ty <$> traverseLit (indexed f d) lit
    Accessor ann ty field e -> Accessor ann ty field <$>  indexed f d e
    ObjectUpdate ann ty orig copyFields updateFields ->
      (\orig' updateFields' -> ObjectUpdate ann ty orig' copyFields updateFields')
      <$> indexed f d orig
      <*> traverse (sequenceA . second (indexed f d)) updateFields
    Abs ann ty ident body -> Abs ann ty ident <$> indexed f (d + 1) body
    App ann ty fE argE -> App ann ty <$> indexed f d fE <*> indexed f d argE
    Case a ty scrutinees alternatives ->
      Case a ty <$> traverse (indexed f d) scrutinees <*> traverseAltE (indexed f d) alternatives
    Let a ty binds e -> Let a ty <$> traverseBinds (indexed f d) binds <*> indexed f d e
    other -> pure other -- ctors and vars don't contain any sub-expressions
   where
     traverseBinds :: forall f. Applicative f => (Expr a -> f (Expr a)) -> [Bind a] -> f [Bind a]
     traverseBinds g binds = traverse go binds
       where
         go :: Bind a -> f (Bind a)
         go = \case
           NonRec ann ident e -> NonRec ann ident <$> g e
           Rec es -> Rec <$> traverse (traverse g) es

     traverseAltE :: forall f. Applicative f => (Expr a -> f (Expr a)) -> [CaseAlternative a] -> f [CaseAlternative a]
     traverseAltE g alts = traverse go alts
       where
         go :: CaseAlternative a -> f (CaseAlternative a)
         go (CaseAlternative binders result) =
           CaseAlternative binders
           <$> helper result -- hellishly complex
         helper :: Either [(Guard a, Expr a)] (Expr a) -> f (Either [(Guard a, Expr a)] (Expr a))
         helper = bitraverse (traverse $  bitraverse g g) g

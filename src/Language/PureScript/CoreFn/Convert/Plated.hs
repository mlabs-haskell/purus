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
import Language.PureScript.Types

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

-- Might be able to do something useful with a non-natural index (think about later)
instance IndexedPlated Natural (Type a) where
  iplate d f = \case
    TypeApp ann t1 t2 -> TypeApp ann <$> indexed f d t1 <*> indexed f d t2
    KindApp ann k1 t1 -> KindApp ann <$> indexed f d k1 <*> indexed f d t1
    ForAll a vis var mbK inner skol ->
      (\mbK' inner' -> ForAll a vis var mbK' inner' skol)
      <$> traverse (indexed f d) mbK
      <*> indexed f d inner
    -- \/ Just for completeness, they shouldn't exist
    ConstrainedType ann c t1 -> ConstrainedType ann c <$> indexed f d t1
    RCons ann lbl t1 t2 -> RCons ann lbl <$> indexed f d t1 <*> indexed f d t2
    KindedType a t1 t2 -> KindedType a <$> indexed f d t1 <*> indexed f d t2
    BinaryNoParensType ann t1 t2 t3 ->
      BinaryNoParensType ann
      <$> indexed f d t1
      <*> indexed f d t2
      <*> indexed f d t3
    ParensInType ann t -> ParensInType ann <$> indexed f d t
    -- nothing else has child types
    other -> pure other

{-# OPTIONS_GHC -Wno-orphans #-} -- has to be here (more or less)
module Language.PureScript.CoreFn.Convert.Plated where

import Prelude hiding (error)
import Data.Bifunctor ( Bifunctor(second) )
import Language.PureScript.CoreFn.Expr
    ( Bind(..),
      CaseAlternative(CaseAlternative),
      Expr(..),
      Guard, exprType )
import Language.PureScript.CoreFn.Desugar.Utils ( traverseLit, showIdent' )
import GHC.Natural ( Natural )
import Data.Bitraversable (Bitraversable(bitraverse))
import Control.Lens.IndexedPlated ( IndexedPlated(..) )
import Control.Lens ( Indexable(indexed) )
import Language.PureScript.Types
import Data.Map (Map)
import qualified Data.Map as M
import Language.PureScript.Names (Ident)
import Data.Text (Text)
import Language.PureScript.CoreFn.Pretty (prettyTypeStr)
import Language.PureScript.Environment (pattern (:->))

type Context = Map Ident SourceType

prettyContext :: Context -> String
prettyContext cxt = concatMap go (M.toList cxt)
  where
    go :: (Ident,SourceType) -> String
    go (ident,ty) = showIdent' ident <> " := " <> prettyTypeStr ty <> "\n"

instance IndexedPlated Context (Expr a) where
  iplate d f = \case
    Literal ann ty lit -> Literal ann ty <$> traverseLit (indexed f d) lit
    Accessor ann ty field e -> Accessor ann ty field <$>  indexed f d e
    ObjectUpdate ann ty orig copyFields updateFields ->
      (\orig' updateFields' -> ObjectUpdate ann ty orig' copyFields updateFields')
      <$> indexed f d orig
      <*> traverse (sequenceA . second (indexed f d)) updateFields
    Abs ann ty ident body -> Abs ann ty ident <$> indexed f (M.insert ident (arg ty) d) body
    App ann ty fE argE -> App ann ty <$> indexed f d fE <*> indexed f d argE
    Case a ty scrutinees alternatives ->
      Case a ty <$> traverse (indexed f d) scrutinees <*> traverseAltE d f alternatives
    Let a ty binds e ->
      Let a ty <$> traverseBinds d f  binds <*> indexed f d e
    other -> pure other -- ctors and vars don't contain any sub-expressions
   where
     arg = \case
       ForAll _ _ _ _ inner _ -> arg inner
       a :-> _ -> a
       other -> other

     traverseBinds :: forall p f. (Indexable Context p, Applicative f) => Context -> p (Expr a) (f (Expr a)) -> [Bind a] -> f [Bind a]
     traverseBinds cxt g binds = traverse (go cxt) binds
       where
         go :: Context -> Bind a -> f (Bind a)
         go gCxt = \case
           NonRec ann ident e ->
             let cxt' = M.insert ident (exprType e) gCxt
             in NonRec ann ident <$> indexed g cxt' e
           Rec es -> Rec <$> goRecursive gCxt es
         goRecursive ::  Context -> [((a, Ident), Expr a)] -> f [((a, Ident), Expr a)]
         goRecursive _ [] = pure []
         goRecursive gCxt (((ann,nm),e):rest) =
           let gCxt' = M.insert nm (exprType e) gCxt
           in (\x xs -> ((ann,nm),x):xs) <$> indexed g gCxt' e <*> goRecursive gCxt' rest
     traverseAltE :: forall p f. (Indexable Context p, Applicative f) => Context -> p (Expr a) (f (Expr a)) -> [CaseAlternative a] -> f [CaseAlternative a]
     traverseAltE cxt g alts = traverse (go cxt) alts
       where
         go :: Context -> CaseAlternative a -> f (CaseAlternative a)
         go gCxt (CaseAlternative binders result) =
           CaseAlternative binders
           <$> helper gCxt result -- hellishly complex
         helper :: Context -> Either [(Guard a, Expr a)] (Expr a) -> f (Either [(Guard a, Expr a)] (Expr a))
         helper gCxt = \case
           Right e -> Right <$> indexed g gCxt e
           Left es ->
             let g' = indexed g gCxt
             in  Left <$> traverse (bitraverse g' g') es

-- Bound tyVars and kinds. Idk if we'll use it but it take like 10 seconds
type TyContext = Map Text (Maybe SourceType )
-- Might be able to do something useful with a non-natural index (think about later)
instance IndexedPlated TyContext SourceType where
  iplate d f = \case
    TypeApp ann t1 t2 -> TypeApp ann <$> indexed f d t1 <*> indexed f d t2
    KindApp ann k1 t1 -> KindApp ann <$> indexed f d k1 <*> indexed f d t1
    ForAll a vis var mbK inner skol ->
      let cxt = M.insert var mbK d
      in (\mbK' inner' -> ForAll a vis var mbK' inner' skol)
          <$> traverse (indexed f d) mbK
          <*> indexed f cxt inner
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

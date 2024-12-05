module Language.Purus.Pipeline.EliminateCases.Utils where

import Prelude

import Data.Text qualified as T


import Language.PureScript.CoreFn.Module
    ( getAllConstructorDecls,
      cdCtorName,
      properToIdent,
      Datatypes )
import Language.PureScript.Names (
    Ident (..),
    ModuleName (..),
    ProperName (..),
    ProperNameType (..),
    Qualified (..),
    QualifiedBy (..),
 )
import Language.Purus.IR (
    Exp (..),
    FVar (FVar),
    Kind,
    Pat (..),
    Ty (..),
    analyzeApp
 )

import Bound (Var (..))
import Control.Lens (
    view,
 )

import Control.Monad.State
    ( StateT,
      MonadState(get),
      MonadTrans(lift),
      evalStateT )
import Data.Matrix
    ( getCol, Matrix(ncols) )
import Data.Tree ( Tree(..), drawForest, drawTree)
import Data.Vector qualified as V

import Data.Bifunctor (Bifunctor (first))
import Data.Foldable.WithIndex (ifind)
import Witherable (imapMaybe)
import Language.Purus.Pretty.Common (prettyStr)

import Prettyprinter
    ( Pretty(..) )
import Language.Purus.Pipeline.EliminateCases.Types
    ( getContent,
      ConstraintContent(..),
      CtorArgPos(CtorArgPos),
      CtorIx(..),
      Expression,
      Identifier(PSVarData),
      Pattern,
      PatternConstraint(..),
      Position(ConstructorArgPos) )

{- Generic Tree Utilities -}
mkTree :: forall (a :: *). [a] -> Maybe (Tree a)
mkTree = \case
    [] -> Nothing
    (x : xs) -> pure $ go x xs
    where
        go :: a -> [a] -> Tree a
        go y ys = Node y $ case ys of
            [] -> []
            (z : zs) -> [go z zs]

ppTree :: Pretty a => Tree a -> String
ppTree = drawTree . fmap prettyStr

ppForest :: Pretty a => [Tree a] -> String
ppForest = drawForest . fmap (fmap prettyStr)

peel :: forall (a :: *). [Tree a] -> [(a, [Tree a])]
peel = map unTree
    where
        unTree :: Tree a -> (a, [Tree a])
        unTree (Node x children) = (x, children)

treeSnoc :: Tree a -> Tree a -> Tree a
treeSnoc (Node x []) new = Node x [new]
treeSnoc (Node x xs) new = Node x $ (`treeSnoc` new) <$> xs

linearConcat :: [Tree a] -> Tree a
linearConcat [Node x xs] = Node x xs
linearConcat (Node x xs:rest) = treeSnoc (linearConcat rest) (Node x xs)
linearConcat [] = error "linear concat: empty list"

{- Specialized Tree Utility

   NOTE: GENERALLY UNSAFE but fine where we use it.
         Have to call this on linear (listlike) trees, e.g. the output for mkForest
         or it will give you something grotesquely incorrect
-}
unTreePatterns :: Tree PatternConstraint -> [PatternConstraint]
unTreePatterns (Node x []) = [x]
unTreePatterns (Node x xs) = x : concatMap unTreePatterns xs -- N.B. there will only ever be one element of xs if we use this in the right place



{- Matrix Utility. It's like... fold? foldMap? Kind of. Don't think there's an existing function anywhere that does this. -}

squishColumnsWith :: forall a. Matrix a -> ([a] -> a) -> [a]
squishColumnsWith mtx f = map (\cx -> f (V.toList $ getCol cx mtx)) [1..(ncols mtx)]

{- Generic Monad Utility -}

-- REVIEW: Is this right?
locally :: (Monad m) => StateT s m a -> StateT s m a
locally act = do
    s <- get
    let result = evalStateT act s
    lift result


{- Utilities for convertion IR Pats to Constraints -}

getCtorIx ::
    Datatypes Kind Ty ->
    Qualified (ProperName 'TypeName) ->
    Qualified (ProperName 'ConstructorName) ->
    Maybe CtorIx
getCtorIx datatypes tn cn = do
    let xs = view cdCtorName <$> getAllConstructorDecls tn datatypes
    (index, _) <- ifind (\_ x -> x == (properToIdent <$> cn)) xs
    pure $ CtorIx index

toConstraint ::
    Datatypes Kind Ty ->
    Position ->
    Pattern ->
    Maybe PatternConstraint
toConstraint datatypes pos pat = case pat of
    VarP nm indx ty -> pure $ pos :@ VarC (PSVarData nm indx ty)
    WildP -> pure $ pos :@ WildC
    LitP lit -> pure $ pos :@ LitC lit
    ConP tn cn ps -> do
        ctorIndex <- getCtorIx datatypes tn cn
        let ps' =
                imapMaybe
                    ( \i x -> do
                        let pos' = ConstructorArgPos tn pos ctorIndex (CtorArgPos i)
                        toConstraint datatypes pos' x
                    )
                    ps
        pure $ pos :@ Constructor tn ctorIndex ps'

{- Predicates -}

irrefutable :: ConstraintContent -> Bool
irrefutable = \case
  VarC _ -> True
  WildC -> True
  _ -> False

isTupleTyName :: Qualified (ProperName 'TypeName) -> Bool
isTupleTyName = \case
    Qualified (ByModuleName (ModuleName "Prim")) (ProperName tNm) -> T.isPrefixOf "Tuple" tNm
    _ -> False

isIrrefutablePat :: Pattern -> Bool
isIrrefutablePat = \case
    WildP -> True
    VarP {} -> True
    _ -> False

-- We don't have objects here so we can ignore them
isBadNestedPat :: Pattern -> Bool
isBadNestedPat = \case
    ConP _ _ ps -> (not (all isIrrefutablePat ps))
    _ -> False

isVarC :: PatternConstraint -> Bool
isVarC (getContent -> c) = case c of
    VarC {} -> True
    _ -> False


{- Tuple "Cracking", for breaking open tupled multi-scrutinee case expression -}

crackTuplePat :: Pattern -> Maybe [Pattern]
crackTuplePat = \case
    ConP qTn _ inner | isTupleTyName qTn -> Just inner
    _ -> Nothing

{- In the case we care about, i.e. where a tuple pattern originated in a
   multi-scrutinee case expression, the scrutinee must be a "literal"
   Tuple Constructor. Nothing else should be possible, which is good because
   the analysis we'd have to do to recover the original tuple arguments for a
   scrutinee w/o a "literal" Tuple would be grotesquely complex and annoying
-}
crackTupleExp :: Expression -> Maybe [Expression]
crackTupleExp e = case first stripSuperfluous <$> analyzeApp e of
    Just (V (F (FVar _ (Qualified (ByModuleName (ModuleName "Prim")) (Ident idnt)))), args)
        | T.isPrefixOf "Tuple" idnt -> Just args
    _ -> Nothing
 where
   stripSuperfluous :: Expression -> Expression
   stripSuperfluous = \case
     TyInstE _ ex -> stripSuperfluous ex
     TyAbs _ ex -> stripSuperfluous ex
     other -> other





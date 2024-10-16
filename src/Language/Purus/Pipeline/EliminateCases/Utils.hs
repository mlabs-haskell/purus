{-# LANGUAGE TypeApplications, TemplateHaskell #-}
module Language.Purus.Pipeline.EliminateCases.Utils where

import Prelude

import Data.Map qualified as M

import Data.Text qualified as T

import Data.Bifunctor (second)
import Data.Foldable (foldl', foldrM)
import Data.List (sortOn, find)
import Data.Maybe (fromJust)
import Data.Traversable (for)

import Language.PureScript.Constants.Prim qualified as C
import Language.PureScript.Constants.Purus qualified as C
import Language.PureScript.CoreFn.Module (
  CtorDecl (..),
  Datatypes,
  cdCtorFields,
  cdCtorName,
  dDataArgs,
  dDataCtors,
  getAllConstructorDecls,
  getConstructorIndexAndDecl,
  lookupCtorType,
  lookupDataDecl,
  tyDict, properToIdent,
 )
import Language.PureScript.CoreFn.TypeLike (
  TypeLike (
    applyType,
    funTy,
    instTy,
    quantify,
    replaceAllTypeVars,
    splitFunTyParts
  ),
  getAllInstantiations,
  getInstantiations,
  safeFunArgTypes,
 )
import Language.PureScript.Names (
  Ident (..),
  ProperName (..),
  ProperNameType (..),
  Qualified (..),
  runIdent,
  showQualified, ModuleName (..), QualifiedBy (..),
 )
import Language.PureScript.Types (
  TypeVarVisibility (TypeVarVisible),
 )

import Language.Purus.Debug (doTrace, doTraceM, prettify)
import Language.Purus.IR (
  Alt (..),
  BVar (BVar),
  BindE (..),
  Exp (..),
  FVar (FVar),
  Lit (..),
  Pat (..),
  Ty (..),
  expTy,
  expTy',
  getPat,
  unsafeAnalyzeApp,
  pattern (:~>), analyzeApp, Kind,
 )
import Language.Purus.IR qualified as IR
import Language.Purus.IR.Utils (
  Vars,
  WithoutObjects,
  fromExp,
  isConstructor,
  toExp,
 )
import Language.Purus.Pipeline.DesugarCore (
  matchVarLamAbs,
 )
import Language.Purus.Pipeline.GenerateDatatypes.Utils (
  analyzeTyApp,
  foldr1Err,
  freshName,
  funResultTy,
  getDestructorTy,
  prettyQI,
  prettyQPN,
 )
import Language.Purus.Pipeline.Monad (
  MonadCounter (next),
  PlutusContext,
 )
import Language.Purus.Pretty.Common (prettyStr)

import Bound (Var (..))
import Bound.Scope (
  Scope,
  abstract,
  instantiate,
  mapBound,
  toScope,
 )
import Control.Lens (
  at,
  view,
  (^.),
  _2,
 )
import Control.Lens.Combinators (transform, over)
import Control.Lens.TH (makeLenses)
import Control.Monad.Except (
  MonadError (throwError),
 )

import Data.Matrix
import Text.Read (readMaybe)
import Data.Map (Map)
import Data.Void (Void)
import Control.Monad.State
import Data.Vector (Vector)
import Data.Vector qualified as V
import Control.Monad.Reader
import Data.Maybe (catMaybes)

-- TODO: Delete this eventually, just want it now to sketch things
type  Pattern = Pat WithoutObjects Ty (Exp WithoutObjects Ty) (Var (BVar Ty) (FVar Ty))
type Expression = Exp WithoutObjects Ty (Vars Ty)
-- If you look at the types, this can only represent "true literals", i.e., is un-nested 
type Literal = Lit WithoutObjects Pattern 

{- We need to handle things differently in a case where we have a tuple pattern
   vs where we just have singular patterns.

-}
mkPatternMatrix :: [Alt WithoutObjects Ty (Exp WithoutObjects Ty) (Vars Ty)]
                -> Maybe (Matrix Pattern)
mkPatternMatrix alts = fromLists <$> pats
  where
    pats = traverse (crackTuplePat . getPat) alts


mkResults :: Datatypes Kind Ty -> Matrix Pattern -> Results
mkResults datatypes mtx = Results $ go (nrows mtx - 1)
  where
    go :: Int -> Map ResultRowIx [ResultConstraint]
    go rowIx
      | rowIx < 0 = M.empty
      | otherwise = M.singleton (ResultRowIx rowIx) cpatRow <> go (rowIx - 1)
      where
        getCtorIx :: Qualified (ProperName 'TypeName)
                  -> Qualified (ProperName 'ConstructorName)
                  -> CtorIx
        getCtorIx tn cn = fst
                          . fromJust
                          $ find (\x -> snd x == (properToIdent <$> cn))
                                 (zip (CtorIx <$> [0..])
                                      $ view cdCtorName <$> getAllConstructorDecls tn datatypes)

        thisRow =  zip [0..] . V.toList $ getRow rowIx mtx
        cpatRow = concatMap toConstraints thisRow
        toConstraints :: (Int,Pattern)
                      ->  [ResultConstraint]
        toConstraints (col,pat) = case pat of
          VarP nm indx ty -> [ResultConstraint mtxPos (VarC nm indx ty)]
          WildP -> [ResultConstraint mtxPos WildC]
          LitP lit -> [ResultConstraint mtxPos (LitC lit)]
          ConP tn cn [] ->
            let ctorIndex :: CtorIx
                ctorIndex = getCtorIx tn cn -- todo: implement
            in [ResultConstraint mtxPos (BareConstructorIndex tn ctorIndex)]
          -- REVIEW: Maybe we should just generate a BareConstructorIndex constraint if all of the arguments are wildcards?
          ConP tn cn ps ->
            let ctorIndex :: CtorIx
                ctorIndex = getCtorIx tn cn
            in concatMap (goNested mtxPos ctorIndex) $ zip (CtorArgPos  <$> [0..]) ps
         where
           mtxPos = M (rowIx,col)
        -- N.b. there's probably a better way to write this
        goNested :: Position -- "outer" position in the matrix
                 -> CtorIx  -- index of the constructor we're looking at
                 -> (CtorArgPos, Pattern) -- position of the argument in the pattern list
                 -> [ResultConstraint]
        goNested mtxPos _ctorIx (argPos,pat) = case pat of
          VarP nm indx ty -> [ResultConstraint here (VarC nm indx ty)]
          WildP -> [ResultConstraint here WildC]
          LitP lit -> [ResultConstraint here (LitC lit)]
          ConP tn cn [] ->
            let ctorIndex :: CtorIx
                ctorIndex = getCtorIx tn cn
            in [ResultConstraint here (BareConstructorIndex tn ctorIndex)]
          ConP tn cn ps ->
            let ctorIndex :: CtorIx
                ctorIndex = getCtorIx tn cn
            in concatMap (goNested here ctorIndex) $ zip (CtorArgPos <$> [0..]) ps

         where
           here = ConstructorArgPos mtxPos _ctorIx argPos


      

tupleNumber :: Qualified (ProperName 'TypeName) -> Maybe Int
tupleNumber = \case
  Qualified (ByModuleName (ModuleName "Prim")) (ProperName tNm)
   | T.isPrefixOf "Tuple" tNm -> readMaybe @Int =<< T.unpack <$> (T.stripPrefix "Tuple" $ tNm)
  _ -> Nothing

isTupleTyName :: Qualified (ProperName 'TypeName) -> Bool
isTupleTyName = \case
  Qualified (ByModuleName (ModuleName "Prim")) (ProperName tNm) -> T.isPrefixOf "Tuple" tNm
  _ -> False

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
crackTupleExp e = case analyzeApp e of
  Just (V (F (FVar a (Qualified (ByModuleName (ModuleName "Prim")) (Ident idnt)))),args)
    | T.isPrefixOf "Tuple" idnt -> Just args
  _ -> Nothing

-- Is it a *literal* tuple (i.e. not: Is it a var with a tuple type or the result of a function call or...)
isTupleExp :: Expression -> Bool
isTupleExp e = case analyzeApp e of
  Just (V (F (FVar a (Qualified (ByModuleName (ModuleName "Prim")) (Ident idnt)))),args)
    | T.isPrefixOf "Tuple" idnt -> True
  _ -> False

isIrrefutablePat :: Pattern -> Bool
isIrrefutablePat = \case
  WildP -> True
  VarP{} -> True
  _ -> False

-- We don't have objects here so we can ignore them
isBadNestedPat :: Pattern -> Bool
isBadNestedPat = \case
  ConP _ _ ps -> all isIrrefutablePat ps
  _           -> False


{-

A position is either:

 - An explicit location in the matrix
 - A constructor position (index & arg position) at a position 


-}

newtype CtorArgPos = CtorArgPos Int deriving (Show, Eq)
newtype CtorIx     = CtorIx Int deriving (Show, Eq)
-- The LHS of a constraint, uniquely identifies the position of a pattern in the pattern matrix
data Position
  = -- A top-level position in the matrix, (row,column)
    M (Int,Int)
    -- A constructor argument local-position (index, argument position) indexed to a position in the matrix
    -- (can nest these but the structure of the type should ensure that we always end up with a matrix position root)
  | ConstructorArgPos Position CtorIx CtorArgPos
  deriving (Show, Eq)


-- This gives us the location of the "cell" in the pattern matrix where a
-- constraint applies. We need something like this because when we "collapse" the
-- set of paths to a result into a tree, we need *all* of the constraints that pertain to a cell
matrixPosition :: Position -> (Int,Int)
matrixPosition = \case
  M pos -> pos
  ConstructorArgPos inner _ _ -> matrixPosition inner

constraintMatrixPosition :: ResultConstraint -> (Int,Int)
constraintMatrixPosition (ResultConstraint pos _) = matrixPosition pos

getRelevantConstraintsWithResults :: (Int,Int) -> [[ResultConstraint]] -> [[ResultConstraint]]
getRelevantConstraintsWithResults pos rows = filter (\x -> constraintMatrixPosition x == pos) <$> rows





{- With the new indexing scheme, constructor patterns are superfluous on the RHS side of a constraint. The only things the RHS of a constraint can contain are:
     1. A Variable Pattern
     2. A wildcard pattern
     3. A Literal pattern

   The "constructor part" of a pattern constraint goes on the LHS because it tells us something about the position/location in the matrix
   (which seems trivial but makes reasoning about this a LOT easier).

   Well, mostly. That scheme doesn't work for bare constructors, e.g. `Nothing. For bare constructors, we
   use a RHS constraint that indicates the constructor index, so really:
     4. A bare constructor index 
-}

data ConstraintPattern
  = VarC Ident Int Ty -- we'll ignore the type but will keep it around so that we can reconstruct the expression without having to re-deduce types 
  | WildC
  | LitC Literal
  | BareConstructorIndex (Qualified (ProperName 'TypeName)) CtorIx
  
{- A constraint on a result. 
-}
data ResultConstraint = ResultConstraint Position ConstraintPattern

newtype ResultRowIx = ResultRowIx {getResultRow :: Int} deriving (Show, Eq, Ord)
newtype Results = Results {getResults :: Map ResultRowIx [ResultConstraint] }



needsTransform :: Expression -> Bool
needsTransform = \case
  CaseE _resTy scrut alts -> isTupleExp scrut
      && any isBadNestedPat (getPat <$> alts)
  _ -> False

note :: String -> Maybe a -> PlutusContext a
note str Nothing = throwError str
note _ (Just x) = pure x



{- General outline of procedure:

We start with a [[ResultConstraint]], which represent the possible paths along a tree from the scrutinee expressions to
each Result. We assume that these are ordered, i.e. that later lists in the list-of-lists represent subsequent alternatives & so on.

The general idea is that we need to "collapse" those lists into a forest (a list of trees) such that each branch in each tree
represents a different path to a distinct result.

We treat distinct variable constraints as non-overlapping, e.g. in `1, Just a -> ... /2, Just b -> ..`, `a` and `b` require their own
branches.

Wildcards require special handling. In the following example:


A := [ M(0,0) ~ 1, M(0,1) @ C(0,0) ~ a]
B := [ M(0,0) ~ _, M(0,1) @ C(0,0) ~ b]
C := [ M(0,0) ~ c, M(0,1) ~ I(1)]

The B branch has a wildcard constraint at (0,0) and the C branch has a `c` Variable constraint at the same position.

If we consider only the sets of "covered inputs", `_` and `c` cover the same sets In the B branch, nothing is bound at (0,0),
so we can justifiably collapse B & C into a tree where we only mention `c`:

|- M(0,0) ~ c
  |-? [M(0,1) @ C(0,0) ~ b]
  |-? [M(0,1) ~ I(1)]

The full expansion of this example prior to tree merging:

M(0,0) ~ 1
  |- M(0,1) @ C(0,0) ~ a
    |- A
M(0,0) ~ c
  |- M(0,1) @ C(0,0) ~ b
    |- B
  | M(0,1) ~ I(1)
    |- C

The `c`/`_` overlap in this example is a special case of a general principle we can use for grouping and collapsing paths:

I: Two constraints with *equivalent coverage* can be collapsed into a tree with a common root.
  I.1: If there is exactly one variable constraint at the position we are examining (e.g. if all but one of the
       constraints are wildcards), we choose the variable constraint.
  I.2: If there are multiple variable constraints, we choose an arbitrary variable. During result insertion, we can keep track of
       results which conform with the path *up to renaming* and re-bind the variables with a `let` expression outside of
       the result.
  I.3: All variables have equivalent coverage with each other (with respect to the same position.)
  I.4: All wildcards have equivalent coverage with each other *and* with all variable constriants.
  I.5: Two literals have equivalent coverage if the literal expression they contain are equivalent
  I.6: Bare constructor constraints have equivalent coverage if they refer to the same constructor.
  I.7: A set of positional constraints for a constructor's arguments has equivalent coverage with respect to
       another constraint or set of constraints if:
         I.7.a: (Single constraint): All of the positional constraints in the set are wildcards and the second constraint is a
                bare constructor constraint with the same index.
         I.7.b: (Constraint set): Every member of the second constraint set shares an index with the index of the first set
                AND each member of the first constraint set has equivalent coverage to the corresponding member in the second
                set (where members correspond if they occur in the same constructor argument position)
                - N.B. this is just a generalization of I.1-I.7.a and is strictly redundant


-}

{-# LANGUAGE TypeApplications, TemplateHaskell #-}
module Language.Purus.Pipeline.EliminateCases.Utils where

import Prelude

import Data.Map qualified as M

import Data.Text qualified as T

import Data.Bifunctor (second)
import Data.Foldable (foldl', foldrM)
import Data.List (sortOn, find, partition)
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
import Data.Tree

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

getResultConstraintPos :: ResultConstraint -> Position
getResultConstraintPos (ResultConstraint pos _) = pos

getResultConstraintPat :: ResultConstraint -> ConstraintPattern
getResultConstraintPat (ResultConstraint _ pat) = pat

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


isIrrefutable :: ConstraintGroup -> Bool
isIrrefutable (SimpleConstraint (ResultConstraint _ p))= case p of
  WildC -> True
  VarC{} -> True
  _ -> False
isIrrefutable _ = False
{- Checks whether two constraints have equivalent coverage. This assumes that each constraint occurs in the same
   *position*.

-}
equivalentCoverage :: ConstraintPattern -> ConstraintPattern -> Bool
equivalentCoverage c1 c2 = case (c1,c2) of
  (VarC _ _ _, VarC _ _ _) -> True
  (VarC _ _ _, WildC)      -> True
  (WildC, VarC _ _ _)      -> True
  (WildC, WildC)           -> True
  (VarC _ _ _, _)          -> False
  (_, VarC _ _ _)          -> False
  (WildC, _)               -> False
  (_, WildC)               -> False 
  (LitC l1, LitC l2)       -> l1 == l2
  (_, LitC _)              -> False
  (LitC _, _)              -> False
  (BareConstructorIndex _ i1, BareConstructorIndex _ i2) -> i1 == i2

equivalentCoverageGroup :: ConstraintGroup -> ConstraintGroup -> Bool
equivalentCoverageGroup (SimpleConstraint rc1) (SimpleConstraint rc2)
  = equivalentCoverage (getResultConstraintPat rc1) (getResultConstraintPat rc2)
equivalentCoverageGroup (SimpleConstraint (ResultConstraint pos1 (BareConstructorIndex _ cix1))) (CtorConstraints pos2 _ cix2 args)
  = pos1 == pos2 && cix1 == cix2 && all isIrrefutable (M.elems args)
equivalentCoverageGroup (CtorConstraints pos2 _ cix2 args)  (SimpleConstraint (ResultConstraint pos1 (BareConstructorIndex _ cix1)))
  = pos1 == pos2 && cix1 == cix2 && all isIrrefutable (M.elems args)
equivalentCoverageGroup (CtorConstraints pos1 _ cix1 args1) (CtorConstraints pos2 _ cix2 args2)
  = pos1 == pos2 && cix1 == cix2 && and (zipWith equivalentCoverageGroup (M.elems args1) (M.elems args2))
equivalentCoverageGroup _ _ = False



{- Description of "collapse" process:

We start with a [[ResultConstraint]] input. The outer and inner lists are ordered such that:
  - Outer List: Follows the order of alternatives
  - Inner List: Left->Right order of scrutinees

We can view this list as representing the set of possible paths to each result (i.e. the thing to the RHS of -> in a case alternative)

Very generally, the process here involes two steps:

Step 1) Grouping: The goal is to produce a forest by merging nodes with equivalent coverage in the same position. (See note
                  below for an explanation of what "equivalent coverage" means here.)

Step 2) Tree merging: The goal is to construct a single tree from the output of Step 1 by recursively inserting subsequent
        (i.e. occurs "later") forests into positions in "prior" forests where they fit. (Where "fit" means: The path up to
        the point where insertion occurs is consistent with constraint at the root of the insertion candidate)


Step 2 is almost trivial and isn't worth explicating in detail here.

For Step 1, grouping should proceed as follows:
  i. We find all constraints at the current position.
    - Technically the matrix doesn't matter anymore here so "position" just refers to the column
    - We probably have to reconstruct positional constraints inside CTors for each row (it's possible to not do that but it makes the implementation much more annoying)
  ii. We group the constraints by equivalence (w/r/t "sets of covered inputs"), keeping track of which
      "remainder" belongs to each group (i.e. which Results' "rest of the path" belong to the group)
  iii. We choose (or construct) a single representation of the equivalence group according to the criteria outlined below
       and insert that as the root of a new subtree
  iv. We recurse over the the "rest of the paths" indexed to each new subtree
-}

data ConstraintGroup
  = SimpleConstraint ResultConstraint
  | CtorConstraints Position (Qualified (ProperName 'TypeName)) CtorIx (Map CtorArgPos ConstraintGroup) -- I think?

isVarC :: ConstraintPattern -> Bool
isVarC = \case
  VarC _ _ _ -> True
  _ -> False

collapse :: (Int,Int) -> [[ResultConstraint]] -> [Tree ConstraintGroup] -- 'Nothing' means 'Done' and is there mainly to keep the fn total
collapse pos [] = []
collapse pos@(r,c) paths = go <$> groups
  where
    groups :: [(ConstraintGroup,[[ResultConstraint]])]
    groups = combine $ splitResults <$> paths
      where
        splitResults :: [ResultConstraint] -> (ConstraintGroup, [ResultConstraint])
        splitResults path = undefined
          where
            (here, rest) = partition (\x -> constraintMatrixPosition x == pos) path
            thisGroup [] = error "empty constraint group"
            thisGroup [x] = case x of
              ResultConstraint xPos innerC -> case innerC of
                WildC -> SimpleConstraint x
                VarC{} -> SimpleConstraint x
                LitC{} -> SimpleConstraint x
                BareConstructorIndex{} -> SimpleConstraint x
            thisGroup xs = mkCtorConstraints xs -- this is annoying to implement but not conceptually hard, do it later

            mkCtorConstraints :: [ResultConstraint] -> ConstraintGroup
            mkCtorConstraints = undefined

        -- super ugly  algorithm but should work
        combine :: [(ConstraintGroup, [ResultConstraint])] -> [(ConstraintGroup, [[ResultConstraint]])]
        combine rg = collapseGroups <$>  goCombine rg
          where
            goCombine :: [(ConstraintGroup, [ResultConstraint])]
                      -> [([ConstraintGroup],[[ResultConstraint]])]
            goCombine [] = []
            goCombine ((cg,rs):rest) =
              let (here,remainder) = partition (equivalentCoverageGroup cg . fst) rest
                  (hereGroups, hereConstraints) = unzip here
                  acc = (cg:hereGroups,rs : hereConstraints)
              in acc : goCombine remainder


    go :: (ConstraintGroup, [[ResultConstraint]]) -> Tree ConstraintGroup
    go (cg,inner) = Node (mkGroupConstraint cg) $ collapse (r,c + 1) inner


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

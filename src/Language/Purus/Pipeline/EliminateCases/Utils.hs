{-# LANGUAGE TypeApplications, TemplateHaskell #-}
module Language.Purus.Pipeline.EliminateCases.Utils (collapse, mkResults) where

import Prelude


import Data.Text qualified as T

import Data.List (find, partition)
import Data.Maybe (fromJust, fromMaybe)
import Data.Traversable (for)

import Language.PureScript.CoreFn.Module (
  Datatypes,
  cdCtorName,
  getAllConstructorDecls, properToIdent,
 )
import Language.PureScript.Names (
  Ident (..),
  ProperName (..),
  ProperNameType (..),
  Qualified (..), ModuleName (..), QualifiedBy (..),
 )

import Language.Purus.IR (
  Alt (..),
  Exp (..),
  FVar (FVar),
  Lit (..),
  Pat (..),
  Ty (..),
  getPat,
  pattern (:~>), analyzeApp, Kind, BVar,
 )
import Language.Purus.IR.Utils (
  Vars,
  WithoutObjects,
 )
import Language.Purus.Pipeline.Monad (
  PlutusContext,
 )

import Bound (Var (..))
import Control.Lens (
  view,
 )
import Control.Monad.Except (
  MonadError (throwError),
 )

import Data.Matrix
import Text.Read (readMaybe)
import Data.Vector qualified as V
import Data.Tree

-- TODO: Delete this eventually, just want it now to sketch things
type  Pattern = Pat WithoutObjects Ty (Exp WithoutObjects Ty) (Var (BVar Ty) (FVar Ty))
type Expression = Exp WithoutObjects Ty (Vars Ty)
-- If you look at the types, this can only represent "true literals", i.e., is un-nested 
type Literal = Lit WithoutObjects Pattern

{-

A position is either:
 - An explicit location in the matrix
 - A constructor position (index & arg position) at a position
-}

newtype CtorArgPos = CtorArgPos Int deriving (Show, Eq, Ord)
newtype CtorIx     = CtorIx Int deriving (Show, Eq, Ord)
-- The LHS of a constraint, uniquely identifies the position of a pattern in the pattern matrix
data Position
  = -- The index of the scrutinee in the list of scrutinees
    ScrutineeRef Int
    -- A constructor argument local-position (index, argument position) indexed to a position in the matrix
    -- (can nest these but the structure of the type should ensure that we always end up with a matrix position root)
  | ConstructorArgPos Position CtorIx CtorArgPos
  deriving (Show, Eq)

instance Ord Position where
  compare (ScrutineeRef p1) (ScrutineeRef p2) = compare p1 p2
  compare (ScrutineeRef _) _      = LT
  compare (ConstructorArgPos p1 cix1 argpos1) (ConstructorArgPos p2 cix2 argpos2) = case compare p1 p2 of
    EQ -> case compare cix1 cix2 of
      EQ -> compare argpos1 argpos2
      other -> other
    other -> other
  compare ConstructorArgPos{} ScrutineeRef{} = GT

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
  = VarC Position Ident Int Ty
  | WildC Position
  | LitC Position Literal
  | Constructor Position (Qualified (ProperName 'TypeName)) CtorIx [ConstraintPattern]

newtype ResultRowIx = ResultRowIx {getResultRow :: Int} deriving (Show, Eq, Ord)

mkPatternMatrix :: [Alt WithoutObjects Ty (Exp WithoutObjects Ty) (Vars Ty)]
                -> Maybe (Matrix Pattern)
mkPatternMatrix alts = fromLists <$> pats
  where
    pats = traverse (crackTuplePat . getPat) alts

mapWithIndex :: (Int -> a -> b) -> [a] -> [b]
mapWithIndex f xs = zipWith f [0..] xs

mkResults :: Datatypes Kind Ty -> Matrix Pattern -> [[ConstraintPattern]]
mkResults datatypes mtx = map go [0..numRows]
  where
    numRows = nrows mtx - 1

    go :: Int -> [ConstraintPattern]
    go rowIx =  cpatRow
      where
        getCtorIx :: Qualified (ProperName 'TypeName)
                  -> Qualified (ProperName 'ConstructorName)
                  -> CtorIx
        getCtorIx tn cn = fst
                          . fromJust
                          $ find (\x -> snd x == (properToIdent <$> cn))
                                 (zip (CtorIx <$> [0..])
                                      $ view cdCtorName <$> getAllConstructorDecls tn datatypes)

        thisRow =  V.toList $ getRow rowIx mtx

        cpatRow = mapWithIndex (\i x -> toConstraint (ScrutineeRef i) x) thisRow

        toConstraint :: Position
                      -> Pattern
                      -> ConstraintPattern
        toConstraint pos pat = case pat of
          VarP nm indx ty -> VarC pos nm indx ty
          WildP ->  WildC pos
          LitP lit -> LitC pos lit
          ConP tn cn ps ->
            let ctorIndex :: CtorIx
                ctorIndex = getCtorIx tn cn
                ps' = mapWithIndex (\i x ->
                        let pos' = ConstructorArgPos pos ctorIndex (CtorArgPos i)
                        in toConstraint pos' x
                        ) ps
            in Constructor pos tn ctorIndex ps'

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

needsTransform :: Expression -> Bool
needsTransform = \case
  CaseE _resTy scrut alts -> isTupleExp scrut
      && any isBadNestedPat (getPat <$> alts)
  _ -> False

note :: String -> Maybe a -> PlutusContext a
note str Nothing = throwError str
note _ (Just x) = pure x

{- Checks whether two constraints have equivalent coverage. This assumes that each constraint occurs in the same
   *position* and does not check positional equality.
-}
equivalentCoverage :: ConstraintPattern -> ConstraintPattern -> Bool
equivalentCoverage c1 c2 = case (c1,c2) of
  (VarC{}, VarC{})       -> True
  (VarC{}, WildC{})      -> True
  (WildC{}, VarC{})      -> True
  (WildC{}, WildC{})     -> True
  (VarC{}, _)            -> False
  (_, VarC{})            -> False
  (WildC{}, _)           -> False
  (_, WildC{})           -> False
  (LitC _ l1, LitC _ l2) -> l1 == l2
  (_, LitC _ _)          -> False
  (LitC _ _, _)          -> False
  (Constructor _ tn1 cix1 fs1, Constructor _ tn2 cix2 fs2) ->
    tn1 == tn2 && cix1 == cix2 && and (zipWith equivalentCoverage fs1 fs2)



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

peel :: forall (a :: *). [[a]] -> Maybe [(a,[a])]
peel xs = sequence $ foldr go [] xs
  where
   go :: [a] -> [Maybe (a, [a])] -> [Maybe (a, [a])]
   go [] acc = acc
   go (y:ys) acc = Just (y,ys) : acc

isVarC :: ConstraintPattern -> Bool
isVarC = \case
  VarC{} -> True
  _ -> False

collapse ::  [[ConstraintPattern]] -> [Tree ConstraintPattern]
collapse [] = []
collapse paths = go <$> groups
  where
    groups :: [(ConstraintPattern,[[ConstraintPattern]])]
    groups = maybe [] combine (peel paths)
      where
        -- super ugly  algorithm but should work
        combine :: [(ConstraintPattern, [ConstraintPattern])] -> [(ConstraintPattern, [[ConstraintPattern]])]
        combine rg = collapseGroups $  goCombine rg
          where
            goCombine :: [(ConstraintPattern, [ConstraintPattern])]
                      -> [([ConstraintPattern],[[ConstraintPattern]])]
            goCombine [] = []
            goCombine ((cg,rs):rest) =
              let (here,remainder) = partition (equivalentCoverage cg . fst) rest
                  (hereGroups, hereConstraints) = unzip here
                  acc = (cg:hereGroups,rs : hereConstraints)
              in acc : goCombine remainder

            collapseGroups :: [([ConstraintPattern],[[ConstraintPattern]])] -> [(ConstraintPattern,[[ConstraintPattern]])]
            collapseGroups [] = []
            collapseGroups ((cgs,res):rest) =
              let here = compressGroup cgs
              in (here,res) : collapseGroups rest

            -- Chooses the "representative pattern" for a constraint group
            {- TODO: Update this

               We expect that members of the group will all be equivalent in coverage here, which means
               that the only combinations we can run into in the list are:
                 - Mixtures of Vars and WildCards
                 - Only *equal* literals
                 - Only bare constructors

               For vars and wildcards we choose an arbitrary variable (we'll have to rename).

               For all wildcards we choose a wildcard.

               For literals, previous steps should enforce the invariant that they're equal, so we just choose the first one.

            -}
            compressGroup :: [ConstraintPattern] -> ConstraintPattern
            compressGroup [] = error "empty constraint set in compressGroup"
            compressGroup [cg] = cg
            compressGroup (v@VarC{}:_) = v
            compressGroup (w@WildC{}:rest) = fromMaybe w (find isVarC rest)
            compressGroup (l@LitC{}:_) = l
            compressGroup (Constructor position qtn cix fields:rest) =
              let unsafeGetFields :: ConstraintPattern -> [ConstraintPattern]
                  unsafeGetFields = \case
                    Constructor _ _ _ fs -> fs
                    _                    -> error "mixture of constructor and non-constructor constraints in compressGroup"

                  allOrigArgFields = fromLists $ fields : (unsafeGetFields <$> rest)

                  argFieldsGroupedByPos = V.toList $ for [0..(ncols allOrigArgFields - 1)] $ \i -> getCol i allOrigArgFields

                  compressedArgFields = compressGroup <$> argFieldsGroupedByPos
              in Constructor position qtn cix compressedArgFields

    go :: (ConstraintPattern, [[ConstraintPattern]]) -> Tree ConstraintPattern
    go (cg,inner) = Node cg $ collapse inner

-- TODO: After collapsing, we need to expand literals and then collapse again.
-- REVIEW: Or maybe we should expand before we do this? That is probably easier but might mess w/ the implicit ordering

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

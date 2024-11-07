{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Language.Purus.Pipeline.EliminateCases.Utils where

import Prelude

import Data.Text qualified as T

import Data.List (find, nub, partition)
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NE
import Data.Maybe (fromJust, fromMaybe, mapMaybe)
import Data.Traversable (for)

import Language.PureScript.CoreFn.Module (
    Datatypes,
    cdCtorName,
    getAllConstructorDecls,
    properToIdent,
 )
import Language.PureScript.Names (
    Ident (..),
    ModuleName (..),
    ProperName (..),
    ProperNameType (..),
    Qualified (..),
    QualifiedBy (..),
    pattern ByNullSourcePos,
 )
import Language.Purus.IR (
    Alt (..),
    BVar (..),
    BindE (NonRecursive),
    Exp (..),
    FVar (FVar),
    Kind,
    Lit (..),
    Pat (..),
    Ty (..),
    analyzeApp,
    expTy,
    getPat,
    getResult,
    pattern (:~>),
 )
import Language.Purus.IR.Utils (
    Vars,
    WithoutObjects,
    fromExp,
    toExp,
 )
import Language.Purus.Pipeline.Monad (
    MonadCounter (..),
    PlutusContext,
 )

import Bound (Var (..))
import Control.Lens (
    makeLenses,
    over,
    view,
 )
import Control.Monad.Except (
    MonadError (throwError),
 )

import Control.Lens.Operators ((%=), (+=))
import Control.Monad.State
import Data.Map (Map)
import Data.Map qualified as M
import Data.Matrix
import Data.Tree
import Data.Vector qualified as V
import Text.Read (readMaybe)

import Control.Monad.Reader (MonadReader (..), Reader, runReader)
import Data.Bifunctor (Bifunctor (first))
import Data.Foldable (traverse_)
import Data.Foldable.WithIndex (ifind)
import Data.Traversable.WithIndex
import Language.Purus.Pipeline.GenerateDatatypes.Utils (freshName)
import Witherable (imapMaybe)

-- TODO: Delete this eventually, just want it now to sketch things
type Pattern = Pat WithoutObjects Ty (Exp WithoutObjects Ty) (Var (BVar Ty) (FVar Ty))
type Expression = Exp WithoutObjects Ty (Vars Ty)

-- If you look at the types, this can only represent "true literals", i.e., is un-nested
type Literal = Lit WithoutObjects Pattern

{-
A position is either:
 - An explicit location in the matrix
 - A constructor position (index & arg position) at a position
-}

newtype CtorArgPos = CtorArgPos Int deriving (Show, Eq, Ord)
newtype CtorIx = CtorIx Int deriving (Show, Eq, Ord)

-- The LHS of a constraint, uniquely identifies the position of a pattern in the pattern matrix
data Position
    = -- The index of the scrutinee in the list of scrutinees
      ScrutineeRef Int
    | -- A constructor argument local-position (index, argument position) indexed to a position in the matrix
      -- (can nest these but the structure of the type should ensure that we always end up with a matrix position root)
      ConstructorArgPos Position CtorIx CtorArgPos
    deriving (Show, Eq)

instance Ord Position where
    compare (ScrutineeRef p1) (ScrutineeRef p2) = compare p1 p2
    compare (ScrutineeRef _) _ = LT
    compare (ConstructorArgPos p1 cix1 argpos1) (ConstructorArgPos p2 cix2 argpos2) = case compare p1 p2 of
        EQ -> case compare cix1 cix2 of
            EQ -> compare argpos1 argpos2
            other -> other
        other -> other
    compare ConstructorArgPos {} ScrutineeRef {} = GT

{- An Identifer is either a PureScript (Ident,Int) pair or a placeholder identifier that will be replaced with a
   fresh variable name/index in the cleanup phase. This isn't *necessary* but it lets us keep the functions here
   self-contains and "pure" (in the sense that we don't need to do all of this in one of the pipeline monads)

   The Int in LocalIdentifiers does not have any particular significance (i.e. it's not a Unique index, but should be unique
   relative to other identifiers generated in the transformations here)
-}
data Identifier
    = PSVarData Ident Int Ty
    | LocalIdentifier Int
    deriving (Show, Eq)

{- With the new indexing scheme, constructor patterns are superfluous on the RHS side of a constraint. The only things the RHS of a constraint can contain are:
     1. A Variable Pattern
     2. A wildcard pattern
     3. A Literal pattern
     4. A Constructor pattern
   NOTE: Primarily for stylistic reasons (i.e. to keep this module as readable and self-contained as possible), we use a different data type
         to represent variables that we generate in the course of these transformations. These will be replaced with "real" Idents when we rebuild the expression
-}

data ConstraintContent
    = VarC Identifier
    | WildC
    | LitC Literal
    | Constructor (Qualified (ProperName 'TypeName)) CtorIx [PatternConstraint]
    deriving (Show, Eq)

-- x ~ Just 2
data PatternConstraint = Position :@ ConstraintContent
    deriving (Show, Eq)

-- data ConstraintPattern = Position :~~~: ConstraintType

newtype ResultRowIx = ResultRowIx {getResultRow :: Int} deriving (Show, Eq, Ord)

getPosition :: PatternConstraint -> Position
getPosition (pos :@ _) = pos

getContent :: PatternConstraint -> ConstraintContent
getContent (_ :@ content) = content

mkTree :: forall (a :: *). [a] -> Maybe (Tree a)
mkTree = \case
    [] -> Nothing
    (x : xs) -> pure $ go x xs
    where
        go :: a -> [a] -> Tree a
        go y ys = Node y $ case ys of
            [] -> []
            (z : zs) -> [go z zs]

mkForest :: Datatypes Kind Ty -> [Alt WithoutObjects Ty (Exp WithoutObjects Ty) (Vars Ty)] -> [(Tree PatternConstraint, Expression)]
mkForest datatypes = mapMaybe go
    where
        getCtorIx ::
            Qualified (ProperName 'TypeName) ->
            Qualified (ProperName 'ConstructorName) ->
            Maybe CtorIx
        getCtorIx tn cn = do
            let xs = view cdCtorName <$> getAllConstructorDecls tn datatypes
            (index, _) <- ifind (\_ x -> x == (properToIdent <$> cn)) xs
            pure $ CtorIx index

        toConstraint ::
            Position ->
            Pattern ->
            Maybe PatternConstraint
        toConstraint pos pat = case pat of
            VarP nm indx ty -> pure $ pos :@ VarC (PSVarData nm indx ty)
            WildP -> pure $ pos :@ WildC
            LitP lit -> pure $ pos :@ LitC lit
            ConP tn cn ps -> do
                ctorIndex <- getCtorIx tn cn
                let ps' =
                        imapMaybe
                            ( \i x -> do
                                let pos' = ConstructorArgPos pos ctorIndex (CtorArgPos i)
                                toConstraint pos' x
                            )
                            ps
                pure $ pos :@ Constructor tn ctorIndex ps'

        go2 :: Int -> Pattern -> Maybe PatternConstraint
        go2 i p = toConstraint (ScrutineeRef i) p

        go :: Alt WithoutObjects Ty (Exp WithoutObjects Ty) (Vars Ty) -> Maybe (Tree PatternConstraint, Expression)
        go (UnguardedAlt pat result) = do
            pats <- crackTuplePat pat
            constraints <- itraverse go2 pats
            (,toExp result) <$> mkTree constraints

-- NOTE: Have to call this on linear (listlike) trees, e.g. the output for mkForest
--       or it will give you something grotesquely incorrect

unTreePatterns :: Tree PatternConstraint -> [PatternConstraint]
unTreePatterns (Node x []) = [x]
unTreePatterns (Node x xs) = x : concatMap unTreePatterns xs -- N.B. there will only ever be one element of xs if we use this in the right place

{-

data T = A Int | B Int String T | C

case t of
  A 1 -> R1
  A _ -> R2
  B 1 "hi" (A 2) -> R3
  B 1 _ C -> R4
  B 2 _ _ -> R5
  B _ _ _ -> R6
  other -> R7

linearized expansion:

case t of
  A $v1 -> case $v1 of
   1 -> R1
  A $v2 -> case $v2 of
   _ -> R2
  B $v3 $v4 $v5 -> case $v3 of
    1 -> case $v4 of
      "hi" -> case $v5 of
        A $v6 -> case $v6 of
          2 -> R3
  B $v7 $v8 $v9 -> case $v7 of
    1 -> case $v8 of
      _ -> case $V9 of
        C -> R4
  B $v10 $v11 $v12 -> case $v10 of
    2 -> case $v11 of
      _ -> case $v12 of
        _ -> R5
  B $v13 $v14 $v15 -> case $v13 of
    _ -> case $v14 of
      _ -> case $v15 of
        _ -> r6
  other -> R7

in list form (partial):

[A $v1, $v1, 1]
[A $v2, $v2, 2]

[t ~ B 1 "hi" A]
[t ~ B $v3 $v4 $v5, $v3 ~ 1, $v4 ~ "hi", $v5 ~ A $v6, $v6 ~ 2]
-}

{- At the point of nested pattern expansion, all of out trees have a "list-like"
   structure (i.e. they're guaranteed to be 'Unary Trees', which are isomorphic to nonempty lists).

   We need a notion of `<> xs` to make this procedure work, and this is that
-}
treeSnoc :: Tree a -> Tree a -> Tree a
treeSnoc (Node x []) new = Node x [new]
treeSnoc (Node x xs) new = Node x $ (\y -> treeSnoc y new) <$> xs

-- The second arg is interpreted as a *linear/unary* tree (which probably isn't how you'd expect this to work)
treeConcat :: Tree a -> [a] -> Tree a
treeConcat t [] = t
treeConcat (Node x []) (y : ys) = treeConcat (Node x [pure y]) ys
treeConcat (Node x xs) ys = Node x $ (\z -> treeConcat z ys) <$> xs

expandNestedPatterns :: [Expression] -> [Tree PatternConstraint] -> Tree PatternConstraint
expandNestedPatterns scrutinees pats = evalState (prependScrutineeBinders =<< traverse go pats) 1
    where
        {- NOTE: This is a little strange and I need to explain what's going on for my own benefit if nothing else.

                 Our "mkForest" machinery that constructs the initial list of trees ignores the scrutinees. There isn't a
                 compelling conceptual reason for this - it just makes the function easier to write & allows us to defer
                 assigning identifiers to scrutinees so that they appear bound in the constraint paths.

                 We need to "restore" the scrutinees at some point before we convert from the trees back into real
                 Purus expressions. The effect of this is that our `[Tree]` must becomes a single Tree, because
                 otherwise we're left only with a set of top-level case alternatives, which isn't what we want.

                 The following function takes a list of "top level" alternatives and wraps them in enclosing infallible matches that
                 serve only to assign variables to each scrutinee, which allows us to resolve `Position`s to Expressions
                 when rebuilding the case expression at the end.

                 An example might be useful here. Assume we have a list of trees representing the top-level alternatives after expansion:

                 [ ScrutineeRef 0 ~ Just $X (...)
                 , ScrutineeRef 0 ~ Nothing (...)
                 ]

                 And assume we have two scrutinee expressions, [Maybe Int, Boolean]

                 Now, for the purpose of transformations internal to THIS MODULE, the first representation is fine. But
                 when we rebuild the case expression, we assume that every position corresponds to a variable that occurs
                 higher up in the decision tree that represents nested case expressions and alternatives. (This is both for practical reasons,
                 i.e. to avoid re-computing complex scrutinee values repeatedly, and also for regularity.)

                 Here, however, the top level scrutinees are not (necessarily) bound to an identifier. E.g. `case Just (1 + 1 / 2 * 5) of`.

                 So we need to create bindings for the scrutinees higher up in the tree. There *is not* a "higher up in the tree", so we have to make it.

                 Assume that there are two scrutinees. We create a placeholder variable for each scrutinee and end up with this for the above list of trees:

                 |- ScrutineeRef 0 ~ $V1
                   |- ScrutineeRef 1 ~ $V2
                      |- ScrutineeRef 0 ~ Just $X
                        |- ...
                      |- ScrutineeRef 0 ~ Nothing
                        |- ...

                 In subsequent steps, we interpret constraints like `ScrutineeRef 0 ~ $V1` as *binders* when the RHS is a variable, and
                 therefore now have all of scrutinees in scope, which allows us to resolve Positions that refer to them.
        -}
        prependScrutineeBinders :: [Tree PatternConstraint] -> State Int (Tree PatternConstraint)
        prependScrutineeBinders branches = do
            scrutineeBinders' <- itraverse (\i _ -> newIdent (ScrutineeRef i)) scrutinees
            let scrutineeBinders = fromJust $ NE.nonEmpty scrutineeBinders'
            pure $ goPrepend scrutineeBinders branches
            where
                goPrepend :: NonEmpty PatternConstraint -> [Tree PatternConstraint] -> Tree PatternConstraint
                goPrepend (x :| []) bs = Node x bs
                goPrepend (x :| xs) bs =
                    let rest = fromJust $ NE.nonEmpty xs
                     in Node x [goPrepend rest bs]

        newIdent :: Position -> State Int PatternConstraint
        newIdent pos = do
            s <- get
            id += 1
            pure $ pos :@ VarC (LocalIdentifier s)

        go :: Tree PatternConstraint -> State Int (Tree PatternConstraint)
        go (Node x []) = pure (Node x [])
        go (Node x [y]) = do
            x' <- expand x
            xs' <- go y
            pure $ x' `treeSnoc` xs'
        go _ = error "non-unary tree in expandNestedPatterns" -- We WANT an error here, if the tree isn't unary then something has gone horribly, irreparably wrong
        expand :: PatternConstraint -> State Int (Tree PatternConstraint)
        expand (pos :@ body) = case body of
            c@(Constructor _ _ []) -> pure . pure $ (pos :@ c)
            Constructor tn cix ps -> do
                {- We need to construct a set of fresh variables with the same positionality and type as the actual arguments-}
                newVars <- traverse (newIdent . getPosition) ps
                -- I think this is right?
                -- x ~ B $v1 $v1 $v3,
                pure $ pure (pos :@ Constructor tn cix newVars) `treeConcat` ps
            other -> pure $ pure (pos :@ other)

tupleNumber :: Qualified (ProperName 'TypeName) -> Maybe Int
tupleNumber = \case
    Qualified (ByModuleName (ModuleName "Prim")) (ProperName tNm)
        | T.isPrefixOf "Tuple" tNm -> readMaybe @Int =<< T.unpack <$> T.stripPrefix "Tuple" tNm
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
    Just (V (F (FVar _ (Qualified (ByModuleName (ModuleName "Prim")) (Ident idnt)))), args)
        | T.isPrefixOf "Tuple" idnt -> Just args
    _ -> Nothing

-- Is it a *literal* tuple (i.e. not: Is it a var with a tuple type or the result of a function call or...)
isTupleExp :: Expression -> Bool
isTupleExp e = case analyzeApp e of
    Just (V (F (FVar _ (Qualified (ByModuleName (ModuleName "Prim")) (Ident idnt)))), _)
        | T.isPrefixOf "Tuple" idnt -> True
    _ -> False

isIrrefutablePat :: Pattern -> Bool
isIrrefutablePat = \case
    WildP -> True
    VarP {} -> True
    _ -> False

-- We don't have objects here so we can ignore them
isBadNestedPat :: Pattern -> Bool
isBadNestedPat = \case
    ConP _ _ ps -> all isIrrefutablePat ps
    _ -> False

needsTransform :: Expression -> Bool
needsTransform = \case
    CaseE _resTy scrut alts ->
        isTupleExp scrut
            && any isBadNestedPat (getPat <$> alts)
    _ -> False

note :: String -> Maybe a -> PlutusContext a
note str Nothing = throwError str
note _ (Just x) = pure x

{- Checks whether two constraints have equivalent coverage. This assumes that each constraint occurs in the same
   *position* and does not check positional equality.

   Partial order?
-}
equivalentCoverage :: ConstraintContent -> ConstraintContent -> Bool
equivalentCoverage c1 c2 = case (c1, c2) of
    (VarC {}, VarC {}) -> True
    (VarC {}, WildC {}) -> True
    (WildC {}, VarC {}) -> True
    (WildC {}, WildC {}) -> True
    (VarC {}, _) -> False
    (_, VarC {}) -> False
    (WildC {}, _) -> False
    (_, WildC {}) -> False
    (LitC l1, LitC l2) -> l1 == l2
    (_, LitC _) -> False
    (LitC _, _) -> False
    (Constructor tn1 cix1 fs1, Constructor tn2 cix2 fs2) ->
        tn1 == tn2 && cix1 == cix2 && and (zipWith equivalentCoverage (getContent <$> fs1) (getContent <$> fs2))

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

peel :: forall (a :: *). [Tree a] -> [(a, [Tree a])]
peel xs = map unTree xs
    where
        unTree :: Tree a -> (a, [Tree a])
        unTree (Node x children) = (x, children)

isVarC :: PatternConstraint -> Bool
isVarC (getContent -> c) = case c of
    VarC {} -> True
    _ -> False

collapse :: Tree PatternConstraint -> Tree PatternConstraint
collapse (Node x xs) = Node x (collapseForest xs)

collapseForest :: [Tree PatternConstraint] -> [Tree PatternConstraint]
collapseForest [] = []
collapseForest paths = go <$> groups
    where
        groups :: [(PatternConstraint, [Tree PatternConstraint])]
        groups = combine (peel paths)
            where
                -- super ugly  algorithm but should work
                combine :: [(PatternConstraint, [Tree PatternConstraint])] -> [(PatternConstraint, [Tree PatternConstraint])]
                combine rg = collapseGroups $ goCombine rg
                    where
                        goCombine ::
                            [(PatternConstraint, [Tree PatternConstraint])] ->
                            [([PatternConstraint], [Tree PatternConstraint])]
                        goCombine [] = []
                        goCombine ((cg, rs) : rest) =
                            let cgContent = getContent cg
                                (here, remainder) = partition (equivalentCoverage cgContent . getContent . fst) rest
                                (hereGroups, hereConstraints) = concat <$> unzip here
                                acc = (cg : hereGroups, rs <> hereConstraints)
                             in acc : goCombine remainder

                        collapseGroups :: [([PatternConstraint], [Tree PatternConstraint])] -> [(PatternConstraint, [Tree PatternConstraint])]
                        collapseGroups [] = []
                        collapseGroups ((cgs, res) : rest) =
                            let here = compressGroup cgs
                             in (here, res) : collapseGroups rest

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
                        compressGroup :: [PatternConstraint] -> PatternConstraint
                        compressGroup [] = error "empty constraint set in compressGroup"
                        compressGroup [cg] = cg
                        compressGroup (v@(_ :@ VarC {}) : _) = v
                        compressGroup (w@(_ :@ WildC {}) : rest) = fromMaybe w (find isVarC rest)
                        compressGroup (l@(_ :@ LitC {}) : _) = l
                        compressGroup (pos :@ Constructor qtn cix fields : rest) =
                            let unsafeGetFields :: PatternConstraint -> [PatternConstraint]
                                unsafeGetFields = \case
                                    _ :@ Constructor _ _ fs -> fs
                                    _ -> error "mixture of constructor and non-constructor constraints in compressGroup"

                                allOrigArgFields = fromLists $ fields : (unsafeGetFields <$> rest)

                                argFieldsGroupedByPos = V.toList $ for [0 .. (ncols allOrigArgFields - 1)] $ \i -> getCol i allOrigArgFields

                                compressedArgFields = compressGroup <$> argFieldsGroupedByPos
                             in pos :@ Constructor qtn cix compressedArgFields

        go :: (PatternConstraint, [Tree PatternConstraint]) -> Tree PatternConstraint
        go (cg, inner) = Node cg $ collapseForest inner

{- Tree merging.

   The point of this is that, after expanding literals and collapsing, we end up with an incomplete forest of case alternative trees.

   For example:

   ```
     case x, y of
       Just 1, "hi"  -> A
       Just 2, "lol" -> B
       Just 1, whatever -> C
       Just _, whatever -> D
       Nothing, _ -> E

  ===> Ends up looking like

   ```
   1.   case x of
   2.     Just $v1 -> case $v1 of
   3.       1 -> case y of
   4.         "hi" -> A
   5.         whatever -> C
   6.       2 -> case y of
   7.         "lol" -> B
   8.       _ -> case y of
   9.         whatever -> D
   10.    Nothing -> case y of
   11.     _ -> E
   ```

   This result is incomplete because some of the branches do not cover inputs that would be covered in the original expression.

   To ameliorate this defect, we need to ensure that every case which was covered in the original expression is covered in the
   result expression.

   The procedure for doing this is as follows:

   1. We collect all of the branches "available" at the position we're attempting to insert into. Intuitively, a branch is "available" if it
      occurs on a later line in the above representation. More technically, a branch is available for filling a hole if:
        I. The branch is located at an equal or lesser depth to the insertion context in the present branch.
           - E.g. if we are checking whether any branches are available to insert after the `"lol" -> B` alternative on line 7 of the example,
             the `_ -> case y of` branch beginning on line 8 is available.
        II. The branch occurs in a subsequent top-level branch.
   2. We winnow the branches to those with constraints that are consistent with the current path to the insertion context we are examining.

-}

mergeTrees :: Tree PatternConstraint -> Tree PatternConstraint
mergeTrees (Node x xs) = Node x $ mergeForests xs

mergeForests :: [Tree PatternConstraint] -> [Tree PatternConstraint]
mergeForests = fmap (fmap snd) . goMerge [] . fmap (makePathsExplicit [])
    where
        makePathsExplicit :: [PatternConstraint] -> Tree PatternConstraint -> Tree ([PatternConstraint], PatternConstraint)
        makePathsExplicit path (Node root children) = Node (path, root) $ makePathsExplicit (path <> [root]) <$> children

        goMerge ::
            [Tree ([PatternConstraint], PatternConstraint)] -> -- outer dependencies
            [Tree ([PatternConstraint], PatternConstraint)] ->
            [Tree ([PatternConstraint], PatternConstraint)]
        goMerge outer here = bottomsUp mergeEm outer here

bottomsUp :: ([Tree a] -> [Tree a] -> Tree a -> Tree a) -> [Tree a] -> [Tree a] -> [Tree a]
bottomsUp _ _ [] = []
bottomsUp f outer (here : rest) =
    let rest' = bottomsUp f outer rest
        here' = f outer rest' here
     in here' : rest'

mergeEm ::
    [Tree ([PatternConstraint], PatternConstraint)] ->
    [Tree ([PatternConstraint], PatternConstraint)] ->
    Tree ([PatternConstraint], PatternConstraint) ->
    Tree ([PatternConstraint], PatternConstraint)
mergeEm outer peers (Node (path, root) children) =
    Node (path, root) children'
    where
        -- Make sure that this "chops" the "head" up to the current node
        outerGroupCandidates = winnowPath path outer
        peerCandidates = winnowPath path peers
        {- Every tree that is consistent with the current path -}
        allCandidates = nub $ outerGroupCandidates <> peerCandidates -- NOTE: Nub probably isn't exactly what we want, we probably want "path nubbing"
        {- This should be all of the children (recursively) PLUS all of the other candidate branches that match up to this point -}
        children' = bottomsUp mergeEm allCandidates children <> allCandidates -- REVIEW/NOTE: Double check whether this should be 'allCandidates'
        -- TODO
        winnowPath :: [PatternConstraint] -> [Tree ([PatternConstraint], PatternConstraint)] -> [Tree ([PatternConstraint], PatternConstraint)]
        winnowPath herePath candidates = filter go candidates
            where
                go :: Tree ([PatternConstraint], PatternConstraint) -> Bool
                go (Node (targetPath, _) _) = consistentPaths herePath targetPath

{- Constraint Consistency:

   In order to determine whether we can insert something in to a "hole" (that is, any forest in any of our trees)
   we need a notion of *consistency* that pertains to constraints.

   This is a bit tricky, because it seems quite close to "equivalent coverage", but isn't exactly that.

   In *principle*, the consistency relation is commutative. In practice, it might be wise to regard the order as
   significant, e.g. if the "on the current path" constraint is more general than (i.e. covers all the same inputs + others as)
   the "insertion candidate" constraint, we can probably get away w/ discarding the less-specific insertion candidate
   (since it will always be covered by the more general case). I'm not doing that now b/c it might interact strangely w/
   wildcards in nested constructors and complicate variable binding, but it is probably useful to do some filtering at the end to
   eliminate obviously unreachable cases.

   The rules for consistency are:
     1. Two constraints that each constrain a separate position are always consistent.
     2. Variables and wildcards are consistent with everything.
     3. Two literals are consistent only if they are equal.
     4. Two constructor constraints are consistent if they pertain to the same type, have the same
        constructor index, and all of their arguments are consistent with the corresponding arguments
        of the other constraint.

   REVIEW: This might be wrong? Particularly w/r/t wildcards in a way that might break things when we
           try to reconstruct the expression. The reason for that is: If a *result* requires a variable to be
           re-bound to something else, then there has to *be* a variable to rebind. This doesn't matter for
           constraints that refer to direct scrutinee positions, but might be a problem w/ constructor positions.

           That is, if we have (using variables instead of positions here, pretend the `x` refers to a specific scrutinee position):
            a) `x ~ Constructor "Maybe" 0  _`
            b) `x ~ Constructor "Maybe" 0 'y'`

           Then whether the constraints are "consistent" (NOTE: rename this to "suitable") depends on whether `a` or `b`
-}
consistentConstraints :: PatternConstraint -> PatternConstraint -> Bool
consistentConstraints (pos1 :@ cOther) (pos2 :@ cHere) =
    pos1 /= pos2 || case (cOther, cHere) of
        (VarC {}, _) -> True -- everything is consistent w/ a variable pattern
        (_, VarC {}) -> True
        (WildC {}, _) -> True -- everything is consistent w/ a wildcard pattern
        (_, WildC {}) -> True
        (LitC l1, LitC l2) -> l1 == l2 -- literals are consistent only if equal
        (LitC {}, _) -> False
        (_, LitC {}) -> False
        (Constructor tn1 cix1 ps1, Constructor tn2 cix2 ps2) ->
            tn1 == tn2
                && cix1 == cix2
                && and (zipWith consistentConstraints ps1 ps2)

{- AFAICT the only way to implement this is to check whether *every* constraint in each path is
   consistent with *every* constraint in the other path.

   TODO? use Map Position Constraint instead of [PatternConstraint]
-}
consistentPaths ::
    [PatternConstraint] -> -- The Path to the current hole context
    [PatternConstraint] -> -- The full path of a candidate for insertion
    Bool
consistentPaths currentPath candidatePath = and $ consistentConstraints <$> currentPath <*> candidatePath

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

\|- M(0,0) ~ c
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

{- Last thing left is to insert the appropriate results.

   The basic idea is simple: We assume we have a list [([ConstraintPath], Expression)] where the LHS of the tuple
   is the original constraint path associated w/ a result, and the RHS is the result expression.

   The core of this is just `find (\x -> consistentPaths (fst x) <CURRENT PATH AT NODE W/O CHILDREN>) <That list>`

   But there are two complications:
     1. Our Trees do not have any room for expressions, so we'll need a new data type. We can't go
        directly to a result expression, because we need to clean up placeholder variables by replacing them w/
        "real" variables (which requires deducing their types).

     2. We need to keep track of variable binding in patterns & also calculate which
        variables need to be rebound (& what they need to be rebound *to*)
        in order to avoid inserting a result which depends on bound variables w/ the wrong identifier.
-}

{- Data types for the case expression skeleton.

   At this point it still makes more sense to work with an abstract notion of
   position instead of explicit named variables. All of the variables which occur in the
   outer scrutinee(s) can easily be replaced at the end, and all of the "new" variables
   must have their types deduced - best to keep that task separate.

   A skeleton is either a prototype case expression (with a Position and a list of (Constraint,Skeleton) pairs)
   or a result conjoined with the necessary variable re-bindings. (I think? Hope I got this right)
-}

data Result = Result Expression [(Position, Identifier, Identifier)]

data Skeleton
    = CaseSkeleton PatternConstraint (Either [Skeleton] Result)

data CaseOf = CaseOf Position [(ConstraintContent, Either CaseOf Result)]

{- Data type for binding context. Needs to track variables bound in each position.
-}

data BindingContext = BindingContext
    { _currentPath :: [PatternConstraint]
    }

makeLenses ''BindingContext

pathBindings :: [PatternConstraint] -> Map Position Identifier
pathBindings ps = go M.empty ps
    where
        go :: Map Position Identifier -> [PatternConstraint] -> Map Position Identifier
        go acc [] = acc
        go acc (pos :@ x : xs) = case x of
            VarC i -> go (M.insert pos i acc) xs
            Constructor _ _ pcs ->
                let acc' = go acc pcs
                 in go acc' xs
            _ -> go acc xs

mkCaseOf :: [([PatternConstraint], Expression)] -> Tree PatternConstraint -> Reader BindingContext CaseOf
mkCaseOf results (Node pc@(pos :@ pat) []) = bindAndUpdatePath pc $ do
    thisPath <- view currentPath
    case compatibleBindings thisPath results of
        Nothing -> error "no compatible result found"
        Just (res, rebindDict) -> pure $ CaseOf pos [(pat, Right $ Result res rebindDict)]
mkCaseOf results (Node pc@(pos :@ pat) children) = bindAndUpdatePath pc $ do
    let (commonRoot :@ _) = fst $ head (peel children)
    protoAlts <- traverse (local id . mkAlt) children
    pure $ CaseOf pos [(pat, Left $ CaseOf commonRoot protoAlts)]
    where
        mkAlt :: Tree PatternConstraint -> Reader BindingContext (ConstraintContent, Either CaseOf Result)
        mkAlt = \case
            Node altPC@(_ :@ altLHS) [] -> bindAndUpdatePath altPC $ do
                thisPath <- view currentPath
                case compatibleBindings thisPath results of
                    Nothing -> error "no compatible result found"
                    Just (res, rebindDict) -> pure (altLHS, Right $ Result res rebindDict)
            other@(Node (_ :@ altLHS) _) -> do
                rhs <- mkCaseOf results other
                pure (altLHS, Left rhs)

{- N.B. This checks whether a constraint path has compatible variable bindings for a
        result. "Compatible" here is pretty weak, all it means is: "Does the path have
        a variable pattern in all of the positions required by the result?".

        This is a separate notion from "consistency" or "coverage equivalence". It's needed
        b/c the above procedure does not guarantee that all of the original variable names
        bound in the input patterns exist in the result - but that's OK, since we can rebind
        them at our leisure. (For convenience we also do a consistency check here to avoid
        having to traverse everything twice, but this is just an implementation detail)

        If the path is compatible with a result, this returns a data type containing
        a list associating the old identifiers with the new identifiers and the expression.
        If it is not compatible, it returns Nothing.
-}
compatibleBindings ::
    [PatternConstraint] ->
    [([PatternConstraint], Expression)] ->
    Maybe (Expression, [(Position, Identifier, Identifier)]) -- (shared position, result identifier (needed), branch identifier (actual))
compatibleBindings _ [] = Nothing
compatibleBindings branchPath ((resultPath, result) : rest)
    | consistentPaths branchPath resultPath = do
        let branchPathBindings = pathBindings branchPath
            resultPathBindings = pathBindings resultPath
            relevantBranchBindings = M.intersection branchPathBindings resultPathBindings
        case getRebound relevantBranchBindings resultPathBindings of
            Nothing -> compatibleBindings branchPath rest
            Just rebinds -> pure (result, rebinds)
    | otherwise = compatibleBindings branchPath rest
    where
        getRebound :: Map Position Identifier -> Map Position Identifier -> Maybe [(Position, Identifier, Identifier)]
        getRebound branchIdents resultIdents =
            M.foldlWithKey'
                ( \acc pos resultIdent ->
                    case M.lookup pos branchIdents of
                        Nothing -> Nothing
                        Just branchIdent -> ((pos,resultIdent, branchIdent) :) <$> acc
                )
                (Just [])
                resultIdents

bindAndUpdatePath :: PatternConstraint -> Reader BindingContext a -> Reader BindingContext a
bindAndUpdatePath pc act = local (over currentPath (<> [pc])) $ act

mkSkeleton :: [([PatternConstraint], Expression)] -> Tree PatternConstraint -> Reader BindingContext Skeleton
mkSkeleton results (Node pc []) = bindAndUpdatePath pc $ do
    thisPath <- view currentPath
    case compatibleBindings thisPath results of
        Nothing -> error "no compatible result found in mkSkeleton"
        Just (res, rebindDict) -> pure $ CaseSkeleton pc (Right (Result res rebindDict))
mkSkeleton results (Node pc children) = bindAndUpdatePath pc $ do
    children' <- traverse (mkSkeleton results) children
    pure $ CaseSkeleton pc (Left children')

-- REVIEW: Is this right?
locally :: (Monad m) => StateT s m a -> StateT s m a
locally act = do
    s <- get
    let result = evalStateT act s
    lift result

rebuildFromCaseOf :: [Expression] -> CaseOf -> PlutusContext Expression
rebuildFromCaseOf scrutinees branch = evalStateT (rebuildFromCaseOf' scrutinees branch) M.empty

rebuildFromCaseOf' :: [Expression] -> CaseOf  -> StateT (Map Position Expression) PlutusContext Expression
rebuildFromCaseOf' scrutinees branch = do
    void $ itraverse mkScrutineeIdentifier scrutinees
    goCase branch
    where
        goCase :: CaseOf -> StateT (Map Position Expression) PlutusContext Expression
        goCase (CaseOf pos children) = do
          s <- get
          case M.lookup pos s of
            Nothing -> error $ "No expression found for position " <> show pos
            Just scrut -> do
              alts <- traverse (goAlt pos) children
              let resTy = expTy id (getResult . head $ alts)
              pure $ CaseE resTy scrut alts

        goAlt :: Position
              -> (ConstraintContent,Either CaseOf Result)
              -> StateT (Map Position Expression) PlutusContext (Alt WithoutObjects Ty (Exp WithoutObjects Ty) (Vars Ty))
        goAlt pos (constraint,rhs) = bindPatternVariables pos constraint >> do
          case rhs of
            Right (Result resultE rebindDict) -> do
              resultRebound <- rebindWithDictionary rebindDict resultE
              purusPattern <- convertPat pos constraint
              pure $ UnguardedAlt purusPattern (fromExp resultRebound)

        rebindWithDictionary :: [(Position,Identifier,Identifier)]
                             -> Expression
                             -> StateT (Map Position Expression) PlutusContext Expression
        rebindWithDictionary dict e = do
          bindings <- traverse mkBinding dict
          pure $ LetE bindings (fromExp e)
         where
           mkBinding :: (Position,Identifier,Identifier)
                     -> StateT (Map Position Expression) PlutusContext (BindE Ty (Exp WithoutObjects Ty) (Vars Ty))
           mkBinding (pos, PSVarData nm indx _, branchIdentifier) = do
             resolvedBranch <- resolveIdentifier pos branchIdentifier
             pure $ NonRecursive nm indx (fromExp resolvedBranch)
           mkBinding _ = error "result branch required a non-PSVarData variable (this should be totally impossible - where could it come from?)"

        convertPat :: Position -> ConstraintContent -> StateT (Map Position Expression) PlutusContext Pattern
        convertPat pos = \case
          VarC (PSVarData nm indx ty) -> pure $ VarP nm indx ty
          VarC identifier -> unsafeExpressionToPattern <$> resolveIdentifier pos identifier
          WildC -> pure WildP
          LitC lit -> pure $ LitP lit
          Constructor qtn ctorix inner -> undefined -- TODO: Need to pass in the datatypes and remember where I put the fn to resolve indices to ctor names


        bindPatternVariables :: Position -> ConstraintContent -> StateT (Map Position Expression) PlutusContext ()
        bindPatternVariables pos = \case
            VarC i -> void (resolveIdentifier pos i)
            Constructor _ _ inner -> traverse_ (\(pos' :@ pat') -> bindPatternVariables pos' pat') inner
            _ -> pure ()

        -- hm. might have to do this earlier? or disable variable requirement checks on top level scrutinees.
        -- I *think* the latter thing should work.
        mkScrutineeIdentifier :: Int -> Expression -> StateT (Map Position Expression) PlutusContext ()
        mkScrutineeIdentifier scrutPos e = do
            u <- lift next
            let pos = ScrutineeRef scrutPos
                ty = expTy id e
                utxt = "$X" <> T.pack (show u)
                uIdent = Ident utxt
                v = V . B $ BVar u ty uIdent
            id %= M.insert pos v

        -- saves us from having to keep track of two maps, should be safe the way we use it
        unsafeExpressionToPattern :: Expression -> Pattern
        unsafeExpressionToPattern = \case
            V (B (BVar bvIx bvTy bvIdent)) -> VarP bvIdent bvIx bvTy
            _other -> error "malformed expression in unsafeExpressionToPattern"

        resolveIdentifier :: Position -> Identifier -> StateT (Map Position Expression) PlutusContext Expression
        resolveIdentifier p i = case i of
            PSVarData idnt indx ty -> do
                let v = V . B $ BVar indx ty idnt
                id %= M.insert p v
                pure v
            LocalIdentifier _ -> do
                s <- get
                case M.lookup p s of
                    Nothing -> do
                        u <- lift next
                        let utxt = "$X" <> T.pack (show u)
                            uIdent = Ident utxt
                            dummyTy = TyCon (Qualified ByNullSourcePos (ProperName utxt))
                            res = V . B $ BVar u dummyTy uIdent
                        id %= M.insert p res
                        pure res
                    Just res -> pure res

eliminateNestedCases :: Datatypes Kind Ty -> Expression -> PlutusContext Expression
eliminateNestedCases datatypes = \case
    caseE@(CaseE resTy scrut alts) -> case crackTupleExp scrut of
        Nothing -> pure caseE
        Just scrutinees -> case traverse (crackTuplePat . getPat) alts of
            Nothing -> pure caseE
            Just _ ->
                -- this case match is used solely as a guard so as to avoid writing new functions when the existing ones will do
                let forest = mkForest datatypes alts
                    expanded = expandNestedPatterns scrutinees $ fst <$> forest
                    collapsed = collapse expanded
                    merged = mergeTrees collapsed
                    results = first unTreePatterns <$> forest
                    cases = runReader (mkCaseOf results merged) (BindingContext [])
                 in rebuildFromCaseOf scrutinees cases
    other -> pure other

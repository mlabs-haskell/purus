{-# LANGUAGE MultiWayIf #-}
{- Eliminates nested & multi-case expression with a process that is roughly analagous to condensenation of truth
   tables into semantic tableaux.
-}

module Language.Purus.Pipeline.EliminateCases.EliminateNested (eliminateNestedCases) where

import Prelude

import Data.Text qualified as T

import Data.List (find, nub, partition)
import Data.Maybe (fromJust, fromMaybe, mapMaybe)

import Language.PureScript.CoreFn.Module
    ( lookupDataDecl,
      cdCtorFields,
      dDataArgs,
      dDataCtors,
      CtorDecl(CtorDecl),
      DataDecl,
      Datatypes )
import Language.PureScript.Names (
    Ident (..),
    ProperName (..),
    ProperNameType (..),
    Qualified (..), runIdent,
 )
import Language.Purus.IR (
    Alt (..),
    BVar (..),
    BindE (NonRecursive),
    Exp (..),
    Kind,
    Pat (..),
    Ty (..),
    expTy,
    getPat,
    getResult
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
    ix,
    _2,
    over,
    view, transformM,
 )
import Control.Lens.Operators (  (+=), (^.), (^?), (.~), (&) )
import Control.Monad.State
    ( foldM,
      void,
      StateT,
      MonadState(get),
      MonadTrans(lift),
      evalState,
      evalStateT,
      State, modify )
import Data.Map (Map)
import Data.Map qualified as M
import Data.Matrix
    ( fromLists )
import Data.Tree ( Tree(..))
import Control.Monad.Reader (MonadReader (..), Reader, runReader)
import Data.Bifunctor (Bifunctor (first), second)
import Data.Foldable (traverse_)
import Data.Foldable.WithIndex (ifind)
import Data.Traversable.WithIndex
    ( TraversableWithIndex(itraverse) )
import Language.Purus.Pipeline.GenerateDatatypes.Utils (analyzeTyApp)
import Language.Purus.Pretty.Common (prettyStr)

import Language.Purus.Debug (prettify, doTrace)
import Language.PureScript.CoreFn.TypeLike (TypeLike(replaceAllTypeVars))
import Language.Purus.Pipeline.EliminateCases.Types
import Language.Purus.Pipeline.EliminateCases.Utils

mkForest :: Datatypes Kind Ty
         -> [Alt WithoutObjects Ty (Exp WithoutObjects Ty) (Vars Ty)]
         -> [(Tree PatternConstraint, Expression)]
mkForest datatypes = mapMaybe go
    where
        go2 :: Int -> Pattern -> Maybe PatternConstraint
        go2 i = toConstraint datatypes (ScrutineeRef i)

        go :: Alt WithoutObjects Ty (Exp WithoutObjects Ty) (Vars Ty) -> Maybe (Tree PatternConstraint, Expression)
        go (UnguardedAlt pat result) = do
            pats <- crackTuplePat pat
            constraints <- itraverse go2 pats
            (,toExp result) <$> mkTree constraints


-- For ordinary nested (but *not* multi-scrutinee) cases. The normal `mkForest` assumes there are tuples that need crackin'.
mkForestNoCrack :: Datatypes Kind Ty
                -> [Alt WithoutObjects Ty (Exp WithoutObjects Ty) (Vars Ty)]
                -> [(Tree PatternConstraint, Expression)]
mkForestNoCrack datatypes = mapMaybe go
  where
    go2 :: Int -> Pattern -> Maybe PatternConstraint

    go :: Alt WithoutObjects Ty (Exp WithoutObjects Ty) (Vars Ty) -> Maybe (Tree PatternConstraint, Expression)
    go (UnguardedAlt pat result) = do
      constraints <- itraverse go2 [pat]
      (,toExp result) <$> mkTree constraints
    go2 i = toConstraint datatypes (ScrutineeRef i)


expandNestedPatterns :: Datatypes Kind Ty
                     -> [Expression]
                     -> [Tree PatternConstraint]
                     -> (Map (Position,Identifier) Expression, [Tree PatternConstraint])
expandNestedPatterns datatypes scrutinees pats = evalState (mkScrutineeDict =<< traverse go pats) 1
    where
        scrutineeTypes = expTy id <$> scrutinees

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
        mkScrutineeDict :: [Tree PatternConstraint] -> State Int (Map (Position,Identifier) Expression, [Tree PatternConstraint])
        mkScrutineeDict branches = do
            scrutineeBinders' <- itraverse (\i e -> newVarC e (ScrutineeRef i)) scrutinees
            let  scrutineeBindDict   = M.fromList $ fst <$> scrutineeBinders'
            pure (scrutineeBindDict,branches)

        newVarC :: Expression -> Position -> State Int (((Position,Identifier),Expression),PatternConstraint)
        newVarC e pos = do
            let ty = expTy id e
            s <- get
            id += 1
            let loc = LocalIdentifier s ty
                pc =  pos :@ VarC loc
                iemapPart = ((pos,loc),e)
            pure (iemapPart,pc)

        newIdent :: Position -> State Int PatternConstraint
        newIdent pos = do
          s <- get
          id += 1
          let deducedTy = unsafeDeduceTypeFromPos datatypes scrutineeTypes pos
          pure $ pos :@ VarC (LocalIdentifier s deducedTy)

        fullyExpanded :: Tree PatternConstraint -> Bool
        fullyExpanded = \case
          Node x [] -> p x
          Node x xs -> p x && all fullyExpanded xs
         where
           p :: PatternConstraint -> Bool
           p (_ :@ pat) = result
            where
              result = case pat of
               VarC{} -> True
               WildC -> True
               LitC{} -> True
               Constructor _ _ args -> all (irrefutable . getContent) args

        go :: Tree PatternConstraint -> State Int (Tree PatternConstraint)
        go (Node x []) = expand x
        go (Node x [y]) = do
            x' <- expand x
            xs' <- go y
            let tempRes = x' `treeSnoc` xs'
            if fullyExpanded tempRes -- TODO: Remove this check, we really shouldn't need it 
              then pure tempRes
              else go tempRes
        go _ = error "non-unary tree in expandNestedPatterns" -- We WANT an error here, if the tree isn't unary then something has gone horribly, irreparably wrong

        expand :: PatternConstraint -> State Int (Tree PatternConstraint)
        expand (pos :@ body) = case body of
            c@(Constructor _ _ []) -> pure . pure $ (pos :@ c)
            ct@(Constructor _ _  ps) | all (irrefutable . getContent) ps -> pure . pure $ pos :@ ct
            Constructor tn cix ps -> do
                (newArguments, deeper) <- foldM mkNewArgsAndDeep ([],[]) ps
                expandedDeeper <- traverse expand deeper
                case expandedDeeper of
                  [] -> pure $ pure (pos :@ Constructor tn cix newArguments)
                  exDeep -> pure $ pure (pos :@ Constructor tn cix newArguments) `treeSnoc` linearConcat exDeep
            other -> pure $ pure (pos :@ other)
          where
            mkNewArgsAndDeep :: ([PatternConstraint],[PatternConstraint])
                             -> PatternConstraint
                             -> State Int ([PatternConstraint],[PatternConstraint])
            mkNewArgsAndDeep (argAcc,deepAcc) (herePos :@ hereConstraint) = case hereConstraint of
              v@(VarC _) -> pure (argAcc <> [herePos :@ v],deepAcc)
              w@WildC    -> pure (argAcc <> [herePos :@ w], deepAcc)
              other      -> do
                dummyVarC <- newIdent herePos
                let here = herePos :@ other
                pure (argAcc <> [dummyVarC], deepAcc <> [here])



{- Checks whether two constraints have equivalent coverage. This assumes that each constraint occurs in the same
   *position* and does not check positional equality.

   Partial order?
-}
equivalentCoverage :: ConstraintContent -> ConstraintContent -> Bool
equivalentCoverage c1 c2 = case (c1, c2) of
    (VarC (PSVarData{}), VarC (PSVarData{})) -> True
    (VarC LocalIdentifier{}, VarC LocalIdentifier{}) -> True
    (VarC PSVarData{}, VarC LocalIdentifier{}) -> False
    (VarC LocalIdentifier{}, VarC PSVarData{}) -> False
    (VarC {}, WildC {}) -> True
    (WildC {}, VarC {}) -> True
    (WildC {}, WildC {}) -> True
    (WildC {}, _) -> False
    (_, WildC {}) -> False
    (LitC l1, LitC l2) -> l1 == l2
    (_, LitC _) -> False
    (LitC _, _) -> False
    (Constructor tn1 cix1 fs1, Constructor tn2 cix2 fs2) ->
        tn1 == tn2 && cix1 == cix2 && and (zipWith equivalentCoverage (getContent <$> fs1) (getContent <$> fs2))
    (_,_) -> False

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

collapse :: [Tree PatternConstraint] -> [Tree PatternConstraint]
collapse = reverse . collapseForest

collapseForest :: [Tree PatternConstraint] -> [Tree PatternConstraint]
collapseForest [] = []
collapseForest paths =  result
  where
    result = map goCollapse grouped
    msg = prettify [
             "\n---------------------\n"
           , "collapseForest'\n\n"
           , "input:\n" <> ppForest paths
           , "groups:\n" <> prettyStr (fmap fst <$> grouped)
           , "\n\nresult:\n" <> ppForest result
           ,  "\n---------------------\n"
           ]
    peeled = peel paths

    -- "better" as in "more likely to be correct", probably poor performance (shouldn't matter w/ the list sizes here)
    betterGroupBy :: forall  (a :: *) (b :: *)
                   . (b -> b -> Bool)
                   -> (a -> b)
                   -> [a]
                   -> [[a]]
    betterGroupBy p f = goGroup []
      where
        goGroup :: [[a]] -> [a] -> [[a]]
        goGroup acc [] = acc
        goGroup acc (x:xs) =
          let xGroup = x : filter (p (f x) . f) xs
              restGroup = filter (not . p (f x) . f) xs
          in goGroup (xGroup:acc) restGroup

    grouped =  betterGroupBy equivalentCoverage (getContent . fst) peeled

    unsafeGetFields :: PatternConstraint -> [PatternConstraint]
    unsafeGetFields = \case
                                _ :@ Constructor _ _ fs -> fs
                                _ -> error "mixture of constructor and non-constructor constraints in compressGroup"

    goCollapse :: [(PatternConstraint, [Tree PatternConstraint])] -> Tree PatternConstraint
    goCollapse [] = error "empty constraint set in collapse"
    goCollapse ((pc,children):rest) = case getContent pc of
      VarC{} -> Node pc $ collapseForest (children <> concatMap snd rest)
      WildC{} ->
        let root =  fromMaybe pc $ find isVarC (fst <$>  rest)
        in Node root $ collapseForest (children <> concatMap snd rest)
      LitC{} -> Node pc $ collapseForest (children <> concatMap snd rest)
      Constructor _ _ [] -> Node pc $ collapseForest (children <> concatMap snd rest)
      Constructor tn cix fields ->
        let
            allOrigArgFields = fromLists $ fields : (unsafeGetFields . fst <$> rest)
            theseFields = squishColumnsWith allOrigArgFields collapseCtorArg
        in Node (pos :@ Constructor tn cix theseFields) $ collapseForest (children <> concatMap snd rest)
     where
       pos = getPosition pc

    collapseCtorArg :: [PatternConstraint] -> PatternConstraint
    collapseCtorArg [] = error "collapseCtorArg: Empty list (should be impossible)"
    collapseCtorArg (x:xs) = case x of
      _ :@ VarC{} -> x
      _ :@ WildC{} -> fromMaybe x (find isVarC xs)
      _ :@ LitC{} -> x
      pos :@ Constructor tn cix fields ->
        let allArgsInGroup = fromLists $ fields : (unsafeGetFields <$> xs)
            args' = squishColumnsWith allArgsInGroup collapseCtorArg
        in pos :@ Constructor tn cix args'

{- Pair of mututally recursive utility functions which are used to convert a [Tree PatternConstraint] into the
   CaseOf representation, which has a structure more like the expression we are trying to rebuild.

   The "Maybe" is because we have to fail (cannot reconstruct an expression) if:
     1. We have an empty forest
     2. Not all of the children in a forest refer to the same position (this means we made a mistake somewhere priot to this)
-}
pinchToCaseOf :: [Tree PatternConstraint] -> Maybe (CaseOf ())
pinchToCaseOf xs = case peel xs of
  (t:ts) | all ((== getPosition (fst t)) . getPosition . fst) ts ->
           Just
             $ CaseOf (getPosition (fst t))
               $ map (squeezeToAlt . first getContent) (t:ts)
  _ -> Nothing

squeezeToAlt :: (ConstraintContent, [Tree PatternConstraint]) -> (ConstraintContent, Either (CaseOf ()) ())
squeezeToAlt (lhs,rest) = case pinchToCaseOf rest of
  Nothing -> (lhs,Right ())
  Just more -> (lhs,Left more)

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

   1. We aggressively merge everything that is "available" for merging using the `bottomsUp` recursion scheme.
   2. We clean up the result by deleting irrelevant, superfluous, contradictory branches.

   This is much easier than trying to winnow the branches during merging, which would require keeping a very complex and
   constantly changing state up-to-date.
-}

mergeForests :: [Tree PatternConstraint] ->  CaseOf () -- Tree PatternConstraint
mergeForests xs = let merged =  goMerge [] . fmap (makePathsExplicit []) $ xs
                      untagged = fmap (fmap snd)  merged
                      cleanedup = mapMaybe cleanup untagged
                      pinched   =  pinchToCaseOf cleanedup 
                  in case pinched of
                        Nothing  ->
                          error $
                            "internal error (pls report to compiler author): "
                            <> "Could not merge forest into one tree during nested case / multi scrutinee elimination. Forest:\n"
                            <> ppForest xs
                        Just itWorked -> itWorked
    where
        makePathsExplicit :: [PatternConstraint] -> Tree PatternConstraint -> Tree ([PatternConstraint], PatternConstraint)
        makePathsExplicit path (Node root children) = Node (path, root) $ makePathsExplicit (path <> [root]) <$> children

        goMerge ::
            [Tree ([PatternConstraint], PatternConstraint)] -> -- outer dependencies
            [Tree ([PatternConstraint], PatternConstraint)] ->
            [Tree ([PatternConstraint], PatternConstraint)]
        goMerge = bottomsUp mergeEm

{- It's easier to just merge EVERYTHING using bottomsUp and then prune the irrelevant/inconsistent bits of the tree.
   This is the pruning function.

   Generally speaking, we prune according to a few criteria which are used to check relations between a prior
   pattern or set of patterns (ancestors in the tree) and a present pattern:
     - We delete branches with a present pattern that straightforwardly conflict with some prior pattern
     - We try to "pinch" trees containing a redundant node, understood as a present node that is more general than
       (or equally general as) a prior node. That is, if we have something like (PureScript source representation)
         ```
           \x -> case x of
              1 -> case x of
                1 -> ...A
              2 -> ...B
         ```
       We attempt to convert to
         ```
           \x -> case x of
             1 -> ...A
             2 -> ...B
         ```

       This is not possible in certain circumstances, but that's OK because failing to catch all of the redundancies
       doesn't impact correct (though will impact performance)
     - Otherwise we leave the present node unmodified and keep recursing.

   Annoyingly (as is not uncommon when working with rose trees), we need a separate function for recursing over child forests, since
   we cannot "pinch" the direct children of a top-level root node unless there is exaclty one child node in its forest.
-}
cleanup :: Tree PatternConstraint -> Maybe (Tree PatternConstraint)
cleanup t = doTrace "cleanup" msg result
  where
    result = catMaybeTree $ runReader (goCleanup t) M.empty
    msg = prettify ["input:\n" <> ppTree t, "result:\n" <> maybe "NOTHING" ppTree result]

    -- we want this, we don't want `sequence`
    catMaybeTree :: Tree (Maybe PatternConstraint) -> Maybe (Tree PatternConstraint)
    catMaybeTree (Node Nothing _) = Nothing
    catMaybeTree (Node (Just x) xs) = Just $ Node x (mapMaybe catMaybeTree xs)

    contradicts :: ConstraintContent -> ConstraintContent -> Bool
    contradicts prior present = case (prior,present) of
      (VarC _, _) -> False
      (WildC, _)  -> False
      (_, WildC)  -> False
      (LitC l1, LitC l2) -> l1 /= l2
      (Constructor qtn cix fs, Constructor qtn' cix' fs') ->
        qtn /= qtn'
        || cix /= cix'
        || or (zipWith (\f f' -> contradicts (getContent f) (getContent f') ) fs fs')
      _ -> True -- REVIEW: I think?

    -- really "moreGeneralThanOrIdenticalTo" but that's too long of a function name :P
    moreGeneralThan :: ConstraintContent -> ConstraintContent -> Bool
    moreGeneralThan prior present = not (irrefutable prior) && irrefutable present || prior == present

    {- If we want to "chop" a tree (because it has redundant constraint or less-specific constraint than a prior binding,
       we need to return a forest [Tree (Maybe PatternConstraint)] instead of a single tree. This is stupid but I dunno how you get around
       having to write this function when working w/ trees.
    -}
    goCleanupForest :: Tree PatternConstraint -> Reader (Map Position ConstraintContent) [Tree (Maybe PatternConstraint)]
    goCleanupForest (Node root@(pos :@ presentPat) children) = do
      r <- ask
      case M.lookup pos r of
        Nothing -> local (M.insert pos presentPat) $ do
          children' <- concat <$> traverse (local id goCleanupForest) children
          pure [Node (Just root) children']
        Just priorPat -> do
          if | priorPat == presentPat ->  concat <$> traverse (local id goCleanupForest) children
             | contradicts priorPat presentPat -> pure []
             | moreGeneralThan priorPat presentPat -> case children of
                 []          -> pure []
                 xs -> concat <$> traverse (local id goCleanupForest) xs
             | otherwise -> local (M.insert pos presentPat) $ do
                 children' <- concat <$> traverse (local id goCleanupForest) children
                 pure [Node (Just root) children']

    goCleanup :: Tree PatternConstraint -> Reader (Map Position ConstraintContent) (Tree (Maybe PatternConstraint))
    goCleanup (Node root@(pos :@ presentPat) children) = do
      r <- ask
      case M.lookup pos r of
        Nothing -> local (M.insert pos presentPat) $ do
          children' <- concat <$> traverse (local id goCleanupForest) children
          pure $ Node (Just root) children'
        Just priorPat -> do
          {- Need to branch here:
              1. If the priorPat straightforwardly conflicts with the presentPat, we want to return Nothing.
                 This handles cases where (e.g.) M0 ~ "hello" is prior and M(0) ~ "goodbyte" is present.

              2. If the priorPat is more specific than the presentPat (or presentPat is more general than priorPat),
                 we want to "skip this level" and look at the children if any exist. NOTE: I think we have to assume
                 that there is only one child at this point. That *appears* to be a safe assumption based on our
                 examples but I don't have a good first-principles argument for it.

                 The idea is that if we encounter something like

                   ```
                   M(0) := Prim.Boolean IX#0 []
                   |
                   +- M(1) := Prim.Boolean IX#0 []
                   |  |
                   |  `- M(0) := _ -- (A)
                   |     |
                   |     `- M(1) := _
                   |
                   `- M(0) := _ -- (B)
                      |
                      `- M(1) := _
                   ```

                We don't know yet whether we need to discard the whole branch until we inspect the children. At (A),
                we will end up discarding the branch, because `M(0) := _` and `M(1) := _` are both more general than
                the prior bindings. At (B), we will *not* end up discarding the branch, because while `M(0) := _` is
                more general than `M(0) := Prim.Boolean IX#0 []`, the M(1) binding in the branch occurs at a place where
                there are no prior bindings for M(1). We have to "chop the tree" to remove the useless pattern constraint.

              3. Otherwise, we add the current position-pattern binding to the context, recurse to the children, and
                 return a `Just` node.
          -}
          if | contradicts priorPat presentPat -> pure $ Node Nothing []
             | moreGeneralThan priorPat presentPat -> case children of
                 []          -> pure $ Node Nothing []
                 [onlyChild] -> goCleanup onlyChild
                 other          -> error
                                    $ "cleanup: I was wrong, apparently we do need to figure out how to 'chop' a tree w/ multiple children:\n"
                                      <> ppForest other
             | otherwise -> local (M.insert pos presentPat) $ do
                 children' <- traverse (local id goCleanup) children
                 pure $ Node (Just root) children'

{- This is the "reducer argument" we supply to `bottomsUp`.

   The first argument is the set of "outer" nodes available for merging, which, in concert with `bottomsUp`, ensures that
   *every node has access to all nodes "below it" in the PureScript source file*. This allows us to reconstruct *total* cases
   from an originally total set of alternatives even when working with expanded multi-scrutinee cases.

   As an example, suppose we have a partial case expression (this will be a tree here but it's easier to read it represented as a case expr):

   ```
     case x of
       Just y -> case y of
         1 -> ...A
       Just z -> ...B
       Nothing -> ...C
   ```

   `Just z` occurs below `Just y, y ~ 1` in the source file and so should be available to fill in the incomplete
    set of alternatives starting with the `1 -> ...` branch. If we are examining the `1 -> ...A` context, then
    the "outer" branches available for merging should be [Just z -> ...B, Nothing -> ...C].

   The second argument is the set of peer dependencies, i.e., co-children of the root node whose forest we are examining.

   Because we process one sub-tree at a time (i.e. *not* forests as a whole), we need to pass in the set of peers available for merging.
   The implementation of `bottomsUp` guarantees that these will always be alternatives at the same level which come *after* the alternative
   we are examining.
-}
mergeEm ::
    [Tree ([PatternConstraint], PatternConstraint)] ->
    [Tree ([PatternConstraint], PatternConstraint)] ->
    Tree ([PatternConstraint], PatternConstraint) ->
    Tree ([PatternConstraint], PatternConstraint)
mergeEm outer peers (Node (path, root) children) =
    Node (path, root) children'
    where
        {- Every tree that could possibly be available when examining the current path -}
        allCandidates =  nub $ outer <> peers
        {- This should be all of the children (recursively) PLUS all of the other candidate branches that match up to this point -}
        children' =  bottomsUp mergeEm allCandidates children <> allCandidates -- REVIEW/NOTE: Double check whether this should be 'allCandidates'

-- See previous comment. This is here instead of in `Utils` b/c it is so tightly bound up with `mergeEm`
bottomsUp :: ([Tree a] -> [Tree a] -> Tree a -> Tree a) -> [Tree a] -> [Tree a] -> [Tree a]
bottomsUp _ _ [] = []
bottomsUp f outer (here : rest) =
    let rest' = bottomsUp f outer rest
        here' = f outer rest' here
     in here' : rest'

consistentResult ::
  [PatternConstraint] ->
  [PatternConstraint] ->
  Bool
consistentResult currentPath' candidateRes = and (consistentConstraintsResult <$> normalized <*> candidateRes)
  where
    normalized = normalizePath currentPath'

    {- Need a stronger criterion than the one in `consistentConstraints`. Here, directionality matters:
       If a result has wildcard constraints in its path, those can only be satisfied by wildcards in the current
       path. (Else we'll always choose the first result for a catchall case)
    -}
    consistentConstraintsResult :: PatternConstraint -> PatternConstraint -> Bool
    consistentConstraintsResult (pos1 :@ branch) (pos2 :@ res) =
      pos1 /= pos2 || case (branch, res) of
        (VarC _, VarC _) -> True
        (WildC, WildC)  -> True
        (_, WildC)      -> True -- this is the important difference, we're not "unifying" here, we're matching unidirectionally
        (LitC l1, LitC l2) -> l1 == l2
        (Constructor tn1 cix1 ps1, Constructor tn2 cix2 ps2) ->
          tn1 == tn2
            && cix1 == cix2
            && and (zipWith consistentConstraintsResult ps1 ps2)
        _ -> False

{- Normalizes a path.

   Constraint elements of a result path are always indexed to a ScrutineeRef `Position` directly, i.e.,
   they always have a form like `M(0) := CONSTRAINT`.

   Elements of a branch path may not be indexed to a ScrutineeRef directly - indeed, expansion requires that they will
   not be. So a branch path will likely have a form like
     ```
      [ M(0) := Prim.Maybe IX#1 [LOCAL#1] -- Just LOCAL#1
      , CTORARG M(0) IX#1 ARG#0 := LOCAL#2 -- Just LOCAL#2
      , CTORARG M(0) IX#1 ARG#0 := Prim.Maybe IX#0 [] -- Just Nothing
      ]
     ```
   This path, for example, is compatible with (indeed, "equivalent" to) a Result path such as:

   ```
   [M(0) := (Prim.Maybe IX#1 [(Prim.Maybe IX#0 [])])] -- Just Nothing
   ```

   But will fail the result compatibility test because it's compatibility is the result of a many-to-one relation between
   the elements of the branch path and a single element of the result path.

   Normalization "compresses" a branch path such that it will, to the maximum extent possible, have a form that mirrors the form
   of a result path.

   We could just as well have normalized by  "expanding" (in a somewhat analogous sense to expansion as performed above) the result path
   - which one to pick is largely a matter of taste. But this leads to more readable traces and errors so this is what we do.

   Note that the "two ways" of representing (ultimately) equivalent paths/constraints is *absolutely essential* to the overall
   procedure of the transformations in this module. We need it in order to (e.g.) perform grouping/pinching when collapsing and
   merging trees.
-}
normalizePath :: [PatternConstraint] -> [PatternConstraint]
normalizePath [] = []
normalizePath pcs = normalizeScrutineeBinders scrutBinders argBinders
  where
    (scrutBinders,argBinders) = partition (\x -> case getPosition x of {ScrutineeRef _ -> True; _ -> False}) pcs


    normalizeScrutineeBinders :: [PatternConstraint] -> [PatternConstraint] -> [PatternConstraint]
    normalizeScrutineeBinders [] _ = []
    normalizeScrutineeBinders (ConstructorArgPos{} :@ _ :_) _ = error "constraint at a ctorArgPos passed in first arg to normalizeScrutineeBinders"
    normalizeScrutineeBinders (ScrutineeRef i :@ rootPat:rest) args = case rootPat of
      VarC{} -> case findMoreSpecificScrut i rest of
        Nothing -> ScrutineeRef i :@ rootPat : normalizeScrutineeBinders atOtherPositions args
        Just moreSpecific -> case moreSpecific of
          v@VarC{} -> ScrutineeRef i :@ v : normalizeScrutineeBinders atOtherPositions args
          l@LitC{} -> ScrutineeRef i :@ l : normalizeScrutineeBinders atOtherPositions args
          WildC{}  -> ScrutineeRef i :@ rootPat : normalizeScrutineeBinders atOtherPositions args -- we shouldn't ever get this but total function
          Constructor tn cix ctorArgs ->
            let (normalizedCtorArgs,argRemainder) = normalizeArgs ctorArgs args
            in ScrutineeRef i :@ Constructor tn cix normalizedCtorArgs : normalizeScrutineeBinders atOtherPositions argRemainder
      Constructor tn cix ctorArgs ->
         let (normalizedCtorArgs,argsRemainder) = normalizeArgs ctorArgs args
         in ScrutineeRef i :@ Constructor tn cix normalizedCtorArgs : normalizeScrutineeBinders atOtherPositions argsRemainder
      _ -> ScrutineeRef i :@ rootPat : normalizeScrutineeBinders atOtherPositions args
     where
       atOtherPositions = filter (\x -> getPosition x /= ScrutineeRef i) rest

       normalizeArgs :: [PatternConstraint] -> [PatternConstraint] -> ([PatternConstraint],[PatternConstraint])
       normalizeArgs [] outerArgs = ([],outerArgs)
       normalizeArgs ctorArgs []  = (ctorArgs,[])
       normalizeArgs (ctorPos :@ ctorPat:restCtorArgs) outerArgs = case reverse outerArgsAtThisPosition of
           [] -> first (ctorPos :@ ctorPat :) $ normalizeArgs restCtorArgs outerArgsElsewhere
           (y:_) -> {- If y is ANYTHING OTHER than a constructor then we're done, it can't be anything else.
                       If it's a constructor, we need to recurse and normalize its arguments -}
             case y of
               Constructor tn' cix' ctorArgs' ->
                 let (normalizedInnerArgs,outerArgsElsewhere') = normalizeArgs ctorArgs' outerArgsElsewhere
                     here = ctorPos :@ Constructor tn' cix' normalizedInnerArgs 
                 in first (here:) $ normalizeArgs restCtorArgs outerArgsElsewhere'
               _ -> first (ctorPos :@ y :) $ normalizeArgs restCtorArgs outerArgsElsewhere
         where
           (outerArgsAtThisPosition,outerArgsElsewhere) = first (map getContent) $ partition (\x -> getPosition x == ctorPos) outerArgs

       -- The matching constraint closest to the end of the list SHOULD be the most specific
       findMoreSpecificScrut :: Int -> [PatternConstraint] -> Maybe ConstraintContent
       findMoreSpecificScrut iref ps = find (\case {VarC{} -> False;WildC{} -> False;_ -> True})
                                     . reverse
                                     . map getContent
                                     $ filter (\x -> getPosition x == ScrutineeRef iref) ps



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

pathBindings :: [PatternConstraint] -> Map Position Identifier
pathBindings = go M.empty
  where
    go :: Map Position Identifier -> [PatternConstraint] -> Map Position Identifier
    go acc [] = acc
    go acc (pos :@ x : xs) = case x of
        VarC i -> go (M.insert pos i acc) xs
        Constructor _ _ pcs ->
            let acc' = go acc pcs
            in go acc' xs
        _ -> go acc xs


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
insertResults :: [([PatternConstraint], Expression)] -> CaseOf () -> Reader BindingContext (CaseOf Result)
insertResults results (CaseOf _pos alts) = CaseOf _pos <$> traverse (insertResultsIntoAlts _pos) alts
  where
    insertResultsIntoAlts :: Position
                          -> (ConstraintContent, Either (CaseOf ()) ())
                          -> Reader BindingContext (ConstraintContent, Either (CaseOf Result) Result)
    insertResultsIntoAlts pos (pat,Right ()) = bindAndUpdatePath (pos :@ pat) $ do
      thisPath <- view currentPath
      case compatibleBindings thisPath results of
        Nothing -> error
                    $ "no compatible result found for path:\n" <> prettyStr thisPath
                       <> "\n normalized:\n" <> prettyStr (normalizePath thisPath)
        Just (res,rebindDict) -> pure (pat,Right $ Result res rebindDict)
    insertResultsIntoAlts pos (pat,Left more) = bindAndUpdatePath (pos :@ pat) $ (pat,) . Left <$> insertResults results more

{- After all of our various pinch/squeeze/group/merge/collapse operations, we should end up with a CaseOf where every
   branch correspond to some result *if we ignore variable scope for the result*. Since we combine branches with roots that have
   equivalent coverage to their peers, it will frequently happen that *many* variables have been erased during these operations.

   We can easily determine whether a variable present in a branch path is compatible with the variable required by a particular
   result: So long as there is *some* variable at the same Position as the required variable (i.e. so long as it hasn't been
   erased and replaced with a WildCard, which ought not ever happen), then the bindings of the path and the result are compatible.

   However, in order to reconstruct the expression, we must determine which variables need to be rebound in the outermost scope of the result.

   This function does that. The first argument is a branch path, the second argument is a list of results (their path and the actual result expression).

   If the paths are compatible, this returns the result expression along with a list of variables that must be rebound (and what they must be
   rebound to).
-}
compatibleBindings ::
    [PatternConstraint] ->
    [([PatternConstraint], Expression)] ->
    Maybe (Expression, [(Position, Identifier, Identifier)]) -- (shared position, result identifier (needed), branch identifier (actual))
compatibleBindings _ [] = Nothing
compatibleBindings branchPath ((resultPath, result) : rest)
    | consistentResult branchPath resultPath = do
        let branchPathBindings = pathBindings branchPath
            resultPathBindings = pathBindings resultPath
            relevantBranchBindings = M.intersection branchPathBindings resultPathBindings
        case getRebound relevantBranchBindings resultPathBindings of
            Nothing -> compatibleBindings branchPath rest
            Just rebinds -> pure (result, rebinds)
    | otherwise = compatibleBindings branchPath rest
    where
        getRebound :: Map Position Identifier -> Map Position Identifier -> Maybe [(Position, Identifier, Identifier)]
        getRebound branchIdents = M.foldlWithKey'
                ( \acc pos resultIdent ->
                    case M.lookup pos branchIdents of
                        Nothing -> Nothing
                        Just branchIdent ->
                          if branchIdent == resultIdent then acc
                          else ((pos,resultIdent, branchIdent) :) <$> acc
                )
                (Just [])

bindAndUpdatePath :: PatternConstraint -> Reader BindingContext a -> Reader BindingContext a
bindAndUpdatePath pc = local (over currentPath (<> [pc]))


{- Reconstructs the expressin from our final `CaseOf` representation.

   There's really only one correct way to do this, which should be pretty obvious
   (even if the implementation is necessarily a bit verbose), so probably doesn't need to
   be explained in detail.
-}
rebuildFromCaseOf :: Map (Position,Identifier) Expression
                  -> Datatypes Kind Ty
                  -> CaseOf Result
                  -> PlutusContext Expression
rebuildFromCaseOf scrutVarDict datatypes branch = evalStateT (rebuildFromCaseOf' scrutVarDict datatypes branch) M.empty

rebuildFromCaseOf' :: Map (Position,Identifier) Expression
                   -> Datatypes Kind Ty
                   -> CaseOf Result
                   -> StateT (Map Position Expression) PlutusContext Expression
rebuildFromCaseOf' scrutVarDict datatypes branch = do
    let scrutPosDict = M.mapKeys fst scrutVarDict
    modify (scrutPosDict <>)
    goCase branch
    where
        scrutIdDict :: Map Identifier Expression
        scrutIdDict = M.mapKeys snd scrutVarDict

        goCase :: CaseOf Result -> StateT (Map Position Expression) PlutusContext Expression
        goCase (CaseOf pos children) = do
          s <- get
          case M.lookup pos s of
            Nothing -> error $ "No expression found for position " <> show pos
            Just scrut -> do
              alts <- traverse (goAlt pos) children
              let resTy = expTy id (getResult . head $ alts)
              pure $ CaseE resTy scrut alts

        goAlt :: Position
              -> (ConstraintContent,Either (CaseOf Result) Result)
              -> StateT (Map Position Expression) PlutusContext (Alt WithoutObjects Ty (Exp WithoutObjects Ty) (Vars Ty))
        goAlt pos (constraint,rhs) = bindPatternVariables pos constraint >> do
          case rhs of
            Right (Result resultE rebindDict) -> do
              resultRebound <- rebindWithDictionary rebindDict resultE
              purusPattern <- convertPat pos constraint
              pure $ UnguardedAlt purusPattern (fromExp resultRebound)
            Left more -> do
              rhs' <- goCase more
              purusPattern <- convertPat pos constraint
              pure $ UnguardedAlt purusPattern (fromExp rhs')

        rebindWithDictionary :: [(Position,Identifier,Identifier)]
                             -> Expression
                             -> StateT (Map Position Expression) PlutusContext Expression
        rebindWithDictionary [] e = pure e
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
          Constructor qtn ctorix inner -> do
            let ctorNm = resolveCtorIx qtn ctorix
                ctorP  = ConP qtn ctorNm
            inner' <- traverse (\(ipos :@ icontent) -> convertPat ipos icontent) inner
            pure $ ctorP inner'
         where
           resolveCtorIx :: Qualified (ProperName 'TypeName)
                         -> CtorIx
                         -> Qualified (ProperName 'ConstructorName)
           resolveCtorIx qtn@(Qualified _ _) (CtorIx i) = case lookupDataDecl qtn datatypes of
             Nothing -> error $ "resolveCtorIx: No datatype named  "
                                <> show qtn
                                <> " exists"
             Just ddecl -> case ifind (\i' _ -> i == i') (ddecl ^. dDataCtors) of
               Nothing -> error $ "resolveCtorIx: No constructor with index "
                                  <> show i
                                  <> " exists for type "
                                  <> show qtn
               Just (snd -> CtorDecl nm _) -> ProperName . runIdent <$> nm

        bindPatternVariables :: Position -> ConstraintContent -> StateT (Map Position Expression) PlutusContext ()
        bindPatternVariables pos = \case
            VarC i -> void (resolveIdentifier pos i)
            Constructor _ _ inner -> traverse_ (\(pos' :@ pat') -> bindPatternVariables pos' pat') inner
            _ -> pure ()

        -- saves us from having to keep track of two maps, should be safe the way we use it
        unsafeExpressionToPattern :: Expression -> Pattern
        unsafeExpressionToPattern = \case
            V (B (BVar bvIx bvTy bvIdent)) -> VarP bvIdent bvIx bvTy
            _other -> error "malformed expression in unsafeExpressionToPattern"

        resolveIdentifier :: Position -> Identifier -> StateT (Map Position Expression) PlutusContext Expression
        resolveIdentifier p i = case i of
            PSVarData idnt indx ty -> do
                let v = V . B $ BVar indx ty idnt
                modify $ M.insert p v
                pure v
            li@(LocalIdentifier _ ty) -> do
              case M.lookup li scrutIdDict of
                Just res -> pure res
                Nothing -> do
                  s <- get
                  case M.lookup p s of
                      Nothing -> do
                          u <- lift next
                          let utxt = "$X" <> T.pack (show u)
                              uIdent = Ident utxt
                              res = V . B $ BVar u ty uIdent
                          modify $ M.insert p res
                          pure res
                      Just res -> pure res

{- NOTE: This supports full elimination of nested patterns, whether they are multi- or single- scrutinee, and whether or not
         they are user-generated or compiler-generated.

         FIXME: We must ensure that the parser changes which forbid nested pattern in user code are rolled back.
-}
eliminateNestedCases' :: Datatypes Kind Ty -> Expression -> PlutusContext Expression
eliminateNestedCases' datatypes = \case
    caseE@(CaseE _ scrut alts) -> case  crackTupleExp scrut of -- *COMPILER GENERATED* multi-cases will always have a literal tuple as the scrutinee
        -- This enables compilation of arbitrary nested case expressions.
        -- We just check for the presence of nested cases and then perform the same transformations
        -- (don't need to crack tuples)
        -- REVIEW @Koz there's probably a better way to do this to reduce code duplication but I can't think of it atm
        Nothing ->
          if any (isBadNestedPat . getPat) alts
          then do
            let forest = mkForestNoCrack datatypes alts
            traceMe $ "forest:\n" <> ppForest (fst <$> forest)
            let (scrutIdDict,expanded) = expandNestedPatterns datatypes [scrut] $ fst <$> forest
            traceMe $ "expanded:\n" <> ppForest expanded
            let collapsed = collapse expanded
            traceMe $ "collapsed:\n" <> ppForest collapsed
            let merged = mergeForests collapsed
            traceMe $ "merged:\n" <> prettyStr merged <> "\n"
            let results = first unTreePatterns <$> forest
            traceMe $ "results:\n" <> prettyStr results <> "\n"
            let cases = runReader (insertResults results merged) (BindingContext [])
            traceMe $ "cases:\n" <> prettyStr cases <> "\n"
            rebuilt <- rebuildFromCaseOf scrutIdDict datatypes cases
            traceMe $ "rebuilt:\n" <> prettyStr rebuilt <> "\n"
            pure rebuilt
          else pure caseE
        Just scrutinees -> case traverse (crackTuplePat . getPat) alts of -- *COMPILER GENERATED* multi cases will always have only tuple constructor patterns
            Nothing -> pure caseE
            Just ps -> {- At this point we know we have something that was originally a multi-case, but we don't know if we actually *need*
                          to perform the transformation. It may turn out that no patterns are nested (though this isn't very likely).

                          Because `ps` represents the "cracked tuples" in the case alternatives, we must perform the transformation if
                          *anything* in `ps` is *NOT* an irrefutable pattern (Var or WildCard)
                       -}
              if all (all isIrrefutablePat) ps
                then pure caseE
                else do
                  let forest = mkForest datatypes alts
                      (scrutIdDict,expanded) = expandNestedPatterns datatypes scrutinees $ fst <$> forest
                      collapsed = collapse expanded
                      merged = mergeForests collapsed
                      results = first unTreePatterns <$> forest
                      cases = runReader (insertResults results merged) (BindingContext [])

                  rebuilt <- rebuildFromCaseOf scrutIdDict datatypes cases

                  traceMe $ "ORIGINAL EXPRESSION:\n" <> prettyStr caseE
                  traceMe $ "INITIAL FOREST:\n" <> ppForest (fst <$> forest)
                  traceMe $ "\nEXPANDED FOREST:\n" <> ppForest expanded
                  traceMe $ "\nCOLLAPSED:\n" <> ppForest collapsed
                  traceMe $ "\nMERGED:\n" <> prettyStr merged
                  traceMe $ "\nRESULTS:\n" <> prettyStr results
                  traceMe $ "\nCASEOF:\n" <> prettyStr cases
                  traceMe $ "\nREBUILT:\n" <> prettyStr rebuilt
                  
                  pure rebuilt
    other -> pure other
  where
    traceMe :: String -> PlutusContext ()
    traceMe = const $ pure () -- Debug.traceM

-- the thing we export
eliminateNestedCases :: Datatypes Kind Ty -> Expression -> PlutusContext Expression
eliminateNestedCases datatypes = transformM (eliminateNestedCases' datatypes)


{- Type deduction. We do not assign types to local variables


-}
unsafeDeduceTypeFromPos :: Datatypes Kind Ty -> [Ty] -> Position ->  Ty
unsafeDeduceTypeFromPos dt scrutTys pos = fromJust $ deduceTypeFromPos dt scrutTys pos

deduceTypeFromPos :: Datatypes Kind Ty -> [Ty] -> Position ->  Maybe Ty
deduceTypeFromPos datatypes scrutTys = \case
  ScrutineeRef i -> Just $ scrutTys !! i
  ConstructorArgPos tn outerPos cix argpos -> case deduceTypeFromPos datatypes scrutTys outerPos of
    Nothing -> Nothing
    Just outerPosCtorTy -> findArgumentType tn outerPosCtorTy cix argpos
 where
   findArgumentType ::  Qualified (ProperName 'TypeName) -> Ty -> CtorIx -> CtorArgPos -> Maybe Ty
   findArgumentType qtn parentTy cix argpos = do
     ddecl <- unifyDeclWith parentTy =<< lookupDataDecl qtn datatypes
     ddecl ^? dDataCtors . ix (getCix cix) . cdCtorFields . ix (getArgPos argpos) . _2

   -- This really just instantiates the args w/ the types in the ctors, maybe there's a cleaner way to write this?
   unifyDeclWith :: Ty -> DataDecl Kind Ty -> Maybe (DataDecl Kind Ty)
   unifyDeclWith targTy ddecl = do
     (_,targs) <- analyzeTyApp targTy -- NOTE: Not checking whether the outermost tycon matches, don't see how it couldn't
     let toInst = zipWith (\(var,_) instTy -> (var,instTy)) (ddecl ^. dDataArgs) targs
         ctors = ddecl ^. dDataCtors
         newCtors = map (over cdCtorFields (map (second (replaceAllTypeVars toInst)))) ctors
     pure $ ddecl & dDataCtors .~ newCtors

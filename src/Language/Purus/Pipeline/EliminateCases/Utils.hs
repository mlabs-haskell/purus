{-# LANGUAGE TypeApplications, TemplateHaskell #-}
module Language.Purus.Pipeline.EliminateCases.Utils where

import Prelude

import Data.Map qualified as M

import Data.Text qualified as T

import Data.Bifunctor (second)
import Data.Foldable (foldl', foldrM)
import Data.List (sortOn)
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
  tyDict,
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
  pattern (:~>), analyzeApp,
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

import PlutusCore.Name.Unique (Unique (Unique))
import PlutusIR (
  Name (Name),
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

{- We need to handle things differently in a case where we have a tuple pattern
   vs where we just have singular patterns.

-}
mkPatternMatrix :: [Alt WithoutObjects Ty (Exp WithoutObjects Ty) (Vars Ty)]
                -> Maybe (Matrix Pattern)
mkPatternMatrix alts = fromLists <$> pats
  where
    pats = traverse (crackTuplePat . getPat) alts

mkResultDictionary :: [Alt WithoutObjects Ty (Exp WithoutObjects Ty) (Vars Ty)]
                   ->    Map Int (Exp WithoutObjects Ty (Vars Ty))
mkResultDictionary alts = M.fromList
                        . zip [0..]
                        . map (\(UnguardedAlt _ e) -> toExp e)
                        $ alts

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

data HoleConstraint =
  (Ident,Int) :== Pattern deriving (Show, Eq)

data Skeleton
  = CaseOf (Ident,Int) [(Pattern,Skeleton)]
  | Hole [HoleConstraint]
  deriving (Show, Eq)

-- p1 `subsumesP` p2 iff p1 is more general than p2
subsumesP :: Pattern -> Pattern -> Bool
subsumesP p1 p2 = case (p1,p2) of
  (WildP,_) -> True
  (VarP{},_) -> True
  (ConP tn1 cn1 ps1, ConP tn2 cn2 ps2)
    | tn1 == tn2 && cn1 == cn2 -> and $ zipWith subsumesP ps1 ps2
  _ -> False

-- the constraint says what a given path actually contains,
-- and we either need to check that the pattern arg subsumes that constraint pat
-- (if not a literal) or that it is == to the conPat (if a literal)
satisfies :: HoleConstraint -> Pattern -> Bool
satisfies (nm :== conPat) pat = case conPat of
  lit@(LitP _) -> pat == lit
  _            -> pat `subsumesP` conPat
                    && not (conPat `subsumesP` pat)


data Cxt
  = Cxt {
      {- It is *vastly* easier to work with this if we assume that the scrutinees are all variables.

         They might not be! Which makes the constraints super annoying to handle. So we "pretend" we're
         working with var scrutinees (at the end we can either let- bind them or replace w/ the corresponding expression
         depending on what makes the most sense).

         The vector should be constructed such that the index of an (Ident,Int) pair corresponds to the pattern(s)
         in the column of our pattern matrix.
      -}
      _scrutineeDict :: Map (Ident,Int) Expression,
      _scrutineeIndex :: Vector (Ident,Int),
      -- the requirements for a result are determined by the structure of the matrix and do not need to be tracked separately (I think?)
      _resultDict :: Map Int Expression, 
      -- need to know "where we are" in the matrix
      _pos :: (Int,Int),
      _patMatrix :: Matrix Pattern,
      -- a conjunction of constraints which have been satisfied along the current path through the skeleton
      _path :: [HoleConstraint]
    }

makeLenses ''Cxt

needsTransform :: Expression -> Bool
needsTransform = \case
  CaseE _resTy scrut alts -> isTupleExp scrut
      && any isBadNestedPat (getPat <$> alts)
  _ -> False

note :: String -> Maybe a -> PlutusContext a
note str Nothing = throwError str
note _ (Just x) = pure x


expand :: Reader Cxt Skeleton
expand = go
  where
    mkConstraint :: (Ident,Int) -> Pattern -> (HoleConstraint, Pattern)
    mkConstraint nm pat = (nm :== pat, pat)

    {-   NOTE TO KOZ: Because of the "lazy" way I'm trying to implement this, this isn't the same thing *you* mean by
                      suitability. That is, this pertains to patterns, such that a pattern is "suitable" if we can insert in
                      the column position of the skeleton corresponding to the current column in our state.

                      For example

                      case x, y of
                        Just 2, whatever -> ...
                        Just 3, whatever -> ...
                        Just y, whatever  -> ...
                        Nothing, whatever -> ...

                     should expand to

                     case x of
                       Just 2  -> ...
                       Just 3  -> ...
                       Just y  -> ...
                       Nothing -> ...

               And the "suitability" stuff *here* refers to how we determine the LHS of the alternatives at our current
               row/column depth.

               We only ever pull from *subsequent* rows *in the same column*.

    -} 
    
    -- assumes that "all the columns to the left of the one 'lower' came from are subsumed by the columns to the left of 'higher' in its row"
    isSuitable :: Pattern -- 'higher'
               -> Pattern -- 'lower'
               -> Bool
    isSuitable (LitP l) (LitP l') = l /= l'
    isSuitable LitP{} other = isIrrefutablePat other
    isSuitable higher lower = lower `subsumesP` higher
                              && not (higher `subsumesP` lower) -- this just excludes unreachable patterns. if we have 'a -> ...' then '_ -> ...' is unreachable

    {- FIXME: This is wrong. It ignores constructors which aren't the same constructor as the pattern we're passing in.
              Maybe we really do need access to the datatype declarations?

              Or, actually, I think that's not true.
    -}
    findSuitable :: Pattern -> Reader Cxt [Pattern]
    findSuitable pat = do
      s <- ask
      let (r,c)  = s ^. pos
          mtrx   = s ^. patMatrix
          cPath  = V.fromList $ s ^. path
          lr     = nrows mtrx - 1 -- row index of last row
      res <- for [(r+1)..lr] $ \rIx -> do
             let row = getRow rIx mtrx
                 preconditions = V.slice 0 c row
                 satisfiesPreconditions = V.and $ V.zipWith satisfies (V.slice 0 c cPath) preconditions
                 candidate = row V.! c
                 isSuitableCandidate = isSuitable pat candidate
             if isSuitableCandidate && satisfiesPreconditions
               then pure $ Just candidate
               else pure Nothing
      pure $ catMaybes res


    go :: Reader Cxt Skeleton
    go = do
      s <- ask
      let (r,c)  = s ^. pos
          mtrx   = s ^. patMatrix
          scruts = s ^. scrutineeIndex
          cPath  = s ^. path
      case safeGet r c mtrx of
        Nothing -> do
          -- TODO: Instead of returning a hole here, we should *immediately* try to fill it
          --       *OR* split the hole and recurse on the split.
          --       (Eventually the notion of a Hole should drop out entirely and
          --       this function will dynamically create the result in one go)
          pure $ Hole cPath
        Just pat -> do
          let thisColScrut =  scruts V.! c -- FIXME: This isn't going to work unless we add additional scrutinees
                                           --        which we have to do in the "Nothing" branch (which isn't really done yet)
          thisColRaw <- (pat:) <$> findSuitable pat
          let thisColConstrained = mkConstraint thisColScrut <$> thisColRaw
          thisCol <- for thisColConstrained $ \(cstrnt,patrn) ->
                       local (over path (<> [cstrnt]) . over (pos . _2) (+ 1)) $ (patrn,) <$> go


          pure $ CaseOf thisColScrut thisCol

eliminateBadPatterns :: Expression -> PlutusContext Expression
eliminateBadPatterns = \case
  CaseE _resTy scrut alts
    | isTupleExp scrut && any isBadNestedPat (getPat <$> alts) -> do
          let altResults  = mkResultDictionary alts
          scrutinees <- note "scrutinees not a literal tuple" $ crackTupleExp scrut
          pmatrix   <- note "can't make a pattern matrix" $ mkPatternMatrix alts
          scrutVars <- traverse (\_ -> next >>= \i -> pure (Ident $ "$V$" <> T.pack (show i),i)) scrutinees
          let scrutDict = M.fromList $ zip scrutVars scrutinees
          let cxt = Cxt scrutDict (V.fromList scrutVars) altResults (0,0) pmatrix []
              skeleton = runReader expand cxt
          {- TODO: If I'm right, then we won't really need "fillholes" - we'll be able to dynamically generate a
                   skeleton which is isomorphic to the nested case expression we're trying to build, and then will
                   just have to clean up the scrutinee variables & ensure that we insert all of the necessary
                   type annotations for the case expresion we're constructing.
          -}
          pure $ fillHoles skeleton
  other -> pure other
 where
   fillHoles = undefined

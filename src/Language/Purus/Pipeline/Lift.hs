{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# HLINT ignore "Use if" #-}
{-# HLINT ignore "Use <&>" #-}
-- has to be here (more or less)
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Move concatMap out" #-}

module Language.Purus.Pipeline.Lift where

import Prelude

import Language.PureScript.CoreFn.Expr (PurusType)
import Language.PureScript.CoreFn.FromJSON ()
import Language.PureScript.CoreFn.Module (Module (..))
import Language.PureScript.CoreFn.TypeLike (
  TypeLike (..),
 )
import Language.PureScript.Names (Ident (..), runIdent)
import Language.PureScript.PSString (PSString)
import Language.Purus.Debug (
  doTraceM,
  prettify,
 )
import Language.Purus.IR (
  Alt (..),
  BVar (..),
  BindE (..),
  Exp (..),
  Lit (..),
  Pat (..),
  expTy,
  expTy',
 )
import Language.Purus.IR.Utils (
  Vars,
  WithObjects,
  allBoundVars,
  asExp,
  containsBVar,
  deepMapMaybeBound,
  foldBinds,
  fromExp,
  mapBind,
  stripSkolems,
  stripSkolemsFromExpr,
  toExp,
  unBVar,
  viaExp,
 )
import Language.Purus.Pipeline.Lift.Types
import Language.Purus.Pipeline.Monad (Inline, MonadCounter (next))
import Language.Purus.Pretty.Common (docString, prettyStr)

import Control.Applicative (Alternative ((<|>)))

import Data.Foldable (foldl', toList)
import Data.Maybe (mapMaybe)

import Data.Map (Map)
import Data.Map qualified as M

import Data.Set (Set)
import Data.Set qualified as S

import Control.Monad.Reader (asks, foldM)

import Debug.Trace (trace)

import Data.Text qualified as T

import Control.Lens (cosmos, over, toListOf, transform, (^..), _1)

import Bound.Scope (abstract)
import Bound.Var (Var (..))

import Prettyprinter

{- Given a collection of declarations that will be lifted, determine for each declaration
   the "deep" (recursive) set of NEW variable dependencies which need to be added
   as additional arguments.

   That is, for:

   ```
   testForLift :: Int -> Boolean
   testForLift x = h x 3
     where
       h a b = g a <= j 4 b
       j c d = c + g d
       g a = if h a x then j x 1 else x * x
   ```

   the resulting map would be:
     h := [x],
     j := [x],
     g := [x]

   `g` references an `x` that is in scope at the original declaration, but will
    become out of scope when `g` is lifted, so must be added as an argument.

   `h` and `j` reference `g` and therefore incur that dependency, so `x` must be
   added as an argument to their lifted declaration bodies as well.

-}
deepAnalysis :: Set ToLift -> Map (Ident, Int) (Set (BVar PurusType)) -- high tide, hold priority, crack lion's eye diamond, flashback...
deepAnalysis toLifts = M.mapWithKey go analyses
  where
    go :: (Ident, Int) -> (Set (Ident, Int), Set (BVar PurusType)) -> Set (BVar PurusType)
    go me (dps, theseUnBoundVars) =
      theseUnBoundVars <> getResult (resolvedDeepChildren S.empty (me, dps))
      where
        getResult :: S.Set (Ident, Int) -> S.Set (BVar PurusType)
        getResult children = S.unions $ snd <$> lookupMany children analyses

        lookupMany :: forall k v t. (Ord k, Foldable t) => t k -> Map k v -> [v]
        lookupMany ks m = mapMaybe (\k -> M.lookup k m) (toList ks)

        resolvedDeepChildren :: S.Set (Ident, Int) -> ((Ident, Int), S.Set (Ident, Int)) -> S.Set (Ident, Int)
        resolvedDeepChildren visited' (nm, deps) =
          let thisStep = mapMaybe (\k -> (k,) . fst <$> M.lookup k analyses) (S.toList deps)
              withThis = S.insert nm visited'
              thisStepWinnowed = filter (\d -> fst d `S.notMember` withThis) thisStep
              newVisited = foldl' (\acc x -> S.insert x acc) withThis (fst <$> thisStep)
           in case thisStepWinnowed of
                [] -> deps
                _ -> deps <> S.unions (resolvedDeepChildren newVisited <$> thisStepWinnowed)

    analyses :: Map (Ident, Int) (Set (Ident, Int), Set (BVar PurusType))
    analyses =
      let allLiftedBinds = S.toList . S.unions $ declarations <$> S.toList toLifts
       in foldBinds
            ( \acc nm scoped ->
                let oosVars = allNewlyOutOfScopeVars M.! nm
                    liftedDeps = getLiftedPeerDeps scoped
                    this = (liftedDeps, oosVars)
                 in M.insert nm this acc
            )
            M.empty
            allLiftedBinds

    -- FIXME: I think the problem is here? yup it was, keeping this as a reminder i changed it in case breaks
    getLiftedPeerDeps :: MonoScoped -> Set (Ident, Int)
    getLiftedPeerDeps scoped =
      let unscoped = toExp scoped
          allComponentHoleIdents = S.fromList $ mapMaybe (fmap unHole . toHole) (unscoped ^.. cosmos)
       in S.intersection allLiftedDeclIdents allComponentHoleIdents

    allLiftedDeclIdents = M.keysSet allNewlyOutOfScopeVars

    allNewlyOutOfScopeVars :: Map (Ident, Int) (Set (BVar PurusType))
    allNewlyOutOfScopeVars = foldMap getNewlyOutOfScopeVars toLifts

    getNewlyOutOfScopeVars :: ToLift -> Map (Ident, Int) (Set (BVar PurusType))
    getNewlyOutOfScopeVars (ToLift varScope _ decls) =
      foldBinds
        (\acc nm body -> M.insert nm (getUnboundVars body) acc)
        M.empty
        (S.toList decls)
      where
        getUnboundVars :: MonoScoped -> Set (BVar PurusType)
        getUnboundVars scoped = asExp scoped $ \e ->
          foldl'
            ( \acc bv ->
                if S.member bv varScope then S.insert bv acc else acc
            )
            S.empty
            (allBoundVars e)

cleanupLiftedTypes :: LiftResult -> Inline LiftResult
cleanupLiftedTypes (LiftResult bs body) = do
  let refreshTypes = mkRefreshTypes bs
      updateVars :: forall (f :: * -> *). (Functor f) => f (Vars PurusType) -> f (Vars PurusType)
      updateVars = fmap refreshTypes
      bs' = map (mapBind (const updateVars)) bs
      body' = updateVars body
  pure $ LiftResult bs' body'
  where
    mkRefreshTypes :: [MonoBind] -> Vars PurusType -> Vars PurusType
    mkRefreshTypes binds v = case v of
      F (LiftedHole hid@(Ident -> hId) hix@(fromInteger -> hIx) _) -> case M.lookup (hId, hIx) refreshDict of
        Nothing -> v
        Just hTy -> F $ LiftedHole hid hix hTy
      _ -> v
      where
        refreshDict =
          foldBinds
            ( \acc nm b ->
                let ty = expTy' id b
                 in M.insert nm ty acc
            )
            M.empty
            binds

{- See [NOTE: 1] for a rough explanation of what this function does. -}
updateAllBinds ::
  Map (Ident, Int) (Set (BVar PurusType)) ->
  MonoExp ->
  [MonoBind] ->
  Inline ([MonoBind], MonoExp)
updateAllBinds deepDict prunedBody _binds = do
  let allLiftedIdents = M.keys deepDict
  allOldToNew <- M.fromList <$> traverse (\nm -> (nm,) <$> mkOldToNew nm) allLiftedIdents
  let adjustedBody = transform (foldl' (\accF nm -> mkUpdateCallSiteBody nm . accF) id allLiftedIdents) prunedBody
      go nm =
        viaExp $
          updateLiftedLambdas allOldToNew nm
            . updateCallSiteLifted allOldToNew nm

      binds = mapBind go <$> _binds

      msg =
        prettify
          [ "Pruned body:\n " <> prettyStr prunedBody
          , "AllDeclIdents:\n " <> prettyStr allLiftedIdents
          , "AdjustedBody:\n " <> prettyStr adjustedBody
          , "Binds:\n" <> concatMap (\x -> prettyStr x <> "\n\n") binds
          , "Deep Dict:\n" <> prettyStr (M.toList (S.toList <$> deepDict))
          ]
  doTraceM "updateAllBinds" msg
  pure (binds, adjustedBody)
  where
    coerceOldToNew ::
      Map (BVar PurusType) (BVar PurusType) ->
      MonoExp ->
      MonoExp
    coerceOldToNew thisOldToNew = deepMapMaybeBound (\bv -> M.lookup bv thisOldToNew)

    updateLiftedLambdas ::
      Map (Ident, Int) (Map (BVar PurusType) (BVar PurusType)) ->
      (Ident, Int) ->
      MonoExp ->
      MonoExp
    updateLiftedLambdas allOldToNew nm e =
      let thisOldToNew = allOldToNew M.! nm
       in mkUpdateLiftedLambdas thisOldToNew (M.elems thisOldToNew) e

    updateCallSiteLifted ::
      Map (Ident, Int) (Map (BVar PurusType) (BVar PurusType)) ->
      (Ident, Int) -> -- The Name of the *enclosing lifted declaration*. We need this to select the correct "renaming dictionary"
      MonoExp ->
      MonoExp
    updateCallSiteLifted allOldToNew declNm = coerceOldToNew thisOldToNew . transform go
      where
        go x = case toHole x of
          Just (Hole hId hIx _) -> case M.lookup (hId, hIx) deepDict of
            Just deep ->
              let liftedWithOldVars = mkUpdateCallSiteLifted deep (hId, hIx) x
                  bvf v = M.lookup v thisOldToNew <|> Just v
                  updatedVarBind = abstract (\case B v -> bvf v; _ -> Nothing) liftedWithOldVars
               in toExp updatedVarBind
            Nothing -> x
          _ -> x
        thisOldToNew = allOldToNew M.! declNm -- has to be here if it's in dict
    regenBVar :: forall t. BVar t -> Inline (BVar t)
    regenBVar (BVar _ bvTy bvIdent) = do
      u <- next
      pure (BVar u bvTy bvIdent)

    {- Things named `oldToNew` here refer to a Map from variables with their original indices
       to variables with newly generated indices.

       Because (afaict from talking to the Plutus guys) we need to maintain the *global* uniqueness
       of indices, we will have one map for each declaration being lifted.

       `allOldToNew` is used to refer to the global map from identifiers to their `oldToNew` map.
    -}
    mkOldToNew :: (Ident, Int) -> Inline (Map (BVar PurusType) (BVar PurusType))
    mkOldToNew nm =
      M.fromList
        <$> foldM
          (\acc bv -> do el <- (bv,) <$> regenBVar bv; pure (el : acc))
          []
          (deepDict M.! nm)

    {- Corresponds to (2) in [NOTE 1]-}
    mkUpdateCallSiteLifted :: Set (BVar PurusType) -> (Ident, Int) -> MonoExp -> MonoExp
    mkUpdateCallSiteLifted new (idnt, indx) me = case toHole me of
      Just (Hole hId hIx _)
        | hIx == indx && hId == idnt -> foldl' AppE me (V . B <$> S.toList new)
        | otherwise -> me
      Nothing -> me

    {- For the expression being lifted, corresponds to (3) in [NOTE 1] -}
    mkUpdateLiftedLambdas ::
      Map (BVar PurusType) (BVar PurusType) ->
      [BVar PurusType] ->
      MonoExp ->
      MonoExp
    mkUpdateLiftedLambdas oldToNew new e = foldl' (\rhs bv -> LamE bv (abstr rhs)) e new
      where
        bvf bv = case M.lookup bv oldToNew of Just bvnew -> Just bvnew; Nothing -> Just bv
        abstr = abstract $ \case B bv -> bvf bv; _ -> Nothing

    {-
       This correspond to (1) in [NOTE 1], i.e., it is used for updating the call sites of the
       declarations being lifted *in the body where of the expression where the lifted declarations were
       originally let- bound.

       When updating the call site, we don't need to re-index variables b/c the originals *Must* be
       in scope at the call site. (This is also the reason why we need a separate function for the
       original call-sites: We don't care about *new* variables/indices because there aren't any new
       vars/indices)
    -}
    mkUpdateCallSiteBody :: (Ident, Int) -> MonoExp -> MonoExp
    mkUpdateCallSiteBody nm@(idnt, indx) x = case toHole x of
      Just hole@(Hole hId hIx _)
        | hIx == indx && hId == idnt ->
            let deep = S.toList $ deepDict M.! nm
             in foldl' AppE (fromHole hole) (V . B <$> deep)
        | otherwise -> x
      Nothing -> x

{- Given a "main" expression (and implicit access to the module context via the
   `Inline` monad), lift all component declarations, transform their bodies,
   and update all call-sites.

   NOTE: The first argument is the name of the 'main' declaration. We need this to handle the case where the main function is
         self-recursive. *IN PRODUCTION* that doesn't matter at all, because a validator or minting policy cannot be
         (sensibly) self-recursive. However, it's essential for testing (we need to evaluate some self-recursive
         functions to check other parts of the compiler).

         Also it's a really weird restriction to forbid self-recursive main functions and I'd like to minimize the number
         of special cases we need to
-}
lift ::
  (Ident, Int) ->
  MonoExp ->
  Inline LiftResult -- we don't put the expression back together yet b/c it's helpful to keep the pieces separate for monomorphization
lift mainNm _e = do
  e <- handleSelfRecursiveMain
  modDict <- mkModDict
  let collectDict = mkDict S.empty modDict e
      prettyCollectDict = docString . indent 2 . align . vcat $ map (\((nm, indx), b) -> pretty nm <> "#" <> pretty indx <> pretty (toExp b) <> hardline) (M.toList collectDict)
      (toLift, prunedExp, _) = collect S.empty collectDict S.empty S.empty e
      deepDict = deepAnalysis toLift
      liftThese = S.toList . S.unions $ declarations <$> S.toList toLift
  (binds, body) <- updateAllBinds deepDict prunedExp liftThese
  result <- cleanupLiftedTypes $ LiftResult binds body
  let msg =
        prettify
          [ "Input Expr:\n" <> prettyStr e
          , "Pruned Expr:\n" <> prettyStr prunedExp
          , "ToLifts:\n" <> prettyStr (S.toList toLift)
          , "Collect Dict:\n" <> prettyCollectDict
          , "Result\n" <> prettyStr result
          ]
  doTraceM "lift" msg
  pure result
  where
    handleSelfRecursiveMain :: Inline MonoExp
    handleSelfRecursiveMain
      | not (uncurry containsBVar mainNm (fromExp _e)) = pure _e
      | otherwise = do
          let (mnNm, mnIx) = mainNm
          u <- next
          let uTxt = T.pack (show u)
              newNm = case mnNm of
                Ident t -> Ident $ t <> "$" <> uTxt
                GenIdent (Just t) i -> GenIdent (Just $ t <> "$" <> uTxt) i -- we only care about a unique ord property for the maps
                GenIdent Nothing i -> GenIdent (Just $ "$" <> uTxt) i
                other -> other
              eTy = expTy id _e
              f = \case
                (BVar bvIx bvTy bvNm) ->
                  if bvIx == mnIx && bvNm == mnNm
                    then Just (BVar u bvTy newNm)
                    else Nothing
              updatedMainBody = deepMapMaybeBound f _e
              syntheticMainBinding = (mainNm, fromExp updatedMainBody)
              abstr = abstract $ \case B bv -> Just bv; _ -> Nothing
              syntheticPrimeBody = abstr . V . B $ BVar mnIx eTy mnNm
              syntheticPrimeBinding = ((newNm, u), syntheticPrimeBody)
              bindingGroup = Recursive [syntheticMainBinding, syntheticPrimeBinding]
          pure $ LetE [bindingGroup] syntheticPrimeBody

    mkDict ::
      Set (Ident, Int) ->
      Map (Ident, Int) MonoScoped ->
      MonoExp ->
      Map (Ident, Int) MonoScoped
    mkDict visited acc me = trace "mkDict" $ case me of
      V F {} -> acc
      (V (B (BVar bvIx _ bvId))) -> case S.member (bvId, bvIx) visited of
        True -> acc
        False -> case M.lookup (bvId, bvIx) acc of
          Nothing -> acc
          Just declBody -> mkDict (S.insert (bvId, bvIx) visited) acc (toExp declBody)
      LitE _ (ObjectL _ fs) -> foldl' (\ac fld -> mkDict visited ac (snd fld)) acc fs
      LitE _ _ -> acc
      AppE e1 e2 -> mkDict visited acc e1 <> mkDict visited acc e2
      CaseE _ scrut alts ->
        let wScrut = mkDict visited acc scrut
         in foldl' (\ac (UnguardedAlt _ body) -> mkDict visited ac (toExp body)) wScrut alts
      AccessorE _ _ _ arg -> mkDict visited acc arg
      ObjectUpdateE _ _ ex _ flds ->
        let wEx = mkDict visited acc ex
         in foldl' (\ac fld -> mkDict visited ac (snd fld)) wEx flds
      TyAbs _ ex -> mkDict visited acc ex
      LamE _ scoped -> mkDict visited acc (toExp scoped)
      LetE decls scoped ->
        let wDeclsTopLevel = foldBinds (\ac nm body -> M.insert nm body ac) acc decls
            wDeclsDeep = foldBinds (\ac _ body -> mkDict visited ac (toExp body)) wDeclsTopLevel decls
         in mkDict visited wDeclsDeep (toExp scoped)
      TyInstE {} -> acc -- TyInst shouldn't exist
    collect ::
      Set (Ident, Int) ->
      Map (Ident, Int) MonoScoped ->
      Set (BVar PurusType) ->
      Set (BVar (KindOf PurusType)) ->
      MonoExp ->
      (Set ToLift, MonoExp, Set (Ident, Int))
    collect visited dict boundVars boundTyVars me = trace "collect" $ case me of
      -- we ignore free variables. For us, a free variable more or less represents "shouldn't/can't be inlined"
      V fv@F {} -> (S.empty, V fv, visited)
      V b@(B (BVar bvIx (stripSkolems -> bvTy) bvIdent)) -> case M.lookup (bvIdent, bvIx) dict of
        Nothing -> (S.empty, V b, visited)
        Just declbody
          | S.member (bvIdent, bvIx) visited -> (S.empty, fromHole $ Hole bvIdent bvIx bvTy, visited)
          | otherwise ->
              let
                visited' = S.insert (bvIdent, bvIx) visited -- NOTE: if something breaks look here
                (collectedToLift, collectedBody, visited'') = collect visited' dict S.empty S.empty (toExp declbody)
                collectedDecl = NonRecursive bvIdent bvIx (fromExp . stripSkolemsFromExpr $ collectedBody)
                here = ToLift S.empty S.empty (S.singleton collectedDecl)
                hole = LiftedHoleTerm (runIdent bvIdent) (fromIntegral bvIx) bvTy
               in
                (S.insert here collectedToLift, hole, visited'')
      LitE t lit -> case lit of
        IntL i -> (S.empty, LitE t (IntL i), visited)
        StringL s -> (S.empty, LitE t (StringL s), visited)
        CharL c -> (S.empty, LitE t (CharL c), visited)
        ObjectL x fs -> case foldl' goField (S.empty, [], visited) fs of
          (bnds, flds, visited') -> (bnds, LitE t $ ObjectL x flds, visited')
      AppE e1 e2 ->
        let (bnds1, e1', vis1) = collect visited dict boundVars boundTyVars e1
            (bnds2, e2', vis2) = collect vis1 dict boundVars boundTyVars e2
         in (bnds1 <> bnds2, AppE e1' e2', vis2)
      CaseE ty scrut alts ->
        let (sBnds, scrut', vis1) = collect visited dict boundVars boundTyVars scrut
            (aBnds, alts', vis2) = collectFromAlts (S.empty, [], vis1) alts
         in (sBnds <> aBnds, CaseE ty scrut' alts', vis2)
      AccessorE x ty fld arg ->
        let (fldBnds, arg', vis1) = collect visited dict boundVars boundTyVars arg
         in (fldBnds, AccessorE x ty fld arg', vis1)
      ObjectUpdateE x ty ex copy flds ->
        let (eBnds, e', vis1) = collect visited dict boundVars boundTyVars ex
            (fldBnds, flds', vis2) = foldl' goField (S.empty, [], vis1) flds
            bnds = eBnds <> fldBnds
         in (bnds, ObjectUpdateE x ty e' copy flds', vis2)
      TyAbs tv ex -> case collect visited dict boundVars (S.insert tv boundTyVars) ex of
        (l, m, r) -> (l, TyAbs tv m, r)
      LamE bv scoped ->
        let (bnds, unscoped, vis1) = collect visited dict (S.insert bv boundVars) boundTyVars (toExp scoped)
            rescoped = abstract (\case B bvx -> Just bvx; _ -> Nothing) unscoped
         in (bnds, LamE bv rescoped, vis1)
      LetE _decls scoped ->
        let decls = mapBind (const $ viaExp stripSkolemsFromExpr) <$> _decls
            -- boundVarsPlusDecls = foldBinds (\acc (nm,indx) body -> S.insert (BVar indx (expTy' id body) nm) acc) boundVars decls
            -- vis1 = foldBinds (\vis nm _ -> S.insert nm vis) visited decls
            (liftedDecls, vis2) = collectFromNestedDeclarations visited boundVars boundTyVars decls
         in over _1 (liftedDecls <>) $ collect vis2 dict boundVars boundTyVars (toExp scoped)
      -- If we run this directly after core desugaring then there should not be any TyInstEs in the AST
      tInst@TyInstE {} ->
        error $
          "Don't know what to do with a TyInst. Seems like it requires backtracking to handle correctly? Ughhhh\n"
            <> prettyStr tInst
      where
        {- We need to "sanitize" the declarations being collected so that any Vars that reference a
           declaration being lifted are transformed into a LiftedHole with the proper type annotation.

           ...also we never properly handled nested `let-` binding groups in the first place and this
           ought to fix that.

           These are all declarations that will be *removed* from their original context, so we don't
           need to return an Exp of any sort (the relevant expressions will be embedded into the decls
           inside the resulting `ToLift`)
        -}
        collectFromNestedDeclarations ::
          Set (Ident, Int) ->
          Set (BVar PurusType) ->
          Set (BVar (KindOf PurusType)) ->
          [MonoBind] ->
          (Set ToLift, Set (Ident, Int))
        collectFromNestedDeclarations vis termBound typeBound liftThese = trace "collectFromNested" $ foldBinds go (S.empty, vis) liftThese
          where
            go :: (Set ToLift, Set (Ident, Int)) -> (Ident, Int) -> MonoScoped -> (Set ToLift, Set (Ident, Int))
            go (liftAcc, visAcc) (nm, indx) scoped =
              let (insideLifted, insideBody, visAcc') = collect visAcc dict termBound typeBound (stripSkolemsFromExpr $ toExp scoped)
                  here = ToLift termBound typeBound (S.singleton $ NonRecursive nm indx (fromExp insideBody))
               in (S.insert here (insideLifted <> liftAcc), visAcc')

        goField ::
          (Set ToLift, [(PSString, MonoExp)], Set (Ident, Int)) ->
          (PSString, MonoExp) ->
          (Set ToLift, [(PSString, MonoExp)], Set (Ident, Int))
        goField (liftAcc, fieldAcc, vis) (nm, fld) = case collect vis dict boundVars boundTyVars fld of
          (bnds, fld', vis1) -> (bnds <> liftAcc, (nm, fld') : fieldAcc, vis1)

        collectFromAlts ::
          (Set ToLift, [MonoAlt], Set (Ident, Int)) ->
          [MonoAlt] ->
          (Set ToLift, [MonoAlt], Set (Ident, Int))
        collectFromAlts acc [] = acc
        collectFromAlts (liftAcc, altAcc, visAcc) (UnguardedAlt pat scoped : rest) =
          let boundInPat = extractPatVarBinders pat
              boundVars' = foldr S.insert boundVars boundInPat
              (bnds, unscoped, vis1) = collect visAcc dict boundVars' boundTyVars (stripSkolemsFromExpr . toExp $ scoped)
              rescoped = abstract (\case B bvx -> Just bvx; _ -> Nothing) unscoped
              thisAlt = UnguardedAlt pat rescoped
              acc' = (liftAcc <> bnds, altAcc <> [thisAlt], vis1)
           in collectFromAlts acc' rest

        extractPatVarBinders ::
          Pat WithObjects PurusType (Exp WithObjects PurusType) (Vars PurusType) ->
          [BVar PurusType]
        extractPatVarBinders = \case
          VarP idnt ix t -> [BVar ix t idnt]
          WildP -> []
          LitP (ObjectL _ ps) -> concatMap (extractPatVarBinders . snd) ps
          _ -> []

usedModuleDecls :: MonoExp -> Inline [MonoBind]
usedModuleDecls e = do
  modDict <- mkModDict
  let deps =
        S.fromList
          . filter (`M.member` modDict)
          . mapMaybe (\case (V (B bv)) -> Just (unBVar bv); _ -> Nothing)
          $ directDeps
  let usedIdents = S.toList $ go modDict deps
  pure $ (\nm@(idn, ind) -> NonRecursive idn ind (modDict M.! nm)) <$> usedIdents
  where
    go :: Map (Ident, Int) MonoScoped -> Set (Ident, Int) -> Set (Ident, Int)
    go dict visited =
      let nextRound = S.foldl' (\acc nm -> dict M.! nm : acc) [] visited
          nextRoundDeps =
            S.fromList
              . filter (\x -> S.notMember x visited && M.member x dict)
              . mapMaybe (\case (V (B bv)) -> Just (unBVar bv); _ -> Nothing)
              $ concatMap (toListOf cosmos . toExp) nextRound
       in case S.null nextRoundDeps of
            True -> visited
            False -> go dict (visited <> nextRoundDeps)

    directDeps = e ^.. cosmos

mkModDict :: Inline (Map (Ident, Int) MonoScoped)
mkModDict = do
  decls <- asks moduleDecls
  pure $ foldBinds (\acc nm b -> M.insert nm b acc) M.empty decls

{- NOTE 1:

   We need three different functions, each of which is (modulo the Inline monad)
   morally a function :: Exp -> Exp

   1) We need a function that updates call sites in the body of the expression(s) inside the
      scope of the original let- bound declaration we're lifting. This situation is distinct from
      updating the call sites in *other lifted expressions*, because in the original body context,
      we are applying variables which *must* be in scope already, and so should use the *original*
      "was-in-scope-before-lifting-but-is-out-of-scope-after-lifting" BVars (which we have acces to).

   2) We need a function that updates call sites *in all lifted expressions* , i.e. (but not exclusively)
      other members of the mutually recursive binding group in which the declaration being lifted is defined (this
      might also occur for other lifted expressions inside the scope of the binding being lifted).

      Here, we need to generate new unique indices for the variables being applied, which must be done in the
      'Inline' monad. We can generate a Map from the old indices to the new indices "all at once" for the totality of
      lifted declarations and pass it around.

      Furthermore, because the new(ly re-indexed) variables must be added as lambda arguments on the LHS of *each*
      other lifted declaration, they must (to preserve global uniqueness) be specific to each lifted declaration.

   3) We need a function that modifies the declaration being lifted with additional lambdas (at the *front*, I think that's easier)
      for each new variable which may have been bound in the original context but is free in the lifted declaration
      (prior to this transformation). We need to ensure that:
         - a: The newly introduced binders contain variables with *fresh* indices
         - b: The "stale" variables (which at this point have the original indices from the scope where they
              are first bound in the context we are lifting *from*) are updated with the corresponding fresh indices
         - c: Any self-recursive calls to the function declaration being lifted are supplied with fresh arguments.

   In practice we combine 2) and 3)

   This is all somewhat complicated by the fact that we cannot deduce the *deep* set of dependencies
   from the structure of a particular to-be-lifted declaration. This is confusing, so to illustrate
   the point, consider:

   ```
    f :: Int -> Boolean
    f x = h x 3
      where
        h a b = g a <= j 4 b
        j c d = c + g d
        g a = if h a x then j x 1 else x * x
   ```

   In our AST, this desugars to (something like):

   ```
    f :: Int -> Boolean
    f = \(x: Int) ->
      let h a b = g a <= j 4 b
          j c d = c + g d
          g a = if h a x then j x 1 else x * x
      in h x 3
   ```

   The bound variable 'x' occurs *directly* in the body of `g` and in no other let-bound declaration.
   A naive attempt at lifting might yield:

   ```
   let h a b = g a <= j 4 b
       j c d = c + g d
       g x a = if h a x then j x 1 else x * x
   in (...)
   ```

   But this is wrong! (If you're reading this, try to spot the error for a second before moving to the next paragraph.)

   The problem with this attempt at lifting is that `g` occurs in both `h` and `j`, and so both `h` and `j`
   need to take an additional argument and apply it to the `g` contained within their declaration body.

   The point of this example is to demonstrate that the full dependencies of a given declaration must be known
   (where full means: the direct dependencies *and* any dependencies of the direct one, and recursively, etc).

   `getDeep` (...if it works...) fetches those dependencies for us (and this is why we had to construct all of those maps above).
-}

{-# LANGUAGE ScopedTypeVariables #-}

{-# HLINT ignore "Use if" #-}
{-# HLINT ignore "Use <&>" #-}
{-# HLINT ignore "Move concatMap out" #-}

module Language.Purus.Pipeline.Lift (lift) where

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
  prettify, doTrace,
 )
import Language.Purus.IR (
  Alt (..),
  BVar (..),
  BindE (..),
  Exp (..),
  Lit (..),
  Pat (..),
  expTy,
  expTy', bvType,
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
  viaExp, unBVar, traverseBind, viaExpM,
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

import Debug.Trace (trace, traceM)

import Data.Text qualified as T

import Control.Lens (cosmos, over, toListOf, transform, (^..), _1, (%=))

import Bound.Scope (abstract)
import Bound.Var (Var (..))

import Prettyprinter (
  Pretty (pretty),
  align,
  hardline,
  indent,
  vcat,
 )
import Control.Monad.State.Strict (StateT, execStateT)
import Control.Monad.State (State, execState)
import Control.Monad.State qualified as St

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
deepAnalysis toLifts =  doTrace "deepAnalysis" (concatMap (\(k,v) -> prettyStr k <> " := " <> prettyStr v <> "\n") . M.toList $ S.toList <$> res) res
  where
    res = M.mapWithKey go analyses

    prettyAnalyses = let bop = M.toList analyses
                     in concatMap (\(a,(b,c)) -> prettyStr a <> " := " <> prettyStr (S.toList b) <> ", " <> prettyStr (S.toList c) <> "\n") bop

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

    -- values are: Other lifted decls the key depends on, implicit args for the key
    analyses :: Map (Ident, Int) (Set (Ident, Int), Set (BVar PurusType))
    analyses = foldl' (\acc (ToLift nm body iArgs peers) ->
                         let holes      = S.fromList $ mapMaybe (fmap unHole . toHole) (toExp body ^.. cosmos)
                             peers'      = S.map unBVar peers
                             liftedDeps = S.intersection peers' holes 
                         in M.insert nm (liftedDeps,iArgs) acc
                        ) M.empty toLifts 

    allLiftedDeclIdents = M.keysSet allNewlyOutOfScopeVars

    allNewlyOutOfScopeVars :: Map (Ident, Int) (Set (BVar PurusType))
    allNewlyOutOfScopeVars = foldl' (\acc (ToLift nm _  oos _) -> M.insert nm oos acc ) M.empty toLifts




cleanupLiftedTypes :: Map (Ident,Int) (Set (BVar PurusType))
                   -> LiftResult
                   -> Inline LiftResult
cleanupLiftedTypes dict (LiftResult bs body) = do
  let bs' = map (mapBind (const $ viaExp (cleanupHoleTypes  dict))) bs
      body' = cleanupHoleTypes dict body
  pure $ LiftResult bs' body'

cleanupHoleTypes :: Map (Ident, Int) (Set (BVar PurusType))
                 -> MonoExp
                 -> MonoExp
cleanupHoleTypes dict = fmap go
  where
    implicitArgDict = map bvType . S.toList <$> dict

    go :: Vars PurusType -> Vars PurusType
    go = \case
      (F (LiftedHole nm i@(fromIntegral -> indx) ty)) ->
        let args = implicitArgDict M.! (Ident nm,indx)
            ty'  = foldr funTy ty args
        in F (LiftedHole nm i ty')
      other -> other 

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
         -- , "AllDeclIdents:\n " <> prettyStr allLiftedIdents
          , "AdjustedBody:\n " <> prettyStr adjustedBody
          -- , "Binds:\n" <> concatMap (\x -> prettyStr x <> "\n\n") binds
         --  , "Deep Dict:\n" <> prettyStr (M.toList (S.toList <$> deepDict))
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


{- If we keep track of the *origin* of a variable, it makes it easiser to determine which vars
   indicate implicit arguments. A lifted argument in scope is never an implicit argument (since it will always
   be available in the new top level scope where lifted declarations are introduced), but a variable
   introduced in a lambda that occurs in the expression being lifted is an implicit arg.
-}
data ScopeSummary = ScopeSummary {
    liftedVars :: Set (Ident,Int)
  , lambdaVars :: Set (Ident,Int)
  } deriving (Show, Eq, Ord)

stripSkolemsBV :: BVar PurusType -> BVar PurusType
stripSkolemsBV (BVar i t n) = BVar i (stripSkolems t) n

-- assumes the skolems have been stripped already
isLiftedVar :: BVar PurusType -> ScopeSummary -> Bool
isLiftedVar bv (ScopeSummary lv _) = S.member (unBVar bv) lv

isLambdaVar :: ScopeSummary -> BVar PurusType -> Bool
isLambdaVar (ScopeSummary _ lamv) bv = S.member (unBVar bv) lamv

emptyScopeSummary :: ScopeSummary
emptyScopeSummary = ScopeSummary S.empty S.empty

bindLiftedDecls :: [MonoBind] -> ScopeSummary -> ScopeSummary
bindLiftedDecls bnds (ScopeSummary liftedVs lamVs) =
  let newLifted = foldBinds (\acc (nm,idx) _ -> S.insert (nm,idx) acc) liftedVs bnds
  in  ScopeSummary newLifted lamVs

-- also used for "pseudo-lambdas" in pattern binders
bindLambdaVars :: [BVar PurusType] -> ScopeSummary -> ScopeSummary
bindLambdaVars bvs (ScopeSummary liftedVs lamVs) =
  let newLamVs = foldl' (\acc bv -> S.insert (unBVar bv) acc) lamVs bvs
  in ScopeSummary liftedVs newLamVs

mkToLift :: ScopeSummary -> Set (BVar PurusType) -> Ident -> Int -> MonoScoped  -> ToLift
mkToLift scope peerVars nm indx body =
  let implicit = filter (isLambdaVar scope) . S.toList $ asExp body findOutOfScopeVars
  in ToLift (nm,indx) body (S.fromList implicit) peerVars 

toLiftToDecl :: ToLift -> MonoBind
toLiftToDecl (ToLift (nm,indx) body _ _ ) = NonRecursive nm indx body
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
  e' <-  handleSelfRecursive mainNm _e
  e  <- stripSkolemsFromExpr <$> embedMain e'
  let (toLift, prunedExp) = collect emptyScopeSummary e
      deepDict = deepAnalysis toLift
      liftThese = toLiftToDecl <$> S.toList toLift
  (binds, body) <- updateAllBinds deepDict prunedExp liftThese
  result <- cleanupLiftedTypes deepDict $ LiftResult binds body
  let msg =
        prettify
          [ "Result\n" <> prettyStr result
          ]
  doTraceM "lift" msg
  pure result
  where
    embedMain :: MonoExp -> Inline MonoExp
    embedMain e = do
      deps <- determineTopLevelDependencies e
      let res | null deps = e
              | otherwise = LetE deps (fromExp e)
      traceM $ "embedMain res:\n" <> prettyStr res
      pure res 

    handleSelfRecursive :: (Ident,Int) -> MonoExp -> Inline MonoExp
    handleSelfRecursive nm@(enm,eix) e
      | not (uncurry containsBVar nm (fromExp e)) = pure e
      | otherwise = pure . V . B $ BVar eix (expTy id e) enm {- -do
          let (mnNm, mnIx) = nm
          u <- next
          let uTxt = T.pack (show u)
              newNm = case mnNm of
                Ident t -> Ident $ t <> "$B" <> uTxt
                GenIdent (Just t) i -> GenIdent (Just $ t <> "$B" <> uTxt) i -- we only care about a unique ord property for the maps
                GenIdent Nothing i -> GenIdent (Just $ "$B" <> uTxt) i
                other -> other
              eTy = expTy id e
              f = \case
                (BVar bvIx bvTy bvNm) ->
                  if bvIx == mnIx && bvNm == mnNm
                    then Just (BVar u bvTy newNm)
                    else Nothing
              updatedMainBody = deepMapMaybeBound f e
              syntheticMainBinding = (nm, fromExp updatedMainBody)
              abstr = abstract $ \case B bv -> Just bv; _ -> Nothing
              syntheticPrimeBody = abstr . V . B $ BVar mnIx eTy mnNm
              syntheticPrimeBinding = ((newNm, u), syntheticPrimeBody)
              bindingGroup = Recursive [syntheticMainBinding, syntheticPrimeBinding]
          pure $ LetE [bindingGroup] syntheticPrimeBody
-}
    collect ::
      ScopeSummary ->
      MonoExp ->
      (Set ToLift, MonoExp)
    collect scope me = case me of
      -- we ignore free variables. For us, a free variable more or less represents "shouldn't/can't be inlined"
      V fv@F {} -> (S.empty, V fv)
      V b@(B (stripSkolemsBV -> bv)) ->
           if isLiftedVar bv scope
           then (S.empty,toHoleTermBV bv)
           else (S.empty, V (B bv))
      LitE t lit -> LitE t <$> collectLit scope lit  -- do later
      AppE e1 e2 ->
        let (lift1,e1') = collect scope e1
            (lift2,e2') = collect scope e2
        in (lift1 <> lift2, AppE e1' e2')
      CaseE ty scrut alts ->
        let (lift1,scrut') = collect scope scrut
            (lift2,alts')   = collectAlts scope alts
        in (lift1 <> lift2, CaseE ty scrut' alts')
      AccessorE x ty fld arg ->
        let (lift1,arg') = collect scope arg
        in (lift1,AccessorE x ty fld arg')
      ObjectUpdateE x ty ex copy flds ->
        let (lift1,ex') = collect scope ex
            (lift2,flds') = collectFields scope flds
        in (lift1 <> lift2, ObjectUpdateE x ty ex' copy flds')
      TyAbs tv ex -> TyAbs tv <$> collect scope ex
      LamE bv body ->
        let newScope = bindLambdaVars [bv] scope
            (lift1,body') =  asExp body $ collect newScope
        in (lift1,LamE bv $ fromExp body')
      LetE _decls body ->
        let peers    = foldBinds (\acc (nm,indx) body'' ->
                                    let bv = stripSkolemsBV $ BVar indx (expTy' id body'') nm
                                    in S.insert bv acc
                                   ) S.empty _decls 
            newScope = bindLiftedDecls _decls scope
            (liftedInBody, body') = asExp body $ collect newScope
            (liftedInDecls,decls) = foldBinds (\(accLifted,accDecls) (nm,indx) dBody ->
                                                   let (liftedHere,bodyE) = asExp dBody $ collect newScope
                                                       declHere = NonRecursive nm indx (fromExp bodyE)
                                                   in (liftedHere <> accLifted,declHere:accDecls)
                                                 ) (S.empty,[]) _decls
            liftedFromThisLet = foldBinds (\acc (nm,indx) dBody -> S.insert (mkToLift newScope peers nm indx dBody) acc) S.empty decls
            allLifted = liftedInBody <> liftedInDecls <> liftedFromThisLet
        in (allLifted,body')
      -- If we run this directly after core desugaring then there should not be any TyInstEs in the AST
      tInst@TyInstE {} ->
        error $
          "If there's a TyInst in the AST during lifting something has gone very wrong. Oh look a tyinst:\n "
            <> prettyStr tInst
      where
        collectLit :: ScopeSummary
                   -> Lit WithObjects (Exp WithObjects PurusType (Vars PurusType))
                   -> (Set ToLift, Lit WithObjects (Exp WithObjects PurusType (Vars PurusType)))
        collectLit scopSum = \case
          ObjectL x fs -> ObjectL x <$> collectFields scopSum fs
          other        -> (S.empty,other)

        collectFields :: ScopeSummary
                      -> [(PSString, Exp WithObjects PurusType (Vars PurusType))]
                      -> (Set ToLift,[(PSString, Exp WithObjects PurusType (Vars PurusType))])
        collectFields scopSum = foldl' (\(accLift,accFields) (lbl,fld) ->
                                          let (lift1,fld') = collect scopSum fld
                                              here = (lbl,fld')
                                          in (lift1 <> accLift, here:accFields)
                                          ) (S.empty,[])

        collectAlts :: ScopeSummary
                    -> [Alt WithObjects PurusType (Exp WithObjects PurusType) (Vars PurusType)]
                    -> (Set ToLift, [Alt WithObjects PurusType (Exp WithObjects PurusType) (Vars PurusType)])
        collectAlts scopSum = foldl' (\(accLift,accAlts) (UnguardedAlt pat body) ->
                                          let patBinders = stripSkolemsBV <$> extractPatVarBinders pat
                                              newScope = bindLambdaVars patBinders scopSum
                                              (lifted,body') = asExp body $ collect newScope
                                              alt = UnguardedAlt pat (fromExp body')
                                          in (lifted <> accLift, alt:accAlts)
                                        ) (S.empty,[]) 

        extractPatVarBinders ::
          Pat WithObjects PurusType (Exp WithObjects PurusType) (Vars PurusType) ->
          [BVar PurusType]
        extractPatVarBinders = \case
          VarP idnt ix t -> [BVar ix t idnt]
          WildP -> []
          LitP (ObjectL _ ps) -> concatMap (extractPatVarBinders . snd) ps
          ConP _ _ ps -> concatMap extractPatVarBinders ps 
          _ -> []

determineTopLevelDependencies :: MonoExp -> Inline [MonoBind]
determineTopLevelDependencies e = do
    modDecls <- asks moduleDecls
    let modDeclMap = foldBinds (\acc nm body -> M.insert nm body acc) M.empty modDecls
        usedIdents = execState  (go modDeclMap e) S.empty
    traceM $ "top level idents " <> prettyStr (S.toList usedIdents)
    pure $ foldl' (\acc (nm,indx) -> NonRecursive nm indx (modDeclMap M.! (nm,indx)):acc) [] usedIdents
  where
    go :: Map (Ident,Int) MonoScoped -> MonoExp -> State (Set (Ident,Int))  ()
    go decls inp = do
      visited <- St.get
      let everyReference = S.fromList $ unBVar <$> allBoundVars inp
          notYetVisited = S.intersection (M.keysSet decls) $ S.difference everyReference visited
      id %= (notYetVisited <>)
      let nextRound = foldl' (\acc nm -> case M.lookup nm decls of {Nothing -> acc; Just b -> toExp b:acc}) [] notYetVisited
      mapM_ (go decls) nextRound

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

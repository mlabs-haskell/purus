module Language.Purus.Pipeline.Inline (inline) where

import Prelude

import Data.Map (Map)
import Data.Map qualified as M

import Data.Set (Set)
import Data.Set qualified as S

import Data.Text qualified as T

import Data.Maybe (mapMaybe)

import Control.Monad.State (
  MonadState (get, put),
  State,
  evalState,
  execState,
  unless,
  when,
 )

import Data.Foldable (find, maximumBy)

import Language.PureScript.Constants.Prim qualified as C
import Language.PureScript.CoreFn.Expr (PurusType)
import Language.PureScript.CoreFn.FromJSON ()
import Language.PureScript.Environment (pattern RecordT)

-- for the instance
import Language.PureScript.Names (
  Ident (GenIdent, Ident),
 )
import Language.PureScript.Types (
  Type (
    KindedType,
    RCons,
    REmpty,
    TypeApp,
    TypeConstructor,
    TypeVar
  ),
  freeTypeVariables,
  isMonoType,
 )

import Language.Purus.IR (
  BVar (..),
  BindE (..),
  Exp (..),
  expTy,
  expTy',
  unsafeAnalyzeApp,
 )
import Language.Purus.IR.Utils (
  Vars,
  allDeclIdentifiers,
  containsBVar,
  deepMapMaybeBound,
  flatBinds,
  foldBinds,
  isConstructorE,
  mapBind,
  mkBVar,
  stripSkolems,
  toExp,
  transformTypesInExp,
  traverseAlt,
  traverseBind,
  unBVar,
  viaExp,
  viaExpM,
 )
import Language.Purus.Pipeline.Inline.Types (
  InlineBodyData (..),
  LoopBreaker (..),
  LoopBreakerScore (..),
  getInlineBody,
  notALoopBreaker,
  traverseBodyData,
 )
import Language.Purus.Pipeline.Lift.Types (
  Hole (Hole),
  LiftResult (LiftResult),
  MonoBind,
  MonoExp,
  MonoScoped,
  toHole,
  unHole,
  pattern LiftedHole,
 )
import Language.Purus.Pipeline.Monad (Inline, MonadCounter (next))

import Algebra.Graph.AdjacencyMap (
  AdjacencyMap (..),
  edgeList,
  edges,
  gmap,
  stars,
  vertexList,
  vertexSet,
 )
import Algebra.Graph.AdjacencyMap.Algorithm (Cycle, scc, topSort)
import Algebra.Graph.NonEmpty.AdjacencyMap (fromNonEmpty)

import Control.Lens.Combinators (at, cosmos, transformM)
import Control.Lens.Operators ((.=), (^..))

import Bound (Var (..))
import Bound.Scope (abstract)

inline :: LiftResult -> Inline MonoExp
inline (LiftResult decls bodyE) = do
  declsPrepared <- inlineInLifted decls
  let plugHoles :: forall (f :: * -> *). (Functor f) => f (Vars PurusType) -> f (Vars PurusType)
      plugHoles = fmap $ \case
        F (LiftedHole hNm hIndx hTy) -> B (BVar (fromInteger hIndx) hTy (Ident hNm))
        other -> other

      inlinedBodyE =
        abstract (\case B bv -> Just bv; _ -> Nothing)
          . plugHoles
          . inlineWithData' declsPrepared
          $ bodyE
      onlyLoopBreakers =
        M.foldlWithKey'
          ( \acc (n, i) v -> case v of
              IsALoopBreaker (plugHoles -> b) ->
                NonRecursive n i b : acc
              _ -> acc
          )
          []
          declsPrepared
      flatLB = M.toList $ flatBinds onlyLoopBreakers
  declsSelfRecHandled <-
    map (\((n, i), b) -> NonRecursive n i b)
      . M.toList
      . M.unions
      <$> traverse (uncurry handleSelfRecursive) flatLB
  cleanup declsSelfRecHandled inlinedBodyE
  where
    {- If we just returned the set of lifted declarations as-is, they might have
       ill-formed types. In particular, they may have out of scope type variables
       (which were in scope in the context that they were originally defined).

       To fix this, we need to introduce type abstractions for any loop breakers that
       haven't been inlined & update the types of those declarations in both their
       lifted peers & in the body of the "main" expression being compiled.

       We are warranted in deferring these abstractions until this stage (and indeed
       need to do so in order to avoid an ocean of superfluous abstractions and instantiations)
       by the fact that a lifted declaration will always be inlined into a context where
       those free type variables are well-scoped (since the definition context always contains
       the call-site contexts). But we have to do it now, or PIR will yell at us.

       As a NOTE to myself: Here, all of the "holes" have been filled, so the variables we need to
       update are BVars, not "Hole-ey" FVars.
    -}
    cleanup :: [MonoBind] -> MonoScoped -> Inline MonoExp
    cleanup [] body = pure $ toExp body
    cleanup breakers body = do
      abstracted <- addTypeAbstractions breakers
      let newTypeDict = foldBinds (\acc nm b -> M.insert nm (expTy' id b) acc) M.empty abstracted
          updateTypes = deepMapMaybeBound $ \(unBVar -> bv) -> case M.lookup bv newTypeDict of
            Nothing -> Nothing
            Just t -> Just $ uncurry mkBVar bv t
          finalDecls = mapBind (const $ viaExp updateTypes) <$> abstracted
          finalBody = viaExp updateTypes body
      case finalDecls of
        [] -> pure . toExp $ finalBody
        _ -> pure $ LetE finalDecls finalBody

    addTypeAbstractions :: [MonoBind] -> Inline [MonoBind]
    addTypeAbstractions = traverse (traverseBind (const go))
      where
        go :: MonoScoped -> Inline MonoScoped
        go = viaExpM $ \e -> do
          let e' = transformTypesInExp stripSkolems e
              t = expTy id e'
              free = freeTypeVariables t
          bvars <- traverse (\(nm, ki) -> next >>= \u -> pure $ BVar u ki (Ident nm)) free
          pure $ foldr TyAbs e' bvars

-- sorry koz. in my heart i know you're right about type synonyms, but...
type InlineState a = State (Map (Ident, Int) InlineBodyData) a

inlineWithData' :: Map (Ident, Int) InlineBodyData -> MonoExp -> MonoExp
inlineWithData' d e = evalState (inlineWithData e) d

handleSelfRecursive :: (Ident, Int) -> MonoScoped -> Inline (Map (Ident, Int) MonoScoped)
handleSelfRecursive (nm, indx) body
  | not (containsBVar nm indx body) = pure $ M.singleton (nm, indx) body
  | otherwise = do
      u <- next
      let uTxt = T.pack (show u)
          newNm = case nm of
            Ident t -> Ident $ t <> "$" <> uTxt
            GenIdent (Just t) i -> GenIdent (Just $ t <> "$" <> uTxt) i -- we only care about a unique ord property for the maps
            GenIdent Nothing i -> GenIdent (Just $ "$" <> uTxt) i
            other -> other
          bodyTy = expTy' id body
          f = \case
            (BVar bvIx bvTy bvNm) ->
              if bvIx == indx && bvNm == nm
                then Just (BVar u bvTy newNm)
                else Nothing
          updatedOriginalBody = viaExp (deepMapMaybeBound f) body
          updatedOriginalDecl = ((nm, indx), updatedOriginalBody)
          abstr = abstract $ \case B bv -> Just bv; _ -> Nothing
          newBreakerDecl = ((newNm, u), abstr . V . B $ BVar indx bodyTy nm)
      pure $ M.fromList [updatedOriginalDecl, newBreakerDecl]

inlineWithData :: MonoExp -> InlineState MonoExp
inlineWithData = transformM go
  where
    go :: MonoExp -> InlineState MonoExp
    go ex =
      get >>= \dict -> case ex of
        fv@(V (F {})) -> case toHole fv of
          Just (Hole hId hIx _) -> case M.lookup (hId, hIx) dict of
            Just (NotALoopBreaker (toExp -> e)) -> pure e
            _ -> pure fv
          _ -> pure fv
        V b@B {} -> pure $ V b
        AppE e1 e2 -> AppE <$> go e1 <*> go e2
        CaseE t scrut alts -> do
          scrut' <- go scrut
          alts' <- traverse (traverseAlt (viaExpM go)) alts
          pure $ CaseE t scrut' alts'
        LamE bv body -> LamE bv <$> viaExpM go body
        LetE decls body -> do
          decls' <- traverse (traverseBind (\_ b -> viaExpM go b)) decls
          body' <- viaExpM go body
          pure $ LetE decls' body'
        AccessorE x t pss e -> AccessorE x t pss <$> go e
        ObjectUpdateE x t e cf fs ->
          (\e' fs' -> ObjectUpdateE x t e' cf fs')
            <$> go e
            <*> traverse (traverse go) fs
        LitE t lit -> LitE t <$> traverse go lit
        TyInstE t e -> TyInstE t <$> go e
        TyAbs bv e -> TyAbs bv <$> go e

doneInlining :: MonoExp -> InlineState Bool
doneInlining me = do
  dct <- get
  let allInlineable = M.keysSet $ M.filter notALoopBreaker dct
  let allHoles = S.fromList $ mapMaybe (fmap unHole . toHole) (me ^.. cosmos)
  pure . S.null $ S.intersection allHoles allInlineable

inlineInLifted :: [MonoBind] -> Inline (Map (Ident, Int) InlineBodyData)
inlineInLifted decls = do
  let breakers = S.map getLoopBreaker . breakLoops $ allLiftedDependencies
      dict =
        foldBinds
          ( \acc nm b ->
              if nm `S.member` breakers
                then M.insert nm (IsALoopBreaker b) acc
                else M.insert nm (NotALoopBreaker b) acc
          )
          M.empty
          decls
  pure . flip execState dict $ update [] (M.keys dict)
  where
    update :: [(Ident, Int)] -> [(Ident, Int)] -> InlineState ()
    update [] [] = pure ()
    update retry [] = update [] retry
    update retry (i : is) = do
      s <- get
      let e = s M.! i
      done1 <- doneInlining . toExp . getInlineBody $ e
      unless done1 $ do
        e' <- go e
        at i .= Just e'
        update (i : retry) is
      when done1 $ do
        update retry is
      where
        go :: InlineBodyData -> InlineState InlineBodyData
        go = traverseBodyData (viaExpM inlineWithData)

    allLiftedDeclarations :: Map (Ident, Int) MonoScoped
    allLiftedDeclarations = flatBinds decls

    allLiftedDependencies :: AdjacencyMap (Ident, Int)
    allLiftedDependencies =
      stars
        . M.toList
        . fmap S.toList
        $ allLiftedDependenciesMap

    allLiftedDependenciesMap :: Map (Ident, Int) (Set (Ident, Int))
    allLiftedDependenciesMap = foldBinds go M.empty decls
      where
        allDeclIDs :: Set (Ident, Int)
        allDeclIDs = allDeclIdentifiers decls

        go ::
          Map (Ident, Int) (Set (Ident, Int)) ->
          (Ident, Int) ->
          MonoScoped ->
          Map (Ident, Int) (Set (Ident, Int))
        go acc nm scoped =
          let unscoped = toExp scoped
              allComponentHoleIdents = S.fromList $ mapMaybe (fmap unHole . toHole) (unscoped ^.. cosmos)
              theseUsedBinds = S.intersection allDeclIDs allComponentHoleIdents
           in M.insert nm theseUsedBinds acc

    -- Example for testing: Design a call graph that has two SCCs in it

    {- I.e. select the necessary number of loop breakers and
            return the set of declarations that will actually be
            inlined (that is: the set with the loop breakers removed)
       NOTE: Since this will be the same at every call site, we should use `inline`
             to construct a static map from declaration identifiers to their inlineable
             declaration groups
    -}
    breakLoops ::
      AdjacencyMap (Ident, Int) ->
      Set LoopBreaker
    breakLoops = evalState breakEm
      where
        breakEm :: State (AdjacencyMap (Ident, Int)) (Set LoopBreaker)
        breakEm = do
          adjaMap <- get
          let stronglyConnected = gmap fromNonEmpty $ scc adjaMap
              revTopoSorted :: [AdjacencyMap (Ident, Int)]
              revTopoSorted = reverse $ either cycleErr id $ topSort stronglyConnected
          case find nonTrivialGroup revTopoSorted of
            Nothing -> pure S.empty
            Just target -> do
              let targIdentifiers = vertexList target
                  scored = score adjaMap <$> targIdentifiers
                  highScore = maximumBy (\a b -> let f = snd . getScore in compare (f a) (f b)) scored
              case highScore of
                LoopBreakerScore (_, Nothing) -> error $ "Could not select a loop breaker for decl group:\n" <> show targIdentifiers
                LoopBreakerScore (nm, _) -> do
                  removeEdgesTerminatingAt nm
                  S.insert (LoopBreaker nm) <$> breakEm
          where
            removeEdgesTerminatingAt :: (Ident, Int) -> State (AdjacencyMap (Ident, Int)) ()
            removeEdgesTerminatingAt nm = do
              s <- get
              let depsEdges = edgeList s
                  depsWinnowed = filter (\x -> snd x /= nm) depsEdges
                  newState = edges depsWinnowed
              put newState

            nonTrivialGroup :: forall x. AdjacencyMap x -> Bool
            nonTrivialGroup x = S.size (vertexSet x) > 1

            cycleErr :: Cycle (AdjacencyMap (Ident, Int)) -> a
            cycleErr cyc = error $ "Fatal Error: Cannot inline, topologically sorted graph of dependencies contains cycles:\n" <> show cyc

        score ::
          AdjacencyMap (Ident, Int) ->
          (Ident, Int) ->
          LoopBreakerScore
        score adjaMap nm = adjustScore $ case M.lookup nm allLiftedDeclarations of
          Just (toExp -> e)
            | typeContainsRow e -> LoopBreakerScore (nm, Nothing)
            | not (isMonoType (expTy id e)) -> LoopBreakerScore (nm, Just 0)
            | otherwise -> case e of
                V _ -> LoopBreakerScore (nm, Just 3)
                LitE {} -> LoopBreakerScore (nm, Just 3)
                appE@(AppE {}) -> case unsafeAnalyzeApp appE of
                  (f, _)
                    | isConstructorE f -> LoopBreakerScore (nm, Just 2)
                    | otherwise -> LoopBreakerScore (nm, Just 1)
                _ -> LoopBreakerScore (nm, Just 1)
          Nothing -> LoopBreakerScore (nm, Nothing)
          where
            {- We want to give higher priority potential loop breakers that "do more breaking" -}
            adjustScore (LoopBreakerScore (nm', Just sc)) =
              let nE = length $ filter (\x -> snd x == nm') (edgeList adjaMap)
                  newScore = sc + nE
               in LoopBreakerScore (nm', Just newScore)
            adjustScore lb = lb

            typeContainsRow :: MonoExp -> Bool
            typeContainsRow e = any isRow allTypeComponents
              where
                allTypeComponents :: [PurusType]
                allTypeComponents = expTy id e ^.. cosmos

                isRow = \case
                  TypeConstructor _ C.Row -> True
                  RecordT _ -> True
                  TypeVar _ _ k -> isRow k
                  RCons {} -> True
                  REmpty {} -> True
                  KindedType _ t1 t2 -> isRow t1 || isRow t2
                  TypeApp _ t1 t2 -> isRow t1 || isRow t2
                  _ -> False

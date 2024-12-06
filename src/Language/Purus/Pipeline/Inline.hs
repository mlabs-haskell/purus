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
  void,
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
  runIdent,
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

import Language.Purus.Debug (doTrace, doTraceM, prettify)
import Language.Purus.IR (
  BVar (..),
  BindE (..),
  Exp (..),
  expTy,
  expTy',
  unsafeAnalyzeApp, bvType,
 )
import Language.Purus.IR.Utils (
  Vars,
  allDeclIdentifiers,
  containsBVar,
  deepMapMaybeBound,
  flatBinds,
  foldBinds,
  foldMBinds,
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
  viaExpM, fromExp,
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
  containsHole,
  pattern LiftedHole,
 )
import Language.Purus.Pipeline.Monad (Inline, MonadCounter (next))
import Language.Purus.Pretty.Common (prettyStr)

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
import Control.Lens.Operators ((.=), (^..), (.~), (%=))

import Bound (Var (..))
import Bound.Scope (abstract)

import Prettyprinter (
  Pretty (pretty),
  align,
  hardline,
  vcat,
  (<+>),
 )
import Control.Monad.State.Strict (StateT, evalStateT, execStateT, MonadTrans (..))


inline :: LiftResult -> Inline MonoExp
inline (LiftResult [] bodyE) = pure bodyE 
inline (LiftResult decls bodyE) = do
  declsPrepared <- inlineInLifted decls
  bodyPrepared  <- inlineWithData' declsPrepared bodyE
  let plugHoles :: forall (f :: * -> *). (Functor f) => f (Vars PurusType) -> f (Vars PurusType)
      plugHoles = fmap $ \case
        F (LiftedHole hNm hIndx hTy) -> B (BVar (fromInteger hIndx) hTy (Ident hNm))
        other -> other

      inlinedBodyE =
        abstract (\case B bv -> Just bv; _ -> Nothing)
          . plugHoles
          $ bodyPrepared
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
  let declsRebuilt = map (\((n, i), b) -> NonRecursive n i b) flatLB
  cleanup declsRebuilt inlinedBodyE
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
      absBody    <- addTypeAbstraction body
      let newTypeDict = foldBinds (\acc nm b -> M.insert nm (expTy' id b) acc) M.empty abstracted
          updateTypes idnt inExp = result
           where
             result = deepMapMaybeBound (\bv@(BVar bvi bvt bvn) -> case M.lookup (bvn,bvi) newTypeDict of
               Nothing -> Nothing
               Just t ->
                 let msg = prettify [ "name: " <> prettyStr idnt
                            , "input type:\n" <> prettyStr (expTy id inExp)
                            , "old bv: " <> prettyStr bv
                            , "new bv: " <> prettyStr t
                            --, "output expression:\n" <> prettyStr result
                            , "output expression type:\n" <> prettyStr (expTy id result)]
                 in doTrace "updateTypes" msg $ Just $ uncurry mkBVar (bvn,bvi) t) inExp

          finalDecls = mapBind (\(i,_) inExp -> viaExp (updateTypes i) inExp) <$> abstracted
          finalBody = viaExp (updateTypes (Ident "body")) body
      case abstracted  of
        [] -> pure . toExp $  finalBody
        _ -> pure $ LetE finalDecls finalBody

    addTypeAbstractions :: [MonoBind] -> Inline [MonoBind]
    addTypeAbstractions = traverse (traverseBind (const addTypeAbstraction))

    addTypeAbstraction :: MonoScoped -> Inline MonoScoped
    addTypeAbstraction = viaExpM $ \e -> do
      let e' = transformTypesInExp stripSkolems e
          t = expTy id e'
          free =  freeTypeVariables t
      bvars <- traverse (\(nm, ki) -> next >>= \u -> pure $ BVar u ki (Ident nm)) free
      let result = foldr TyAbs e' (  bvars)
          msg =
            prettify
              [ "INPUT EXPR:\n" <> prettyStr e
              , "INPUT EXPR TY:\n" <> prettyStr t <> "\n\n" <> show (void t)
              , "FREE TY VARS IN INPUT:\n" <> prettyStr free
              , "RESULT:\n" <> prettyStr result
              , "RESULT TY:\n" <> prettyStr (expTy id result)
              ]
      doTraceM "addTypeAbstraction" msg
      pure result

-- sorry koz. in my heart i know you're right about type synonyms, but...
type InlineState a = StateT (Map (Ident, Int) InlineBodyData) Inline a

inlineWithData' :: Map (Ident, Int) InlineBodyData -> MonoExp -> Inline MonoExp
inlineWithData' d e = evalStateT (inlineWithData e) d

handleSelfRecursive :: (Ident, Int) -> InlineBodyData -> Inline (Map (Ident, Int) InlineBodyData)
handleSelfRecursive nm lb@(IsALoopBreaker _) = pure $ M.singleton nm lb
handleSelfRecursive (nm, indx) (NotALoopBreaker body)
  | not (containsHole (nm,indx) $ toExp body) = pure $ M.singleton (nm, indx) (NotALoopBreaker body)
  | otherwise = pure $ M.singleton (nm,indx) (IsALoopBreaker body) {- -do
      u <- next
      let uTxt = T.pack (show u)
          newNm = case nm of
            Ident t -> Ident $ t <> "$B" <> uTxt
            GenIdent (Just t) i -> GenIdent (Just $ t <> "$B" <> uTxt) i -- we only care about a unique ord property for the maps
            GenIdent Nothing i -> GenIdent (Just $ "$B" <> uTxt) i
            other -> other
          bodyTy = expTy' id body
          f = \case
            F (LiftedHole (Ident -> hNm) (fromInteger -> hIx) hTy)
              | hNm == nm && hIx == indx -> F $ LiftedHole  (runIdent newNm) (fromIntegral u) hTy 
            other -> other
          updatedOriginalBody = fmap f body
          updatedOriginalDecl = ((nm, indx), updatedOriginalBody)
          abstr = abstract $ \case B bv -> Just bv; _ -> Nothing
          newBreakerDecl = ((newNm, u), abstr . V .  F $ LiftedHole (runIdent nm)  (fromIntegral indx) bodyTy)
          pretendLet = LetE [Recursive [updatedOriginalDecl, newBreakerDecl]] (abstr . V . B $ BVar indx bodyTy nm)
          msg = prettify ["handleSelfRecursive", "INPUT:\n" <> prettyStr (toExp body), "OUTPUT:\n" <> prettyStr pretendLet]
      doTraceM "handleSelfRecursive" msg 
      pure $ M.fromList [NotALoopBreaker <$> updatedOriginalDecl, IsALoopBreaker <$> newBreakerDecl]
-}
inlineWithData :: MonoExp -> InlineState MonoExp
inlineWithData = transformM go''
  where
    go'' ex = do
      res <- go ex
      let msg = prettify ["INPUT:\n" <> prettyStr ex, "RESULT:\n" <> prettyStr res]
      doTraceM "inlineWithData" msg
      pure res
    go :: MonoExp -> InlineState MonoExp
    go ex =
      get >>= \dict -> case ex of
        fv@(V (F {})) -> case toHole fv of
          Just (Hole hId hIx hTy) -> case M.lookup (hId, hIx) dict of
            Just (NotALoopBreaker (toExp -> e)) -> do
              let msg =
                    prettify
                      [ "INPUT:\n" <> prettyStr fv
                      , "RESULT:\n" <> prettyStr e
                      ]
              doTraceM "inlineWithData" msg
              pure e
            _ -> pure . V . B $ BVar hIx hTy hId 
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

doneInlining :: MonoExp -> Bool
doneInlining me =  null $ mapMaybe (fmap unHole . toHole) (me ^.. cosmos)

prettyDict :: Map (Ident, Int) InlineBodyData -> String
prettyDict =
  show
    . align
    . vcat
    . map (\((i, n), b) -> pretty (runIdent i) <+> "#" <+> pretty n <+> "=" <+> pretty (toExp $ getInlineBody b) <+> hardline)
    . M.toList

inlineInLifted :: [MonoBind] -> Inline (Map (Ident, Int) InlineBodyData)
inlineInLifted decls = do
  let breakers = S.map getLoopBreaker . breakLoops $ allLiftedDependencies
  dict <-
        foldMBinds
          ( \acc nm b ->
              if nm `S.member` breakers
                then pure $ M.insert nm (IsALoopBreaker b) acc
                else do
                  selfRecGroup <- handleSelfRecursive nm (NotALoopBreaker b)
                  pure $ selfRecGroup <> acc
          )
          M.empty
          decls
  res <- flip execStateT dict $ update [] (M.keys dict)
  let msg = prettify ["Input:\n" <> prettyStr decls, "Result:\n" <> prettyDict res, "Breakers:\n" <> prettyStr (S.toList breakers)]
  doTraceM "inlineLifted" msg
  pure res
  where
    update :: [(Ident, Int)] -> [(Ident, Int)] -> InlineState ()
    update [] [] = pure ()
    update retry [] = do
      let msg = "RETRY:\n" <> prettyStr retry
      doTraceM "update" msg
      update [] retry
    update retry (i : is) = do
      doTraceM "update" (prettify ["GO", "RETRY STACK:\n" <> prettyStr retry, "ACTIVE STACK:\n" <> prettyStr (i : is)])
      s <- get
      let eData = s M.! i
          e     = getInlineBody eData
          done1 = doneInlining $ toExp e
      unless done1 $ do
        e' <- go eData
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
    breakLoops adjMap = doTrace "breakLoops" msg result
      where
        result = evalState breakEm adjMap
        msg = "RESULT:\n" <> prettyStr (S.toList result)

        breakEm :: State (AdjacencyMap (Ident, Int)) (Set LoopBreaker)
        breakEm = do
          adjaMap <- get
          let stronglyConnected = gmap fromNonEmpty $ scc adjaMap
              revTopoSorted :: [AdjacencyMap (Ident, Int)]
              revTopoSorted = reverse $ either cycleErr id $ topSort stronglyConnected
          doTraceM "breakEm" ("RevTopoSorted:\n" <> prettyStr (vertexList <$> revTopoSorted))
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

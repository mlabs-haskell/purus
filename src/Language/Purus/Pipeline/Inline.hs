module Language.Purus.Pipeline.Inline where

import Prelude
import Bound.Scope (fromScope, abstract)
import Data.Map qualified as M
import Language.Purus.IR (
  BindE (..),
  Exp (..), expTy, unsafeAnalyzeApp, BVar (..), expTy',
 )
{- import Language.PureScript.CoreFn.Convert.Monomorphize.Utils
    ( freshUnique, Monomorphizer ) -}
import Language.Purus.IR.Utils
import Language.PureScript.CoreFn.FromJSON ()

import Data.Foldable (find, maximumBy)
import Data.Map (Map)
import Control.Monad
import Data.Set (Set)
import Data.Set qualified as S
import Language.Purus.Pipeline.Lift
import Algebra.Graph.AdjacencyMap
    ( gmap, stars, vertexList, AdjacencyMap(..), vertexSet, edgeList, edges )
import Algebra.Graph.AdjacencyMap.Algorithm (scc, topSort, Cycle)
import Algebra.Graph.NonEmpty.AdjacencyMap (fromNonEmpty)
import Control.Lens.Combinators (cosmos, transformM, ix)
import Control.Lens.Operators ((^..), (.=) )
import Language.PureScript.Types
import Language.PureScript.CoreFn.Expr (PurusType)
import Language.PureScript.Constants.Prim qualified as C
import Language.PureScript.Environment (pattern RecordT)
import Bound (Var(..))
import Data.Text qualified as T
import Language.Purus.Debug
import Language.Purus.Pretty.Common (prettyAsStr)
import Control.Monad.State.Strict
import Prettyprinter
import Language.PureScript.Names
import Data.Maybe (mapMaybe)

newtype LoopBreakerScore = LoopBreakerScore {getScore :: ((Ident,Int),Maybe Int)} deriving (Show,Eq,Ord)

newtype LoopBreaker = LoopBreaker {getLoopBreaker :: (Ident,Int)}
  deriving newtype (Show,Eq,Ord,Pretty)

-- NOTE: HANDLE SELF RECURSION LAST

-- We need to keep track of which things are loop breakers and which aren't (so we don't inline the loop breakers).
data InlineBodyData
     = NotALoopBreaker MonoScoped
     | IsALoopBreaker MonoScoped deriving (Show, Eq)

-- idk this is ugly. also isn't there some category theory magic for this?
unBodyData :: InlineBodyData -> (MonoScoped -> InlineBodyData,MonoScoped)
unBodyData = \case
  NotALoopBreaker ms -> (NotALoopBreaker,ms)
  IsALoopBreaker ms  -> (IsALoopBreaker,ms)

getInlineBody :: InlineBodyData -> MonoScoped
getInlineBody = \case
  NotALoopBreaker s -> s
  IsALoopBreaker  s -> s

traverseBodyData :: Applicative f => (MonoScoped -> f MonoScoped) -> InlineBodyData -> f InlineBodyData
traverseBodyData f = \case
  NotALoopBreaker b -> NotALoopBreaker <$> f b
  IsALoopBreaker b -> IsALoopBreaker <$> f b

isALoopBreaker :: InlineBodyData -> Bool
isALoopBreaker = \case IsALoopBreaker{} -> True;_ -> False

notALoopBreaker :: InlineBodyData -> Bool
notALoopBreaker = \case NotALoopBreaker{} -> True;_ -> False

inline :: LiftResult -> Monomorphizer MonoExp
inline (LiftResult decls bodyE) = do
  declsPrepared <- inlineInLifted decls
  let plugHoles :: forall (f :: * -> *). Functor f => f (Vars PurusType) -> f (Vars PurusType)
      plugHoles = fmap $ \case
        F (LiftedHole hNm hIndx hTy) -> B (BVar (fromInteger hIndx) hTy (Ident hNm))
        other -> other

      inlinedBodyE = abstract (\case B bv -> Just bv;_ -> Nothing)
                     . plugHoles
                     . inlineWithData' declsPrepared
                     $ bodyE
      onlyLoopBreakers = M.foldlWithKey' (\acc (n,i) v -> case v of
                                             IsALoopBreaker (plugHoles -> b) ->
                                               NonRecursive n i b:acc
                                             _ -> acc) [] declsPrepared
      flatLB = M.toList $ flatBinds onlyLoopBreakers
  declsSelfRecHandled  <-  map (\((n,i),b) -> NonRecursive n i b)
          . M.toList
          . M.unions
          <$>  traverse (uncurry handleSelfRecursive) flatLB
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
   cleanup :: [MonoBind] -> MonoScoped -> Monomorphizer MonoExp
   cleanup [] body = pure $ toExp body
   cleanup breakers body = do
     abstracted <- addTypeAbstractions breakers
     let newTypeDict = foldBinds (\acc nm b -> M.insert nm (expTy' id b) acc) M.empty abstracted
         updateTypes = deepMapMaybeBound $ \(unBVar -> bv) -> case M.lookup bv newTypeDict of
                         Nothing -> Nothing
                         Just t  -> Just $ uncurry mkBVar bv t
         finalDecls = mapBind (const $ viaExp updateTypes) <$> abstracted
         finalBody  = viaExp updateTypes body
     case finalDecls of
       [] -> pure . toExp $ finalBody
       _  -> pure $ LetE M.empty finalDecls finalBody



   addTypeAbstractions :: [MonoBind] -> Monomorphizer [MonoBind]
   addTypeAbstractions = traverse (traverseBind (const go))
     where
       go :: MonoScoped -> Monomorphizer MonoScoped
       go = viaExpM $ \e -> do
         let e' = transformTypesInExp stripSkolems e
             t = expTy id e'
             free = freeTypeVariables t
         bvars <- traverse (\(nm,ki) -> freshUnique >>= \u -> pure $ BVar u ki (Ident nm)) free
         let result =  foldr TyAbs e' bvars
             msg = prettify [ "INPUT EXPR:\n" <> prettyAsStr e
                            , "INPUT EXPR TY:\n" <> prettyAsStr t <> "\n\n" <> show (void t)
                            , "FREE TY VARS IN INPUT:\n" <> prettyAsStr free
                            , "RESULT:\n" <> prettyAsStr result
                            , "RESULT TY:\n" <> prettyAsStr (expTy id result)
                            ]
         doTraceM "addTypeAbstractions" msg
         pure result



-- sorry koz. in my heart i know you're right about type synonyms, but...
type InlineState a = State (Map (Ident,Int) InlineBodyData) a

inlineWithData' :: Map (Ident,Int) InlineBodyData -> MonoExp -> MonoExp
inlineWithData' d e = evalState (inlineWithData e) d

handleSelfRecursive ::  (Ident,Int) -> MonoScoped -> Monomorphizer (Map (Ident,Int) MonoScoped)
handleSelfRecursive (nm,indx) body
  | not (containsBVar nm indx body) = pure $ M.singleton (nm,indx) body
  | otherwise = do
      u <- freshUnique
      let uTxt = T.pack (show u)
          newNm = case nm of
                    Ident t -> Ident $ t <> "$" <> uTxt
                    GenIdent (Just t) i -> GenIdent (Just $ t <> "$" <> uTxt) i -- we only care about a unique ord property for the maps
                    GenIdent Nothing i ->  GenIdent (Just $ "$" <> uTxt) i
                    other ->  other
          bodyTy = expTy' id body
          f = \case (BVar bvIx bvTy bvNm) ->
                      if  bvIx == indx && bvNm == nm
                      then Just (BVar u bvTy newNm)
                      else Nothing
          updatedOriginalBody = viaExp (deepMapMaybeBound f) body
          updatedOriginalDecl = ((nm,indx), updatedOriginalBody)
          abstr = abstract $ \case {B bv -> Just bv; _ -> Nothing}
          newBreakerDecl      = ((newNm,u), (abstr . V . B $ BVar indx bodyTy nm))
      pure $ M.fromList [updatedOriginalDecl,newBreakerDecl]

inlineWithData :: MonoExp -> InlineState MonoExp
inlineWithData  =  transformM go''
  where
    go'' ex = do
      res <- go ex
      let msg = prettify ["INPUT:\n" <> prettyAsStr ex, "RESULT:\n" <> prettyAsStr res]
      doTraceM "inlineWithData" msg
      pure res
    go :: MonoExp ->  InlineState MonoExp
    go ex = get >>= \dict -> case ex of
      fv@(V (F{})) -> case toHole fv of
        Just (Hole hId hIx _) -> case M.lookup (hId,hIx) dict of
          Just (NotALoopBreaker (toExp -> e)) -> do
             let msg = prettify [ "INPUT:\n" <> prettyAsStr fv
                               , "RESULT:\n" <> prettyAsStr e
                               ]
             doTraceM "inlineWithData" msg
             pure e
          _ -> pure fv
        _ -> pure fv
      AppE e1 e2  -> AppE <$> go e1 <*> go e2
      CaseE t scrut alts -> do
        scrut' <- go scrut
        alts' <- traverse (traverseAlt (viaExpM go)) alts
        pure $ CaseE t scrut' alts'
      LamE bv body -> LamE bv <$> viaExpM go body
      LetE _REMOVE decls body -> do
        decls' <- traverse (traverseBind (\_ b -> viaExpM go b)) decls
        body' <- viaExpM go body
        pure $ LetE _REMOVE decls' body'
      AccessorE x t pss e -> AccessorE x t pss <$> go e
      ObjectUpdateE x t e cf fs ->
        (\e' fs' -> ObjectUpdateE x t e' cf fs')
          <$> go e
          <*> traverse (traverse go) fs
      LitE t lit -> LitE t <$> traverse go lit
      TyInstE t e -> TyInstE t <$> go e
      TyAbs bv e -> TyAbs bv <$> go e
      other -> pure other


doneInlining ::  MonoExp -> InlineState Bool
doneInlining me = do
  dct <- get
  let allInlineable = M.keysSet $ M.filter notALoopBreaker dct
      allHoles = S.fromList $ mapMaybe (fmap unHole . toHole)  (me ^.. cosmos)
      result = S.null $ S.intersection allHoles allInlineable
      msg = prettify [ "Input Expr:\n" <> prettyAsStr me
                     , "All Inlineable Vars:\n" <> prettyAsStr (S.toList allInlineable)
                     , "All Holes in expr:\n" <> prettyAsStr (S.toList allHoles)
                     , "Not yet inlined:\n" <> prettyAsStr (S.toList $ S.intersection allHoles allInlineable)
                     , "Are we done?: " <> show result
                     ]
  doTraceM "doneInlining" msg
  pure result

remainingInlineTargets :: MonoExp -> InlineState (Set (Ident,Int))
remainingInlineTargets me = do
    dct <- get
    let allInlineable = M.keysSet $ M.filter notALoopBreaker dct
        allHoles = S.fromList $ mapMaybe (fmap unHole . toHole)  (me ^.. cosmos)
    pure $ S.intersection allHoles allInlineable

prettyDict :: Map (Ident,Int) InlineBodyData -> String
prettyDict = show
             . align
             . vcat
             . map (\((i,n),b) -> pretty (runIdent i) <+> "#" <+> pretty n <+> "=" <+> pretty (toExp $ getInlineBody b) <+> hardline)
             . M.toList

inlineInLifted :: [MonoBind] -> Monomorphizer (Map (Ident,Int) InlineBodyData)
inlineInLifted decls = do
  let breakers = S.map getLoopBreaker . breakLoops $ allLiftedDependencies
      dict = foldBinds (\acc nm b ->
                          if nm `S.member` breakers
                          then M.insert nm (IsALoopBreaker b) acc
                          else M.insert nm (NotALoopBreaker b) acc)
                       M.empty
                       decls
  let res = flip execState dict $ update [] (M.keys dict)
      msg = prettify ["Input:\n" <> prettyAsStr decls, "Result:\n" <> prettyDict res, "Breakers:\n" <> prettyAsStr (S.toList breakers)]
  doTraceM "inlineLifted" msg
  pure res
 where
   update :: [(Ident,Int)] -> [(Ident,Int)] -> InlineState ()
   update [] [] = pure ()
   update retry [] = do
     let msg = "RETRY:\n" <> prettyAsStr retry
     doTraceM "update" msg
     update [] retry
   update retry (i:is) = do
     doTraceM "update" (prettify ["GO","RETRY STACK:\n" <> prettyAsStr retry, "ACTIVE STACK:\n" <> prettyAsStr (i:is)])
     s <- get
     let e = s M.! i
     done1 <- doneInlining . toExp . getInlineBody $ e
     unless done1 $ do
      e' <- go e
      ix i .= e'
      update (i:retry) is
     when done1 $ do
       update retry is
    where
      go :: InlineBodyData -> InlineState InlineBodyData
      go = traverseBodyData (viaExpM inlineWithData)

   allLiftedDeclarations :: Map (Ident,Int) MonoScoped
   allLiftedDeclarations = flatBinds decls

   allLiftedDependencies :: AdjacencyMap (Ident,Int)
   allLiftedDependencies = stars
                           . M.toList
                           . fmap S.toList
                           $ allLiftedDependenciesMap

   allLiftedDependenciesMap :: Map (Ident,Int) (Set (Ident,Int))
   allLiftedDependenciesMap = foldBinds go M.empty decls
     where
       allDeclIDs :: Set (Ident,Int)
       allDeclIDs = allDeclIdentifiers decls

       go :: Map (Ident,Int) (Set (Ident,Int))
          -> (Ident,Int)
          -> MonoScoped
          -> Map (Ident,Int) (Set (Ident,Int))
       go acc nm scoped =
         let unscoped = toExp scoped
             allComponentHoleIdents = S.fromList $ mapMaybe (fmap unHole . toHole)  (unscoped ^.. cosmos)
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
   breakLoops :: AdjacencyMap (Ident,Int)
              -> Set LoopBreaker
   breakLoops adjMap = doTrace "breakLoops" msg result
    where
      result = evalState breakEm adjMap
      msg = "RESULT:\n" <> prettyAsStr (S.toList result)

      breakEm :: State (AdjacencyMap (Ident,Int)) (Set LoopBreaker)
      breakEm = do
        adjaMap <- get
        let stronglyConnected = gmap fromNonEmpty $ scc adjaMap
            revTopoSorted :: [AdjacencyMap (Ident,Int)]
            revTopoSorted = reverse $ either cycleErr id $ topSort stronglyConnected
        doTraceM "breakEm" ("RevTopoSorted:\n" <> prettyAsStr (vertexList <$> revTopoSorted))
        case find nonTrivialGroup revTopoSorted of
          Nothing -> pure S.empty
          Just target -> do
            let targIdentifiers = vertexList target
                scored          = score adjaMap <$> targIdentifiers
                highScore       = maximumBy (\a b -> let f = snd . getScore in compare (f a) (f b)) scored
            case highScore of
              LoopBreakerScore (_,Nothing) -> error $ "Could not select a loop breaker for decl group:\n" <> show targIdentifiers
              LoopBreakerScore (nm,_) -> do
                removeEdgesTerminatingAt nm
                S.insert (LoopBreaker nm) <$> breakEm
        where
          removeEdgesTerminatingAt :: (Ident,Int) -> State (AdjacencyMap (Ident,Int)) ()
          removeEdgesTerminatingAt nm = do
            s <- get
            let depsEdges    = edgeList s
                depsWinnowed = filter (\x -> snd x /= nm) depsEdges
                newState     = edges depsWinnowed
            put newState

          nonTrivialGroup :: forall x. AdjacencyMap x -> Bool
          nonTrivialGroup x = S.size (vertexSet x) > 1

          cycleErr :: Cycle (AdjacencyMap (Ident,Int)) -> a
          cycleErr cyc      = error $ "Fatal Error: Cannot inline, topologically sorted graph of dependencies contains cycles:\n" <> show cyc

      score :: AdjacencyMap (Ident,Int)
            -> (Ident,Int)
            -> LoopBreakerScore
      score adjaMap nm = adjustScore $ case M.lookup nm allLiftedDeclarations of
              Just (toExp -> e)
                | typeContainsRow e -> LoopBreakerScore (nm,Nothing)
                | not (isMonoType (expTy id e)) -> LoopBreakerScore (nm,Just 0)
                | otherwise -> case e of
                    V _ -> LoopBreakerScore (nm,Just 3)
                    LitE{} -> LoopBreakerScore (nm,Just 3)
                    appE@(AppE{}) -> case unsafeAnalyzeApp appE of
                      (f,_) | isConstructorE f -> LoopBreakerScore (nm,Just 2)
                            | otherwise -> LoopBreakerScore (nm,Just 1)

                    _ -> LoopBreakerScore (nm,Just 1)
              Nothing -> LoopBreakerScore (nm,Nothing)
        where
          {- We want to give higher priority potential loop breakers that "do more breaking" -}
          adjustScore (LoopBreakerScore (nm', Just sc))
            = let nE = length $ filter (\x -> snd x == nm') (edgeList adjaMap)
                  newScore = sc + nE
              in LoopBreakerScore (nm',Just newScore)
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
                RCons{} -> True
                REmpty{}  -> True
                KindedType _ t1 t2 -> isRow t1 || isRow t2
                TypeApp _ t1 t2 -> isRow t1 || isRow t2
                _ -> False
findDeclGroup  ::
  (Ident,Int) ->
  [BindE ty (Exp x ty) a] ->
  Maybe (BindE ty (Exp x ty) a)
findDeclGroup  _ [] = Nothing
findDeclGroup  (ident,indx) (NonRecursive ident' bvix expr : rest)
  | ident == ident' && bvix == indx =  Just $ NonRecursive ident' bvix expr
  | otherwise = findDeclGroup (ident,indx) rest
findDeclGroup nm (Recursive xs : rest) = case find (\x -> fst x == nm) xs of
  Nothing -> findDeclGroup nm rest
  Just _ -> Just (Recursive xs)

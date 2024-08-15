module Language.PureScript.CoreFn.Convert.Inline.Inline where

import Prelude
import Bound.Scope (fromScope, abstract)
import Data.Map qualified as M
import Language.PureScript.CoreFn.Convert.IR (
  BindE (..),
  Exp (..), expTy, unsafeAnalyzeApp, BVar (..), expTy',
 )
import Language.PureScript.CoreFn.Convert.Monomorphize.Utils
import Language.PureScript.CoreFn.Convert.IR.Utils
import Language.PureScript.CoreFn.FromJSON ()

import Data.Foldable (find, maximumBy)
import Data.Map (Map)
import Control.Monad
import Data.Set (Set)
import Data.Set qualified as S
import Language.PureScript.CoreFn.Convert.Inline.Lift
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
import Language.PureScript.CoreFn.Convert.Debug
import Language.PureScript.CoreFn.Pretty.Common (prettyAsStr)
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
  let inlinedBodyE = abstract (\case B bv -> Just bv;_ -> Nothing)
                     . inlineWithData' declsPrepared
                     $ bodyE
      onlyLoopBreakers = M.foldlWithKey' (\acc (n,i) v -> case v of
                                             IsALoopBreaker b ->
                                               NonRecursive n i b:acc
                                             _ -> acc) [] declsPrepared
  pure $ LetE M.empty onlyLoopBreakers inlinedBodyE

-- this probably isn't very performant, we don't memoize or cache
-- intermediate results, which may end up getting recomputed many times
-- NOTE: if I fucked up the loop breaker selection then this will loop forever

-- sorry koz. in my heart i know you're right about type synonyms, but...
type InlineState a = State (Map (Ident,Int) InlineBodyData) a

inlineWithData' :: Map (Ident,Int) InlineBodyData -> MonoExp -> MonoExp
inlineWithData' d e = evalState (inlineWithData e) d

handleSelfRecursive ::  (Ident,Int) -> MonoScoped -> Monomorphizer (Map (Ident,Int) InlineBodyData)
handleSelfRecursive (nm,indx) body = do
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
          updatedOriginalDecl = ((nm,indx),NotALoopBreaker updatedOriginalBody)
          abstr = abstract $ \case {B bv -> Just bv; _ -> Nothing}
          newBreakerDecl      = ((newNm,u),IsALoopBreaker (abstr . V . B $ BVar indx bodyTy nm))
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
      bv@(V (B (BVar bvix _ bvident))) -> case M.lookup (bvident,bvix) dict of
        Just (NotALoopBreaker (toExp -> e)) -> do
           let msg = prettify [ "INPUT:\n" <> prettyAsStr bv
                             , "RESULT:\n" <> prettyAsStr e
                             ]
           doTraceM "inlineWithData" msg
           pure e
        _ -> pure bv
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
      allBoundVarsSet = S.fromList . fmap unBVar . allBoundVars $ me
      result = S.null $ S.intersection allBoundVarsSet allInlineable
      msg = prettify [ "Input Expr:\n" <> prettyAsStr me
                     , "All Inlineable Vars:\n" <> prettyAsStr (S.toList allInlineable)
                     , "All Bound Vars in expr:\n" <> prettyAsStr (S.toList allBoundVarsSet)
                     , "Not yet inlined:\n" <> prettyAsStr (S.toList $ S.intersection allBoundVarsSet allInlineable)
                     , "Are we done?: " <> show result
                     ]
  doTraceM "doneInlining" msg
  pure result 

remainingInlineTargets :: MonoExp -> InlineState (Set (Ident,Int))
remainingInlineTargets me = do
    dct <- get
    let allInlineable = M.keysSet $ M.filter notALoopBreaker dct
        allBoundVarsSet = S.fromList . fmap unBVar . allBoundVars $ me
    pure $ S.intersection allBoundVarsSet allInlineable

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
  let res = flip execState dict $  update [] (M.keys dict)
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
                           $ mapRepr
     where
       mapRepr = foldBinds go M.empty decls

       allDeclIDs :: Set (Ident,Int)
       allDeclIDs = allDeclIdentifiers decls

       go :: Map (Ident,Int) (Set (Ident,Int))
          -> (Ident,Int)
          -> MonoScoped
          -> Map (Ident,Int) (Set (Ident,Int))
       go acc nm scoped =
         let unscoped = join <$> fromScope scoped
             allComponentBinds = S.fromList $ unBVar <$> allBoundVars unscoped
             theseUsedBinds = S.intersection allDeclIDs allComponentBinds
         in M.insert nm theseUsedBinds acc

   flatBinds :: [MonoBind] -> Map (Ident,Int) MonoScoped
   flatBinds = foldBinds (\acc nm scoped -> M.insert nm scoped acc) M.empty

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
                scored          = score <$> targIdentifiers
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

      score :: (Ident,Int)
            -> LoopBreakerScore
      score nm = case M.lookup nm allLiftedDeclarations of
              Just (toExp -> e)
                | typeContainsRow e -> LoopBreakerScore (nm,Nothing)
                | otherwise -> case e of
                    V _ -> LoopBreakerScore (nm,Just 3)
                    LitE{} -> LoopBreakerScore (nm,Just 3)
                    appE@(AppE{}) -> case unsafeAnalyzeApp appE of
                      (f,_) | isConstructorE f -> LoopBreakerScore (nm,Just 2)
                            | otherwise -> LoopBreakerScore (nm,Just 0)
                    _ -> LoopBreakerScore (nm,Just 0)
              Nothing -> LoopBreakerScore (nm,Nothing)
        where
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

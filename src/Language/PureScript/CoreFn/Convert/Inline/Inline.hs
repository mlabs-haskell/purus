module Language.PureScript.CoreFn.Convert.Inline.Inline where

import Prelude
import Bound.Scope (fromScope, abstract)
import Data.Map qualified as M
import Language.PureScript.CoreFn.Convert.IR (
  BindE (..),
  Exp (..), expTy, unsafeAnalyzeApp, BVar (..), expTy',
 )
import Language.PureScript.CoreFn.Convert.Monomorphize.Utils
    ( Monomorphizer,
      unBVar,
      allBoundVars, foldBinds, allDeclIdentifiers, isConstructorE, foldMBindsWith, isSelfRecursiveNR, viaExp, freshUnique, deepMapMaybeBound, toExp, fromExp, viaExpM, traverseAlt, traverseBind )
import Language.PureScript.CoreFn.FromJSON ()
import Language.PureScript.Names (Ident (..))

import Control.Monad.Reader ( join )
import Data.Foldable (find, foldl', maximumBy, traverse_)
import Data.Map (Map)
import Control.Monad
import Data.Set (Set)
import Data.Set qualified as S
import Language.PureScript.CoreFn.Convert.Inline.Lift
import Algebra.Graph.AdjacencyMap
    ( gmap, stars, vertexList, AdjacencyMap(..), vertexSet )
import Algebra.Graph.AdjacencyMap.Algorithm (scc)
import Algebra.Graph.NonEmpty.AdjacencyMap (fromNonEmpty)
import Data.Maybe (catMaybes, mapMaybe)
import Control.Lens.Combinators (cosmos, transform, use, transformM, ix)
import Control.Lens.Operators ((^..), (.=) )
import Language.PureScript.Types
import Language.PureScript.CoreFn.Expr (PurusType)
import Language.PureScript.Constants.Prim qualified as C
import Language.PureScript.Environment (pattern RecordT)
import Prettyprinter (Pretty)
import Bound (Var(..))
import Data.Text qualified as T
import Language.PureScript.CoreFn.Convert.Debug
import Language.PureScript.CoreFn.Pretty.Common (prettyAsStr)
import Control.Monad.State.Strict
import Prettyprinter 
import Language.PureScript.Names 

newtype LoopBreakerScore = LoopBreakerScore {getScore :: ((Ident,Int),Maybe Int)} deriving (Show,Eq,Ord)

newtype LoopBreaker = LoopBreaker {getLoopBreaker :: (Ident,Int)}
  deriving stock (Show,Eq,Ord)
  deriving newtype Pretty

-- We need to keep track of which things are loop breakers and which aren't (so we don't inline the loop breakers).
-- This doesn't matter when we're inlining *within* the mutually recursive binding group, but will when we try to inline
-- expressions in the top-level body scoped by the lifted `let-` bindings
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
                     . inlineWithDataDeep' declsPrepared
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

inlineWithDataDeep' :: Map (Ident,Int) InlineBodyData -> MonoExp -> MonoExp
inlineWithDataDeep' d e = evalState (inlineWithDataDeep e) d

inlineWithDataDeep :: MonoExp -> InlineState MonoExp
inlineWithDataDeep  =  transformM go
  where
    go :: MonoExp ->  InlineState MonoExp
    go ex = get >>= \dict -> case ex of
      bv@(V (B (BVar bvix _ bvident))) -> case M.lookup (bvident,bvix) dict of
        Just (NotALoopBreaker (toExp -> e)) -> do
               let msg = prettify [ "INPUT:\n" <> prettyAsStr bv
                              , "RESULT:\n" <> prettyAsStr e
                              ]
               doTraceM "inlineWithData" msg
               pure e
        Just (IsALoopBreaker (toExp -> e)) -> do
          pure bv
        Nothing -> pure bv
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

inlineWithData' :: Map (Ident,Int) InlineBodyData -> MonoExp -> MonoExp
inlineWithData' d e = evalState (inlineWithData e) d 

inlineWithData :: MonoExp -> InlineState MonoExp
inlineWithData  =  transformM go
  where
    go :: MonoExp ->  InlineState MonoExp
    go ex = get >>= \dict -> case ex of
      bv@(V (B (BVar bvix _ bvident))) -> case M.lookup (bvident,bvix) dict of
        Just (NotALoopBreaker (toExp -> e)) -> do
           let msg = prettify [ "INPUT:\n" <> prettyAsStr bv
                             , "RESULT:\n" <> prettyAsStr e
                             ]
           doTraceM "inlineWithData" msg
           pure e
        Just (IsALoopBreaker (toExp -> e)) -> do
          pure bv
        Nothing -> pure bv
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

prettyDict :: (Map (Ident,Int) InlineBodyData) -> String
prettyDict = show
             . align
             . vcat
             . map (\((i,n),b) -> pretty (runIdent i) <+> "#" <+> pretty n <+> "=" <+> pretty (toExp $ getInlineBody b) <+> hardline)
             . M.toList 
    
inlineInLifted :: [MonoBind] ->  Monomorphizer (Map (Ident,Int) InlineBodyData)
inlineInLifted decls = do
  dict <- foldMBindsWith goNonRec goRec  M.empty decls
  let res = flip execState dict $  update [] (M.keys dict)
      msg = prettify ["Input:\n" <> prettyAsStr decls, "Result:\n" <> prettyDict res]
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
    where
      go :: InlineBodyData -> InlineState InlineBodyData
      go = traverseBodyData (viaExpM inlineWithData) 

   goNonRec :: Map (Ident,Int) InlineBodyData
            -> Ident
            -> Int
            -> MonoScoped
            -> Monomorphizer (Map (Ident,Int) InlineBodyData)
   goNonRec acc nm indx body
     | isSelfRecursiveNR (NonRecursive nm indx body) = M.union acc <$>  handleSelfRecursive (nm,indx) body
     | otherwise = pure $ M.insert (nm,indx) (NotALoopBreaker body) acc

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


   goRec :: Map (Ident,Int) InlineBodyData
         -> [((Ident,Int),MonoScoped)] -- should be a NonEmpty but that's a deep design flaw in the PS compiler that's hard to fix
         -> Monomorphizer (Map (Ident,Int) InlineBodyData)
   goRec acc recGroup = do
     let recGroupDict = flatBinds [Recursive recGroup]
         loopBreakerSet = breakLoops recGroup
         loopBreakerIdents = fmap getLoopBreaker . S.toList $ loopBreakerSet
         loopBreakerDict = M.fromList $ mapMaybe (\nm -> (nm,) . IsALoopBreaker <$> M.lookup nm recGroupDict) loopBreakerIdents
         nonLoopBreakerIdents = S.difference (M.keysSet recGroupDict) (M.keysSet loopBreakerDict)
         nonLoopBreakerDict = M.fromList $ mapMaybe (\nm -> (nm,) . NotALoopBreaker <$> M.lookup nm recGroupDict) (S.toList nonLoopBreakerIdents)
     doTraceM "goRec" ("BREAKERS:\n" <> prettyAsStr (S.toList loopBreakerSet))
     pure $ acc <> loopBreakerDict <> nonLoopBreakerDict

   mkCycleGraph :: [MonoBind] -> AdjacencyMap (Ident,Int)
   mkCycleGraph decls' = stars
                      . M.toList
                      . fmap S.toList
                      $ mapRepr
     where
       mapRepr = foldBinds go M.empty decls'

       allDeclIDs :: Set (Ident,Int)
       allDeclIDs = allDeclIdentifiers decls'

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



   {- I.e. select the necessary number of loop breakers and
           return the set of declarations that will actually be
           inlined (that is: the set with the loop breakers removed)
      NOTE: Since this will be the same at every call site, we should use `inline`
            to construct a static map from declaration identifiers to their inlineable
            declaration groups 
   -}
   breakLoops :: [((Ident,Int),MonoScoped)]
              -> Set LoopBreaker
   breakLoops xs = doTrace "breakLoops" (prettyAsStr . fmap fst $ xs) $ 
     let stronglyConnected = scc $ mkCycleGraph [Recursive xs]
         declMap = flatBinds [Recursive xs]
     in case S.toList $ vertexSet stronglyConnected of
         [] -> S.empty
         -- [_] -> S.empty -- might be wrong, might not work right w/ self-recursive declarations
         _ ->
           let broken :: AdjacencyMap (Set LoopBreaker)
               broken  = gmap (breakEm declMap . fromNonEmpty) stronglyConnected

               result = S.unions $ vertexSet broken
               msg = prettify ["BREAKERS:\n" <> prettyAsStr (S.toList result)
                              ]
           in doTrace "breakLoops" msg result 
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

      score :: Map (Ident,Int) MonoScoped
            -> (Ident,Int)
            -> LoopBreakerScore
      score declMap nm = case M.lookup nm declMap of
              Just (fmap join . fromScope -> e)
                | typeContainsRow e -> LoopBreakerScore (nm,Nothing)
                | otherwise -> case e of
                    V _ -> LoopBreakerScore (nm,Just 3)
                    LitE{} -> LoopBreakerScore (nm,Just 3)
                    appE@(AppE{}) -> case unsafeAnalyzeApp appE of
                      (f,_) | isConstructorE f -> LoopBreakerScore (nm,Just 2)
                            | otherwise -> LoopBreakerScore (nm,Just 0)
                    _ -> LoopBreakerScore (nm,Just 0)
              Nothing -> LoopBreakerScore (nm,Nothing)

      breakEm :: Map (Ident,Int) MonoScoped
              -> AdjacencyMap (Ident,Int)
              -> Set LoopBreaker
      breakEm declMap analyzed = case highScore of
          LoopBreakerScore (nm,Nothing) -> error $ "Could not select a loop breaker for decl group:\n" <> show allNodes
          LoopBreakerScore (nm,_) ->
            let nodesWithoutBreaker = filter (/= nm) allNodes
                nextRound = catMaybes $ foldl' (\acc x -> let x' = (x,) <$> M.lookup x declMap in x' : acc) [] nodesWithoutBreaker
            in S.insert (LoopBreaker nm) $  breakLoops nextRound
        where
          allNodes = vertexList analyzed
          scored   = score declMap <$> allNodes
          highScore = maximumBy (\a b -> let f = snd . getScore in compare (f a) (f b)) scored

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

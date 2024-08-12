module Language.PureScript.CoreFn.Convert.Inline.Inline where

import Prelude
import Bound.Scope (fromScope)
import Data.Map qualified as M
import Language.PureScript.CoreFn.Convert.IR (
  BindE (..),
  Exp (..), expTy, unsafeAnalyzeApp,
 )
import Language.PureScript.CoreFn.Convert.Monomorphize.Utils
    ( Monomorphizer,
      unBVar,
      allBoundVars, foldBinds, allDeclIdentifiers, MonoError (MonoError), isConstructorE )
import Language.PureScript.CoreFn.FromJSON ()
import Language.PureScript.Names (Ident (..))

import Control.Monad.Reader ( join )
import Data.Foldable (find, foldl', maximumBy)
import Data.Map (Map)
import Data.Set (Set)
import Data.Set qualified as S
import Language.PureScript.CoreFn.Convert.Inline.Lift
import Algebra.Graph.AdjacencyMap
    ( gmap, stars, vertexList, AdjacencyMap(..), vertexSet )
import Control.Monad.Except (throwError)
import Algebra.Graph.AdjacencyMap.Algorithm (scc)
import Algebra.Graph.NonEmpty.AdjacencyMap (fromNonEmpty)
import Data.Maybe (catMaybes, mapMaybe)
import Control.Lens.Combinators (cosmos)
import Control.Lens.Operators ((^..))
import Language.PureScript.Types
import Language.PureScript.CoreFn.Expr (PurusType)
import Language.PureScript.Constants.Prim qualified as C
import Language.PureScript.Environment (pattern RecordT)


mkCycleGraph :: [MonoBind] -> AdjacencyMap (Ident,Int)
mkCycleGraph decls = stars
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

newtype LoopBreakerScore = LoopBreakerScore {getScore :: ((Ident,Int),Maybe Int)} deriving (Show,Eq,Ord)

-- N.B. This returns the inlined expression corresponding to the declaration
--      named in the second argument. We'll have to transform or traverse to ensure
--      that everything is inlined.
inline :: [MonoBind] -> (Ident,Int) -> Monomorphizer MonoExp
inline decls targIdent = case findDeclGroup targIdent decls of
  Nothing -> throwError . MonoError
    $ "Couldn't find a declaration for identifier " <> show (fst targIdent) <> "#" <> show (snd targIdent )
  Just declGroup -> case declGroup of
    NonRecursive idnt indx body -> inlineSingleMaybeSelfRec idnt indx body
    Recursive xs -> do
      undefined
 where
   -- Might have to re-unique the bvars and binders?
   inlineSingleMaybeSelfRec = undefined

   {- I.e. select the necessary number of loop breakers and
           return the set of declarations that will actually be
           inlined (that is: the set with the loop breakers removed)
      NOTE: Since this will be the same at every call site, we should use `inline`
            to construct a static map from declaration identifiers to their inlineable
            declaration groups 
   -}
   breakLoops :: [((Ident,Int),MonoScoped)]
              -> [((Ident,Int),MonoScoped)]
   breakLoops xs =
     let stronglyConnected = scc $ mkCycleGraph [Recursive xs]
         declMap = flatBinds [Recursive xs]
     in case S.toList $ vertexSet stronglyConnected of
         [] -> xs
         [_] -> xs
         _ ->
           let broken :: AdjacencyMap [(Ident, Int)]
               broken  = gmap (breakEm declMap . fromNonEmpty) stronglyConnected

               flattened = S.toList . S.fromList . concat . vertexList $ broken
           in mapMaybe (\x -> (x,) <$> M.lookup x declMap) flattened
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
              -> [(Ident,Int)]
      breakEm declMap analyzed = case highScore of
          LoopBreakerScore (nm,Nothing) -> error $ "Could not select a loop breaker for decl group:\n" <> show allNodes
          LoopBreakerScore (nm,_) ->
            let nodesWithoutBreaker = filter (/= nm) allNodes
                nextRound = catMaybes $ foldl' (\acc x -> let x' = (x,) <$> M.lookup x declMap in x' : acc) [] nodesWithoutBreaker
            in fst <$> breakLoops nextRound
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

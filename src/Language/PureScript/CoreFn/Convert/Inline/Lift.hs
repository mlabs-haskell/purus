{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# HLINT ignore "Use if" #-}
{-# HLINT ignore "Use <&>" #-}
-- has to be here (more or less)
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Move concatMap out" #-}

module Language.PureScript.CoreFn.Convert.Inline.Lift where

import Prelude

import Bound.Scope (Scope (..), abstract, fromScope)
import Bound.Var (Var (..))
import Data.Map qualified as M
import Language.PureScript.CoreFn.Convert.IR.Utils
import Language.PureScript.CoreFn.Convert.IR (
  Alt (..),
  BVar (..),
  BindE (..),
  Exp (..),
  FVar (..),
  Lit (..),
  Pat (..),
  expTy', expTy,
 )
import Language.PureScript.CoreFn.Convert.Monomorphize.Utils
    ( Monomorphizer,
      freshUnique,
      findInlineDeclGroup,
    )
import Language.PureScript.CoreFn.Expr (PurusType)
import Language.PureScript.CoreFn.FromJSON ()
import Language.PureScript.CoreFn.TypeLike (
  TypeLike (..),
 )
import Language.PureScript.Names (Ident (..), Qualified (..), QualifiedBy (..), ModuleName (..), runIdent)
import Language.PureScript.PSString (PSString)

import Control.Applicative (Alternative ((<|>)))
import Control.Lens.Operators ((^..))
import Control.Lens.Plated (cosmos, transform)
import Control.Monad.Reader ( join, foldM, asks )
import Data.Bifunctor (Bifunctor (bimap, first))
import Data.Foldable (foldl', toList)
import Data.Map (Map)
import Data.Maybe ( catMaybes, mapMaybe )
import Data.Set (Set)
import Data.Set qualified as S
import Language.PureScript.CoreFn.Convert.Debug
    ( doTraceM, prettify )
import Language.PureScript.CoreFn.Pretty.Common (prettyAsStr, (<::>))
import Prettyprinter ( Pretty(pretty), align, vcat, hardline, indent, (<+>) )
import Control.Monad (void)
import Language.PureScript.Types (Type(..))
import Control.Lens (view, _2, toListOf)
import Data.Text (Text)
import Debug.Trace (trace)
import Control.Lens (over,_1)
-- TODO (IMPORTANT!): Add type abstractions to the declarations with free tyvars and
--                    update the types of the variables which correspond to their
--                    identifiers.

-- sorry Koz i really want to be able to fit type sigs on one line
type MonoExp = Exp WithObjects PurusType (Vars PurusType)
type MonoScoped = Scope (BVar PurusType) (Exp WithObjects PurusType) (Vars PurusType)
type MonoBind = BindE PurusType (Exp WithObjects PurusType) (Vars PurusType)
type MonoAlt = Alt WithObjects PurusType (Exp WithObjects PurusType) (Vars PurusType)

{- The thing that our Lift function gives us.

   This is essentially equivalent to a LetE expression,
   but it's useful to keep the lifted declarations separate from
   the body expression, since we'll be inlining and monomorphizing
   with those declarations.
-}
data LiftResult = LiftResult
  { liftedDecls :: [MonoBind]
  , trimmedExp :: MonoExp
  }

{- When we lift expressions, we have to abstract any types mentioned in the
   expressions which are well-scoped in the original context we are lifting from,
   but are no longer in scope in the lifted expression.

   This will usually turn a non-quantified expression into a quantified expression.

   This is bad. It can (e.g.) cause case expressions that return functions to have
   branches with *different* types. E.g.

   ```haskell
   -- Assume there's a (y :: a) for some a in scope
   let f :: Int -> a -> a
       f x t = (...)
   in case (z :: Maybe Int) of
         Just w -> f w
         Nothing -> \_ -> y

   -- f gets lifted to:
   f :: forall (t1 :: Type). Int -> t1 -> t1
   f = /\(t1 :: Type) -> (...)
   ```

   In a situation like that, we lack sufficient information to "force" the type to its "original"
   form. In this particular example, it is not even clear how we could possibly know what
   `t1` needs to be instantiated to (well ok *we*, i.e., competent human beings, can know it
   but it's not clear that a general procedure for recovering the information from the context exists)

   The easiest solution here is to pack that information into variables. Either bound or free variables
   would work here, but we can use the ModuleName field in the Qualified Ident inside of an FVar to
   very clearly indicate that a "hole" representing an inline target exists.

   This pattern synonym (it's bidirectional btw) allows us to construct and deconstruct a
   representation of those "holes".

   NOTE: This will break a bunch of stuff, as up to this point we haven't been using FVars for anything
         meaningful. But that should be OK, since all of these FVars will be eliminated during inlining
         (and it's very easy to throw a *useful* error if we made a mistake and missed one)
-}
pattern LiftedHole :: Text -> Integer -> t -> FVar t
pattern LiftedHole nm indx ty
  = FVar ty
   ( Qualified
     (ByModuleName (ModuleName "$LIFTED")) -- just has to be an arbitrary illegal module name
     (GenIdent (Just nm) indx)
   )

pattern LiftedHoleTerm :: Text -> Integer -> t -> Exp x t (Vars t)
pattern LiftedHoleTerm nm indx ty = V (F (LiftedHole nm indx ty))

-- concrete representation, avoid manual Text<->Ident & Integer<->Int conversion
data Hole t = Hole Ident Int t

toHole :: Exp x t (Vars t) -> Maybe (Hole t)
toHole (LiftedHoleTerm nm indx ty) = Just $ Hole (Ident nm) (fromIntegral indx) ty
toHole _ = Nothing

fromHole :: Hole t -> Exp x t (Vars t)
fromHole (Hole hId hIx hTy) = LiftedHoleTerm (runIdent hId) (fromIntegral hIx) hTy 

-- dunno if we should ignore the type?
fillsHole :: BVar t -> Exp x t (Vars t) -> Bool
fillsHole (BVar bvIx bvTy bvIdent) = \case
  LiftedHoleTerm (Ident -> i) (fromIntegral -> indx) t -> bvIx == indx && bvIdent == i
  _ -> False

unHole :: Hole t -> (Ident,Int)
unHole (Hole hId hIx _) = (hId,hIx)

instance Pretty LiftResult where
  pretty (LiftResult decls expr) =
    let mkPrettyDeclWithTySig acc (i,u) scoped =
          let unscoped = join <$> fromScope scoped
              ty       = expTy id unscoped
              prettyBody = pretty (NonRecursive i u scoped)
              prettySig  = pretty i <::> pretty ty
              prettyWithSig = align $ vcat [prettySig,prettyBody,hardline]
          in prettyWithSig : acc

        prettyDecls = foldBinds mkPrettyDeclWithTySig [] decls

    in align $ vcat
         [ "let"
         , indent 2 . align . vcat $ prettyDecls
         , "in" <+> align (pretty expr)
         ]

{- Intermediate data type for recording the scope at the
   place where a group of declarations occurs in the AST.

   Without this scope information, lifting is impossible.
-}
data ToLift = ToLift
  { varScopeAtDecl :: Set (BVar PurusType)
  , tyVarScopeAtDecl :: Set (BVar (KindOf PurusType))
  , declarations :: [MonoBind]
  }
  deriving (Show, Eq)

instance Pretty ToLift where
  pretty ToLift {..} =
    pretty $
      prettify
        [ "------ToLift-------"
        , "Var Scope:\n" <> prettyAsStr (S.toList varScopeAtDecl)
        , "Ty Var Scope:\n " <> prettyAsStr (S.toList tyVarScopeAtDecl)
        , "Decls:\n" <> prettyAsStr declarations
        , "-------------------"
        ]


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
deepAnalysis :: [ToLift] -> Map (Ident,Int) (Set (BVar PurusType)) -- high tide, hold priority, crack lion's eye diamond, flashback...
deepAnalysis toLifts = M.mapWithKey go analyses
  where
    go :: (Ident,Int) -> (Set (Ident,Int),Set (BVar PurusType)) -> Set (BVar PurusType)
    go me (dps,theseUnBoundVars)  =
        theseUnBoundVars <> getResult (resolvedDeepChildren S.empty (me,dps))
      where
        getResult :: S.Set (Ident, Int) -> S.Set (BVar PurusType)
        getResult children = S.unions $ snd <$> lookupMany children analyses


        lookupMany :: forall k v t. (Ord k, Foldable t) => t k -> Map k v -> [v]
        lookupMany ks m = mapMaybe (\k -> M.lookup k m) (toList ks)

        resolvedDeepChildren :: S.Set (Ident, Int) -> ((Ident,Int),S.Set (Ident, Int)) -> S.Set (Ident, Int)
        resolvedDeepChildren visited' (nm,deps) =
          let thisStep = mapMaybe (\k -> (k,) . fst <$> M.lookup k analyses) (S.toList deps)
              withThis = S.insert nm visited'
              thisStepWinnowed = filter (\d -> fst d `S.notMember` withThis) thisStep
              newVisited = foldl' (\acc x -> S.insert x acc) withThis (fst <$> thisStep)
           in case thisStepWinnowed of
                [] -> deps
                _ -> deps <> S.unions (resolvedDeepChildren newVisited <$> thisStepWinnowed)

    analyses :: Map (Ident,Int) (Set (Ident,Int), Set (BVar PurusType))
    analyses =
      let allLiftedBinds = concatMap declarations toLifts
      in  foldBinds
            (\acc nm scoped ->
               let oosVars = allNewlyOutOfScopeVars M.! nm
                   liftedDeps = getLiftedPeerDeps  scoped
                   this = (liftedDeps,oosVars)
               in M.insert nm this acc
            )
            M.empty
            allLiftedBinds

    -- FIXME: I think the problem is here? yup it was, keeping this as a reminder i changed it in case breaks
    getLiftedPeerDeps :: MonoScoped ->  Set (Ident,Int)
    getLiftedPeerDeps scoped =
          let unscoped = join <$> fromScope scoped
              allComponentHoleIdents = S.fromList $ mapMaybe (fmap unHole . toHole)  (unscoped ^.. cosmos)
          in  S.intersection allLiftedDeclIdents allComponentHoleIdents

    allLiftedDeclIdents = M.keysSet allNewlyOutOfScopeVars

    allNewlyOutOfScopeVars :: Map (Ident,Int) (Set (BVar PurusType))
    allNewlyOutOfScopeVars = foldMap getNewlyOutOfScopeVars toLifts

    getNewlyOutOfScopeVars :: ToLift -> Map (Ident,Int) (Set (BVar PurusType))
    getNewlyOutOfScopeVars (ToLift varScope _ decls)=
      foldBinds
        (\acc nm body -> M.insert nm (getUnboundVars body) acc)
        M.empty
        decls
      where
       getUnboundVars ::  MonoScoped -> Set (BVar PurusType)
       getUnboundVars scoped = asExp scoped $ \e ->
         foldl'
           (\acc bv ->
              if S.member bv varScope then S.insert bv acc else acc)
           S.empty
           (allBoundVars e)


cleanupLiftedTypes :: LiftResult  -> Monomorphizer LiftResult
cleanupLiftedTypes (LiftResult bs body) = do
  abstracted <- addTypeAbstractions bs
  let refreshTypes = mkRefreshTypes abstracted
      updateVars = viaExp (deepMapMaybeBound refreshTypes)
      bs' = map (mapBind (const updateVars)) abstracted
      body' = deepMapMaybeBound refreshTypes body
  pure $ LiftResult  bs' body'
 where
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

  mkRefreshTypes :: [MonoBind] -> BVar PurusType -> Maybe (BVar PurusType)
  mkRefreshTypes binds = \bv -> M.lookup (unBVar bv) refreshDict
    where
      refreshDict = foldBinds
                      (\acc nm@(i,u) b ->
                         let bv = BVar u (expTy' id b) i
                         in M.insert nm bv acc)
                      M.empty
                      binds

{- See [NOTE: 1] for a rough explanation of what this function does. -}
updateAllBinds :: Map (Ident,Int) (Set (BVar PurusType))
               -> MonoExp
               -> [MonoBind]
               -> Monomorphizer ([MonoBind], MonoExp)
updateAllBinds deepDict prunedBody _binds  = do
  let allLiftedIdents = M.keys deepDict
  allOldToNew <- M.fromList <$> traverse (\nm -> (nm,) <$> mkOldToNew nm) allLiftedIdents
  let adjustedBody = transform (foldl' (\accF nm -> mkUpdateCallSiteBody nm  . accF) id allLiftedIdents) prunedBody
      go nm =
        viaExp $
          updateLiftedLambdas allOldToNew nm
            . updateCallSiteLifted allOldToNew nm

      binds = mapBind go <$> _binds

      msg =
        prettify
          [ "Pruned body:\n " <> prettyAsStr prunedBody
          , "AllDeclIdents:\n " <> prettyAsStr allLiftedIdents
          , "AdjustedBody:\n " <> prettyAsStr adjustedBody
          , "Binds:\n" <> concatMap (\x -> prettyAsStr x <> "\n\n") binds
          , "Deep Dict:\n" <> prettyAsStr (M.toList (S.toList <$> deepDict))
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
          Just hole@(Hole hId hIx hTy) -> case M.lookup (hId, hIx) deepDict of
            Just deep ->
              let liftedWithOldVars = mkUpdateCallSiteLifted deep (hId, hIx) x
                  bvf v = M.lookup v thisOldToNew <|> Just v
                  updatedVarBind = abstract (\case B v -> bvf v; _ -> Nothing) liftedWithOldVars
               in join <$> fromScope updatedVarBind
            Nothing -> x
          _ -> x
        thisOldToNew = allOldToNew M.! declNm -- has to be here if it's in dict


    regenBVar :: forall t. BVar t -> Monomorphizer (BVar t)
    regenBVar (BVar _ bvTy bvIdent) = do
      u <- freshUnique
      pure (BVar u bvTy bvIdent)

    {- Things named `oldToNew` here refer to a Map from variables with their original indices
       to variables with newly generated indices.

       Because (afaict from talking to the Plutus guys) we need to maintain the *global* uniqueness
       of indices, we will have one map for each declaration being lifted.

       `allOldToNew` is used to refer to the global map from identifiers to their `oldToNew` map.
    -}
    mkOldToNew :: (Ident,Int) -> Monomorphizer (Map (BVar PurusType) (BVar PurusType))
    mkOldToNew nm =
      M.fromList
        <$> foldM
          (\acc bv -> do el <- (bv,) <$> regenBVar bv; pure (el : acc))
          []
          (deepDict M.! nm)

    {- Corresponds to (2) in [NOTE 1]-}
    mkUpdateCallSiteLifted :: Set (BVar PurusType) -> (Ident, Int) -> MonoExp -> MonoExp
    mkUpdateCallSiteLifted new (idnt, indx) me = case toHole me of
      Just (Hole hId hIx hTy)
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
    mkUpdateCallSiteBody :: (Ident, Int) ->  MonoExp -> MonoExp
    mkUpdateCallSiteBody nm@(idnt, indx) x = case toHole x of
      Just hole@(Hole hId hIx hTy)
        | hIx == indx && hId == idnt ->
            let deep = S.toList $ deepDict M.! nm
             in foldl' AppE (fromHole hole) (V . B <$> deep)
        | otherwise -> x 
      Nothing -> x 

{- Given a "main" expression (and implicit access to the module context via the
   `Monomorphizer` monad), lift all component declarations, transform their bodies,
   and update all call-sites. 
-}
lift ::
  MonoExp ->
  Monomorphizer LiftResult -- we don't put the expression back together yet b/c it's helpful to keep the pieces separate for monomorphization
lift e = do
  modDict <- mkModDict
  let collectDict = mkDict modDict e
      (toLift, prunedExp,_) = collect S.empty collectDict S.empty S.empty e
      deepDict = deepAnalysis toLift
      liftThese = concatMap declarations toLift
  (binds, body) <- updateAllBinds deepDict prunedExp liftThese
  result <- pure $ LiftResult binds body {- cleanupLiftedTypes $ --  LiftResult binds body -}
  let msg =
        prettify
          [ "Input Expr:\n" <> prettyAsStr e
          , "Pruned Expr:\n" <> prettyAsStr prunedExp
          , "ToLifts:\n" <> prettyAsStr toLift
          , "Binds1:\n" <> prettyAsStr binds
          , "Result\n" <> prettyAsStr result
           ]
  doTraceM "lift" msg
  pure result 
 where

    doneLifting :: Set (Ident,Int) -> Map (Ident,Int) MonoScoped -> MonoExp -> Bool
    doneLifting s d expr = trace "doneLifting" $ go s  eCosmos
      where
        go :: Set (Ident,Int) -> [MonoExp] -> Bool
        go _ [] = True
        go visited (ex:es) =  case ex of
          LetE{} -> False
          V (B (BVar bvIx _ bvId)) ->
            if S.member (bvId,bvIx) visited
            then go visited es
            else case M.lookup (bvId,bvIx) d of
                   Nothing -> True
                   Just declBody ->
                     let visited' = S.insert (bvId,bvIx) visited
                     in doneLifting visited' d (toExp declBody) && go visited' es
          _ -> True

        eCosmos = expr ^.. cosmos

    mkDict :: Map (Ident,Int) MonoScoped
           -> MonoExp
           -> Map (Ident,Int) MonoScoped
    mkDict acc me = trace "mkDict" $ case me of
      V F{} -> acc
      bvE@(V (B (BVar bvIx _ bvId))) -> case doneLifting S.empty acc bvE of
        True -> acc
        False -> case M.lookup (bvId,bvIx) acc of
          Nothing -> acc
          Just declBody -> mkDict acc (toExp declBody)
      LitE _ (ObjectL _ fs) -> foldl' (\ac fld -> mkDict ac (snd fld)) acc fs
      LitE _ _ -> acc
      AppE e1 e2 -> mkDict acc e1 <> mkDict acc e2
      CaseE _ scrut alts ->
        let wScrut = mkDict acc scrut
        in foldl' (\ac (UnguardedAlt _ _ body) -> mkDict ac (toExp body)) wScrut alts
      AccessorE _ _ _ arg -> mkDict acc arg
      ObjectUpdateE _ _ ex _ flds ->
        let wEx = mkDict acc ex
        in foldl' (\ac fld -> mkDict ac (snd fld)) wEx flds
      TyAbs _ ex -> mkDict acc ex
      LamE _ scoped -> mkDict acc (toExp scoped)
      LetE _ decls scoped ->
        let wDeclsTopLevel = foldBinds (\ac nm body -> M.insert nm body ac) acc decls
            wDeclsDeep     = foldBinds (\ac _ body -> mkDict ac (toExp body)) wDeclsTopLevel decls
        in mkDict wDeclsDeep (toExp scoped)
      TyInstE{} -> acc -- TyInst shouldn't exist

    collect ::
      Set (Ident,Int) ->
      Map (Ident,Int) MonoScoped ->
      Set (BVar PurusType) ->
      Set (BVar (KindOf PurusType)) ->
      MonoExp ->
      ([ToLift], MonoExp, Set (Ident,Int))
    collect visited dict boundVars boundTyVars me = trace "collect" $ case me of
      -- we ignore free variables. For us, a free variable more or less represents "shouldn't/can't be inlined"
      V fv@F{} -> ([], V fv, visited)
      V b@(B (BVar bvIx (stripSkolems -> bvTy) bvIdent)) -> case M.lookup (bvIdent,bvIx) dict of
        Nothing -> ([], V b, visited)
        Just declbody
          | S.member (bvIdent,bvIx) visited -> ([],fromHole $ Hole bvIdent bvIx bvTy,visited)
          | otherwise ->
          let visited' = S.insert (bvIdent,bvIx) visited
              (collectedToLift,collectedBody,visited'') = collect visited' dict S.empty S.empty (toExp declbody)
              collectedDecl = NonRecursive bvIdent bvIx (fromExp . stripSkolemsFromExpr $ collectedBody)
              here = ToLift S.empty S.empty [collectedDecl]
              hole = LiftedHoleTerm (runIdent bvIdent) (fromIntegral bvIx) bvTy
          in (here:collectedToLift, hole, visited'')
      LitE t lit -> case lit of
        IntL i -> ([], LitE t (IntL i),visited)
        StringL s -> ([], LitE t (StringL s),visited)
        CharL c -> ([], LitE t (CharL c),visited)
        ObjectL x fs -> case foldl' goField ([], [], visited) fs of
          (bnds, flds, visited') -> (bnds, LitE t $ ObjectL x flds,visited')
      AppE e1 e2 ->
        let (bnds1, e1', vis1) = collect visited dict boundVars boundTyVars e1
            (bnds2, e2', vis2) = collect vis1 dict boundVars boundTyVars e2
         in (bnds1 <> bnds2, AppE e1' e2', vis2)
      CaseE ty scrut alts ->
        let (sBnds, scrut',vis1) = collect visited dict boundVars boundTyVars scrut
            (aBnds, alts',vis2) = collectFromAlts ([], [], vis1) alts
         in (sBnds <> aBnds, CaseE ty scrut' alts',vis2)
      AccessorE x ty fld arg ->
        let (fldBnds, arg',vis1) = collect visited dict boundVars boundTyVars arg
         in (fldBnds, AccessorE x ty fld arg',vis1)
      ObjectUpdateE x ty ex copy flds ->
        let (eBnds, e', vis1) = collect visited dict boundVars boundTyVars ex
            (fldBnds, flds', vis2) = foldl' goField ([], [], vis1) flds
            bnds = eBnds <> fldBnds
         in (bnds, ObjectUpdateE x ty e' copy flds', vis2)
      TyAbs tv ex -> case collect visited dict boundVars (S.insert tv boundTyVars) ex of
        (l,m,r) -> (l,TyAbs tv m, r)
      LamE bv scoped ->
        let (bnds, unscoped,vis1) = collect visited dict (S.insert bv boundVars) boundTyVars (join <$> fromScope scoped)
            rescoped = abstract (\case B bvx -> Just bvx; _ -> Nothing) unscoped
         in (bnds, LamE bv rescoped,vis1)
      LetE _ _decls scoped ->
        let decls = mapBind (const $ viaExp stripSkolemsFromExpr) <$> _decls
            boundVarsPlusDecls = foldBinds (\acc (nm,indx) body -> S.insert (BVar indx (expTy' id body) nm) acc) boundVars decls
            vis1 = foldBinds (\vis nm _ -> S.insert nm vis) visited decls
            (liftedDecls,vis2) = collectFromNestedDeclarations vis1 boundVarsPlusDecls boundTyVars decls
         in over _1 (liftedDecls <>) $ collect vis2 dict  boundVarsPlusDecls boundTyVars (join <$> fromScope scoped)
      -- If we run this directly after core desugaring then there should not be any TyInstEs in the AST
      tInst@TyInstE {} ->
        error $
          "Don't know what to do with a TyInst. Seems like it requires backtracking to handle correctly? Ughhhh\n"
            <> prettyAsStr tInst
      where
        {- We need to "sanitize" the declarations being collected so that any Vars that reference a
           declaration being lifted are transformed into a LiftedHole with the proper type annotation.

           ...also we never properly handled nested `let-` binding groups in the first place and this
           ought to fix that.

           These are all declarations that will be *removed* from their original context, so we don't
           need to return an Exp of any sort (the relevant expressions will be embedded into the decls
           inside the resulting `ToLift`)
        -}
        collectFromNestedDeclarations :: Set (Ident,Int)
                                      -> Set (BVar PurusType)
                                      -> Set (BVar (KindOf PurusType))
                                      -> [MonoBind]
                                      -> ([ToLift],Set (Ident,Int))
        collectFromNestedDeclarations vis termBound typeBound liftThese = trace "collectFromNested" $ foldBinds go ([],vis) liftThese
          where
            go :: ([ToLift],Set (Ident,Int)) -> (Ident,Int) -> MonoScoped -> ([ToLift],Set (Ident,Int))
            go (liftAcc,visAcc) (nm,indx) scoped =
              let (insideLifted,insideBody,visAcc') = collect visAcc dict termBound typeBound (stripSkolemsFromExpr $ toExp scoped)
                  here = ToLift termBound typeBound [NonRecursive nm indx (fromExp insideBody)]
              in (here:(insideLifted <> liftAcc),visAcc')

        goField ::
          ([ToLift], [(PSString, MonoExp)], Set (Ident,Int)) ->
          (PSString, MonoExp) ->
          ([ToLift], [(PSString, MonoExp)], Set (Ident,Int))
        goField (liftAcc, fieldAcc, vis) (nm, fld) = case collect vis dict boundVars boundTyVars fld of
          (bnds, fld',vis1) -> (bnds <> liftAcc,(nm,fld'):fieldAcc,vis1)

        collectFromAlts ::
          ([ToLift], [MonoAlt], Set (Ident,Int)) ->
          [MonoAlt] ->
          ([ToLift], [MonoAlt], Set (Ident,Int))
        collectFromAlts acc [] = acc
        collectFromAlts (liftAcc, altAcc, visAcc) (UnguardedAlt _bs pat scoped : rest) =
          let boundInPat = extractPatVarBinders pat
              boundVars' = foldr S.insert boundVars boundInPat
              (bnds, unscoped,vis1) = collect visAcc dict boundVars' boundTyVars (stripSkolemsFromExpr . toExp $  scoped)
              rescoped = abstract (\case B bvx -> Just bvx; _ -> Nothing) unscoped
              thisAlt = UnguardedAlt _bs pat rescoped
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

usedModuleDecls :: MonoExp -> Monomorphizer [MonoBind]
usedModuleDecls e = do
    modDict <- mkModDict
    let deps = S.fromList
               . filter (`M.member` modDict)
               . mapMaybe (\case (V (B bv)) -> Just (unBVar bv);_ -> Nothing)
               $ directDeps
    let usedIdents = S.toList $ go modDict deps
    pure $ (\nm@(idn,ind) -> NonRecursive idn ind (modDict M.! nm)) <$> usedIdents
  where
    go :: Map (Ident,Int) MonoScoped -> Set (Ident,Int) -> Set (Ident,Int)
    go dict visited =
      let nextRound = S.foldl' (\acc nm -> dict M.! nm:acc) [] visited
          nextRoundDeps = S.fromList
                          . filter (\x -> S.notMember x visited && M.member x dict)
                          . mapMaybe (\case (V (B bv)) -> Just (unBVar bv);_ -> Nothing)
                          $ concatMap (toListOf cosmos . toExp)  nextRound  
      in case S.null nextRoundDeps of
           True -> visited
           False  -> go dict (visited <> nextRoundDeps)

    directDeps = e ^.. cosmos

mkModDict :: Monomorphizer (Map (Ident,Int) MonoScoped)
mkModDict = do
  decls <- view _2
  pure $ foldBinds (\acc nm b  -> M.insert nm b acc) M.empty decls

{- NOTE 1:

   We need three different functions, each of which is (modulo the Monomorphizer monad)
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
      'Monomorphizer' monad. We can generate a Map from the old indices to the new indices "all at once" for the totality of
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


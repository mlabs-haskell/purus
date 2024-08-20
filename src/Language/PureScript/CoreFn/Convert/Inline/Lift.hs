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
import Language.PureScript.Names (Ident (..), Qualified (..), QualifiedBy (..))
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

    getLiftedPeerDeps :: MonoScoped ->  Set (Ident,Int)
    getLiftedPeerDeps scoped =
          let unscoped = join <$> fromScope scoped
              allComponentBoundIdents = S.fromList $ unBVar <$> allBoundVars unscoped
          in  S.intersection allLiftedDeclIdents allComponentBoundIdents

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
        go = \case
          var@(V (B (BVar bvIx _ bvId))) -> case M.lookup (bvId, bvIx) deepDict of
            Just deep ->
              let liftedWithOldVars = mkUpdateCallSiteLifted deep (bvId, bvIx) var
                  bvf v = M.lookup v thisOldToNew <|> Just v
                  updatedVarBind = abstract (\case B v -> bvf v; _ -> Nothing) liftedWithOldVars
               in join <$> fromScope updatedVarBind
            Nothing -> var
          other -> other
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

    {- Correspond to (2) in [NOTE 1]-}
    mkUpdateCallSiteLifted :: Set (BVar PurusType) -> (Ident, Int) -> MonoExp -> MonoExp
    mkUpdateCallSiteLifted new (idnt, indx) = \case
      var@(V (B (BVar bvIndx _ bvIdent)))
        | bvIndx == indx && bvIdent == idnt ->
            foldl' AppE var (V . B <$> S.toList new)
      other -> other

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
    mkUpdateCallSiteBody nm@(idnt, indx) = \case
      var@(V (B (BVar bvIndx _ bvIdent)))
        | bvIndx == indx && bvIdent == idnt ->
            let deep = S.toList $ deepDict M.! nm
             in foldl' AppE var (V . B <$> deep)
      other -> other

{- Given a "main" expression (and implicit access to the module context via the
   `Monomorphizer` monad), lift all component declarations, transform their bodies,
   and update all call-sites. 
-}
lift ::
  MonoExp ->
  Monomorphizer LiftResult -- we don't put the expression back together yet b/c it's helpful to keep the pieces separate for monomorphization
lift e = do
  modDict <- mkModDict
  let (toLift, prunedExp) = collect modDict S.empty S.empty e
      deepDict = deepAnalysis toLift
      liftThese = concatMap declarations toLift
  (binds, body) <- updateAllBinds deepDict prunedExp liftThese
  result <- cleanupLiftedTypes $ LiftResult binds body
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

    collect ::
      Map (Ident,Int) MonoScoped ->
      Set (BVar PurusType) ->
      Set (BVar (KindOf PurusType)) ->
      MonoExp ->
      ([ToLift], MonoExp)
    collect dict boundVars boundTyVars = \case
      -- we ignore free variables. For us, a free variable more or less represents "shouldn't/can't be inlined"
      V fv@F{} -> ([], V fv)
      V b@(B  bv) -> case M.lookup (unBVar bv)  dict of
        Nothing -> ([], V b)
        Just declbody ->
          let BVar bvIx _ bvIdent = bv
              (collectedToLift,collectedBody) = collect dict S.empty S.empty (toExp declbody)
              collectedDecl = NonRecursive bvIdent bvIx (fromExp collectedBody)
              here = ToLift S.empty S.empty [collectedDecl]
          in (here:collectedToLift, V b)
      LitE t lit -> case lit of
        IntL i -> ([], LitE t (IntL i))
        StringL s -> ([], LitE t (StringL s))
        CharL c -> ([], LitE t (CharL c))
        ObjectL x fs -> case foldl' goField ([], []) fs of
          (bnds, flds) -> (bnds, LitE t $ ObjectL x flds)
      AppE e1 e2 ->
        let (bnds1, e1') = collect dict boundVars boundTyVars e1
            (bnds2, e2') = collect dict boundVars boundTyVars e2
         in (bnds1 <> bnds2, AppE e1' e2')
      CaseE ty scrut alts ->
        let (sBnds, scrut') = collect dict boundVars boundTyVars scrut
            (aBnds, alts') = collectFromAlts ([], []) alts
         in (sBnds <> aBnds, CaseE ty scrut' alts')
      AccessorE x ty fld arg ->
        let (fldBnds, arg') = collect dict boundVars boundTyVars arg
         in (fldBnds, AccessorE x ty fld arg')
      ObjectUpdateE x ty ex copy flds ->
        let (eBnds, e') = collect dict boundVars boundTyVars ex
            (fldBnds, flds') = foldl' goField ([], []) flds
            bnds = eBnds <> fldBnds
         in (bnds, ObjectUpdateE x ty e' copy flds')
      TyAbs tv ex -> collect dict boundVars (S.insert tv boundTyVars) ex
      LamE bv scoped ->
        let (bnds, unscoped) = collect dict (S.insert bv boundVars) boundTyVars (join <$> fromScope scoped)
            rescoped = abstract (\case B bvx -> Just bvx; _ -> Nothing) unscoped
         in (bnds, LamE bv rescoped)
      LetE _ decls scoped ->
        let here = ToLift boundVars boundTyVars decls
         in first (here :) $ collect dict boundVars boundTyVars (join <$> fromScope scoped)
      -- If we run this directly after core desugaring then there should be any TyInstEs in the AST
      tInst@TyInstE {} ->
        error $
          "Don't know what to do with a TyInst. Seems like it requires backtracking to handle correctly? Ughhhh\n"
            <> prettyAsStr tInst
      where
        goField ::
          ([ToLift], [(PSString, MonoExp)]) ->
          (PSString, MonoExp) ->
          ([ToLift], [(PSString, MonoExp)])
        goField acc (nm, fld) = case collect dict boundVars boundTyVars fld of
          (bnds, fld') -> bimap (<> bnds) ((nm, fld') :) acc

        collectFromAlts ::
          ([ToLift], [MonoAlt]) ->
          [MonoAlt] ->
          ([ToLift], [MonoAlt])
        collectFromAlts acc [] = acc
        collectFromAlts acc (UnguardedAlt _bs pat scoped : rest) =
          let boundInPat = extractPatVarBinders pat
              boundVars' = foldr S.insert boundVars boundInPat
              (bnds, unscoped) = collect dict boundVars' boundTyVars (join <$> fromScope scoped)
              rescoped = abstract (\case B bvx -> Just bvx; _ -> Nothing) unscoped
              thisAlt = UnguardedAlt _bs pat rescoped
              acc' = bimap (<> bnds) (<> [thisAlt]) acc
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




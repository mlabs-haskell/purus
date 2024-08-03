{-# OPTIONS_GHC -Wno-orphans #-} -- has to be here (more or less)
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use if" #-}
{-# HLINT ignore "Use <&>" #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Language.PureScript.CoreFn.Convert.MonomorphizeV3  where

import Prelude

import Language.PureScript.CoreFn.Expr (PurusType)
import Language.PureScript.CoreFn.Convert.IR
    ( Exp(..),
      FVar(..),
      Lit(..),
      BindE(..),
      BVar(..),
      expTy',
      Alt(..),
      Alt, Pat(..), expTy )
import Language.PureScript.Names (Ident(..), Qualified (..), QualifiedBy (..))
import Language.PureScript.CoreFn.FromJSON ()
import Data.Map qualified as M
import Language.PureScript.PSString (PSString)
import Language.PureScript.CoreFn.Convert.DesugarCore
    ( WithObjects, Vars )
import Bound.Var (Var(..))
import Bound.Scope (fromScope, Scope(..), abstract)
import Language.PureScript.CoreFn.TypeLike
    ( TypeLike(..) )
import Language.PureScript.CoreFn.Convert.Monomorphize.Utils
    ( findInlineDeclGroup,
      freshUnique,
      isBuiltin,
      isConstructor,
      Monomorphizer )
import Control.Lens.Plated ( cosmos, transform )
import Language.PureScript.CoreFn.Pretty.Common (prettyAsStr)
import Control.Lens.Operators ( (^..) )
import Data.Set qualified as S
import Data.Set (Set)
import Data.Bifunctor ( Bifunctor(bimap, first) )
import Data.Maybe
import Data.Foldable (foldl')
import Control.Monad.Reader
import Prettyprinter
import Data.Map (Map)
import Control.Applicative (Alternative((<|>)))
import Data.Functor.Identity (runIdentity)

-- sorry koz i really want to be able to fit type sigs on one line

type MonoExp = Exp WithObjects PurusType (Vars PurusType)
type MonoScoped = Scope (BVar PurusType) (Exp WithObjects PurusType) (Vars PurusType)
type MonoBind = BindE PurusType (Exp WithObjects PurusType) (Vars PurusType)
type MonoAlt = Alt WithObjects PurusType (Exp WithObjects PurusType) (Vars PurusType)


foldL :: Foldable t => b -> t a -> (b -> a -> b) -> b
foldL e xs  f = foldl' f e xs

foldR :: Foldable t => b -> t a -> (a -> b -> b) -> b
foldR e xs f = foldr f e xs

data LiftResult
  = LiftResult {
      liftedDecls :: [MonoBind],
      trimmedExp :: MonoExp
    }

instance Pretty LiftResult where
  pretty (LiftResult decls expr) = pretty $ LetE M.empty decls (abstract (\case {B bv -> Just bv;_ -> Nothing}) expr)

data ToLift = ToLift { varScopeAtDecl :: Set (BVar PurusType)
                     , tyVarScopeAtDecl :: Set (BVar (KindOf PurusType))
                     , declarations :: [MonoBind]}
  deriving (Show, Eq)

allBoundVars :: MonoExp -> [BVar PurusType]
allBoundVars e = S.toList . S.fromList $ flip mapMaybe everything $ \case
    V (B bv) -> Just bv
    _        -> Nothing
  where
    everything = e ^.. cosmos

data Analysis
 = Analysis {
     {- Function that updates the call site of the declaration being analyzed
        in the *BODY* of the original let- binding where the declaration was found.

        In practice this amounts to applying additional arguments for each
        additional argument to the declaration. Since these additional arguments are
        just those variables/constants which are *in-scope* in the original expression,
        we already know what they are.
     -}
     declIdent :: BVar PurusType,
     newOutOfScopeVars :: [BVar PurusType]
     }

mkBVar :: Ident -> Int -> t -> BVar t
mkBVar idnt indx ty = BVar indx ty idnt

unBVar :: BVar t -> (Ident,Int)
unBVar (BVar indx _ idnt) = (idnt,indx)



-- N.B. we're using Set instead of [] mainly to ensure that everything has the same order

allDeclIdentifiers :: [MonoBind] -> Set (Ident,Int)
allDeclIdentifiers [] = S.empty
allDeclIdentifiers (b:rest) = case b of
  NonRecursive nm indx _  -> S.insert (nm,indx) $ allDeclIdentifiers rest
  Recursive xs -> let rest' = allDeclIdentifiers rest
                  in foldl' (\acc ((nm,indx),_) -> S.insert (nm,indx) acc) rest' xs

-- term level dependencies
data Deps = Deps {
       -- The Bound Variable corresponding to the declaration's identifier/index/type.
       -- We'll need this to transform the AST in peer-dependencies
       ddeclIdent :: (Ident,Int),
       ddeclDeps :: Set (Ident,Int)
     } deriving (Show, Eq, Ord)

dependencies :: Set (Ident,Int) -> MonoBind -> Set Deps
dependencies dict = \case
  NonRecursive nm indx scoped -> S.singleton (go nm indx scoped)
  Recursive xs -> S.fromList $ (\((nm,indx),scoped) -> go nm indx scoped) <$> xs
 where
   go nm indx scoped =
    let unscoped = join <$> fromScope scoped
        allComponents = S.fromList $ unBVar <$> allBoundVars unscoped
        declID = (nm,indx)
        deps = S.intersection dict allComponents
    in Deps declID deps

-- TODO: Remove all of the stuff this does that now gets shifted to `updateAllBinds`
analyzeLift :: ToLift ->  ([MonoBind],[Analysis])
analyzeLift (ToLift varScope _ decls) = foldl' go ([],[]) decls
  where
    go :: ([MonoBind],[Analysis]) -> MonoBind -> ([MonoBind],[Analysis])
    go acc = \case
      NonRecursive idnt indx scoped ->
        let (_,_,rescoped,analysis) = doAnalysis idnt indx scoped
        in bimap (NonRecursive idnt indx rescoped:) (analysis:) acc
      Recursive xs ->
        let analyzed = map (\((idnt,indx),scoped) -> doAnalysis idnt indx scoped) xs
            (recbinds,analyses) = foldl' (\acc' (a,b,c,d) -> bimap (((a,b),c):) (d:) acc') ([],[]) analyzed
        in bimap (Recursive recbinds:) (analyses <>) acc

    doAnalysis :: Ident -> Int -> MonoScoped -> (Ident,Int,MonoScoped,Analysis)
    doAnalysis idnt indx scoped =
        let unscoped = join <$> fromScope scoped
            newlyOutOfScopeBVars = foldL [] (allBoundVars unscoped) $ \acc' bv ->
              if S.member bv varScope
              then bv:acc'
              else acc'

            analysis = Analysis  (BVar indx (expTy' id scoped) idnt) newlyOutOfScopeBVars
        in  (idnt,indx,scoped,analysis)

traverseBind :: forall (f :: * -> *)
              . Applicative f
              => ((Ident,Int) -> MonoScoped -> f MonoScoped)
              -> MonoBind
              -> f MonoBind
traverseBind f = \case
    NonRecursive nm i b  -> curry goNonRec nm i b
    Recursive xs -> Recursive <$> traverse (\(nm,body) -> (nm,) <$> f nm body) xs
  where
    goNonRec i@(nm,indx) body = NonRecursive nm indx <$> f i body

mapBind :: ((Ident, Int) -> MonoScoped -> MonoScoped) -> MonoBind -> MonoBind
mapBind f = runIdentity . traverseBind (\a b -> pure $ f a b)


updateAllBinds :: MonoExp -> [MonoBind] -> [Analysis] -> Monomorphizer ([MonoBind],MonoExp)
updateAllBinds prunedBody _binds _analyses = do
    allOldToNew <- traverse  (uncurry mkOldToNew) dict
    let adjustedBody = transform (foldl' (\accF (nm,(d,a)) -> mkUpdateCallSiteBody nm d a . accF) id (M.toList dict)) prunedBody
        go nm = abstract (\case {B bv -> Just bv; _ -> Nothing})
                        . updateLiftedLambdas allOldToNew nm 
                        . updateCallSiteLifted allOldToNew
                        . fmap join
                        . fromScope
        binds = mapBind go  <$> _binds
    pure (binds,adjustedBody)
  where
    updateLiftedLambdas :: Map (Ident,Int) (Map (BVar PurusType) (BVar PurusType))
                        -> (Ident,Int)
                        -> MonoExp
                        -> MonoExp
    updateLiftedLambdas allOldToNew nm e =
      let thisOldToNew = allOldToNew M.! nm
          (d,a)        = dict M.! nm
          deep         = S.toList $ getDeep d a -- this is inefficient but we need to make sure the order matches everywhere
      in mkUpdateLiftedLambdas thisOldToNew deep e 

    updateCallSiteLifted :: Map (Ident,Int) (Map (BVar PurusType) (BVar PurusType))
                         -> MonoExp
                         -> MonoExp
    updateCallSiteLifted allOldToNew =  transform $ \case
      var@(V (B (BVar bvIx _ bvId))) -> case  M.lookup (bvId,bvIx) dict of
        Just (d,a) ->
          let deep = S.toList $ getDeep  d a
              liftedWithOldVars = mkUpdateCallSiteLifted deep (bvId,bvIx) var
              thisOldToNew = allOldToNew M.! (bvId,bvIx) -- has to be here if it's in dict
              bvf v = M.lookup v thisOldToNew <|> Just v
              updatedVarBind = abstract (\case {B v -> bvf v; _ -> Nothing }) liftedWithOldVars
          in join <$> fromScope updatedVarBind
        Nothing -> var
      other -> other

    allDeclIdents :: Set (Ident,Int)
    allDeclIdents = allDeclIdentifiers _binds

    allDeps :: Map (Ident,Int) Deps
    allDeps =
      let rawDeps = S.unions $ dependencies allDeclIdents <$> _binds
      in foldl' (\acc d@Deps{..} -> M.insert ddeclIdent d acc) M.empty rawDeps

    allAnalyses :: Map (Ident,Int) Analysis
    allAnalyses = foldl' (\acc a@Analysis{..} -> M.insert (unBVar declIdent) a acc) M.empty _analyses

    dict :: Map (Ident,Int) (Deps,Analysis)
    dict = foldl' (\acc idnt -> case M.lookup idnt allDeps of
                      Just dep -> case M.lookup idnt allAnalyses of
                        Just analysis -> M.insert idnt (dep,analysis) acc
                        _ -> acc
                      _ -> acc) M.empty allDeclIdents

    getDeep ::  Deps -> Analysis -> S.Set (BVar PurusType)
    getDeep  Deps{..} Analysis{..} =
      let shallow = newOutOfScopeVars
      in foldl' (\acc dnm -> case M.lookup dnm dict of
                                  Just (targDeps,targAna) ->
                                    S.union acc (getDeep targDeps targAna)
                                  _ -> acc
                                ) (S.fromList shallow) ddeclDeps

    regenBVar :: forall t. BVar t -> Monomorphizer (BVar t)
    regenBVar (BVar _ bvTy bvIdent) = do
           u <- freshUnique
           pure (BVar u bvTy bvIdent)

    mkOldToNew ::  Deps -> Analysis -> Monomorphizer (Map (BVar PurusType) (BVar PurusType))
    mkOldToNew d a = M.fromList
               <$> foldM
                     (\acc bv -> do {el <- (bv,) <$> regenBVar bv; pure (el:acc)})
                     []
                     (getDeep  d a)

    -- for *any* occurrance of the expression in itself or in another lifted expression
    mkUpdateCallSiteLifted :: [BVar PurusType] -> (Ident,Int) -> MonoExp -> MonoExp
    mkUpdateCallSiteLifted new (idnt,indx) = \case
          var@(V (B (BVar bvIndx _ bvIdent)))
            | bvIndx == indx && bvIdent == idnt ->
                foldl' AppE var (V . B <$> new)
          other -> other

    -- for the expression being lifted
    mkUpdateLiftedLambdas :: Map (BVar PurusType) (BVar PurusType)
                          -> [BVar PurusType]
                          -> MonoExp
                          -> MonoExp
    mkUpdateLiftedLambdas oldToNew new e = foldl' (\rhs bv -> LamE bv (abstr rhs)) e new
          where
            bvf bv = case M.lookup bv oldToNew of {Just bvnew -> Just bvnew; Nothing -> Just bv;}
            abstr = abstract $ \case {B bv -> bvf bv;_ -> Nothing}

    -- when updating the call site, we don't need to re-index variables b/c the originals *Must* be
    -- in scope at the call site
    mkUpdateCallSiteBody :: (Ident,Int) -> Deps -> Analysis -> MonoExp -> MonoExp
    mkUpdateCallSiteBody nm@(idnt,indx) d a  = \case
     var@(V (B (BVar bvIndx _ bvIdent)))
        | bvIndx == indx && bvIdent == idnt ->
            let deep = S.toList $ getDeep d a
            in  foldl' AppE var (V . B <$> deep)
     other -> other



{- FIXME: Not adding necessary arguments in

-}

lift :: MonoExp
     -> Monomorphizer LiftResult -- we don't put the expression back together yet b/c it's helpful to keep the pieces separate for monomorphization
lift e = do
  (monoBinds1,body) <- updateAllBinds prunedExp monoBinds1' analyses
  monoBinds2 <- usedInScopeDecls
  let monoBinds = monoBinds1 <> monoBinds2
  pure $ LiftResult monoBinds body
 where
    (monoBinds1',analyses) = bimap concat concat . unzip $   map analyzeLift toLift
    (toLift,prunedExp) = collect S.empty S.empty e

    collect :: Set (BVar PurusType)
            -> Set (BVar (KindOf PurusType))
            -> MonoExp
            -> ([ToLift],MonoExp)
    collect boundVars boundTyVars = \case
      V x -> ([],V x)
      LitE t lit -> case lit of
        IntL i -> ([],LitE t (IntL i))
        StringL s -> ([],LitE t (StringL s))
        CharL c   -> ([],LitE t (CharL c))
        ObjectL x fs -> case foldl' goField ([],[]) fs of
              (bnds,flds) -> (bnds,LitE t $ ObjectL x flds)
      AppE e1 e2 ->
          let (bnds1,e1') = collect boundVars boundTyVars e1
              (bnds2,e2') = collect boundVars boundTyVars e2
          in (bnds1 <> bnds2, AppE e1' e2')
      CaseE ty scrut alts ->
          let (sBnds,scrut') = collect boundVars boundTyVars scrut
              (aBnds,alts')  = collectFromAlts ([],[]) alts
          in (sBnds <> aBnds, CaseE ty scrut' alts')
      AccessorE x ty fld arg ->
        let (fldBnds, arg') = collect boundVars boundTyVars arg
        in (fldBnds, AccessorE x ty fld arg')
      ObjectUpdateE x ty ex copy flds ->
        let (eBnds,e') = collect boundVars boundTyVars ex
            (fldBnds,flds') = foldl' goField ([],[]) flds
            bnds = eBnds <> fldBnds
        in (bnds,ObjectUpdateE x ty e' copy flds')
      TyAbs tv ex -> collect boundVars (S.insert tv boundTyVars) ex
      LamE bv scoped ->
        let (bnds,unscoped) = collect (S.insert bv boundVars) boundTyVars (join <$> fromScope scoped)
            rescoped = abstract (\case {B bvx -> Just bvx; _ -> Nothing}) unscoped
        in (bnds,LamE bv rescoped)
      LetE _ decls scoped ->
        let here = ToLift boundVars boundTyVars decls
        in first (here:) $ collect boundVars boundTyVars (join <$> fromScope scoped)
      -- If we run this directly after core desugaring then there should be any TyInstEs in the AST
      tInst@TyInstE{} -> error $ "Don't know what to do with a TyInst. Seems like it requires backtracking to handle correctly? Ughhhh\n"
                                 <> prettyAsStr tInst
     where
       goField :: ([ToLift],[(PSString,MonoExp)])
               -> (PSString, MonoExp)
               -> ([ToLift],[(PSString,MonoExp)])
       goField acc (nm,fld) = case collect boundVars boundTyVars fld of
                (bnds,fld') -> bimap (<> bnds) ((nm,fld'):) acc

       collectFromAlts :: ([ToLift],[MonoAlt])
                       -> [MonoAlt]
                       -> ([ToLift],[MonoAlt])
       collectFromAlts acc [] = acc
       collectFromAlts acc (UnguardedAlt _bs pat scoped:rest) =
         let boundInPat = extractPatVarBinders pat
             boundVars' = foldr S.insert boundVars boundInPat
             (bnds,unscoped) = collect boundVars' boundTyVars (join <$> fromScope scoped)
             rescoped = abstract (\case {B bvx -> Just bvx; _ -> Nothing}) unscoped
             thisAlt = UnguardedAlt _bs pat rescoped
             acc' = bimap (<> bnds) (<> [thisAlt]) acc
         in collectFromAlts acc' rest

       extractPatVarBinders :: Pat WithObjects PurusType (Exp WithObjects PurusType) (Vars PurusType)
                            -> [BVar PurusType]
       extractPatVarBinders = \case
         VarP idnt ix t -> [BVar ix t idnt]
         WildP -> []
         LitP (ObjectL _ ps) -> concatMap (extractPatVarBinders . snd) ps
         _ -> []

    everything = e ^.. cosmos

    -- NOTE: Don't forget to abstract the main expression (much later on when we put it back together)
    usedInScopeDecls :: Monomorphizer [BindE PurusType (Exp WithObjects PurusType) (Vars PurusType)]
    usedInScopeDecls =  catMaybes <$> traverse lookupDecl (mapMaybe  getLiftableIdent everything)


    getLiftableIdent = \case
      V (F (FVar a qi@(Qualified (ByModuleName mn) ident)))
        | not (isConstructor qi || isBuiltin qi) -> Just ident
      _ -> Nothing

    lookupDecl :: Ident
               -> Monomorphizer (Maybe (BindE PurusType (Exp WithObjects PurusType) (Vars PurusType)))
    lookupDecl  ident = do
      -- TODO: Ideally we should have a `Map ModuleName [BindE (...)]`
      --       so we know we're looking in the correct module.
      --       Since we don't have a linker at the moment though, there's not much point right now
            inScopeDecls <- asks snd
            pure $ findInlineDeclGroup ident inScopeDecls


    {- NOTE 1:

       We need three different functions, each of which is (modulo the Monomorphizer monad)
       morally a function :: Exp -> Exp

       1) We need a function that updates call sites in the body of the expression(s) inside the
          scope of the original let- bound declaration we're lifting. This situation is distinct from
          updating the call sites in *other lifted expressions*, because in the original body context,
          we are applying variables which *must* be in scope already, and so should use the *original*
          "was-in-scope-before-lifting-but-is-out-of-scope-after-lifting" BVars (which we have acces to).

       2) We need a function that updates call sites *in other lifted expressions* , i.e. (but not exclusively)
          other members of the mutually recursive binding group in which the declaration being lifted is defined (this
          might also occur for other lifted expressions inside the scope of the binding being lifted).

          Here, we need to generate new unique indices for the variables being applied, so this must be a monadic action.

          Furthermore, because the new(ly re-indexed) variables must be added as lambda arguments on the LHS of *each*
          other lifted declaration, they must (to preserve global uniqueness) be specific to each lifted declaration.


       3) We need a function that modifies the declaration being lifted with additional lambdas (at the *front*, I think that's easier)
          for each new variable which may have been bound in the original context but is free in the lifted declaration
          (prior to this transformation). We need to ensure that:
             - a: The newly introduced binders contain variables with *fresh* indices
             - b: The "stale" variables (which at this point have the original indices from the scope where they
                  are first bound in the context we are lifting *from*) are updated with the corresponding fresh indices
             - c: Any self-recursive calls to the function declaration being lifted are supplied with fresh arguments.


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

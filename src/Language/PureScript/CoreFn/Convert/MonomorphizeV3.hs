{-# OPTIONS_GHC -Wno-orphans #-} -- has to be here (more or less)
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications  #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use if" #-}
{-# HLINT ignore "Use <&>" #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Language.PureScript.CoreFn.Convert.MonomorphizeV3  where

import Prelude

import Language.PureScript.CoreFn.Ann (Ann)
import Language.PureScript.CoreFn.Expr (PurusType)
import Language.PureScript.CoreFn.Module ( Module(..) )
import Language.PureScript.CoreFn.Convert.IR
    ( Exp(..),
      FVar(..),
      Lit(..),
      BindE(..),
      ppExp,
      unsafeAnalyzeApp,
      BVar(..),
      expTy,
      expTy',
      FuncType(..),
      Alt(..),
      Alt, Pat(..) )
import Language.PureScript.Names (Ident(..), Qualified (..), QualifiedBy (..), ModuleName (..), showQualified, showIdent, pattern ByNullSourcePos, runIdent)
import Language.PureScript.Types
    ( RowListItem(..), SourceType, Type(..), replaceTypeVars, isMonoType )
import Language.PureScript.CoreFn.Desugar.Utils ( showIdent' )
import Language.PureScript.Environment (pattern (:->), pattern RecordT, function, getFunArgTy)
import Language.PureScript.CoreFn.Pretty (prettyTypeStr)
import Language.PureScript.CoreFn.FromJSON ()
import Data.Text qualified as T
import Data.List (find)
import Data.Map (Map)
import Data.Map qualified as M
import Language.PureScript.PSString (PSString, prettyPrintString)
import Language.PureScript.AST.SourcePos (SourceAnn)
import Control.Lens
    ( (&) )
import Control.Monad (join, foldM)
import Control.Monad.RWS.Class (MonadReader(ask), MonadState (..), asks)
import Control.Monad.RWS (RWST(..))
import Control.Monad.Except (throwError)
import Language.PureScript.CoreFn.Convert.DesugarCore
    ( desugarCoreModule, WithObjects, IR_Decl, Vars )
import Bound.Var (Var(..))
import Bound.Scope (mapBound, fromScope, toScope, Scope(..), abstract)
import Language.PureScript.CoreFn.TypeLike
    ( TypeLike(..), quantify, getAllInstantiations, instantiateWithArgs )
import Language.PureScript.CoreFn.Convert.Monomorphize.Utils

import Data.Text (Text)
import GHC.IO (throwIO)
import Data.Char (isUpper)
import Control.Lens.Plated
import Prettyprinter (Pretty)
import Language.PureScript.CoreFn.Pretty.Common (prettyAsStr)
import Data.Bifunctor (first)
import Language.PureScript.CoreFn.Convert.Debug
import Control.Lens.Operators
import Data.Set qualified as S
import Data.Set (Set)
import Data.Bifunctor
import Data.Maybe
import Data.Foldable (foldl')
import Control.Monad.Reader
import Prettyprinter
{-
f :: Int -> Bool
f = \x -> h x 3
  where
    h a b = g a <= j 4 b
    j c d = c + g d
    g a = if h a x then j x 1 else x * x

-->

let h a b = g a <= j 4 b
    j c d = c + g d
    g x a = if h a x then j x 1 else x * x
in \(x :: Int) -> h x 3

-}
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
     updateCallSite :: MonoExp ->  MonoExp,
     declIdent :: BVar PurusType
   }

data BindType = RecBind | NonRecBind deriving (Show, Eq)



analyzeLift :: ToLift -> Monomorphizer ([MonoBind],[Analysis])
analyzeLift (ToLift varScope _ decls) = foldM go ([],[]) decls
  where
    regenBVar :: forall t. BVar t -> Monomorphizer (BVar t,BVar t -> BVar t)
    regenBVar (BVar bvOld bvTy bvIdent) = do
      u <- freshUnique
      let f = \case
                BVar bvX bvT bvI | bvX == bvOld -> BVar u bvT bvI
                other -> other
      pure (BVar u bvTy bvIdent,f)


    go :: ([MonoBind],[Analysis]) -> MonoBind -> Monomorphizer ([MonoBind],[Analysis])
    go acc = \case
      NonRecursive idnt indx scoped -> do
        (_,_,rescoped,analysis) <- doAnalysis idnt indx scoped
        pure $ bimap (NonRecursive idnt indx rescoped:) (analysis:) acc
      Recursive xs -> do
        analyzed <- traverse (\((idnt,indx),scoped) -> doAnalysis idnt indx scoped) xs
        let (recbinds,analyses) = foldl' (\acc' (a,b,c,d) -> bimap (((a,b),c):) (d:) acc') ([],[]) analyzed
        pure $ bimap (Recursive recbinds:) (analyses <>) acc

    doAnalysis :: Ident -> Int -> MonoScoped -> Monomorphizer (Ident,Int,MonoScoped,Analysis)
    doAnalysis idnt indx scoped = do
        let unscoped = join <$> fromScope scoped
            newlyOutOfScopeBVars = foldL [] (allBoundVars unscoped) $ \acc' bv ->
              if S.member bv varScope
              then bv:acc'
              else acc'
        bodyAdjusted <- foldM (\rhs bv -> do
                                 (new,f) <- regenBVar bv
                                 let abstr = abstract (\case {B bvX -> Just (f bvX);_ -> Nothing})
                                 pure $ LamE new (abstr rhs)) unscoped newlyOutOfScopeBVars

        let updateCallSiteFun = \case
              AppE var@(V (B (BVar bvIndx _ _))) args | bvIndx == indx ->
                 let newFun = foldl' AppE var (V . B <$> newlyOutOfScopeBVars)
                 in AppE newFun args
              other -> other
            analysis = Analysis updateCallSiteFun (BVar indx (expTy' id scoped) idnt)
            this = abstract (\case {B bv -> Just bv; _ -> Nothing}) bodyAdjusted
        pure (idnt,indx,this,analysis)



lift :: MonoExp
     -> Monomorphizer LiftResult -- we don't put the expression back together yet b/c it's helpful to keep the pieces separate for monomorphization
lift e = do
  (monoBinds1,analyses) <- bimap concat concat . unzip <$> traverse analyzeLift toLift
  let callSiteFun = runAnalyses analyses
      resultBody = transform callSiteFun prunedExp
  monoBinds2 <- usedInScopeDecls
  let monoBinds = monoBinds1 <> monoBinds2
  pure $ LiftResult monoBinds resultBody
 where
    -- TODO: Add traces here if it doesn't miraculously Just Work (TM)
    runAnalyses = foldr (\Analysis{..} acc -> updateCallSite . acc) id

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

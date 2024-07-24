{-# OPTIONS_GHC -Wno-orphans #-} -- has to be here (more or less)
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications  #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use if" #-}
{-# HLINT ignore "Use <&>" #-}
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
      Alt )
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
import Control.Monad.RWS.Class (MonadReader(ask))
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

{- Entry point for inlining monomorphization.

   Broadly, we deduce the monomorphic type for a polymorphic function
   by looking at the arguments the function is applied to. Without
   arguments, we cannot deduce the monomorphic type at all, and so
   this function is `pure` if the provided expression is anything other than
   than an `AppE`
-}
monomorphize ::
  Exp WithObjects PurusType (Var (BVar PurusType) (FVar PurusType))  ->
  Monomorphizer (Exp WithObjects PurusType (Var (BVar PurusType) (FVar PurusType)))
monomorphize  xpr =  case xpr of
  appE@(AppE{}) ->  do
    (f_,args) <- traverse (traverse monomorphize) $ unsafeAnalyzeApp appE
    let (vars,f) = stripTypeAbstractions f_
    if isBuiltinE f || isConstructorE f then do
           let result =  unsafeApply id f args
           doTraceM "monomorphize" ("INPUT:\n" <> prettyAsStr xpr <> "\n\nINPUT TY:\n" <> prettyAsStr (expTy id xpr) <> "\n\nOUTPUT:\n" <> prettyAsStr result <> "\n\nOUTPUT TY:\n" <> prettyAsStr (expTy id result))
           pure result
    else do
      let result = monomorphizeWithBoundTyVars vars f args
      doTraceM "monomorphize" ("INPUT:\n" <> prettyAsStr xpr <> "\n\nINPUT TY:\n" <> prettyAsStr (expTy id xpr) <> "\n\nOUTPUT:\n" <> prettyAsStr result <> "\n\nOUTPUT TY:\n" <> prettyAsStr (expTy id result))
      pure result
  other -> doTrace "monomorphize" ("UNCHANGED:\n" <> ppExp other) $ pure other


monomorphizeWithBoundTyVars :: [(Int,Ident,PurusType)]
                            -> Exp WithObjects PurusType (Vars PurusType)
                            -> [Exp WithObjects PurusType (Vars PurusType)]
                            -> Exp WithObjects PurusType (Vars PurusType)
monomorphizeWithBoundTyVars [] f args = unsafeApply id f args
monomorphizeWithBoundTyVars bvars f args = unsafeApply id monoF args
  where
    monoF = rebuildMonomorphizedFunction instantiations f bvars

    idents = (\(a,b,c) -> runIdent b) <$> bvars

    fT = snd . stripQuantifiers . expTy id $ f
    fTs = splitFunTyParts fT
    argTs = expTy id <$> args

    instantiations = getInstantiations idents fTs argTs

    rebuildMonomorphizedFunction :: M.Map Text PurusType
                                 -> Exp WithObjects PurusType (Vars PurusType)
                                 -> [(Int,Ident,PurusType)]
                                 -> Exp WithObjects PurusType (Vars PurusType)
    rebuildMonomorphizedFunction varMap fE [] = updateTypes (M.toList varMap) fE
    rebuildMonomorphizedFunction varMap fE ((indx,nm,k):rest) = case M.lookup (runIdent nm) varMap of
      Nothing -> TyAbs (BVar indx k nm) $ rebuildMonomorphizedFunction varMap fE rest
      Just t -> rebuildMonomorphizedFunction varMap fE rest


    getInstantiations :: [Text]
                     -> [PurusType]
                     -> [PurusType]
                     -> M.Map Text PurusType
    getInstantiations [] _ _ = M.empty
    getInstantiations _ [] _ = M.empty
    getInstantiations _ _ []  = M.empty
    getInstantiations (var:vars) fs@(fE:fEs) as@(aE:aEs) = case instantiates var aE fE of
      Nothing -> getInstantiations [var] fEs aEs
                 <> getInstantiations vars fs as
      Just t -> M.insert var t $ getInstantiations vars fs as 

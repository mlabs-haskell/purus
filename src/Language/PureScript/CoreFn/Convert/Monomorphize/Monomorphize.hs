{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use if" #-}
{-# HLINT ignore "Use <&>" #-}
module Language.PureScript.CoreFn.Convert.Monomorphize.Monomorphize where
import Prelude

import Language.PureScript.CoreFn.Expr (PurusType)
import Language.PureScript.CoreFn.Convert.IR
    ( Exp(..),
      FVar(..),
      ppExp,
      unsafeAnalyzeApp,
      BVar(..),
      expTy )
import Language.PureScript.Names (Ident(..), runIdent)
import Language.PureScript.CoreFn.FromJSON ()
import Data.Map qualified as M
import Language.PureScript.CoreFn.Convert.DesugarCore
    ( WithObjects, Vars )
import Bound.Var (Var(..))
import Language.PureScript.CoreFn.TypeLike
    ( TypeLike(..) )
import Language.PureScript.CoreFn.Convert.Monomorphize.Utils
import Data.Text (Text)
import Language.PureScript.CoreFn.Pretty.Common (prettyAsStr)
import Language.PureScript.CoreFn.Convert.Debug
import Data.Bifunctor

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
monomorphizeWithBoundTyVars [] f args = doTrace "monomorphizeWithBoundTyVars" msg $ unsafeApply id f args
  where
    msg = prettify ["UNCHANGED", "Fun:\n" <> prettyAsStr f, "Args:\n" <> prettyAsStr args]
monomorphizeWithBoundTyVars bvars f args = doTrace "monomorphizeWithBoundTyVars" msg result 
  where
    msg = prettify [ "CHANGED"
                   , "Fun:\n" <> prettyAsStr f
                   , "Args:\n" <> prettyAsStr args
                   , "Bound Idents:\n" <> prettyAsStr idents
                   , "Fun Ty (stripped):\n" <> prettyAsStr fT
                   , "Arg Types:\n" <> prettyAsStr argTs
                   , "Instantiations:\n" <> prettyAsStr (M.toList instantiations)
                   , "Monomorphized Function:\n" <> prettyAsStr monoF
                   , "Monomorphized Fun ty:\n" <> prettyAsStr (expTy id monoF)
                   , "Result:\n" <> prettyAsStr result
                   , "Result ty:\n" <> prettyAsStr (expTy id result)]

    result = unsafeApply id monoF args

    (instThese,monoF') = rebuildMonomorphizedFunction instantiations f bvars

    monoF = foldr TyInstE monoF' (reverse instThese)

    idents = (\(_,b,_) -> runIdent b) <$> bvars

    fT = snd . stripQuantifiers . expTy id $ f
    fTs = splitFunTyParts fT
    argTs = expTy id <$> args

    instantiations = getInstantiations idents fTs argTs

    rebuildMonomorphizedFunction :: M.Map Text PurusType
                                 -> Exp WithObjects PurusType (Vars PurusType)
                                 -> [(Int,Ident,PurusType)]
                                 -> ([PurusType],Exp WithObjects PurusType (Vars PurusType))
    rebuildMonomorphizedFunction varMap fE [] = ([],fE)
    rebuildMonomorphizedFunction varMap fE ((indx,nm,k):rest) = case M.lookup (runIdent nm) varMap of
      Nothing -> second (TyAbs (BVar indx k nm)) $ rebuildMonomorphizedFunction varMap fE rest
      Just t -> first (t:) $ rebuildMonomorphizedFunction varMap fE rest

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

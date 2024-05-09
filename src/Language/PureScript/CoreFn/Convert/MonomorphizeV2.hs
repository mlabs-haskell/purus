{-# OPTIONS_GHC -Wno-orphans #-} -- has to be here (more or less)
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiWayIf #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use if" #-}
module Language.PureScript.CoreFn.Convert.MonomorphizeV2  where

import Prelude
import Data.Bifunctor
import Data.Maybe

import Language.PureScript.CoreFn.Ann (Ann)
import Language.PureScript.CoreFn.Expr (PurusType)
import Language.PureScript.CoreFn.Module ( Module(..) )
import Language.PureScript.CoreFn.Convert.IR (Exp(..), FVar(..), Alt(..), Lit(..), BindE(..), ppExp, unsafeAnalyzeApp, BVar, Ty, expTy)
import Language.PureScript.Names (Ident(..), Qualified (..), QualifiedBy (..), pattern ByNullSourcePos, ModuleName (..))
import Language.PureScript.Types
    ( rowToList, RowListItem(..), SourceType, Type(..), replaceTypeVars, isMonoType )
import Language.PureScript.CoreFn.Pretty.Common ( analyzeApp )
import Language.PureScript.CoreFn.Desugar.Utils ( showIdent' )
import Language.PureScript.Environment (pattern (:->), pattern ArrayT, pattern RecordT, function, getFunArgTy)
import Language.PureScript.CoreFn.Pretty (prettyTypeStr)
import Language.PureScript.CoreFn.FromJSON ()
import Data.Aeson qualified as Aeson
import Data.Text qualified as T
import Data.List (find, foldl')
import Data.Map (Map)
import Data.Map qualified as M
import Language.PureScript.Label (Label(runLabel))
import Language.PureScript.PSString (PSString, prettyPrintString)
import Language.PureScript.AST.SourcePos (SourceAnn, pattern NullSourceSpan)
import Data.Bitraversable (Bitraversable(bitraverse))
import Control.Lens.IndexedPlated ( itransform, itransformM )
import Control.Lens
    ( Identity(runIdentity),
      (<&>),
      (&),
      (^?),
      preview,
      (^.),
      (.~),
      Ixed(ix), view )
import Control.Monad.RWS.Class (MonadReader(ask), gets, modify')
import Control.Monad.RWS (RWST(..))
import Control.Monad.Except (throwError)
import Control.Exception
import Data.Text (Text)
import Debug.Trace (trace, traceM)
import Language.PureScript.CoreFn.Convert.DesugarCore (WithObjects, desugarCore)
import Bound (fromScope)
import Bound.Var (Var(..))
import Bound.Scope (instantiateEither, Scope)
import Language.PureScript.CoreFn.TypeLike

import Language.PureScript.CoreFn.Convert.Monomorphize.Utils

{- This is the entry point for monomorphization. Typically,
   you will search the module for a 'main' decl and use its
   body as the Exp argument
-}
monomorphizeExpr ::
  Module IR_Decl Ann ->
  Exp WithObjects PurusType (FVar PurusType) ->
  Either MonoError (Exp WithObjects PurusType (FVar PurusType))
monomorphizeExpr m@Module{..} expr =
  runRWST (monomorphizeA  expr) (moduleName,moduleDecls) (MonoState M.empty 0) & \case
    Left err -> Left err
    Right (a,_,_) -> Right a

monomorphizeA ::
  forall a.
  Exp WithObjects PurusType (FVar PurusType)  ->
  Monomorphizer (Exp WithObjects PurusType (FVar PurusType))
monomorphizeA  xpr = trace ("monomorphizeA " <>  "\n  " <> ppExp xpr)  $ case xpr of
  app@(AppE _ arg) ->  do
    let (f,args) = unsafeAnalyzeApp app
    traceM $ "FUN: " <> ppExp f
    traceM $ "ARGS: " <> show (ppExp <$> args)
    let types = concatMap (splitFunTyParts . expTy F)  args
    traceM $ "ARG TYPES:" <> show (prettyTypeStr <$> types)
     -- maybe trace or check that the types match?
     -- need to re-quantify? not sure. CHECK!

    if isBuiltin f
      then pure app
      else either (uncurry gLet) id <$> handleFunction  f args
  other -> pure other
 where
   -- N.B. we need qualified names in the vars to write this, will fix later
   isBuiltin = undefined

   isMonomorphizedVar :: Exp WithObjects PurusType (FVar PurusType)  -> Bool
   isMonomorphizedVar (V (FVar sty  _)) = snd (stripQuantifiers sty) == sty
   isMonomorphizedVar _ = error "IsMonomorphizedVar called on BVar (I think that shouldn't happen and indicates a mistakes?)"

handleFunction ::  Exp WithObjects PurusType (FVar PurusType)
               -> [Exp WithObjects PurusType (FVar PurusType)] -- TODO: List could be empty?
               -> Monomorphizer (Either ([BindE PurusType (Exp WithObjects PurusType) (FVar PurusType)], Exp WithObjects PurusType (FVar PurusType)) (Exp WithObjects PurusType (FVar PurusType)))
-- handleFunction d exp args | isBuiltin exp = trace ("handleFunction: Builtin") $ pure . Right $ unsafeApply exp args
handleFunction  e [] = trace ("handleFunction FIN: " <> ppExp e) $ pure (pure e)
handleFunction  expr@(LamE (ForAll _ _ var _ inner  _) ident body'') (arg:args) = do
  traceM  ("handleFunction abs:\n  " <> ppExp expr <> "\n  " <> show (ppExp <$> (arg:args)))
  let t = expTy F arg
  traceM $ prettyTypeStr t
  let polyArgT = getFunArgTy inner
      -- WRONG! Probably need to check all of the args at once
      doInstantiate = case instantiates var t polyArgT of
                        Just tx -> replaceTypeVars var tx
                        Nothing -> id
      body' = updateVarTy d ident t body''
      cxt   = M.insert ident t d
  handleFunction  body' args  >>= \case
    Left (binds,body) -> do
      let bodyT = exprType body
          funT  = doInstantiate $ function t bodyT
          e' = Abs ann funT  ident body
      pure $ Left (binds, App nullAnn e' arg)
    Right body -> do
      let bodyT = exprType body
          funT  = doInstantiate $ function t bodyT
          e' = Abs ann funT ident body
      pure $ Right $ App nullAnn e' arg -- Abs ann (function t bodyT) ident body)
handleFunction  v@(V (FVar ty  qn)) es = trace ("handleFunction VarGo: " <> ppExp v) $ do
  traceM (ppExp v)
  traceM (show $ ppExp <$> es)
  e' <- either (uncurry gLet) id <$> inlineAs d ty qn
  handleFunction e' es
handleFunction e es | isMonoType (exprType e)  = pure . Right $ unsafeApply e es
handleFunction e es = throwError $ MonoError d
                        $ "Error in handleFunction:\n  "
                        <> ppExp e
                        <> "\n  " <> show (ppExp <$> es)
                        <> "\n  is not an abstraction or variable"

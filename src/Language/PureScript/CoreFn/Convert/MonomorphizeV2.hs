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
import Language.PureScript.CoreFn.Convert.IR (Exp(..), FVar(..), Alt(..), Lit(..), BindE(..), ppExp, unsafeAnalyzeApp, BVar (..), Ty, expTy, expTy', FuncType (..))
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
import Data.Functor.Identity (Identity(..))
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
  runRWST (monomorphize  expr) (moduleName,moduleDecls) (MonoState M.empty 0) & \case
    Left err -> Left err
    Right (a,_,_) -> Right a

monomorphize ::
  forall a.
  Exp WithObjects PurusType (FVar PurusType)  ->
  Monomorphizer (Exp WithObjects PurusType (FVar PurusType))
monomorphize  xpr = trace ("monomorphizeA " <>  "\n  " <> ppExp xpr)  $ case xpr of
  app@(AppE _ arg) ->  do
    let (f,args) = unsafeAnalyzeApp app
    traceM $ "FUN: " <> ppExp f
    traceM $ "ARGS: " <> show (ppExp <$> args)
    let types = concatMap (splitFunTyParts . expTy F)  args
    traceM $ "ARG TYPES:" <> show (prettyTypeStr <$> types)

    if isBuiltin f
      then pure app
      else handleFunction  f args
  other -> pure other
 where
   -- N.B. we need qualified names in the vars to write this, will fix later
   isBuiltin = undefined

   isMonomorphizedVar :: Exp WithObjects PurusType (FVar PurusType)  -> Bool
   isMonomorphizedVar (V (FVar sty  _)) = snd (stripQuantifiers sty) == sty
   isMonomorphizedVar _ = error "IsMonomorphizedVar called on BVar (I think that shouldn't happen and indicates a mistakes?)"

handleFunction ::  Exp WithObjects PurusType (FVar PurusType)
               -> [Exp WithObjects PurusType (FVar PurusType)] -- TODO: List could be empty?
               -> Monomorphizer (Exp WithObjects PurusType (FVar PurusType))
-- handleFunction d exp args | isBuiltin exp = trace ("handleFunction: Builtin") $ pure . Right $ unsafeApply exp args
handleFunction  e [] = trace ("handleFunction FIN: " <> ppExp e) $ pure e
handleFunction  expr@(LamE (ForAll _ _ var _ inner  _) (BVar bvIx bvTy bvIdent) body'') (arg:args) = do
  traceM  ("handleFunction abs:\n  " <> ppExp expr <> "\n  " <> show (ppExp <$> (arg:args)))
  let t = expTy F arg
  traceM $ prettyTypeStr t
  let polyArgT = getFunArgTy inner
      -- WRONG! Probably need to check all of the args at once
      doInstantiate = case instantiates var t polyArgT of
                        Just tx -> replaceTypeVars var tx
                        Nothing -> id
      body' = updateVarTyS bvIx bvIdent t body''
  body <- transverseScopeViaExp (flip handleFunction args) body'
  let bodyT = expTy' F body
      funT  = doInstantiate $ function t bodyT
      firstArgT = headArg funT
      e' = LamE funT (BVar bvIx firstArgT bvIdent) body
  pure $  AppE  e' arg -- Abs ann (function t bodyT) ident body)
handleFunction  v@(V (FVar ty  qn)) es = trace ("handleFunction VarGo: " <> ppExp v) $ do
  traceM (ppExp v)
  traceM (show $ ppExp <$> es)
  e' <- either (uncurry gLet) id <$> inlineAs  ty qn
  handleFunction e' es
handleFunction e es | isMonoType (expTy F e)  = pure $ unsafeApply e es
handleFunction e es = throwError $ MonoError
                        $ "Error in handleFunction:\n  "
                        <> ppExp e
                        <> "\n  " <> show (ppExp <$> es)
                        <> "\n  is not an abstraction or variable"
inlineAs ::
  PurusType ->
  Qualified Ident ->
  Monomorphizer (Either
                  ( [BindE PurusType (Exp WithObjects PurusType) (FVar PurusType)]
                  , Exp WithObjects PurusType (FVar PurusType))
                  (Exp WithObjects PurusType (FVar PurusType)))
-- TODO: Review whether this has any purpose here \/
inlineAs ty nm@(Qualified (ByModuleName (ModuleName "Builtin")) idnt) = trace ("inlineAs BUILTIN:\n  " <> "IDENT: " <> showIdent' idnt <> "\n  TYPE: " <> prettyTypeStr ty)
  $ pure . Right $ undefined -- Var nullAnn ty nm
-- TODO: Probably can inline locally bound variables? FIX: Keep track of local name bindings
inlineAs _ (Qualified (BySourcePos _) ident) = throwError $ MonoError  $ "can't inline bound variable " <> showIdent' ident
inlineAs ty qmn@(Qualified (ByModuleName mn') ident) = trace ("inlineAs: " <> showIdent' ident <> " :: " <>  prettyTypeStr ty) $ ask >>= \(mn,modDict) ->
  if | mn == mn' -> do
      let msg = "Couldn't find a declaration with identifier " <> showIdent' ident <> " to inline as " <> prettyTypeStr ty
      note msg  (findInlineDeclGroup ident modDict) >>= \case
        NonRecursive _ e -> do
          e' <- monomorphizeWithType ty e
          pure . Right $ e'
        Recursive xs -> do
          traceM $ "RECURSIVE GROUP:\n" <> concatMap (\((_,xId),t) -> showIdent' xId <> " :: " <> ppExp t <> "\n") xs
          let msg' = "Target expression with identifier " <> showIdent' ident <> " not found in mutually recursive group"
          (targIdent,targExpr) <- note msg' $ find (\x -> fst x == ident) (first snd <$> xs) -- has to be there
          fresh <- freshen targIdent
          let initialRecDict = M.singleton targIdent (fresh,ty,targExpr)
          dict <- collectRecBinds initialRecDict ty targExpr
          let renameMap = (\(i,t,_) -> (i,t)) <$> dict
              bindingMap = M.elems dict
              cxt = foldl' (\acc (idx,tyx)-> M.insert idx tyx acc)  $ (\(a,b,_) -> (a,b)) <$> M.elems dict
          binds <- traverse (\(newId,newTy,oldE) -> makeBind renameMap newId newTy oldE) bindingMap
          case M.lookup targIdent renameMap of
            Just (newId,newTy) -> pure $ Left (binds, undefined ) {- -Var nullAnn newTy (Qualified ByNullSourcePos newId)) -}
            Nothing -> throwError
                       $ MonoError
                       $ "Couldn't inline " <> showIdent' ident <> " - identifier didn't appear in collected bindings:\n  "  <> show renameMap
     -- TODO: This is a temporary hack to get builtins working w/o a real linker.
     | otherwise -> throwError $ MonoError "Imports aren't supported!"
 where
   makeBind :: Map Ident (Ident,SourceType) ->  Ident -> SourceType -> Exp WithObjects PurusType (FVar PurusType) -> Monomorphizer (BindE PurusType (Exp WithObjects PurusType) (FVar PurusType))
   makeBind renameDict newIdent t e = trace ("makeBind: " <> showIdent' newIdent) $ do
     e' <- updateFreeVars renameDict   <$> monomorphizeWithType t  e
     pure $ NonRecursive newIdent e'

   -- Find a declaration body in the *module* scope
   findDeclarationBody :: Ident -> Monomorphizer (Maybe (Exp WithObjects PurusType (FVar PurusType)))
   findDeclarationBody nm = go <$> getModBinds
    where
      go :: [BindE PurusType (Exp WithObjects PurusType) (FVar PurusType)] -> Maybe (Exp WithObjects PurusType (FVar PurusType))
      go [] = Nothing
      go (b:bs) = case b of
        NonRecursive nm' e -> if nm' == nm then Just e else go bs
        Recursive xs -> case find (\x -> snd (fst x) == nm) xs of
          Nothing -> go bs
          Just ((_,_),e) -> Just e

   {- RECURSIVE BINDINGS

      First, we need to walk the target expression and collect a list of all of the used
      bindings and the type that they must be when monomorphized, and the new identifier for their
      monomorphized/instantiated version. (We *don't* change anything here)
   -}
   collectMany :: Map Ident (Ident, SourceType, Exp WithObjects PurusType (FVar PurusType)) -> PurusType ->  [Exp WithObjects PurusType (FVar PurusType)] -> Monomorphizer (Map Ident (Ident, SourceType, Exp WithObjects PurusType (FVar PurusType)))
   collectMany acc _  [] = trace "collectMany" $ pure acc
   collectMany acc t  (x:xs) = do
     xBinds <- collectRecBinds acc t x
     let acc' = acc <> xBinds
     collectMany acc' t xs

   collectRecFieldBinds :: Map Ident (Ident, SourceType, Exp WithObjects PurusType (FVar PurusType))
                        -> M.Map PSString (RowListItem SourceAnn)
                        -> [(PSString, Exp WithObjects PurusType (FVar PurusType))]
                        -> Monomorphizer (Map Ident (Ident, SourceType, Exp WithObjects PurusType (FVar PurusType)))
   collectRecFieldBinds visited _ [] =  pure visited
   collectRecFieldBinds visited cxt ((lbl,e):rest) = trace "collectRecFieldBinds" $ do
     RowListItem{..} <- note ("No type for field with label " <> T.unpack (prettyPrintString lbl) <> " when collecting record binds")
                          $ M.lookup lbl cxt
     this <- collectRecBinds visited rowListType e
     collectRecFieldBinds (visited <> this) cxt rest

   collectFun :: Map Ident (Ident, SourceType, Exp WithObjects PurusType (FVar PurusType))
              -> Exp WithObjects PurusType (FVar PurusType)
              -> [SourceType]
              -> Monomorphizer (Map Ident (Ident, SourceType, Exp WithObjects PurusType (FVar PurusType)))
   collectFun visited e [t] = trace ("collectFun FIN:\n  " <> ppExp e <> " :: " <> prettyTypeStr t)  $ do
     rest <- collectRecBinds visited t e
     pure $ visited <> rest
   collectFun visited  e@(LamE (ForAll{}) idx body'') (t:ts) = trace ("collectFun:\n  " <> ppExp e <> "\n  " <> prettyTypeStr t <> "\n" <> show ts)  $ do
      let body' = updateVarTyS idx t body''
      collectFun visited  body' ts

   collectFun visited (V (FVar _ (Qualified (ByModuleName _) nm))) (t:ts)= trace ("collectFun VAR: " <> showIdent' nm) $ do
     case M.lookup nm visited of
       Nothing -> do
         let t' = foldr1 function (t:ts)
             msg =  "Couldn't find a declaration with identifier " <> showIdent' nm <> " to inline as " <> prettyTypeStr t
         declBody <- note  msg =<< findDeclarationBody nm
         freshNm <- freshen nm
         let visited' = M.insert nm (freshNm,t',declBody) visited
         collectRecBinds visited' t' declBody
       Just _ -> pure visited

   collectFun _  e _ = throwError $ MonoError $ "Unexpected expression in collectFun:\n  " <> ppExp e

   collectRecBinds ::
     Map Ident (Ident, SourceType, Exp WithObjects PurusType (FVar PurusType)) ->
     PurusType ->
     Exp WithObjects PurusType (FVar PurusType) ->
     Monomorphizer (Map Ident (Ident, SourceType, Exp WithObjects PurusType (FVar PurusType)))
   collectRecBinds visited t e = trace ("collectRecBinds:\n  " <> ppExp e <> "\n  " <> prettyTypeStr t) $ case e of
     LitE _ (ArrayL arr) -> trace "crbARRAYLIT" $ case t of
       ArrayT inner -> do
         innerBinds <- collectMany visited inner  arr
         pure $ visited <> innerBinds
       _ -> throwError $ MonoError ("Failed to collect recursive binds: " <> prettyTypeStr t <> " is not an Array type")
     LitE _ (ObjectL _ fs) -> trace "crbOBJLIT" $ case t of
         RecordT fields -> do
           let fieldMap = mkFieldMap fields
           innerBinds <- collectRecFieldBinds visited fieldMap fs
           pure $ visited <> innerBinds
         _ -> throwError $ MonoError ("Failed to collect recursive binds: " <> prettyTypeStr t <> " is not a Record type")
     LitE _ _  -> trace "crbLIT" $ pure visited
     CtorE _ _ _ _ -> trace "crbCTOR" $ pure visited
     ObjectUpdateE _ _ _ _ updateFields -> trace "crbOBJUPDATE" $ case t of
        RecordT fields -> do
          let fieldMap = mkFieldMap fields
          -- idk. do we need to do something to the original expression or is this always sufficient?
          innerBinds <- collectRecFieldBinds visited fieldMap updateFields
          pure $ visited <> innerBinds
        _ -> throwError $ MonoError  ("Failed to collect recursive binds: " <> prettyTypeStr t <> " is not a Record type")
     AccessorE _ _ _ _ -> trace "crbACCSR" $ pure visited -- idk. given (x.a :: t) we can't say what x is
     absE@(LamE _ _ _) -> trace ("crbABS TOARGS: " <> prettyTypeStr t) $ collectFun visited absE (toArgs t)
     app@(AppE _ e2) -> trace "crbAPP" $ do
       let (f,args) = unsafeAnalyzeApp app
           types = (expTy F <$> args) <> [t]
       funBinds' <- collectFun visited f types  -- collectRecBinds visited funTy d e1
       let funBinds = visited <> funBinds'
       argBinds <- collectRecBinds funBinds (head types)  e2
       pure $ funBinds <> argBinds
     V (FVar _ (Qualified (ByModuleName _) nm)) -> trace ("crbVAR: " <> showIdent' nm)  $ case M.lookup nm visited of
       Nothing -> findDeclarationBody nm >>= \case
         Nothing -> throwError $ MonoError   $ "No declaration correponding to name " <> showIdent' nm <> " found in the module"
         Just ex -> do
           freshNm <- freshen nm
           let this = (freshNm,t,ex)
           pure $ M.insert nm this visited
       Just _ -> pure visited  -- might not be right, might need to check that the types are equal? ugh keeping track of scope is a nightmare
     V (FVar _ (Qualified _ nm)) -> trace ("crbVAR_: " <> showIdent' nm) $ pure visited
     CaseE _ _  alts -> trace "crbCASE" $ do
       let flatAlts = concatMap extractAndFlattenAlts alts
       aInner <- collectMany visited t  flatAlts
       pure $ visited <> aInner
     LetE _ _ ex ->
       -- not sure abt this
       collectRecBinds visited t ex

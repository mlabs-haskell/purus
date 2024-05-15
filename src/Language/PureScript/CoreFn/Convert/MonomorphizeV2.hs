{-# OPTIONS_GHC -Wno-orphans #-} -- has to be here (more or less)
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use if" #-}
module Language.PureScript.CoreFn.Convert.MonomorphizeV2  where

import Prelude
import Data.Bifunctor ( Bifunctor(second) )

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
import Language.PureScript.Names (Ident(..), Qualified (..), QualifiedBy (..), ModuleName (..))
import Language.PureScript.Types
    ( RowListItem(..), SourceType, Type(..), replaceTypeVars, isMonoType )
import Language.PureScript.CoreFn.Desugar.Utils ( showIdent' )
import Language.PureScript.Environment (pattern (:->), pattern ArrayT, pattern RecordT, function, getFunArgTy)
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
import Control.Monad.RWS.Class (MonadReader(ask))
import Control.Monad.RWS (RWST(..))
import Control.Monad.Except (throwError)
import Debug.Trace (trace, traceM)
import Language.PureScript.CoreFn.Convert.DesugarCore (WithObjects)
import Bound (fromScope)
import Bound.Var (Var(..))
import Bound.Scope (Scope (..), toScope, mapBound)
import Language.PureScript.CoreFn.TypeLike
    ( TypeLike(splitFunTyParts, instantiates) )
import Language.PureScript.CoreFn.Convert.Monomorphize.Utils
    ( IR_Decl,
      Monomorphizer,
      MonoState(MonoState),
      MonoError(..),
      transverseScopeViaExp,
      getModBinds,
      note,
      freshen,
      freshBVar,
      qualifyNull,
      gLet,
      updateVarTyS,
      updateVarTyS',
      unsafeApply,
      findInlineDeclGroup,
      mkFieldMap,
      extractAndFlattenAlts,
      joinScope,
      updateFreeVars,
      scopedToExp )

{- This is the entry point for monomorphization. Typically,
   you will search the module for a 'main' decl and use its
   body as the Exp argument
-}
monomorphizeExpr ::
  Module IR_Decl Ann ->
  Exp WithObjects PurusType (FVar PurusType) ->
  Either MonoError (Exp WithObjects PurusType (FVar PurusType))
monomorphizeExpr Module{..} expr =
  runRWST (monomorphize  expr) (moduleName,moduleDecls) (MonoState M.empty 0) & \case
    Left err -> Left err
    Right (a,_,_) -> Right a

monomorphize ::
  Exp WithObjects PurusType (FVar PurusType)  ->
  Monomorphizer (Exp WithObjects PurusType (FVar PurusType))
monomorphize  xpr = trace ("monomorphizeA " <>  "\n  " <> ppExp xpr)  $ case xpr of
  app@(AppE _ _) ->  do
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
   isBuiltin = \case
     V (FVar _ (Qualified (ByModuleName (ModuleName "Builtin")) _ )) -> True
     _ -> False

handleFunction ::  Exp WithObjects PurusType (FVar PurusType)
               -> [Exp WithObjects PurusType (FVar PurusType)] -- TODO: List could be empty?
               -> Monomorphizer (Exp WithObjects PurusType (FVar PurusType))
-- handleFunction d exp args | isBuiltin exp = trace ("handleFunction: Builtin") $ pure . Right $ unsafeApply exp args
handleFunction  e [] = trace ("handleFunction FIN: " <> ppExp e) $ pure e
handleFunction  expr@(LamE (ForAll _ _ var _ inner  _) bv@(BVar bvIx _ bvIdent) body'') (arg:args) = do
  traceM  ("handleFunction abs:\n  " <> ppExp expr <> "\n  " <> show (ppExp <$> (arg:args)))
  let t = expTy F arg
  traceM $ prettyTypeStr t
  let polyArgT = getFunArgTy inner
      -- WRONG! Probably need to check all of the args at once
      doInstantiate = case instantiates var t polyArgT of
                        Just tx -> replaceTypeVars var tx
                        Nothing -> id
      body' = updateVarTyS bv t body''
  body <- transverseScopeViaExp (flip handleFunction args) body'
  let bodyT = expTy' F body
      funT  = doInstantiate $ function t bodyT
      firstArgT = headArg funT
      e' = LamE funT (BVar bvIx firstArgT bvIdent) body
  pure $  AppE  e' arg -- Abs ann (function t bodyT) ident body)
handleFunction  v@(V (FVar ty  qn)) es = trace ("handleFunction VarGo: " <> ppExp v) $ do
  traceM (ppExp v)
  traceM (show $ ppExp <$> es)
  e' <- inlineAs ty qn
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
  Monomorphizer (Exp WithObjects PurusType (FVar PurusType))
-- TODO: Review whether this has any purpose here \/
inlineAs ty nm@(Qualified (ByModuleName (ModuleName "Builtin")) _ ) = do
  pure $ V (FVar ty nm)
-- TODO: Probably can inline locally bound variables? FIX: Keep track of local name bindings
inlineAs ty (Qualified (ByModuleName mn') ident) = trace ("inlineAs: " <> showIdent' ident <> " :: " <>  prettyTypeStr ty) $ ask >>= \(mn,modDict) ->
  if  mn ==  mn' then  do
      let msg = "Couldn't find a declaration with identifier " <> showIdent' ident <> " to inline as " <> prettyTypeStr ty
      note msg  (findInlineDeclGroup ident modDict) >>= \case
        NonRecursive _ e -> do
          e' <- transverseScopeViaExp (monomorphizeWithType ty) e
          scopedToExp e'
        Recursive xs -> do
          let msg' = "Target expression with identifier " <> showIdent' ident <> " not found in mutually recursive group"
          (targIdent,targExpr) <- note msg' $ find (\x -> fst x == ident)  xs -- has to be there
          fresh <- freshen targIdent
          let initialRecDict = M.singleton targIdent (fresh,ty,targExpr)
          dict <- collectRecBinds initialRecDict ty targExpr
          let renameMap = (\(i,t,_) -> (i,t)) <$> dict
              bindingMap = M.elems dict
          binds <- traverse (\(newId,newTy,oldE) -> makeBind renameMap newId newTy oldE) bindingMap
          case M.lookup targIdent renameMap of
            Just (newId,newTy) -> do
              let body = pure (FVar newTy $ qualifyNull newId)
              pure $ gLet binds body
            Nothing -> throwError
                       $ MonoError
                       $ "Couldn't inline " <> showIdent' ident <> " - identifier didn't appear in collected bindings:\n  "  <> show renameMap
     -- TODO: This is a temporary hack to get builtins working w/o a real linker.
     else  throwError $ MonoError "Imports aren't supported yet!"
 where
   makeBind :: Map Ident (Ident,SourceType)
            ->  Ident
            -> SourceType
            -> Scope (BVar PurusType) (Exp WithObjects PurusType) (FVar PurusType)
            -> Monomorphizer (BindE PurusType (Exp WithObjects PurusType) (FVar PurusType))
   makeBind renameDict newIdent t e = trace ("makeBind: " <> showIdent' newIdent) $ do
     e' <- transverseScopeViaExp (fmap (updateFreeVars renameDict) . monomorphizeWithType t)  e
     pure $ NonRecursive newIdent e'

   -- Find a declaration body in the *module* scope
   findDeclarationBody :: Ident -> Monomorphizer (Maybe (Scope (BVar PurusType) (Exp WithObjects PurusType) (FVar PurusType)))
   findDeclarationBody nm = go <$> getModBinds
    where
      go :: [BindE PurusType (Exp WithObjects PurusType) (FVar PurusType)]
         -> Maybe (Scope (BVar PurusType) (Exp WithObjects PurusType) (FVar PurusType))
      go [] = Nothing
      go (b:bs) = case b of
        NonRecursive nm' e -> if nm' == nm then Just e else go bs
        Recursive xs -> case find (\x -> fst x == nm) xs of
          Nothing -> go bs
          Just (_,e) -> Just e

   {- RECURSIVE BINDINGS

      First, we need to walk the target expression and collect a list of all of the used
      bindings and the type that they must be when monomorphized, and the new identifier for their
      monomorphized/instantiated version. (We *don't* change anything here)
   -}
   collectMany :: Map Ident (Ident, SourceType, Scope (BVar PurusType) (Exp WithObjects PurusType) (FVar PurusType))
               -> PurusType
               ->  [Scope (BVar PurusType) (Exp WithObjects PurusType) (FVar PurusType)]
               -> Monomorphizer (Map Ident (Ident, SourceType, Scope (BVar PurusType) (Exp WithObjects PurusType) (FVar PurusType)))
   collectMany acc _  [] = trace "collectMany" $ pure acc
   collectMany acc t  (x:xs) = do
     xBinds <- collectRecBinds acc t x
     let acc' = acc <> xBinds
     collectMany acc' t xs

   collectRecFieldBinds :: Map Ident (Ident, SourceType, Scope (BVar PurusType) (Exp WithObjects PurusType) (FVar PurusType))
                        -> M.Map PSString (RowListItem SourceAnn)
                        -> [(PSString, Scope (BVar PurusType) (Exp WithObjects PurusType) (FVar PurusType))]
                        -> Monomorphizer (Map Ident (Ident, SourceType, Scope (BVar PurusType) (Exp WithObjects PurusType) (FVar PurusType)))
   collectRecFieldBinds visited _ [] =  pure visited
   collectRecFieldBinds visited cxt ((lbl,e):rest) = trace "collectRecFieldBinds" $ do
     RowListItem{..} <- note ("No type for field with label " <> T.unpack (prettyPrintString lbl) <> " when collecting record binds")
                          $ M.lookup lbl cxt
     this <- collectRecBinds visited rowListType e
     collectRecFieldBinds (visited <> this) cxt rest

   collectFun :: Map Ident (Ident, SourceType, Scope (BVar PurusType) (Exp WithObjects PurusType) (FVar PurusType))
              -> Scope (BVar PurusType) (Exp WithObjects PurusType) (FVar PurusType)
              -> [SourceType]
              -> Monomorphizer (Map Ident (Ident, SourceType, Scope (BVar PurusType) (Exp WithObjects PurusType) (FVar PurusType)))
   collectFun visited e [t] = trace ("collectFun FIN:\n  " <> {- -ppExp e <> " :: " -}  prettyTypeStr t)  $ do
     rest <- collectRecBinds visited t e
     pure $ visited <> rest
   collectFun visited  e (t:ts) =
     trace ("collectFun:\n  " <> ppExp (fromScope e) <> "\n  " <> prettyTypeStr t <> "\n" <> show ts)  $
      case fromScope e of
        LamE (ForAll{}) bv  body'' -> do
          let body' = joinScope $ updateVarTyS' bv t  body''
          collectFun visited  body' ts

        (V (F (FVar _ (Qualified (ByModuleName _) nm)))) -> trace ("collectFun VAR: " <> showIdent' nm) $ do
         case M.lookup nm visited of
           Nothing -> do
             let t' = foldr1 function (t:ts)
                 msg =  "Couldn't find a declaration with identifier " <> showIdent' nm <> " to inline as " <> prettyTypeStr t
             declBody <- note  msg =<< findDeclarationBody nm
             freshNm <- freshen nm
             let visited' = M.insert nm (freshNm,t',declBody) visited
             collectRecBinds visited' t' declBody
           Just _ -> pure visited
        other -> throwError $ MonoError $ "Unexpected expression in collectFun:\n  " <> ppExp other
   collectFun _ _ [] = throwError $ MonoError "Ran out of types in collectFun"

   collectRecBinds ::
     Map Ident (Ident, SourceType, Scope (BVar PurusType) (Exp WithObjects PurusType) (FVar PurusType)) ->
     PurusType ->
     Scope (BVar PurusType) (Exp WithObjects PurusType) (FVar PurusType) ->
     Monomorphizer (Map Ident (Ident, SourceType, Scope (BVar PurusType) (Exp WithObjects PurusType) (FVar PurusType)))
   collectRecBinds visited t e = trace ("collectRecBinds:\n  " <> ppExp (fromScope e) <> "\n  " <> prettyTypeStr t) $ case fromScope e of
     LitE _ (ArrayL arr) -> trace "crbARRAYLIT" $ case t of
       ArrayT inner -> do
         innerBinds <- collectMany visited inner (toScope <$> arr)
         pure $ visited <> innerBinds
       _ -> throwError $ MonoError ("Failed to collect recursive binds: " <> prettyTypeStr t <> " is not an Array type")
     LitE _ (ObjectL _ fs) -> trace "crbOBJLIT" $ case t of
         RecordT fields -> do
           let fieldMap = mkFieldMap fields
           innerBinds <- collectRecFieldBinds visited fieldMap (second toScope <$> fs)
           pure $ visited <> innerBinds
         _ -> throwError $ MonoError ("Failed to collect recursive binds: " <> prettyTypeStr t <> " is not a Record type")
     LitE _ _  -> trace "crbLIT" $ pure visited
     CtorE _ _ _ _ -> trace "crbCTOR" $ pure visited
     ObjectUpdateE _ _ _ _ updateFields -> trace "crbOBJUPDATE" $ case t of
        RecordT fields -> do
          let fieldMap = mkFieldMap fields
          -- idk. do we need to do something to the original expression or is this always sufficient?
          innerBinds <- collectRecFieldBinds visited fieldMap (second toScope <$> updateFields)
          pure $ visited <> innerBinds
        _ -> throwError $ MonoError  ("Failed to collect recursive binds: " <> prettyTypeStr t <> " is not a Record type")
     AccessorE _ _ _ _ -> trace "crbACCSR" $ pure visited -- idk. given (x.a :: t) we can't say what x is
     absE@(LamE _ _ _) -> trace ("crbABS TOARGS: " <> prettyTypeStr t) $ collectFun visited (toScope absE) (splitFunTyParts t)
     app@(AppE _ e2) -> trace "crbAPP" $ do
       let (f,args) = unsafeAnalyzeApp app
           types = (expTy id <$> args) <> [t]
       funBinds' <- collectFun visited (toScope f) types  -- collectRecBinds visited funTy d e1
       let funBinds = visited <> funBinds'
       argBinds <- collectRecBinds funBinds (head types) (toScope e2)
       pure $ funBinds <> argBinds
     V (F (FVar _ (Qualified (ByModuleName _) nm))) -> trace ("crbVAR: " <> showIdent' nm)  $ case M.lookup nm visited of
       Nothing -> findDeclarationBody nm >>= \case
         Nothing -> throwError $ MonoError   $ "No declaration correponding to name " <> showIdent' nm <> " found in the module"
         Just ex -> do
           freshNm <- freshen nm
           let this = (freshNm,t,ex)
           pure $ M.insert nm this visited
       Just _ -> pure visited  -- might not be right, might need to check that the types are equal? ugh keeping track of scope is a nightmare
     V (F (FVar _ (Qualified _ nm))) -> trace ("crbVAR_: " <> showIdent' nm) $ pure visited
     CaseE _ _  alts -> trace "crbCASE" $ do
       let flatAlts = concatMap (fmap joinScope . extractAndFlattenAlts) alts
       aInner <- collectMany visited t flatAlts
       pure $ visited <> aInner
     LetE _ _ ex ->
       -- not sure abt this
       collectRecBinds visited t (joinScope ex)
     -- TODO: Figure out what to do w/ bound vars
     V (B bv) -> throwError $ MonoError $  "collectRecBinds: Not sure what to do with BVars yet: " <> show bv



-- I think this one actually requires case analysis? dunno how to do it w/ the lenses in less space (w/o having prisms for types which seems dumb?)
-- This *forces* the expression to have the provided type (and returns nothing if it cannot safely do that)
monomorphizeWithType ::
  PurusType ->
  Exp WithObjects PurusType (FVar PurusType) ->
  Monomorphizer (Exp WithObjects PurusType (FVar PurusType))
monomorphizeWithType ty expr
  | expTy F expr == ty = pure expr
  | otherwise = trace ("monomorphizeWithType:\n  " <> ppExp expr <> "\n  " <> prettyTypeStr ty) $ case expr of
      LitE ty' (ArrayL arr) -> case ty' of
        ArrayT inner -> LitE ty . ArrayL <$> traverse (monomorphizeWithType inner)  arr
        _ -> throwError $ MonoError  ("Failed to collect recursive binds: " <> prettyTypeStr ty <> " is not a Record type")

      LitE _ (ObjectL ext fs) -> case ty of
        RecordT fields -> do
          let fieldMap = mkFieldMap fields
          LitE ty . ObjectL ext <$> monomorphizeFieldsWithTypes fieldMap  fs
        _ -> throwError $ MonoError ("Failed to collect recursive binds: " <> prettyTypeStr ty <> " is not a Record type")

      LitE _ lit -> pure $ LitE ty lit

      CtorE _ tName cName fs -> pure $ CtorE ty tName cName fs

      ObjectUpdateE ext _ orig copyFields updateFields -> case ty of
        RecordT fields -> do
          let fieldMap = mkFieldMap fields
          -- idk. do we need to do something to the original expression or is this always sufficient?
          updateFields' <- monomorphizeFieldsWithTypes fieldMap updateFields
          pure $ ObjectUpdateE ext ty orig copyFields updateFields'
        _ -> throwError $ MonoError ("Failed to collect recursive binds: " <> prettyTypeStr ty <> " is not a Record type")

      -- TODO: IMPORTANT! We need something like 'freshen' for BVar indices
      AccessorE ext _ str e -> pure $ AccessorE ext ty str e -- idk?
      fun@(LamE _ bv@(BVar oldIx _ oldIdent) body) -> trace ("MTABs:\n  " <> ppExp fun <> " :: " <> prettyTypeStr ty) $ do
        case ty of
          (a :-> b) ->  do
              -- REVIEW: If something is weirdly broken w/ bound vars look here first
              freshBV <- freshBVar a
              let replaceBVar = mapBound $ \x -> if x == bv then freshBV else x
                  body' = replaceBVar $ updateVarTyS bv a body
              body'' <- transverseScopeViaExp (monomorphizeWithType b)  body'
              pure $ LamE ty freshBV body''
          _ -> throwError $ MonoError  "Abs isn't a function"

      app@(AppE _ e2) -> trace ("MTAPP:\n  " <> ppExp app) $  do
        let (f,args) = unsafeAnalyzeApp app
        e1' <-  handleFunction f args
        pure $ AppE e1' e2

      V a -> pure $ V a -- idk

      CaseE _ scrut alts -> do
        let f = monomorphizeWithType ty
            goAlt :: Alt WithObjects PurusType (Exp WithObjects PurusType) (FVar PurusType)
                   -> Monomorphizer (Alt WithObjects PurusType (Exp WithObjects PurusType) (FVar PurusType))
            goAlt (UnguardedAlt bindings binders result) =
               UnguardedAlt bindings binders <$> transverseScopeViaExp f result
        CaseE ty scrut <$> traverse goAlt alts

      LetE a binds e -> LetE a binds <$> transverseScopeViaExp (monomorphizeWithType ty) e
  where
    monomorphizeFieldsWithTypes :: M.Map PSString (RowListItem SourceAnn) -> [(PSString, Exp WithObjects PurusType (FVar PurusType))] -> Monomorphizer [(PSString, Exp WithObjects PurusType (FVar PurusType))]
    monomorphizeFieldsWithTypes _ [] = pure []
    monomorphizeFieldsWithTypes cxt ((lbl,e):rest) = do
      RowListItem{..} <- note  ("No type for field with label " <> T.unpack (prettyPrintString lbl) <> " when monomorphizing record")
                         $ M.lookup lbl cxt
      rest' <- monomorphizeFieldsWithTypes cxt rest
      e' <- monomorphizeWithType rowListType e
      pure $ (lbl,e') : rest'

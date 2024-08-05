{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- has to be here (more or less)
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use if" #-}
{-# HLINT ignore "Use <&>" #-}

module Language.PureScript.CoreFn.Convert.Monomorphize.Inline where

import Bound.Scope (Scope (..), abstract, fromScope, mapBound, toScope)
import Bound.Var (Var (..))
import Control.Lens.Combinators (cosmos, filtered)
import Control.Lens.Operators ((^..))
import Control.Monad (foldM, join)
import Control.Monad.Except (throwError)
import Control.Monad.RWS.Class (MonadReader (ask))
import Data.Bifunctor (first)
import Data.List (find)
import Data.Map (Map)
import Data.Map qualified as M
import Data.Text (Text)
import Data.Text qualified as T
import Language.PureScript.AST.SourcePos (SourceAnn)
import Language.PureScript.CoreFn.Convert.Debug (
  doTrace,
  doTraceM,
  prettify,
 )
import Language.PureScript.CoreFn.Convert.DesugarCore (
  Vars,
  WithObjects,
 )
import Language.PureScript.CoreFn.Convert.IR (
  Alt (..),
  BVar (..),
  BindE (..),
  Exp (..),
  FVar (..),
  Lit (..),
  expTy,
  expTy',
  ppExp,
  unsafeAnalyzeApp,
 )
import Language.PureScript.CoreFn.Convert.Monomorphize.Monomorphize (
  getInstantiations,
  monomorphize,
 )
import Language.PureScript.CoreFn.Convert.Monomorphize.Utils (
  MonoError (MonoError),
  Monomorphizer,
  containsBVar,
  extractAndFlattenAlts,
  findInlineDeclGroup,
  freshBVar,
  freshUnique,
  freshen,
  gLet,
  getModBinds,
  inlineable,
  isBuiltin,
  isBuiltinE,
  isConstructor,
  isConstructorE,
  mkFieldMap,
  note,
  stripTypeAbstractions,
  tyAbstractExpr,
  updateFreeVars,
  updateVarTyS,
 )
import Language.PureScript.CoreFn.Desugar.Utils (showIdent')
import Language.PureScript.CoreFn.Expr (PurusType)
import Language.PureScript.CoreFn.FromJSON ()
import Language.PureScript.CoreFn.Pretty (prettyTypeStr)
import Language.PureScript.CoreFn.Pretty.Common (prettyAsStr)
import Language.PureScript.CoreFn.TypeLike (
  TypeLike (..),
 )
import Language.PureScript.Environment (function, pattern RecordT, pattern (:->))
import Language.PureScript.Names (Ident (..), Qualified (..), QualifiedBy (..), runIdent, showIdent, showQualified, pattern ByNullSourcePos)
import Language.PureScript.PSString (PSString, prettyPrintString)
import Language.PureScript.Types (
  RowListItem (..),
  SourceType,
  Type (ForAll),
 )
import Prelude

-- FIXME: Rewrite this
inlineEverything ::
  Exp WithObjects PurusType (Var (BVar PurusType) (FVar PurusType)) ->
  Monomorphizer (Exp WithObjects PurusType (Var (BVar PurusType) (FVar PurusType)))
inlineEverything xp = do
  res <- go xp
  let msg =
        prettify
          [ "INPUT:\n" <> prettyAsStr xp
          , "INPUT TY:\n" <> prettyAsStr (expTy id xp)
          , "OUTPUT:\n" <> prettyAsStr res
          , "OUTPUT TY:\n" <> prettyAsStr (expTy id res)
          ]
  doTraceM "inlineEverything" msg
  pure res
  where
    go ::
      Exp WithObjects PurusType (Var (BVar PurusType) (FVar PurusType)) ->
      Monomorphizer (Exp WithObjects PurusType (Var (BVar PurusType) (FVar PurusType)))
    go = \case
      (V (F (FVar ty qi))) | not (isBuiltin qi || isConstructor qi) -> do
        step1 <- inlineAs ty qi
        let fvars1 =
              step1
                ^.. cosmos
                  . filtered (\case V _ -> True; _ -> False)
                  . filtered (not . isBuiltinE)
                  . filtered (not . isConstructorE)
        case fvars1 of
          [] -> pure step1
          _ -> go step1
      AppE e1 e2 -> do
        e1' <- go e1
        e2' <- go e2
        pure $ AppE e1' e2'
      CaseE t scrut alts -> do
        scrut' <- go scrut
        alts' <- traverse goAlt alts
        pure $ CaseE t scrut' alts'
      LamE bv body -> do
        LamE bv <$> scopeHelper body
      LetE binds decls body -> do
        let goDecl = \case
              NonRecursive ident bvix expr ->
                NonRecursive ident bvix <$> scopeHelper expr
              Recursive xs ->
                Recursive <$> traverse (\(i, ex) -> (i,) <$> scopeHelper ex) xs
        LetE binds <$> traverse goDecl decls <*> scopeHelper body
      AccessorE x t pss e -> AccessorE x t pss <$> go e
      ObjectUpdateE x t e cf fs ->
        (\e' fs' -> ObjectUpdateE x t e' cf fs')
          <$> go e
          <*> traverse (\(nm, expr) -> (nm,) <$> go expr) fs
      LitE t lit -> LitE t <$> traverse go lit
      TyInstE t e -> TyInstE t <$> go e
      TyAbs bv e -> TyAbs bv <$> go e
      other -> pure other

    scopeHelper ::
      Scope (BVar PurusType) (Exp WithObjects PurusType) (Var (BVar PurusType) (FVar PurusType)) ->
      Monomorphizer (Scope (BVar PurusType) (Exp WithObjects PurusType) (Var (BVar PurusType) (FVar PurusType)))
    scopeHelper scoped = do
      let unscoped = join <$> fromScope scoped
      transformed <- go unscoped
      let rescoped = abstract (\case B bv@(BVar {}) -> Just bv; _ -> Nothing) transformed
      pure rescoped

    goAlt (UnguardedAlt bs pat body) = UnguardedAlt bs pat <$> scopeHelper body

{- | Monomorphizing inliner. Looks up the provided
     identifier in the module context (TODO: linker that lets us support multiple modules)
     and attempts to forcibly assign the provided type to that expression, then
     inlines the monomorphized expression.

     Inlines and monomorphizes recursively, so that all free variables in the expression being
     inlined are monomorphized to the appropriate type and themselves inlined. (I.e. inlines
     everything and monomorphizes everything to the maximum extent possible)
-}
addTypeInstantiations :: PurusType -> Exp WithObjects PurusType (Vars PurusType) -> Exp WithObjects PurusType (Vars PurusType)
addTypeInstantiations monoType expToInst = go instantiations vars expToInst
  where
    go :: M.Map Text PurusType -> [(Text, KindOf PurusType)] -> Exp WithObjects PurusType (Vars PurusType) -> Exp WithObjects PurusType (Vars PurusType)
    go _ [] e = e
    go instMap ((v, k) : rest) e = case M.lookup v instMap of
      Nothing -> go instMap rest e
      Just ty -> TyInstE ty $ go instMap rest e

    eTyQuantified = expTy id expToInst

    (vars, eTy) = first (map (\(_, b, c) -> (b, c))) $ stripQuantifiers eTyQuantified

    monoArgs = splitFunTyParts monoType

    polyArgs = splitFunTyParts eTy

    instantiations = getInstantiations (fst <$> vars) polyArgs monoArgs

inlineAs ::
  PurusType ->
  Qualified Ident ->
  Monomorphizer (Exp WithObjects PurusType (Var (BVar PurusType) (FVar PurusType)))
inlineAs t ident = do
  res <- inlineAs' t ident
  let resInsted = res -- addTypeInstantiations t res
  let msg =
        prettify
          [ "IDENT: " <> T.unpack (showQualified runIdent ident)
          , "TY:\n" <> prettyAsStr t
          , "RESULT RAW:\n" <> prettyAsStr res
          , "RESULT RAW TY:\n" <> prettyAsStr (expTy id res)
          , "RESULT INSTED:\n" <> prettyAsStr resInsted
          , "RESULTED ISNTED TY:\n" <> prettyAsStr (expTy id resInsted)
          ]
  doTraceM "inlineAs" msg
  pure resInsted

inlineAs' ::
  PurusType ->
  Qualified Ident ->
  Monomorphizer (Exp WithObjects PurusType (Var (BVar PurusType) (FVar PurusType)))
-- TODO/REVIEW: Check whether this has any purpose here \/
{-
inlineAs' ty@(ForAll{}) nm = do
  let (_,inner) = stripQuantifiers ty
  innerRes <- inlineAs inner nm
  case innerRes of
    LamE bv body -> pure $ LamE (quantify lt) bv body
    other -> pure other
-}
inlineAs' ty inNm
  | not (inlineable inNm) = pure (V . F $ FVar ty inNm)
  | otherwise = case inNm of
      -- TODO: Linker so we can make this work no matter what the source module is
      (Qualified (ByModuleName mn') ident) ->
        ask >>= \(mn, modDict) ->
          if mn == mn'
            then do
              let msg = "Couldn't find a declaration with identifier " <> showIdent' ident <> " to inline as " <> prettyTypeStr ty
              note msg (findInlineDeclGroup ident modDict) >>= \case
                NonRecursive nrid nrix e -> monomorphizeWithTypeRec ty nrid nrix e
                Recursive xs -> do
                  let msg' = "Target expression with identifier " <> showIdent' ident <> " not found in mutually recursive group"
                  ((targIdent, targIx), targExpr) <- note msg' $ find (\x -> fst (fst x) == ident) xs -- has to be there
                  newIx <- freshUnique
                  let newTargIdent = Ident $ showIdent targIdent <> T.pack (show newIx)
                  let initialRecDict = M.singleton (targIdent, targIx) ((newTargIdent, newIx), ty, targExpr)
                  dict <- collectRecBinds ty initialRecDict targExpr
                  let renameMap = (\(i, t, _) -> (i, t)) <$> dict
                      bindingMap = (\(old, (a, b, c)) -> (old, a, b, join <$> fromScope c)) <$> M.toList dict
                  binds <- traverse (\(oldIx, newId, newTy, oldE) -> makeBind renameMap newId newTy oldIx oldE) bindingMap
                  case M.lookup (targIdent, targIx) renameMap of
                    Just ((newId, newIx'), newTy) -> do
                      let body = pure $ B (BVar newIx' newTy newId)
                      pure $ gLet binds body
                    Nothing ->
                      throwError $
                        MonoError $
                          "Couldn't inline "
                            <> showIdent' ident
                            <> " - identifier didn't appear in collected bindings:\n  "
                            <> show renameMap
            else -- TODO: This is a temporary hack to get builtins working w/o a real linker.
              throwError $ MonoError "Imports aren't supported yet!"
      wrong@(Qualified (BySourcePos _) _) ->
        throwError $
          MonoError $
            "Cannot inline variable qualified by SourcePos. Such a variable should be bound (and ergo not exist at this stage of compilation)"
              <> "\n Variable: "
              <> show wrong
  where
    {- Arguments:
         1. A renaming dictionary,
         2. An Identifier which represents the new name of a binding (and should be present in the renaming dictionary),
         3. The new, possibly monomorphic (hopefully more-monomorphic at least) type which will be forcibly assigned to the body expression),
         4. A (possibly polymorphic) expression that will become the decl body

       Constructs a BindE where the Ident is the 2nd arg and the body is
       the 4th arg forcibly assigned the type of the 3rd arg.
    -}
    makeBind ::
      Map (Ident, Int) ((Ident, Int), SourceType) -> -- Map OldName (NewName,TypeToAssign)
      (Ident, Int) -> -- NewName
      SourceType -> -- TypeToAssign
      (Ident, Int) -> -- Old Name & Index
      Exp WithObjects PurusType (Var (BVar PurusType) (FVar PurusType)) -> -- Declaration body
      Monomorphizer (BindE PurusType (Exp WithObjects PurusType) (Var (BVar PurusType) (FVar PurusType)))
    makeBind renameDict (newIdent, newIx) t (oldIdent, oldIx) e = doTrace "makeBind" (showIdent' newIdent) $ do
      let renameDictForUpdate = M.mapKeys fst . M.map (first fst) $ renameDict
          renamed = abstract (\case B bv -> Just bv; _ -> Nothing) $ updateFreeVars renameDictForUpdate e
      e' <- monomorphizeWithTypeRec t newIdent newIx renamed
      let abstr = abstract $ \case
            fv@(F (FVar a (Qualified ByNullSourcePos fvId))) | fvId == oldIdent -> case M.lookup (fvId, oldIx) renameDict of
              Nothing -> Nothing
              Just ((newIdent', newIx'), newTy) -> Just (BVar newIx' newTy newIdent')
            B bv@BVar {} -> Just bv
            _ -> Nothing

      pure $ NonRecursive newIdent newIx (abstr e')

    -- Find a declaration body in the *module* scope
    findDeclarationBody :: Ident -> Monomorphizer (Maybe (Int, Scope (BVar PurusType) (Exp WithObjects PurusType) (Var (BVar PurusType) (FVar PurusType))))
    findDeclarationBody nm = go <$> getModBinds
      where
        go ::
          [BindE PurusType (Exp WithObjects PurusType) (Var (BVar PurusType) (FVar PurusType))] ->
          Maybe (Int, Scope (BVar PurusType) (Exp WithObjects PurusType) (Var (BVar PurusType) (FVar PurusType)))
        go [] = Nothing
        go (b : bs) = case b of
          NonRecursive nm' bvix e -> if nm' == nm then Just (bvix, e) else go bs
          Recursive xs -> case find (\x -> fst (fst x) == nm) xs of
            Nothing -> go bs
            Just ((_, bvix), e) -> Just (bvix, e)

    {- RECURSIVE BINDINGS

       First, we need to walk the target expression and collect a list of all of the used
       bindings and the type that they must be when monomorphized, and the new identifier for their
       monomorphized/instantiated version. (We *don't* change anything here)

       Each of these `collectX` functions is specialized tool for collecting used bindings and
       deducing the type to which they should be assigned (based, initially, on the SourceType
       argument to `inlineAs`)
    -}

    -- Top-level collection function
    collectRecBinds ::
      PurusType ->
      Map (Ident, Int) ((Ident, Int), SourceType, Scope (BVar PurusType) (Exp WithObjects PurusType) (Var (BVar PurusType) (FVar PurusType))) ->
      Scope (BVar PurusType) (Exp WithObjects PurusType) (Var (BVar PurusType) (FVar PurusType)) ->
      Monomorphizer (Map (Ident, Int) ((Ident, Int), SourceType, Scope (BVar PurusType) (Exp WithObjects PurusType) (Var (BVar PurusType) (FVar PurusType))))
    collectRecBinds t visited e = doTrace "collectRecBinds" (prettyAsStr (fromScope e) <> "\n  " <> prettyTypeStr t) $ case join <$> fromScope e of
      LitE _ (ObjectL _ fs) -> doTrace "collectRecBinds" "crbOBJLIT" $ case t of
        RecordT fields -> do
          let fieldMap = mkFieldMap fields
          innerBinds <- collectRecFieldBinds visited fieldMap fs
          pure $ visited <> innerBinds
        _ -> throwError $ MonoError ("Failed to collect recursive binds: " <> prettyTypeStr t <> " is not a Record type")
      LitE _ _ -> doTrace "collectRecBinds" "crbLIT" $ pure visited
      ObjectUpdateE _ _ _ _ updateFields -> doTrace "collectRecBinds" "crbOBJUPDATE" $ case t of
        RecordT fields -> do
          let fieldMap = mkFieldMap fields
          innerBinds <- collectRecFieldBinds visited fieldMap updateFields
          pure $ visited <> innerBinds
        _ -> throwError $ MonoError ("Failed to collect recursive binds: " <> prettyTypeStr t <> " is not a Record type")
      AccessorE {} -> doTrace "collectRecBinds" "crbACCSR" $ pure visited -- idk. given (x.a :: t) we can't say what x is.
      absE@(LamE _ _) -> doTrace "collectRecBinds" ("crbABS TOARGS: " <> prettyTypeStr t) $ do
        let scoped = F <$> toScope absE
        collectFun visited scoped (splitFunTyParts t)
      app@(AppE _ e2) -> doTrace "collectRecBinds" "crbAPP" $ do
        let (f, args) = unsafeAnalyzeApp app
            types = (expTy id <$> args) <> [t]
            f' = F <$> toScope f
            e2' = F <$> toScope e2
        funBinds' <- collectFun visited f' types
        let funBinds = visited <> funBinds'
        argBinds <- collectRecBinds (head types) funBinds e2'
        pure $ funBinds <> argBinds
      V (F (FVar _ (Qualified (ByModuleName _) nm))) -> doTrace "collectRecBinds" ("crbVAR: " <> showIdent' nm) $ case M.lookup nm (M.mapKeys fst visited) of
        Nothing ->
          findDeclarationBody nm >>= \case
            Nothing -> throwError $ MonoError $ "No declaration correponding to name " <> showIdent' nm <> " found in the module"
            Just (oldIx, ex) -> do
              u <- freshUnique
              let freshNm = Ident $ showIdent nm <> T.pack (show u)
              let this = ((freshNm, u), t, ex)
              pure $ M.insert (nm, oldIx) this visited
        Just _ -> pure visited -- might not be right, might need to check that the types are equal? ugh keeping track of scope is a nightmare
      V (F (FVar _ (Qualified _ nm))) -> doTrace "collectRecBinds" ("crbVAR_: " <> showIdent' nm) $ pure visited
      CaseE _ _ alts -> doTrace "collectRecBinds" "crbCASE" $ do
        let flatAlts = concatMap extractAndFlattenAlts alts
        foldM (collectRecBinds t) visited flatAlts
      LetE _ _ ex -> collectRecBinds t visited ex
      TyInstE _ inner -> collectRecBinds t visited (toScope . fmap F $ inner)
      TyAbs _ inner -> collectRecBinds t visited (toScope . fmap F $ inner)
      _ -> error "collectRecBinds"

    -- Collect from the fields of an object
    collectRecFieldBinds ::
      Map (Ident, Int) ((Ident, Int), SourceType, Scope (BVar PurusType) (Exp WithObjects PurusType) (Var (BVar PurusType) (FVar PurusType))) ->
      M.Map PSString (RowListItem SourceAnn) ->
      [(PSString, Exp WithObjects PurusType (Var (BVar PurusType) (FVar PurusType)))] ->
      Monomorphizer (Map (Ident, Int) ((Ident, Int), SourceType, Scope (BVar PurusType) (Exp WithObjects PurusType) (Var (BVar PurusType) (FVar PurusType))))
    collectRecFieldBinds visited _ [] = pure visited
    collectRecFieldBinds visited cxt ((lbl, e) : rest) = doTrace "collectRecFieldBinds" (prettyAsStr lbl <> "\n" <> prettyAsStr e) $ do
      RowListItem {..} <-
        note ("No type for field with label " <> T.unpack (prettyPrintString lbl) <> " when collecting record binds") $
          M.lookup lbl cxt
      this <- collectRecBinds rowListType visited (toScope . fmap F $ e)
      collectRecFieldBinds (visited <> this) cxt rest

    -- Collect from a function expression
    collectFun ::
      Map (Ident, Int) ((Ident, Int), SourceType, Scope (BVar PurusType) (Exp WithObjects PurusType) (Var (BVar PurusType) (FVar PurusType))) ->
      Scope (BVar PurusType) (Exp WithObjects PurusType) (Var (BVar PurusType) (FVar PurusType)) ->
      [SourceType] ->
      Monomorphizer (Map (Ident, Int) ((Ident, Int), SourceType, Scope (BVar PurusType) (Exp WithObjects PurusType) (Var (BVar PurusType) (FVar PurusType))))
    collectFun visited e [t] = doTrace "collectFun" ("FIN:\n  " <> {- -ppExp e <> " :: " -} prettyTypeStr t) $ do
      rest <- collectRecBinds t visited e
      pure $ visited <> rest
    collectFun visited e (t : ts) =
      doTrace "collectFun" (prettyAsStr (fromScope e) <> "\n  " <> prettyTypeStr t <> "\n" <> show ts) $
        case join <$> fromScope e of
          LamE _ body'' -> collectFun visited body'' ts
          (V (F (FVar _ (Qualified (ByModuleName _) nm)))) -> doTrace "collectFun" ("VAR: " <> showIdent' nm) $ do
            let msg = "Couldn't find a declaration with identifier " <> showIdent' nm <> " to inline as " <> prettyTypeStr t
            (bvix, declBody) <- note msg =<< findDeclarationBody nm
            case M.lookup (nm, bvix) visited of
              Nothing -> do
                let t' = foldr1 function (t : ts)
                freshNm <- freshen nm
                u <- freshUnique
                let visited' = M.insert (nm, bvix) ((freshNm, u), t', declBody) visited
                collectRecBinds t' visited' declBody
              Just _ -> pure visited
          other -> throwError $ MonoError $ "\n\nUnexpected expression in collectFun:\n\n" <> ppExp other <> "\n\n" <> show other
    collectFun _ _ [] = throwError $ MonoError "Ran out of types in collectFun"

monomorphizeWithTypeRec ::
  PurusType ->
  Ident ->
  Int ->
  Scope (BVar PurusType) (Exp WithObjects PurusType) (Var (BVar PurusType) (FVar PurusType)) ->
  Monomorphizer (Exp WithObjects PurusType (Var (BVar PurusType) (FVar PurusType)))
monomorphizeWithTypeRec ty idnt indx expr | containsBVar idnt indx expr = do
  body <- monomorphizeWithType ty (join <$> fromScope expr)
  let abstr = abstract (\case B bv -> Just bv; _ -> Nothing)
      rescoped = abstr body
      result = LetE M.empty [NonRecursive idnt indx rescoped] (toScope (V . B $ BVar indx ty idnt))
      msg =
        prettify
          [ "TY ARG:\n" <> prettyAsStr ty
          , "INPUT:\n" <> prettyAsStr (fromScope expr)
          , "INPUT TY:\n" <> prettyAsStr (expTy' id expr)
          , "OUTPUT:\n" <> prettyAsStr result
          , "OUTPUT TY:\n" <> prettyAsStr (expTy id result)
          ]
  doTraceM "monomorphizeWithType" msg
  pure result
monomorphizeWithTypeRec ty _ _ expr = do
  result <- monomorphizeWithType ty (join <$> fromScope expr)
  let msg =
        prettify
          [ "TY ARG:\n" <> prettyAsStr ty
          , "INPUT:\n" <> prettyAsStr (fromScope expr)
          , "INPUT TY:\n" <> prettyAsStr (expTy' id expr)
          , "OUTPUT:\n" <> prettyAsStr result
          , "OUTPUT TY:\n" <> prettyAsStr (expTy id result)
          ]
  doTraceM "monomorphizeWithType" msg
  pure result

{- | "Forcibly" assigns the provided type to the provided expression.
     Works recursively, so that all of the types of all sub-expressions are
     also forcibly assigned so as to conform with the provided type.

     E.g. `monomorphizeWithType (Int -> Int) (\(x :: a) -> (f :: a -> Int) x)`
     should yield `(\(x :: Int) -> (f :: Int -> Int) x)`

     This is used during inlining. At the point where we attempt to inline a
     polymorphic function, we should know which types the type variables
     ought to be instantiated to (if it can be known).
-}
monomorphizeWithType ::
  PurusType ->
  Exp WithObjects PurusType (Var (BVar PurusType) (FVar PurusType)) ->
  Monomorphizer (Exp WithObjects PurusType (Var (BVar PurusType) (FVar PurusType)))
monomorphizeWithType ty expr | expTy id expr == ty = pure expr
monomorphizeWithType ty expr = doTrace "monomorphizeWithType" ("INPUT:\n" <> ppExp expr <> "\n\nINPUT TY:\n" <> prettyTypeStr ty) $ case expr of
  LitE _ (ObjectL ext fs) -> case ty of
    RecordT fields -> do
      let fieldMap = mkFieldMap fields
      LitE ty . ObjectL ext <$> monomorphizeFieldsWithTypes fieldMap fs
    _ -> throwError $ MonoError ("Failed to collect recursive binds: " <> prettyTypeStr ty <> " is not a Record type")
  LitE _ lit -> pure $ LitE ty lit
  ObjectUpdateE ext _ orig copyFields updateFields -> case ty of
    RecordT fields -> do
      let fieldMap = mkFieldMap fields
      -- idk. do we need to do something to the original expression or is this always sufficient?
      updateFields' <- monomorphizeFieldsWithTypes fieldMap updateFields
      pure $ ObjectUpdateE ext ty orig copyFields updateFields'
    _ -> throwError $ MonoError ("Failed to collect recursive binds: " <> prettyTypeStr ty <> " is not a Record type")
  AccessorE ext _ str e -> pure $ AccessorE ext ty str e -- idk?
  fun@(LamE bv body) -> doTrace "monomorphizeWithType" ("ABS:\n  " <> ppExp fun <> "\n\nTARGET TY:\n" <> prettyTypeStr ty <> "\n\nABS TY:\n" <> prettyAsStr (expTy id fun)) $
    case ty of
      (a :-> b) -> do
        let
          -- replaceBVar = mapBound $ \x -> if x == bv then freshBV else x
          body' = fmap join . fromScope $ {- -replaceBVar $-} updateVarTyS bv a body
        body'' <- fmap F . toScope <$> monomorphizeWithType b body'
        pure $ LamE bv body''
      _ -> case stripQuantifiers ty of
        (bvars, innerT) -> do
          -- throwError $ MonoError  $ "Expected Function Type for Expression:\n\n" <> prettyAsStr fun <> "\n\nbut got type:\n\n" <> prettyAsStr other
          fun' <- monomorphizeWithType innerT (LamE bv body)
          tyAbstractExpr bvars fun'
  app@(AppE {}) -> doTrace "monomorphizeWithType" ("APP:\n  " <> ppExp app) $ monomorphize app
  V a -> case a of
    F (FVar _ b) -> pure . V . F $ FVar ty b -- idk
    other -> pure $ V other
  CaseE _ scrut alts -> do
    let f = monomorphizeWithType ty
        goAlt ::
          Alt WithObjects PurusType (Exp WithObjects PurusType) (Var (BVar PurusType) (FVar PurusType)) ->
          Monomorphizer (Alt WithObjects PurusType (Exp WithObjects PurusType) (Var (BVar PurusType) (FVar PurusType)))
        goAlt (UnguardedAlt bindings binders result) = do
          let unscoped = join <$> fromScope result
              abstr = abstract (\case B bv -> Just bv; _ -> Nothing)
          unscoped' <- f unscoped
          pure $ UnguardedAlt bindings binders (abstr unscoped')

    CaseE ty scrut <$> traverse goAlt alts
  LetE a binds e -> do
    let unscoped = join <$> fromScope e
    LetE a binds . fmap F . toScope <$> monomorphizeWithType ty unscoped

  -- REVIEW: Not sure about this

  -- type instantiations shouldn't be touched here
  other -> pure other
  where
    monomorphizeFieldsWithTypes ::
      M.Map PSString (RowListItem SourceAnn) ->
      [(PSString, Exp WithObjects PurusType (Var (BVar PurusType) (FVar PurusType)))] ->
      Monomorphizer [(PSString, Exp WithObjects PurusType (Var (BVar PurusType) (FVar PurusType)))]
    monomorphizeFieldsWithTypes _ [] = pure []
    monomorphizeFieldsWithTypes cxt ((lbl, e) : rest) = do
      RowListItem {..} <-
        note ("No type for field with label " <> T.unpack (prettyPrintString lbl) <> " when monomorphizing record") $
          M.lookup lbl cxt
      rest' <- monomorphizeFieldsWithTypes cxt rest
      e' <- monomorphizeWithType rowListType e
      pure $ (lbl, e') : rest'

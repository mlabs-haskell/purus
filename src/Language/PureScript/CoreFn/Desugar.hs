{- HLINT ignore "Use void" -}
{- HLINT ignore "Use <$" -}
{-# LANGUAGE TypeApplications #-}
module Language.PureScript.CoreFn.Desugar(moduleToCoreFn) where

import Prelude
import Protolude (ordNub, orEmpty, zipWithM, MonadError (..), sortOn)

import Data.Maybe (mapMaybe)
import Data.List.NonEmpty qualified as NEL
import Data.Map qualified as M

import Language.PureScript.AST.Literals (Literal(..))
import Language.PureScript.AST.SourcePos (pattern NullSourceSpan, SourceSpan(..))
import Language.PureScript.CoreFn.Ann (Ann, ssAnn)
import Language.PureScript.CoreFn.Binders (Binder(..))
import Language.PureScript.CoreFn.Expr (Bind(..), CaseAlternative(..), Expr(..), Guard, exprType)
import Language.PureScript.CoreFn.Meta (Meta(..))
import Language.PureScript.CoreFn.Module (Module(..))
import Language.PureScript.Crash (internalError)
import Language.PureScript.Environment (
  pattern (:->),
  pattern ArrayT,
  DataDeclType(..),
  Environment(..),
  NameKind(..),
  isDictTypeName,
  lookupConstructor,
  lookupValue,
  purusFun,
  NameVisibility (..),
  tyBoolean,
  tyFunction,
  tyString,
  tyChar,
  tyInt,
  tyNumber )
import Language.PureScript.Label (Label(..))
import Language.PureScript.Names (
  pattern ByNullSourcePos, Ident(..),
  ModuleName,
  ProperName(..),
  ProperNameType(..),
  Qualified(..),
  QualifiedBy(..),
  mkQualified,
  runIdent,
  coerceProperName,
  Name (DctorName))
import Language.PureScript.PSString (PSString)
import Language.PureScript.Types (
  pattern REmptyKinded,
  SourceType,
  Type(..),
  srcTypeConstructor,
  srcTypeVar, srcTypeApp, quantify, eqType, containsUnknowns)
import Language.PureScript.AST.Binders qualified as A
import Language.PureScript.AST.Declarations qualified as A
import Language.PureScript.AST.SourcePos qualified as A
import Language.PureScript.Constants.Prim qualified as C
import Control.Monad.State.Strict (MonadState, gets, modify)
import Control.Monad.Writer.Class ( MonadWriter )
import Language.PureScript.TypeChecker.Kinds ( kindOf )
import Language.PureScript.TypeChecker.Types
    ( checkTypeKind,
      SplitBindingGroup(SplitBindingGroup),
      TypedValue'(TypedValue'),
      typeDictionaryForBindingGroup,
      infer )
import Data.List.NonEmpty qualified as NE
import Language.PureScript.TypeChecker.Unify (unifyTypes)
import Control.Monad (forM, (>=>))
import Language.PureScript.Errors
    ( MultipleErrors, errorMessage', SimpleErrorMessage(..))
import Debug.Trace (traceM)
import Language.PureScript.CoreFn.Pretty ( ppType )
import Data.Text qualified as T
import Language.PureScript.Pretty.Values (renderValue)
import Language.PureScript.TypeChecker.Monad
    ( bindLocalVariables,
      bindNames,
      getEnv,
      makeBindingGroupVisible,
      warnAndRethrowWithPositionTC,
      withBindingGroupVisible,
      CheckState(checkEnv, checkCurrentModule) )
import Language.PureScript.CoreFn.Desugar.Utils
    ( binderToCoreFn,
      dedupeImports,
      exportToCoreFn,
      externToCoreFn,
      findQualModules,
      getConstructorMeta,
      getLetMeta,
      getModuleName,
      getTypeClassArgs,
      getValueMeta,
      importToCoreFn,
      inferType,
      instantiatePolyType,
      pTrace,
      printEnv,
      properToIdent,
      purusTy,
      reExportsToCoreFn,
      showIdent',
      ssA,
      toReExportRef,
      traverseLit,
      wrapTrace,
      M, unwrapRecord, withInstantiatedFunType,
      )
import Text.Pretty.Simple (pShow)
import Data.Text.Lazy qualified as LT
import Data.Set qualified as S

{-
    CONVERSION MACHINERY

    NOTE: We run this *after* the initial typechecking/desugaring phase, using the Environment returned from that
          initial pass. It's important to keep that in mind, for a few reasons:
            - We know that everything is well-typed/scoped/properly renamed/desugared/etc. This assumption lets us safely do a bunch of things that wouldn't otherwise be safe.
            - We have access to all of the type signatures for top-level declarations
            - We have to fix the "lies" in the type signatures that emerge after desugaring, e.g. types w/ a class constraint represent values that take an additional dict argument

   NOTE: All of the "pure" conversion functions (i.e. which don't require the typechecker monad stack) are in Language.PureScript.CoreFn.Desugar.Utils.
         This module is hard enough to understand, best to minimize its size.
-}

-- | Desugars a module from AST to CoreFn representation.
moduleToCoreFn :: forall m. M m => A.Module -> m (Module Ann)
moduleToCoreFn  (A.Module _ _ _ _ Nothing) =
  internalError "Module exports were not elaborated before moduleToCoreFn"
moduleToCoreFn (A.Module modSS coms mn decls (Just exps)) = do
  setModuleName
  let importHelper ds = fmap (ssAnn modSS,) (findQualModules ds)
      imports = dedupeImports $ mapMaybe importToCoreFn decls ++ importHelper decls
      exps' = ordNub $ concatMap exportToCoreFn exps
      reExps = M.map ordNub $ M.unionsWith (++) (mapMaybe (fmap reExportsToCoreFn . toReExportRef) exps)
      externs = ordNub $ mapMaybe externToCoreFn decls
  decls' <- concat <$> traverse (declToCoreFn mn) decls
  pure $ Module modSS coms mn (spanName modSS) imports exps' reExps externs decls'
 where
   setModuleName = modify $ \cs ->
     cs {checkCurrentModule = Just mn}

{- | Given a SourcePos and Identifier, look up the type of that identifier, also returning its NameVisiblity.

     NOTE: Local variables should all be qualified by their SourcePos, whereas imports (and maybe top level decls in the module? can't remember)
           are qualified by their ModuleName. What we do here is first look for a "local" type for the identifier using the provided source position,
           then, if that fails, look up the identifier in the "global" scope using a module name.

           I *think* this is fine but I'm not *certain*.
-}
lookupType :: forall m. M m => A.SourcePos -> Ident -> m (SourceType,NameVisibility)
lookupType sp tn = do
  mn <- getModuleName
  env <- gets checkEnv
  case M.lookup  (Qualified (BySourcePos sp) tn) (names env) of
    Nothing -> case M.lookup (mkQualified tn mn) (names env) of
      Nothing -> do
        pEnv <- printEnv
        error $ "No type found for " <> show tn <> "\n  in env:\n" <> pEnv
      Just (ty,_,nv) -> do
        traceM $ "lookupType: " <> showIdent' tn <> " :: " <> ppType 10 ty
        pure (ty,nv)
    Just (ty,_,nv) -> do
      traceM $ "lookupType: " <> showIdent' tn <> " :: " <> ppType 10 ty
      pure (ty,nv)

{- Converts declarations from their AST to CoreFn representation, deducing types when possible & inferring them when not possible.

   TODO: The module name can be retrieved from the monadic context and doesn't need to be passed around
-}

-- newtype T = T Foo turns into T :: Foo -> Foo
declToCoreFn :: forall m. M m => ModuleName -> A.Declaration -> m [Bind Ann]
declToCoreFn _ (A.DataDeclaration (ss, com) Newtype name _ [ctor]) = wrapTrace ("decltoCoreFn NEWTYPE " <> show name) $ case A.dataCtorFields ctor of
  [(_,wrappedTy)] -> do
    -- traceM (show ctor)
    let innerFunTy = quantify $ purusFun wrappedTy wrappedTy
    pure [NonRec (ss, [], declMeta) (properToIdent $ A.dataCtorName ctor) $
      Abs (ss, com, Just IsNewtype) innerFunTy (Ident "x") (Var (ssAnn ss) (purusTy wrappedTy) $ Qualified ByNullSourcePos (Ident "x"))]
  _ -> error "Found newtype with multiple fields"
  where
  declMeta = isDictTypeName (A.dataCtorName ctor) `orEmpty` IsTypeClassConstructor
-- Reject newtypes w/ multiple constructors
declToCoreFn _ d@(A.DataDeclaration _ Newtype _ _ _) =
  error $ "Found newtype with multiple constructors: " ++ show d
-- Data declarations get turned into value declarations for the constructor(s)
declToCoreFn  mn (A.DataDeclaration (ss, com) Data tyName _ ctors) = wrapTrace ("declToCoreFn DATADEC " <>  T.unpack (runProperName tyName)) $ do
  traverse go ctors
 where
  go ctorDecl = do
    env <- gets checkEnv
    let ctor = A.dataCtorName ctorDecl
        (_, _, ctorTy, fields) = lookupConstructor  env (Qualified (ByModuleName mn) ctor)
    pure $ NonRec (ssA ss) (properToIdent ctor) $ Constructor (ss, com, Nothing) (purusTy ctorTy) tyName ctor fields
-- NOTE: This should be OK because you can data declarations can only appear at the top-level.
declToCoreFn mn (A.DataBindingGroupDeclaration ds) = wrapTrace  "declToCoreFn DATA GROUP DECL" $ concat <$> traverse (declToCoreFn  mn) ds
-- Essentially a wrapper over `exprToCoreFn`. Not 100% sure if binding the type of the declaration is necessary here?
-- NOTE: Should be impossible to have a guarded expr here, make it an error
declToCoreFn  mn (A.ValueDecl (ss, _) name _ _ [A.MkUnguarded e]) = wrapTrace ("decltoCoreFn VALDEC " <> show name) $ do
  (valDeclTy,nv) <- lookupType (spanStart ss) name
  bindLocalVariables [(ss,name,valDeclTy,nv)] $ do
      expr <- exprToCoreFn mn ss (Just valDeclTy) e -- maybe wrong? might need to bind something here?
      pure [NonRec (ssA ss) name expr]
-- Recursive binding groups. This is tricky. Calling `typedOf` saves us a lot of work, but it's hard to tell whether that's 100% safe here
declToCoreFn  mn (A.BindingGroupDeclaration ds) = wrapTrace "declToCoreFn BINDING GROUP" $ do
  let typed  = NE.toList $ extractTypeAndPrepareBind <$> ds
      toBind = snd <$> typed
  recBody <- bindLocalVariables toBind $ traverse goRecBindings typed
  pure [Rec recBody]
 where
   -- If we only ever call this on a top-level binding group then this should be OK, all the exprs should be explicitly typed
   extractTypeAndPrepareBind :: ((A.SourceAnn, Ident), NameKind,  A.Expr) -> (A.Expr, (SourceSpan,Ident,SourceType,NameVisibility))
   extractTypeAndPrepareBind (((ss',_),ident),_,A.TypedValue _ e ty) = (e,(ss',ident,ty,Defined))
   extractTypeAndPrepareBind (((ss',_),ident),_,_) = error $ "Top level declaration " <> (showIdent' ident) <> " should have a type annotation, but does not"

   goRecBindings :: (A.Expr, (SourceSpan,Ident,SourceType,NameVisibility)) -> m ((Ann, Ident), Expr Ann)
   goRecBindings (expr,(ss',ident,ty,nv)) = do
     expr' <- exprToCoreFn mn ss' (Just ty) expr
     pure ((ssA ss',ident), expr')
-- TODO: Avoid catchall case
declToCoreFn _ _ = pure []

-- Desugars expressions from AST to typed CoreFn representation.
exprToCoreFn :: forall m. M m => ModuleName -> SourceSpan -> Maybe SourceType -> A.Expr -> m (Expr Ann)
-- Literal case is straightforward
exprToCoreFn mn _ mTy astLit@(A.Literal ss lit) = wrapTrace ("exprToCoreFn LIT " <> renderValue 100 astLit) $ do
  litT <- purusTy <$> inferType mTy astLit
  lit' <- traverseLit (exprToCoreFn mn ss Nothing) lit
  pure $ Literal (ss, [], Nothing)  litT lit'
-- Accessor case is straightforward
exprToCoreFn mn ss mTy accessor@(A.Accessor name v) = wrapTrace ("exprToCoreFn ACCESSOR " <> renderValue 100 accessor) $ do
  expT <- purusTy <$> inferType mTy accessor
  expr  <- exprToCoreFn mn ss Nothing v
  pure $ Accessor (ssA ss) expT name expr
-- Object update is straightforward (this is basically a monadic wrapper around the old non-typed exprToCoreFn)
exprToCoreFn mn ss mTy objUpd@(A.ObjectUpdate obj vs) = wrapTrace ("exprToCoreFn OBJ UPDATE " <> renderValue 100 objUpd) $ do
  expT <- purusTy <$> inferType mTy objUpd
  obj' <- exprToCoreFn mn ss Nothing obj
  vs' <- traverse (\(lbl,val) -> exprToCoreFn mn ss Nothing val >>= \val' -> pure (lbl,val')) vs
  pure $
    ObjectUpdate
      (ssA ss)
      expT
      obj'
      (mTy >>= unchangedRecordFields (fmap fst vs))
      vs'
  where
  -- TODO: Optimize/Refactor Using Data.Set
  -- Return the unchanged labels of a closed record, or Nothing for other types or open records.
  unchangedRecordFields :: [PSString] -> Type a -> Maybe [PSString]
  unchangedRecordFields updated (TypeApp _ (TypeConstructor _ C.Record) row) =
    collect row
    where
      collect :: Type a -> Maybe [PSString]
      collect (REmptyKinded _ _) = Just []
      collect (RCons _ (Label l) _ r) = (if l `elem` updated then id else (l :)) <$> collect r
      collect _ = Nothing
  unchangedRecordFields _ _ = Nothing
-- Lambda abstraction. See the comments on `instantiatePolyType` above for an explanation of the strategy here.
exprToCoreFn mn _ (Just t) (A.Abs (A.VarBinder ssb name) v) = wrapTrace ("exprToCoreFn " <> showIdent' name) $
  withInstantiatedFunType mn t $ \a b -> do
      body <-  bindLocalVariables [(ssb,name,a,Defined)] $ exprToCoreFn mn ssb (Just b) v
      pure $ Abs (ssA ssb) (purusFun a b) name body

-- By the time we receive the AST, only Lambdas w/ a VarBinder should remain
-- TODO: Better failure message if we pass in 'Nothing' as the (Maybe Type) arg for an Abstraction
exprToCoreFn _  _ t lam@(A.Abs _ _) =
  internalError $ "Abs with Binder argument was not desugared before exprToCoreFn mn: \n" <> show lam <> "\n\n" <> show (const () <$> t)
-- Ad hoc machinery for handling desugared type class dictionaries. As noted above, the types "lie" in generated code.
-- NOTE: Not 100% sure this is necessary anymore now that we have instantiatePolyType
-- TODO: Investigate whether still necessary
exprToCoreFn mn ss mTy app@(A.App v1 v2)
  | isDictCtor v2 && isDictInstCase v1 = wrapTrace "exprToCoreFn APP DICT" $ do
      v2' <- exprToCoreFn mn ss Nothing v2
      toBind <- mkDictInstBinder v1
      v1' <- bindLocalVariables toBind $ exprToCoreFn mn ss Nothing v1
      appT <- inferType mTy app
      pure $ App (ss, [], Just IsSyntheticApp) (purusTy appT) v1' v2'


  | otherwise  = wrapTrace "exprToCoreFn APP" $ do
      appT <- inferType mTy app
      v1' <- exprToCoreFn mn ss Nothing v1
      v2' <- exprToCoreFn mn ss Nothing v2
      pure $ App (ss, [], (isDictCtor v1 || isSynthetic v2) `orEmpty` IsSyntheticApp) (purusTy appT) v1' v2'
  where
  mkDictInstBinder = \case
    A.TypedValue _ e _ -> mkDictInstBinder e
    A.Abs (A.VarBinder _ss1 (Ident "dict")) (A.Case [A.Var _ (Qualified _ (Ident "dict"))] [A.CaseAlternative [A.ConstructorBinder _ cn@(Qualified _ _) _] [A.MkUnguarded _acsr]]) -> do
      let className :: Qualified (ProperName 'ClassName) = coerceProperName <$> cn
      args' <- getTypeClassArgs className
      let args = zipWith (\i _ -> srcTypeVar $ "dictArg" <> T.pack (show @Int i)) [1..] args'
          dictTyCon = srcTypeConstructor  (coerceProperName <$> cn)
          dictTyFreeVars = foldl srcTypeApp dictTyCon args
          ty = quantify dictTyFreeVars
      pure [(A.NullSourceSpan,Ident "dict",ty,Defined)]
    _ -> error "invalid dict accesor expr"

  isDictInstCase = \case
    A.TypedValue _ e _ -> isDictInstCase e
    A.Abs (A.VarBinder _ss1 (Ident "dict")) (A.Case [A.Var _ (Qualified ByNullSourcePos (Ident "dict"))] [A.CaseAlternative [A.ConstructorBinder _ (Qualified _ name) _] [A.MkUnguarded _acsr]]) -> isDictTypeName name
    _ -> False

  isDictCtor = \case
    A.Constructor _ (Qualified _ name) -> isDictTypeName name
    _ -> False
  isSynthetic = \case
    A.App v3 v4            -> isDictCtor v3 || isSynthetic v3 && isSynthetic v4
    A.Accessor _ v3        -> isSynthetic v3
    A.Var NullSourceSpan _ -> True
    A.Unused{}             -> True
    _                      -> False
-- Dunno what to do here. Haven't encountered an Unused so far, will need to see one to figure out how to handle them
exprToCoreFn _ _  _ (A.Unused _) = -- ????? need to figure out what this _is_
  error "Don't know what to do w/ exprToCoreFn A.Unused"
-- Variables should *always* be bound & typed in the Environment before we encounter them.
-- NOTE: Not sure if we should ignore a type passed in? Generally we shouldn't *pass* types here, but bind variables
exprToCoreFn _ _ _ (A.Var ss ident) = wrapTrace ("exprToCoreFn VAR " <> show ident) $
  gets checkEnv >>= \env -> case lookupValue env ident of
    Just (ty,_,_) -> pure $ Var (ss, [], getValueMeta env ident) (purusTy ty) ident
    Nothing -> do
      -- pEnv <- printEnv
      traceM $ "No known type for identifier " <> show ident -- <> "\n    in:\n" <> LT.unpack (pShow $ names env)
      error "boom"
-- If-Then-Else Turns into a case expression
exprToCoreFn mn ss mTy ifte@(A.IfThenElse cond th el) = wrapTrace "exprToCoreFn IFTE" $ do
  -- NOTE/TODO: Don't need to call infer separately here
  ifteTy <- inferType mTy ifte
  condE <- exprToCoreFn mn ss (Just tyBoolean) cond
  thE <- exprToCoreFn mn ss Nothing th
  elE <- exprToCoreFn mn ss Nothing  el
  pure $ Case (ss, [], Nothing) (purusTy ifteTy) [condE]
    [ CaseAlternative [LiteralBinder (ssAnn ss) $ BooleanLiteral True]
                      (Right thE)
    , CaseAlternative [NullBinder (ssAnn ss)]
                      (Right elE) ]
-- Constructor case is straightforward, we should already have all of the type info
exprToCoreFn _ _  mTy ctor@(A.Constructor ss name) = wrapTrace ("exprToCoreFn CTOR " <> show name) $ do
  env <- gets checkEnv
  let ctorMeta = getConstructorMeta env name
  ctorType <- inferType mTy ctor
  pure $ Var (ss, [], Just ctorMeta) (purusTy ctorType) $ fmap properToIdent name
-- Case expressions
exprToCoreFn mn ss mTy astCase@(A.Case vs alts) = wrapTrace "exprToCoreFn CASE" $ do
  traceM $ renderValue 100 astCase
  caseTy <- inferType mTy astCase -- the return type of the branches. This will usually be passed in.
  traceM "CASE.1"
  ts <- traverse (infer >=> pure . tvType)  vs -- extract type information for the *scrutinees* (need this to properly type the binders. still not sure why exactly this is a list)
  traceM $ ppType 100 caseTy
  pTrace vs
  vs' <- traverse (exprToCoreFn mn ss Nothing) vs -- maybe zipWithM
  alts' <- traverse (altToCoreFn mn ss caseTy ts) alts -- see explanation in altToCoreFn. We pass in the types of the scrutinee(s)
  pure $ Case (ssA ss) (purusTy caseTy) vs' alts'
 where
   tvType (TypedValue' _ _ t) = t
-- We prioritize the supplied type over the inferred type, since a type should only ever be passed when known to be correct.
-- (I think we have to do this - the inferred type is "wrong" if it contains a class constraint)
exprToCoreFn  mn ss  (Just ty) (A.TypedValue _ v _) = wrapTrace "exprToCoreFn TV1" $
  exprToCoreFn mn ss (Just ty) v
exprToCoreFn mn ss Nothing (A.TypedValue _ v ty) = wrapTrace "exprToCoreFn TV2" $
  exprToCoreFn mn ss (Just ty) v
-- Let bindings. Complicated.
exprToCoreFn  mn ss _ (A.Let w ds v) = wrapTrace "exprToCoreFn LET" $ case NE.nonEmpty ds of
  Nothing -> error "declarations in a let binding can't be empty"
  Just _ -> do
    (decls,expr) <- transformLetBindings mn ss [] ds v -- see transformLetBindings
    pure $ Let (ss, [], getLetMeta w) (exprType expr) decls expr
exprToCoreFn  mn _ ty (A.PositionedValue ss _ v) = wrapTrace "exprToCoreFn POSVAL" $
  exprToCoreFn mn ss ty v
exprToCoreFn _ _   _ e =
  error $ "Unexpected value in exprToCoreFn mn: " ++ show e

-- Desugars case alternatives from AST to CoreFn representation.
altToCoreFn ::  forall m
             . M m
            => ModuleName
            -> SourceSpan
            -> SourceType -- The "return type", i.e., the type of the expr to the right of the -> in a case match branch (we always know this)
            -> [SourceType] -- The types of the *scrutinees*, i.e. the `x` in `case x of (...)`. NOTE: Still not sure why there can be more than one
            -> A.CaseAlternative
            -> m (CaseAlternative Ann)
altToCoreFn  mn ss ret boundTypes (A.CaseAlternative bs vs) = wrapTrace "altToCoreFn" $ do
    env <- gets checkEnv
    bTypes <- M.unions <$> zipWithM inferBinder' boundTypes bs -- Inferring the types for binders requires some special machinery & knowledge of the scrutinee type. NOTE: Not sure why multiple binders?
    let toBind = (\(n',(ss',ty')) -> (ss',n',ty',Defined)) <$> M.toList bTypes
        binders = binderToCoreFn env mn ss <$> bs
    traceM $ concatMap (\x -> show x <> "\n") toBind
    ege <- go toBind vs
    pure $ CaseAlternative binders ege
  where
  go :: [(SourceSpan, Ident, SourceType, NameVisibility)] -> [A.GuardedExpr] -> m (Either [(Guard Ann, Expr Ann)] (Expr Ann))
  go toBind [A.MkUnguarded e] = wrapTrace "altToCoreFn GO" $ do
    expr <- bindLocalVariables toBind $ exprToCoreFn mn ss (Just ret) e -- need to bind all variables that occur in the binders. We know the type of the right hand side (as it was passed in)
    pure $ Right expr
  -- NOTE: Not sure whether this works / TODO: Make a test case that uses guards in case expressions
  go toBind gs = bindLocalVariables toBind $ do
    ges <- forM gs $ \case
      A.GuardedExpr g e -> do
        let cond = guardToExpr g
        condE <- exprToCoreFn mn ss (Just tyBoolean) cond -- (Just tyBoolean)?
        eE    <- exprToCoreFn mn ss (Just ret) e
        pure (condE,eE)
    pure . Left $ ges
  guardToExpr [A.ConditionGuard cond] = cond
  guardToExpr _ = internalError "Guard not correctly desugared"

{- Dirty hacks. If something breaks, odds are pretty good that it has something do with something here.

   These two functions are adapted from utilities in Language.PureScript.TypeChecker.Types:
     - transformLetBindings is a modification of inferLetBindings
     - inferBinder' is a modification of inferBinder'

   We need functions that perform the same tasks as those in TypeChecker.Types, but we cannot use the
   existing functions because they call instantiatePolyTypeWithUnknowns. Instantiating a polytype to
   an unknown type is correct *during the initial typechecking phase*, but it is disastrous for us
   because we need to preserve the quantifiers explicitly in the typed AST.

   Both of these functions work for reasonably simple examples, but may fail in more complex cases.
   The primary reason for this is: I'm not sure how to write PS source that contains some of the
   weirder cases in the AST. We'll have to deal with any problems once we have examples that
   clearly isolate the problematic syntax nodes.
-}
transformLetBindings :: forall m. M m => ModuleName -> SourceSpan -> [Bind Ann] -> [A.Declaration] -> A.Expr -> m ([Bind Ann], Expr Ann)
transformLetBindings mn ss seen [] ret = (seen,) <$> withBindingGroupVisible (exprToCoreFn mn ss Nothing ret)
transformLetBindings mn _ss seen ((A.ValueDecl sa@(ss,_) ident nameKind [] [A.MkUnguarded (A.TypedValue checkType val ty)]) : rest) ret =
  wrapTrace ("transformLetBindings VALDEC TYPED " <> showIdent' ident <> " :: " <> ppType 100 ty ) $
  bindNames (M.singleton (Qualified (BySourcePos $ spanStart ss) ident) (ty, nameKind, Defined)) $ do
    thisDecl <- declToCoreFn mn (A.ValueDecl sa ident nameKind [] [A.MkUnguarded (A.TypedValue checkType val ty)])
    let seen' = seen ++ thisDecl
    transformLetBindings mn _ss seen' rest ret
-- TODO: Write a question where I ask what can legitimately be inferred as a type in a let binding context
transformLetBindings mn _ss seen (A.ValueDecl sa@(ss,_) ident nameKind [] [A.MkUnguarded val] : rest) ret = wrapTrace ("transformLetBindings VALDEC " <> showIdent' ident <> " = " <> renderValue 100 val) $ do
  ty <- inferType Nothing val {- FIXME: This sometimes gives us a type w/ unknowns, but we don't have any other way to get at the type -}
  if not (containsUnknowns ty)
    then bindNames (M.singleton (Qualified (BySourcePos $ spanStart ss) ident) (ty, nameKind, Defined)) $ do
      thisDecl <- declToCoreFn mn (A.ValueDecl sa ident nameKind [] [A.MkUnguarded (A.TypedValue False val ty)])
      let seen' = seen ++ thisDecl
      transformLetBindings mn _ss seen' rest ret
    else  error
            $ "The inferred type for let-bound identifier \n   '"
              <> showIdent' ident
              <> "'\ncontains unification variables:\n "
              <> ppType 1000 ty
              <> "\nIf this let-bound identifier occurs in a user-defined `let-binding`, please add a type signature for '" <> showIdent' ident <> "'"
              <> "\nIf the identifier occurs in a compiler-generated `let-binding` with guards (e.g. in a guarded case branch), try removing the guarded expression (e.g. use a normal if-then expression)"
-- NOTE/TODO: This is super hack-ey. Ugh.
transformLetBindings mn _ss seen (A.BindingGroupDeclaration ds : rest) ret = wrapTrace "transformLetBindings BINDINGGROUPDEC" $ do
  SplitBindingGroup untyped typed dict <- typeDictionaryForBindingGroup Nothing . NEL.toList $ fmap (\(i, _, v) -> (i, v)) ds
  if null untyped
    then do
      let ds' =  flip map typed $ \((sann,iden),(expr,_,ty,_)) -> A.ValueDecl sann iden Private [] [A.MkUnguarded (A.TypedValue False expr ty)]
      bindNames dict $ do
        makeBindingGroupVisible
        thisDecl <- concat <$> traverse (declToCoreFn mn) ds'
        let seen' = seen ++ thisDecl
        transformLetBindings mn _ss seen' rest ret
    -- Because this has already been through the typechecker once, every value in the binding group should have an explicit type. I hope.
    else error
         $ "untyped binding group element in mutually recursive LET binding group after initial typechecker pass: \n"
         <> LT.unpack (pShow untyped)
transformLetBindings _ _ _ _ _ = error "Invalid argument to TransformLetBindings"


-- | Infer the types of variables brought into scope by a binder *without* instantiating polytypes to unknowns.
-- TODO: Check whether unifyTypes needed
inferBinder'
  :: forall m
   . (MonadState CheckState m, MonadError MultipleErrors m, MonadWriter MultipleErrors m)
  => SourceType
  -> A.Binder
  -> m (M.Map Ident (SourceSpan, SourceType))
inferBinder' _ A.NullBinder = return M.empty
inferBinder' val (A.LiteralBinder _ (StringLiteral _)) = wrapTrace "inferBinder' STRLIT" $ unifyTypes val tyString >> return M.empty
inferBinder' val (A.LiteralBinder _ (CharLiteral _)) = wrapTrace "inferBinder' CHARLIT" $ unifyTypes val tyChar >> return M.empty
inferBinder' val (A.LiteralBinder _ (NumericLiteral (Left _))) = wrapTrace "inferBinder' LITINT" $ unifyTypes val tyInt >> return M.empty
inferBinder' val (A.LiteralBinder _ (NumericLiteral (Right _))) = wrapTrace "inferBinder' NUMBERLIT" $ unifyTypes val tyNumber >> return M.empty
inferBinder' val (A.LiteralBinder _ (BooleanLiteral _)) = wrapTrace "inferBinder' BOOLLIT" $ unifyTypes val tyBoolean >> return M.empty
inferBinder' val (A.VarBinder ss name) = wrapTrace ("inferBinder' VAR " <> T.unpack (runIdent name))  $ return $ M.singleton name (ss, val)
inferBinder' val (A.ConstructorBinder ss ctor binders) = wrapTrace ("inferBinder' CTOR: " <> show ctor) $ do
  env <- getEnv
  case M.lookup ctor (dataConstructors env) of
    Just (_, _, ty, _) -> do
      traceM (ppType 100 ty)
      let (args, ret) = peelArgs ty
      -- unifyTypes ret val -- TODO: Check whether necesseary?
      M.unions <$> zipWithM inferBinder' (reverse args) binders
    _ -> throwError . errorMessage' ss . UnknownName . fmap DctorName $ ctor
  where
  -- REVIEW: Instantiating the quantifier might not be safe here?
  peelArgs :: Type a -> ([Type a], Type a) -- NOTE: Not sure if we want to "peel constraints" too. Need to think of an example to test.
  peelArgs = go []
    where
    go args (ForAll _ _ _ _ innerTy _) = go args innerTy
    go args (TypeApp _ (TypeApp _ fn arg) ret) | eqType fn tyFunction = go (arg : args) ret
    go args ret = (args, ret)
inferBinder' val (A.LiteralBinder _ (ObjectLiteral props)) = wrapTrace "inferBinder' OBJECTLIT" $ do
  let props' = sortOn fst props
  case unwrapRecord val of
    Left notARecord -> error
         $ "Internal error while desugaring binders to CoreFn: \nType "
           <> ppType 100 notARecord
           <> "\n is not a record type"
    Right rowItems -> do
      let typeKeys = S.fromList $ fst <$> rowItems
          exprKeys = S.fromList $ fst <$> props'
          -- The type-level labels are authoritative
          diff = S.difference typeKeys exprKeys
      if S.null diff
        then deduceRowProperties (M.fromList rowItems) props' -- M.unions <$> zipWithM inferBinder' (snd <$> rowItems) (snd <$> props')
        else error $ "Error. Object literal in a pattern match is missing fields: " <> show diff
  where
  deduceRowProperties :: M.Map PSString SourceType -> [(PSString,A.Binder)] -> m (M.Map Ident (SourceSpan,SourceType))
  deduceRowProperties types [] = pure M.empty
  deduceRowProperties types ((lbl,bndr):rest) = case M.lookup lbl types of
    Nothing -> error $ "Cannot deduce type information for record with label " <> show lbl -- should be impossible after typechecking
    Just ty -> do
      x <- inferBinder' ty bndr
      xs <- deduceRowProperties types rest
      pure $ M.union x xs
-- TODO: Remove ArrayT pattern synonym
inferBinder' (ArrayT val) (A.LiteralBinder _ (ArrayLiteral binders)) = wrapTrace "inferBinder' ARRAYLIT" $  do
  M.unions <$> traverse (inferBinder' val) binders
inferBinder' _ (A.LiteralBinder _ (ArrayLiteral _)) = internalError "bad type in array binder "
inferBinder' val (A.NamedBinder ss name binder) = wrapTrace ("inferBinder' NAMEDBINDER " <> T.unpack (runIdent name)) $
  warnAndRethrowWithPositionTC ss $ do
    m <- inferBinder' val binder
    return $ M.insert name (ss, val) m
inferBinder' val (A.PositionedBinder pos _ binder) = wrapTrace "inferBinder' POSITIONEDBINDER" $
  warnAndRethrowWithPositionTC pos $ inferBinder' val binder
inferBinder' val (A.TypedBinder ty binder) = wrapTrace "inferBinder' TYPEDBINDER" $ do
  (elabTy, kind) <- kindOf ty
  checkTypeKind ty kind -- NOTE: Check whether we really need to do anything except inferBinder' the inner
  unifyTypes val elabTy
  inferBinder' elabTy binder
inferBinder' _ A.OpBinder{} =
  internalError "OpBinder should have been desugared before inferBinder'"
inferBinder' _ A.BinaryNoParensBinder{} =
  internalError "BinaryNoParensBinder should have been desugared before inferBinder'"
inferBinder' _ A.ParensInBinder{} =
  internalError "ParensInBinder should have been desugared before inferBinder'"

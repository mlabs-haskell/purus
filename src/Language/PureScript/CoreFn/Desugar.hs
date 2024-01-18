module Language.PureScript.CoreFn.Desugar(moduleToCoreFn) where

import Prelude
import Protolude (ordNub, orEmpty)


import Data.Function (on)
import Data.Maybe (mapMaybe)
import Data.Tuple (swap)
import Data.List.NonEmpty qualified as NEL
import Data.Map qualified as M

import Language.PureScript.AST.Literals (Literal(..))
import Language.PureScript.AST.SourcePos (pattern NullSourceSpan, SourceSpan(..))
import Language.PureScript.AST.Traversals (everythingOnValues)
import Language.PureScript.CoreFn.Ann (Ann, ssAnn)
import Language.PureScript.CoreFn.Binders (Binder(..))
import Language.PureScript.CoreFn.Expr (Bind(..), CaseAlternative(..), Expr(..), Guard, PurusType, exprType)
import Language.PureScript.CoreFn.Meta (ConstructorType(..), Meta(..))
import Language.PureScript.CoreFn.Module (Module(..))
import Language.PureScript.Crash (internalError)
import Language.PureScript.Environment (DataDeclType(..), Environment(..), NameKind(..), isDictTypeName, lookupConstructor, lookupValue, purusFun, NameVisibility (..), tyBoolean)
import Language.PureScript.Label (Label(..))
import Language.PureScript.Names (pattern ByNullSourcePos, Ident(..), ModuleName, ProperName(..), ProperNameType(..), Qualified(..), QualifiedBy(..), disqualify, getQual, mkQualified, showIdent, runIdent)
import Language.PureScript.PSString (PSString)
import Language.PureScript.Types (pattern REmptyKinded, SourceType, Type(..))
import Language.PureScript.AST.Binders qualified as A
import Language.PureScript.AST.Declarations qualified as A
import Language.PureScript.AST.SourcePos qualified as A
import Language.PureScript.Constants.Prim qualified as C
import Control.Monad.Supply.Class (MonadSupply)
import Control.Monad.State.Strict (MonadState, gets, modify, MonadIO (liftIO))
import Control.Monad.Writer.Class ( MonadWriter )
import Language.PureScript.TypeChecker (CheckState (checkEnv, checkCurrentModule), withBindingGroupVisible, bindLocalVariables, withScopedTypeVars, bindNames, replaceAllTypeSynonyms, kindOfWithScopedVars, warnAndRethrowWithPositionTC, makeBindingGroupVisible, bindLocalTypeVariables)
import Control.Monad.Error (MonadError)
import Language.PureScript.TypeChecker.Types
    ( kindType,
      checkTypeKind,
      freshTypeWithKind,
      SplitBindingGroup(SplitBindingGroup),
      TypedValue'(TypedValue'),
      BindingGroupType(RecursiveBindingGroup),
      typesOf,
      typeDictionaryForBindingGroup,
      checkTypedBindingGroupElement,
      typeForBindingGroupElement,
      infer,
      check, tvToExpr, instantiatePolyTypeWithUnknowns )
import Data.List.NonEmpty qualified as NE
import Language.PureScript.TypeChecker.Unify (unifyTypes, replaceTypeWildcards, freshType)
import Control.Monad (forM, (<=<))
import Language.PureScript.TypeChecker.Skolems (introduceSkolemScope)
import Language.PureScript.Errors (MultipleErrors, parU)
import Debug.Trace (traceM)
import Language.PureScript.CoreFn.Pretty
import qualified Data.Text as T
import Language.PureScript.Pretty.Values (renderValue)
type M m = (MonadSupply m, MonadState CheckState m, MonadError MultipleErrors m, MonadWriter MultipleErrors m)

purusTy :: Type a -> PurusType
purusTy = fmap (const ())

unFun :: Type a -> Either (Type a) (Type a,Type a)
unFun = \case
  TypeApp _ (TypeApp _ (TypeConstructor _ C.Function) a) b -> Right (a,b)
  other -> Left other



-- We're going to run this *after* a pass of the unmodified typechecker, using the Env of the already-typechecked-by-the-default-checker module
-- That *should* allow us to avoid repeating the entire TC process, and simply infer/lookup types when we need them. Hopefully.

-- | Desugars a module from AST to CoreFn representation.
moduleToCoreFn :: forall m. M m => A.Module -> m (Module Ann)
moduleToCoreFn  (A.Module _ _ _ _ Nothing) =
  internalError "Module exports were not elaborated before moduleToCoreFn"
moduleToCoreFn mod@(A.Module modSS coms mn decls (Just exps)) = do
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
  -- Creates a map from a module name to the re-export references defined in
  -- that module.
reExportsToCoreFn :: (ModuleName, A.DeclarationRef) -> M.Map ModuleName [Ident]
reExportsToCoreFn (mn', ref') = M.singleton mn' (exportToCoreFn ref')

toReExportRef :: A.DeclarationRef -> Maybe (ModuleName, A.DeclarationRef)
toReExportRef (A.ReExportRef _ src ref) =
      fmap
        (, ref)
        (A.exportSourceImportedFrom src)
toReExportRef _ = Nothing

  -- Remove duplicate imports
dedupeImports :: [(Ann, ModuleName)] -> [(Ann, ModuleName)]
dedupeImports = fmap swap . M.toList . M.fromListWith const . fmap swap

ssA :: SourceSpan -> Ann
ssA ss = (ss, [], Nothing)


lookupType :: forall m. M m => A.SourcePos -> Ident -> m (SourceType,NameVisibility)
lookupType sp tn = do
  mn <- Language.PureScript.CoreFn.Desugar.moduleName
  env <- gets checkEnv
  printEnv >>= traceM
  case M.lookup  (Qualified (BySourcePos sp) tn) (names env) of
    Nothing -> case M.lookup (mkQualified tn mn) (names env) of
      Nothing -> error $ "No type found for " <> show tn
      Just (ty,nk,nv) -> do
        traceM $ "lookupType: " <> T.unpack (showIdent tn) <> " :: " <> ppType 10 ty
        pure (ty,nv)
    Just (ty,nk,nv) -> do
      traceM $ "lookupType: " <> T.unpack (showIdent tn) <> " :: " <> ppType 10 ty
      pure (ty,nv)
 where
     printEnv :: m String
     printEnv = do
       env <- gets checkEnv
       let ns = map (\(i,(st,_,_)) -> (i,st)) . M.toList $ names env
       pure $ concatMap (\(i,st) -> "ENV:= " <> T.unpack (runIdent . disqualify $  i) <> " :: " <> ppType 10 st <> "\n") ns



lookupType' :: forall m. M m => A.SourcePos -> Ident -> m (SourceType,NameVisibility)
lookupType' sp tn = do
  traceM $ "lookupType': " <> show tn
  env <- gets checkEnv
  --traceM $ show env
  case M.lookup (Qualified (BySourcePos sp) tn) (names env) of
    Nothing -> error $ "(2) No type found for " <> show tn
    Just (ty,nk,nv) -> do
      traceM $ "lookupType: " <> T.unpack (showIdent tn) <> " :: " <> ppType 10 ty
      pure (ty,nv)


lookupCtorDeclTy :: M m => ModuleName -> A.DataConstructorDeclaration -> m SourceType
lookupCtorDeclTy mn (A.DataConstructorDeclaration ann ctorName fields)= do
  env <- gets checkEnv
  case M.lookup (mkQualified ctorName mn) (dataConstructors env) of
    Nothing -> error $ "No constr decl info found for " <> show ctorName
    Just (_declType,_tyName,ty,_idents) -> pure ty



moduleName :: M m => m ModuleName
moduleName = gets checkCurrentModule >>= \case
  Just mn -> pure mn
  Nothing -> error "No module name found in checkState"

-- Desugars member declarations from AST to CoreFn representation.
declToCoreFn :: forall m. M m => ModuleName -> A.Declaration -> m [Bind Ann]
declToCoreFn mn (A.DataDeclaration (ss, com) Newtype name _ [ctor]) = case A.dataCtorFields ctor of
  [(_,wrappedTy)] -> do
    -- declTy <- lookupType mn name           // might need this?
    let innerFunTy = purusFun wrappedTy wrappedTy
    pure [NonRec ((ss, [], declMeta)) (properToIdent $ A.dataCtorName ctor) $
      Abs (ss, com, Just IsNewtype) innerFunTy (Ident "x") (Var (ssAnn ss) (purusTy wrappedTy) $ Qualified ByNullSourcePos (Ident "x"))]
  _ -> error "Found newtype with multiple fields"
  where
  declMeta = isDictTypeName (A.dataCtorName ctor) `orEmpty` IsTypeClassConstructor
declToCoreFn _ d@(A.DataDeclaration _ Newtype _ _ _) =
  error $ "Found newtype with multiple constructors: " ++ show d
declToCoreFn  mn (A.DataDeclaration (ss, com) Data tyName _ ctors) =
  traverse go ctors
 where
  go ctorDecl = do
    env <- gets checkEnv
    let ctor = A.dataCtorName ctorDecl
        (_, _, ctorTy, fields) = lookupConstructor  env (Qualified (ByModuleName mn) ctor)
    -- ctorDeclTy <- lookupCtorDeclTy mn ctorDecl
    pure $ NonRec (ssA ss) (properToIdent ctor) $ Constructor (ss, com, Nothing) (purusTy ctorTy) tyName ctor fields
declToCoreFn mn (A.DataBindingGroupDeclaration ds) =
  concat <$> traverse (declToCoreFn  mn) ds
declToCoreFn  mn (A.ValueDecl (ss, com) name _ _ [A.MkUnguarded e]) = do
  --traceM $ "decltoCoreFn " <> show name
  -- env <- gets checkEnv
  (valDeclTy,nv) <- lookupType (spanStart ss) name
  traceM $ "decltoCoreFn " <> show name <> " :: " <> ppType 10 valDeclTy
  bindLocalVariables [(ss,name,valDeclTy,nv)] $ do
      expr <- exprToCoreFn mn ss (Just valDeclTy)  e -- maybe wrong? might need to bind something here?
      pure [NonRec (ssA ss) name expr]

declToCoreFn  mn (A.BindingGroupDeclaration ds) = do
  let stripped :: [((A.SourceAnn, Ident), A.Expr)] = NE.toList $  (\(((ss, com), name), _, e) -> (((ss, com), name), e)) <$> ds
  types <- typesOf RecursiveBindingGroup mn stripped -- kind of redundant, this has already been performed in normal typechecking so we could just look up the types for each value decl ident
  -- types <- traverse lookupTypes stripped
  recBody <- bindLocalVariables (prepareBind <$> types) $ traverse goRecBindings types
  pure [Rec recBody]
 where
   prepareBind :: ((A.SourceAnn, Ident), (A.Expr, SourceType)) -> (SourceSpan, Ident, SourceType, NameVisibility)
   prepareBind (((ss',_),ident),(e,sty)) = (ss',ident,sty,Defined)

   -- lookupTypes :: ((A.SourceAnn, Ident), A.Expr) -> m ((A.SourceAnn, Ident), (A.Expr, SourceType))
   -- lookupTypes ((ann,ident),exp) = lookupType mn ident >>= \(ty,_) -> pure ((ann,ident),(exp,ty))

   goRecBindings ::  ((A.SourceAnn, Ident), (A.Expr, SourceType)) -> m ((Ann, Ident), Expr Ann)
   goRecBindings ((ann,ident),(expr,ty)) = do
     expr' <- exprToCoreFn mn (fst ann) (Just ty) expr
     pure ((ssA $ fst ann,ident), expr')
declToCoreFn _ _ = pure []

traverseLit :: forall m a b. Monad m => (a -> m b) -> Literal a -> m (Literal b)
traverseLit f = \case
  NumericLiteral x -> pure $ NumericLiteral x
  StringLiteral x -> pure $ StringLiteral x
  CharLiteral x -> pure $ CharLiteral x
  BooleanLiteral x -> pure $ BooleanLiteral x
  ArrayLiteral xs  -> ArrayLiteral <$> traverse f xs
  ObjectLiteral xs -> ObjectLiteral <$> traverse (\(str,x) -> f x >>= \b -> pure (str,b)) xs

inferType :: M m => Maybe SourceType -> A.Expr -> m SourceType
inferType (Just t) _ = pure t
inferType Nothing e = infer e >>= \case
  TypedValue' _ _ t -> pure t

-- Desugars expressions from AST to CoreFn representation.
exprToCoreFn :: forall m. M m => ModuleName -> SourceSpan ->  Maybe SourceType -> A.Expr -> m (Expr Ann)
exprToCoreFn mn _ mTy astLit@(A.Literal ss lit) = do
  litT <- purusTy <$> inferType mTy astLit
  lit' <- traverseLit (exprToCoreFn mn ss Nothing) lit
  pure $ Literal (ss, [], Nothing)  litT lit'

exprToCoreFn mn ss mTy  accessor@(A.Accessor name v) = do
  expT <- purusTy <$> inferType mTy accessor
  expr  <- exprToCoreFn mn ss Nothing v
  pure $ Accessor (ssA ss) expT name expr

exprToCoreFn mn ss mTy objUpd@(A.ObjectUpdate obj vs) = do
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
exprToCoreFn mn ss (Just ty) lam@(A.Abs (A.VarBinder ssb name) v) = do
  traceM $ "exprToCoreFn lam " <>  T.unpack (showIdent name) <> " :: " <>  ppType 10 ty

  case ty of
    ft@(ForAll ann vis var mbk qty mSkol) -> case unFun qty of
      Right (a,b) ->  do
        traceM "ForAll branch"
        traceM $ "arg: " <> ppType 10 a
        traceM $ "result: " <> ppType 10 b
        let toBind = [(ssb, name, a, Defined)]
        bindLocalVariables toBind $ do
          body <- exprToCoreFn mn ss (Just b) v
          pure $ Abs (ssA ssb) (ForAll () vis var (purusTy <$> mbk) (purusFun a b) mSkol) name body
      Left e -> error $ "All lambda abstractions should have either a function type or a quantified function type: " <> ppType 10 e
    other -> case unFun other of
      Right (a,b) -> do
        let toBind = [(ssb, name, a, Defined )]
        bindLocalVariables toBind $ do
          body <- exprToCoreFn mn ss (Just b) v
          pure $ Abs (ssA ssb) (purusFun a b) name body
      Left e ->  error $ "All lambda abstractions should have either a function type or a quantified function type: " <> ppType 10 e
  -- error "boom"

  {- (unFun <$> inferType (Just ty) lam) >>= \case
    Right (a,b) -> do
      traceM $ "function lam " <> ppType 10 ty -- prettyPrintType 0 (purusFun a b)
      let toBind = [(ssb, name, a, Defined )]
      bindLocalVariables toBind $ do
        body <- exprToCoreFn mn ss Nothing v -- (Just b) v
        pure $ Abs (ssA ssb) {- (purusFun a b) -} (purusTy ty) name body
    Left _ty -> do
      traceM $ "??? lam " <> prettyPrintType 0 _ty
      body <- exprToCoreFn mn ss Nothing v
      pure $ Abs (ssA ssb) (purusTy ty) name body
-}
exprToCoreFn _  _ _ lam@(A.Abs _ _) =
  internalError $ "Abs with Binder argument was not desugared before exprToCoreFn mn" <> show lam
exprToCoreFn mn ss mTy app@(A.App v1 v2) = do
  appT <- inferType mTy  app
  v1' <- exprToCoreFn mn ss Nothing v1
  v2' <- exprToCoreFn mn ss Nothing v2
  pure $ App (ss, [], (isDictCtor v1 || isSynthetic v2) `orEmpty` IsSyntheticApp) (purusTy appT) v1' v2'
  where
  isDictCtor = \case
    A.Constructor _ (Qualified _ name) -> isDictTypeName name
    _ -> False
  isSynthetic = \case
    A.App v3 v4            -> isDictCtor v3 || isSynthetic v3 && isSynthetic v4
    A.Accessor _ v3        -> isSynthetic v3
    A.Var NullSourceSpan _ -> True
    A.Unused{}             -> True
    _                      -> False
exprToCoreFn mn ss  _ (A.Unused _) = -- ????? need to figure out what this _is_
  error "Don't know what to do w/ exprToCoreFn A.Unused"
  -- pure $ Var (ss, com, Nothing) C.I_undefined
-- exprToCoreFn mn _ (Just ty) (A.Var ss ident) = gets checkEnv >>= \env ->
--   pure $ Var (ss, [], getValueMeta env ident) (purusTy ty) ident
exprToCoreFn mn _ _ (A.Var ss ident) =
  gets checkEnv >>= \env -> case lookupValue env ident of
    Just (ty,_,_) -> pure $ Var (ss, [], getValueMeta env ident) (purusTy ty) ident
    Nothing -> error $ "No known type for identifier " <> show ident
exprToCoreFn mn ss mTy ifte@(A.IfThenElse cond th el) = do
  ifteTy <- inferType mTy ifte
  condE <- exprToCoreFn mn ss (Just tyBoolean) cond
  thE <- exprToCoreFn mn ss Nothing th
  elE <- exprToCoreFn mn ss Nothing  el
  pure $ Case (ss, [], Nothing) (purusTy ifteTy) [condE]
    [ CaseAlternative [LiteralBinder (ssAnn ss) $ BooleanLiteral True]
                      (Right thE)
    , CaseAlternative [NullBinder (ssAnn ss)] -- *
                      (Right elE) ]
exprToCoreFn mn _  mTy ctor@(A.Constructor ss name) = do
  env <- gets checkEnv
  let ctorMeta = getConstructorMeta env name
  ctorType <- inferType mTy ctor
  pure $ Var (ss, [], Just ctorMeta) (purusTy ctorType) $ fmap properToIdent name
exprToCoreFn mn ss mTy astCase@(A.Case vs alts) = do
  caseTy <- inferType mTy astCase
  vs' <- traverse (exprToCoreFn mn ss Nothing) vs
  alts' <- traverse (altToCoreFn mn ss) alts
  pure $ Case (ssA ss) (purusTy caseTy) vs' alts'
exprToCoreFn  mn ss  (Just ty) (A.TypedValue _ v _) =
  exprToCoreFn mn ss (Just ty) v
exprToCoreFn mn ss Nothing (A.TypedValue _ v ty) =
  exprToCoreFn mn ss (Just ty) v
exprToCoreFn  mn ss mTy astLet@(A.Let w ds v) = case NE.nonEmpty ds of
  Nothing -> error "declarations in a let binding can't be empty"
  Just ds' -> do
    traceM $ "exprToCoreFn LET: " <> show astLet
    types <- typesOf RecursiveBindingGroup  mn $  fmap stripDecls ds
    traceM $ concatMap (\x -> show x <> "\n\n") types
    bindLocalVariables (prepareBind <$> types) $ do
      printEnv
      expr <- exprToCoreFn mn ss Nothing v
      decls <- concat <$> traverse (declToCoreFn mn) (toValueDecl <$> types)
    -- (ds', expr) <- transformLetBindings mn ss [] ds v
      pure $ Let (ss, [], getLetMeta w) (exprType expr) decls expr
   where
     toValueDecl ::  ((A.SourceAnn, Ident), (A.Expr, SourceType)) -> A.Declaration
     toValueDecl ((ss',ident),(exp,ty)) = A.ValueDecl ss' ident Public [] [A.MkUnguarded exp]

     printEnv :: m ()
     printEnv = do
       env <- gets checkEnv
       let ns = map (\(i,(st,_,_)) -> (i,st)) . M.toList $ names env
       mapM_ (\(i,st) -> traceM $  T.unpack (runIdent . disqualify $  i) <> " :: " <> ppType 10 st) ns

     prepareBind :: ((A.SourceAnn, Ident), (A.Expr, SourceType)) -> (SourceSpan, Ident, SourceType, NameVisibility)
     prepareBind (((ss',_),ident),(e,sty)) = (ss',ident,sty,Defined)

     transformBind :: ((Ann, Ident), Expr Ann) -> (SourceSpan, Ident, SourceType, NameVisibility)
     transformBind (((ss',_,_),ident),expr) = (ss',ident,const (ss',[]) <$> exprType expr, Defined)
     -- Everything here *should* be a value declaration. I hope?
     stripDecls ::  A.Declaration-> ((A.SourceAnn, Ident),  A.Expr)
     stripDecls = \case
       A.ValueDecl ann ident nKind [] [A.MkUnguarded e] -> ((ann,ident), e)
       other -> error $ "let bindings should only contain value declarations w/ desugared binders and a single expr. this doesn't: " <> show other
exprToCoreFn  mn _ ty (A.PositionedValue ss _ v) =
  exprToCoreFn mn ss ty v
exprToCoreFn _ _   _ e =
  error $ "Unexpected value in exprToCoreFn mn: " ++ show e

transformLetBindings :: forall m. M m => ModuleName -> SourceSpan -> [Bind Ann] -> [A.Declaration] -> A.Expr -> m ([Bind Ann], Expr Ann)
transformLetBindings mn ss seen [] ret =(seen,) <$> withBindingGroupVisible (exprToCoreFn mn ss Nothing ret)
-- for typed values (this might be wrong?)
transformLetBindings mn _ss seen ((A.ValueDecl sa@(ss,_) ident nameKind [] [A.MkUnguarded (A.TypedValue checkType val ty)]) : rest) ret = do
  TypedValue' _ val' ty'' <- warnAndRethrowWithPositionTC ss $ do
    ((args, elabTy), kind) <- kindOfWithScopedVars ty
    checkTypeKind ty kind
    let dict = M.singleton (Qualified (BySourcePos $ spanStart ss) ident) (elabTy, nameKind, Undefined)
    ty' <- introduceSkolemScope <=< replaceAllTypeSynonyms <=< replaceTypeWildcards $ elabTy
    if checkType
      then withScopedTypeVars mn args $ bindNames dict $ check val ty'
      else return (TypedValue' checkType val elabTy)
  bindNames (M.singleton (Qualified (BySourcePos $ spanStart ss) ident) (ty'', nameKind, Defined)) $ do
    thisDecl <- declToCoreFn mn (A.ValueDecl sa ident nameKind [] [A.MkUnguarded (A.TypedValue checkType val' ty'')])
    let seen' = seen ++ thisDecl
    transformLetBindings mn _ss seen' rest ret
-- untyped values
transformLetBindings mn _ss seen (A.ValueDecl sa@(ss,_) ident nameKind [] [A.MkUnguarded val] : rest) ret = do
  valTy <- freshTypeWithKind kindType
  TypedValue' _ val' valTy' <- warnAndRethrowWithPositionTC ss $ do
    let dict = M.singleton (Qualified (BySourcePos $ spanStart ss) ident) (valTy, nameKind, Undefined)
    bindNames dict $ infer val
  warnAndRethrowWithPositionTC ss $ unifyTypes valTy valTy'
  bindNames (M.singleton (Qualified (BySourcePos $ spanStart ss) ident) (valTy', nameKind, Defined)) $ do
    thisDecl <- declToCoreFn mn (A.ValueDecl sa ident nameKind [] [A.MkUnguarded val'])
    let seen' = seen ++ thisDecl
    transformLetBindings mn _ss seen' rest ret
transformLetBindings mn _ss seen (A.BindingGroupDeclaration ds : rest) ret = do
  SplitBindingGroup untyped typed dict <- typeDictionaryForBindingGroup Nothing . NEL.toList $ fmap (\(i, _, v) -> (i, v)) ds
  ds1' <- parU typed $ \e -> checkTypedBindingGroupElement mn e dict
  ds2' <- forM untyped $ \e -> typeForBindingGroupElement e dict
  let ds' = NEL.fromList [(ident, Private, val') | (ident, (val', _)) <- ds1' ++ ds2']
  bindNames dict $ do
    makeBindingGroupVisible
    thisDecl <- declToCoreFn mn (A.BindingGroupDeclaration ds')
    let seen' = seen ++ thisDecl
    transformLetBindings mn _ss seen' rest ret
transformLetBindings _ _ _ _ _ = error "Invalid argument to TransformLetBindings"


-- Desugars case alternatives from AST to CoreFn representation.
altToCoreFn ::  forall m. M m => ModuleName -> SourceSpan -> A.CaseAlternative -> m (CaseAlternative Ann)
altToCoreFn  mn ss (A.CaseAlternative bs vs) = do
    env <- gets checkEnv
    let binders = binderToCoreFn env mn ss <$> bs
    ege <- go vs
    pure $ CaseAlternative binders ege
  where
  go :: [A.GuardedExpr] -> m (Either [(Guard Ann, Expr Ann)] (Expr Ann))
  go [A.MkUnguarded e] = do
    expr <- exprToCoreFn mn ss Nothing e
    pure $ Right expr
  go gs = do
    ges <- forM gs $ \case
      A.GuardedExpr g e -> do
        let cond = guardToExpr g
        condE <- exprToCoreFn mn ss Nothing cond
        eE    <- exprToCoreFn mn ss Nothing e
        pure (condE,eE)
    pure . Left $ ges
  guardToExpr [A.ConditionGuard cond] = cond
  guardToExpr _ = internalError "Guard not correctly desugared"

-- This should ONLY ever be used to create a type in contexts where one doesn't make sense
tUnknown :: forall a. a -> Type a
tUnknown x = TUnknown x (-1)

-- I'm not sure how to type Binders. Likely we need a new syntatic construct? But if the sub-terms are well-typed we should be able to give binder a placeholder type? idk
-- Desugars case binders from AST to CoreFn representation.
binderToCoreFn ::  Environment -> ModuleName -> SourceSpan -> A.Binder -> Binder Ann
binderToCoreFn  env mn _ss (A.LiteralBinder ss lit) =
  let lit' = binderToCoreFn env mn ss <$> lit
      ty = tUnknown (ss,[])
  in  LiteralBinder (ss, [], Nothing) lit'
binderToCoreFn _ mn ss A.NullBinder =
  let ty = tUnknown (ss,[])
  in NullBinder (ss, [], Nothing)
binderToCoreFn _ mn _ss  (A.VarBinder ss name) =
  let ty = tUnknown (ss,[])
  in  VarBinder (ss, [], Nothing) name
binderToCoreFn env mn _ss (A.ConstructorBinder ss dctor@(Qualified mn' _) bs) =
  let (_, tctor, _, _) = lookupConstructor env dctor
      ty = tUnknown (ss,[])
      args = binderToCoreFn env mn _ss <$> bs
  in  ConstructorBinder (ss, [], Just $ getConstructorMeta env dctor) (Qualified mn' tctor) dctor args
binderToCoreFn env mn _ss (A.NamedBinder ss name b) =
  let ty = tUnknown (ss,[])
      arg = binderToCoreFn env mn _ss b
  in  NamedBinder (ss, [], Nothing) name arg
binderToCoreFn env mn _ss (A.PositionedBinder ss _ b) =
  binderToCoreFn env mn ss  b
binderToCoreFn env mn ss  (A.TypedBinder _ b) =
  binderToCoreFn env mn ss  b
binderToCoreFn _ _ _  A.OpBinder{} =
  internalError "OpBinder should have been desugared before binderToCoreFn"
binderToCoreFn _ _ _  A.BinaryNoParensBinder{} =
  internalError "BinaryNoParensBinder should have been desugared before binderToCoreFn"
binderToCoreFn _ _ _  A.ParensInBinder{} =
  internalError "ParensInBinder should have been desugared before binderToCoreFn"

-- Gets metadata for let bindings.
getLetMeta :: A.WhereProvenance -> Maybe Meta
getLetMeta A.FromWhere = Just IsWhere
getLetMeta A.FromLet = Nothing

-- Gets metadata for values.
getValueMeta :: Environment -> Qualified Ident -> Maybe Meta
getValueMeta env name =
  case lookupValue env name of
    Just (_, External, _) -> Just IsForeign
    _ -> Nothing

-- Gets metadata for data constructors.
getConstructorMeta :: Environment -> Qualified (ProperName 'ConstructorName) -> Meta
getConstructorMeta env ctor =
  case lookupConstructor env ctor of
    (Newtype, _, _, _) -> IsNewtype
    dc@(Data, _, _, fields) ->
      let constructorType = if numConstructors (ctor, dc) == 1 then ProductType else SumType
      in IsConstructor constructorType fields
  where

  numConstructors
    :: (Qualified (ProperName 'ConstructorName), (DataDeclType, ProperName 'TypeName, SourceType, [Ident]))
    -> Int
  numConstructors ty = length $ filter (((==) `on` typeConstructor) ty) $ M.toList $ dataConstructors env

  typeConstructor
    :: (Qualified (ProperName 'ConstructorName), (DataDeclType, ProperName 'TypeName, SourceType, [Ident]))
    -> (ModuleName, ProperName 'TypeName)
  typeConstructor (Qualified (ByModuleName mn') _, (_, tyCtor, _, _)) = (mn', tyCtor)
  typeConstructor _ = internalError "Invalid argument to typeConstructor"

-- | Find module names from qualified references to values. This is used to
-- ensure instances are imported from any module that is referenced by the
-- current module, not just from those that are imported explicitly (#667).
findQualModules :: [A.Declaration] -> [ModuleName]
findQualModules decls =
 let (f, _, _, _, _) = everythingOnValues (++) fqDecls fqValues fqBinders (const []) (const [])
 in f `concatMap` decls

fqDecls :: A.Declaration -> [ModuleName]
fqDecls (A.TypeInstanceDeclaration _ _ _ _ _ _ q _ _) = getQual' q
fqDecls (A.ValueFixityDeclaration _ _ q _) = getQual' q
fqDecls (A.TypeFixityDeclaration _ _ q _) = getQual' q
fqDecls _ = []

fqValues :: A.Expr -> [ModuleName]
fqValues (A.Var _ q) = getQual' q
fqValues (A.Constructor _ q) = getQual' q
fqValues _ = []

fqBinders :: A.Binder -> [ModuleName]
fqBinders (A.ConstructorBinder _ q _) = getQual' q
fqBinders _ = []

getQual' :: Qualified a -> [ModuleName]
getQual' = maybe [] return . getQual

-- | Desugars import declarations from AST to CoreFn representation.
importToCoreFn :: A.Declaration -> Maybe (Ann, ModuleName)
-- TODO: We probably *DO* want types here
importToCoreFn (A.ImportDeclaration (ss, com) name _ _) = Just ((ss, com, Nothing), name)
importToCoreFn _ = Nothing

-- | Desugars foreign declarations from AST to CoreFn representation.
externToCoreFn :: A.Declaration -> Maybe Ident
externToCoreFn (A.ExternDeclaration _ name _) = Just name
externToCoreFn _ = Nothing

-- | Desugars export declarations references from AST to CoreFn representation.
-- CoreFn modules only export values, so all data constructors, instances and
-- values are flattened into one list.
exportToCoreFn :: A.DeclarationRef -> [Ident]
exportToCoreFn (A.TypeRef _ _ (Just dctors)) = fmap properToIdent dctors
exportToCoreFn (A.TypeRef _ _ Nothing) = []
exportToCoreFn (A.TypeOpRef _ _) = []
exportToCoreFn (A.ValueRef _ name) = [name]
exportToCoreFn (A.ValueOpRef _ _) = []
exportToCoreFn (A.TypeClassRef _ _) = []
exportToCoreFn (A.TypeInstanceRef _ name _) = [name]
exportToCoreFn (A.ModuleRef _ _) = []
exportToCoreFn (A.ReExportRef _ _ _) = []

-- | Converts a ProperName to an Ident.
properToIdent :: ProperName a -> Ident
properToIdent = Ident . runProperName

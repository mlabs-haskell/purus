{- This module is a part of a hack intended to solve a problem arising from the structure of the PS compiler pipeline:
  - We need CoreFn `Expr (Type Ann)` which contains annotates AST nodes with inferred type information
  - PS performs typechecking on the Language.PureScript.AST Expr type, which we don't have access to in the `codegen` function part of the pipeline
  - We need to modify the AST -> CoreFn desguaring phase so that it annotates the AST w/ type information
  - The most sensible way to do that is to do inference & conversion all at once during typechecking
  - We can't do that without disassembling the `moduleToCoreFn` function from the Desugar module

This is a very rough draft ATM. In a more polished version these should all be rewritten to `Reader Env (...)` functions

-}

module Language.PureScript.CoreFn.Typed (moduleToCoreFn) where

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
import Language.PureScript.CoreFn.Typed.Expr (Bind(..), CaseAlternative(..), Expr(..), Guard, PurusType)
import Language.PureScript.CoreFn.Meta (ConstructorType(..), Meta(..))
import Language.PureScript.CoreFn.Typed.Module (Module(..))
import Language.PureScript.Crash (internalError)
import Language.PureScript.Environment (DataDeclType(..), Environment(..), NameKind(..), isDictTypeName, lookupConstructor, lookupValue, purusFun, NameVisibility (..), tyBoolean)
import Language.PureScript.Label (Label(..))
import Language.PureScript.Names (pattern ByNullSourcePos, Ident(..), ModuleName, ProperName(..), ProperNameType(..), Qualified(..), QualifiedBy(..), getQual, mkQualified)
import Language.PureScript.PSString (PSString)
import Language.PureScript.Types (pattern REmptyKinded, SourceType, Type(..))
import Language.PureScript.AST.Binders qualified as A
import Language.PureScript.AST.Declarations qualified as A
import Language.PureScript.AST.SourcePos qualified as A
import Language.PureScript.Constants.Prim qualified as C
import Control.Monad.Supply.Class (MonadSupply)
import Control.Monad.State.Strict (MonadState, gets, modify)
import Control.Monad.Writer.Class ( MonadWriter )
import Language.PureScript.TypeChecker (CheckState (checkEnv, checkCurrentModule), withBindingGroupVisible, bindLocalVariables, withScopedTypeVars, bindNames, replaceAllTypeSynonyms, kindOfWithScopedVars, warnAndRethrowWithPositionTC, makeBindingGroupVisible)
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
      check )
import Data.List.NonEmpty qualified as NE
import Language.PureScript.TypeChecker.Unify (unifyTypes, replaceTypeWildcards)
import Control.Monad (forM, (<=<))
import Language.PureScript.TypeChecker.Skolems (introduceSkolemScope)
import Language.PureScript.Errors (MultipleErrors, parU)
import Debug.Trace (traceM)
import Language.PureScript.Pretty.Types ( prettyPrintType )
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

  {-
  let imports = mapMaybe importToCoreFn decls ++ fmap (ssAnn modSS,) (findQualModules decls)
      imports' = dedupeImports imports
      exps' = ordNub $ concatMap exportToCoreFn exps
      reExps = M.map ordNub $ M.unionsWith (++) (mapMaybe (fmap reExportsToCoreFn . toReExportRef) exps)
      externs = ordNub $ mapMaybe externToCoreFn decls
      decls' = concatMap (declToCoreFn env mn) decls
  in  Module modSS coms mn (spanName modSS) imports' exps' reExps externs decls' -}
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


lookupType :: M m => ModuleName -> ProperName 'TypeName -> m SourceType
lookupType mn tn = do
  env <- gets checkEnv
  case M.lookup (mkQualified tn mn) (types env) of
    Nothing -> error $ "No type found for " <> show tn
    Just (ty,kind) -> pure ty

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
  traceM $ "decltoCoreFn " <> show name
  env <- gets checkEnv
  let mValDeclTy = lookupValue env (mkQualified name mn)
  case mValDeclTy of
    Just(valDeclTy,nameKind,nameVis) -> bindLocalVariables [(ss,name,valDeclTy,nameVis)] $ do
      expr <- exprToCoreFn mn ss (Just valDeclTy)  e -- maybe wrong? might need to bind something here?
      pure $ [NonRec (ssA ss) name expr]
    Nothing -> error $ "No type found for value declaration " <> show name
declToCoreFn  mn (A.BindingGroupDeclaration ds) = do
  let stripped :: [((A.SourceAnn, Ident), A.Expr)] = NE.toList $  (\(((ss, com), name), _, e) -> (((ss, com), name), e)) <$> ds
  types <- typesOf RecursiveBindingGroup mn stripped -- kind of redundant, this has already been performed in normal typechecking so we could just look up the types for each value decl ident
  recBody <- traverse goRecBindings types
  pure [Rec recBody]
 where
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
exprToCoreFn mn ss mTy lam@(A.Abs (A.VarBinder ssb name) v) = do
  traceM $ "exprToCoreFn lam " <> (show name)
  (unFun <$> inferType mTy lam) >>= \case
    Right (a,b) -> do
      traceM $ "function lam " <> prettyPrintType 0 (purusFun a b)
      let toBind = [(ssb, name, a, Defined )]
      bindLocalVariables toBind $ do
        body <- exprToCoreFn mn ss (Just b) v
        pure $ Abs (ssA ssb) (purusFun a b) name body
    Left ty -> do
      traceM $ "??? lam " <> prettyPrintType 0 ty
      body <- exprToCoreFn mn ss (Just ty) v
      pure $ Abs (ssA ssb) (purusTy ty) name body

exprToCoreFn _  _ _ (A.Abs _ _) =
  internalError "Abs with Binder argument was not desugared before exprToCoreFn mn"
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
exprToCoreFn mn _ (Just ty) (A.Var ss ident) = gets checkEnv >>= \env ->
  pure $ Var (ss, [], getValueMeta env ident) (purusTy ty) ident
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
    [ CaseAlternative [LiteralBinder (ssAnn ss) $ BooleanLiteral True] -- no clue what the binder type should be but we'll probably never inspect it
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
exprToCoreFn  mn ss  _ (A.TypedValue _ v ty) =
  exprToCoreFn mn ss (Just ty) v
exprToCoreFn  mn ss mTy astLet@(A.Let w ds v) = do
  letTy <- inferType mTy astLet
  (ds', expr) <- transformLetBindings mn ss [] ds v
  pure $ Let (ss, [], getLetMeta w) (purusTy letTy) ds' expr
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

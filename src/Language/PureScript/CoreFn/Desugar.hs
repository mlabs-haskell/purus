{- HLINT ignore "Use void" -}
{- HLINT ignore "Use <$" -}

module Language.PureScript.CoreFn.Desugar(moduleToCoreFn) where

import Prelude
import Protolude (ordNub, orEmpty, zipWithM, MonadError (..), sortOn, Bifunctor (bimap))

import Data.Maybe (mapMaybe)
import Data.List.NonEmpty qualified as NEL
import Data.Map qualified as M

import Language.PureScript.AST.Literals (Literal(..))
import Language.PureScript.AST.SourcePos (SourceSpan(..), SourceAnn)
import Language.PureScript.CoreFn.Ann (Ann, ssAnn)
import Language.PureScript.CoreFn.Binders (Binder(..))
import Language.PureScript.CoreFn.Expr (Bind(..), CaseAlternative(..), Expr(..), Guard)
import Language.PureScript.CoreFn.Utils (exprType, stripQuantifiers)
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
  NameVisibility (..),
  tyBoolean,
  tyFunction,
  tyString,
  tyChar,
  tyInt,
  tyNumber,
  function,
  pattern RecordT )
import Language.PureScript.Label (Label(..))
import Language.PureScript.Names (
  pattern ByNullSourcePos, Ident(..),
  ModuleName,
  ProperName(..),
  Qualified(..),
  QualifiedBy(..),
  mkQualified,
  runIdent,
  coerceProperName,
  Name (DctorName), ProperNameType (..), disqualify)
import Language.PureScript.PSString (PSString)
import Language.PureScript.Types (
  pattern REmptyKinded,
  SourceType,
  Type(..), quantify, eqType, containsUnknowns, rowToList, RowListItem (..))
import Language.PureScript.AST.Binders qualified as A
import Language.PureScript.AST.Declarations qualified as A
import Language.PureScript.AST.SourcePos qualified as A
import Language.PureScript.Constants.Prim qualified as C
import Control.Monad.State.Strict (MonadState, gets, modify)
import Control.Monad.Writer.Class ( MonadWriter )
import Language.PureScript.TypeChecker.Kinds ( kindOf )
import Data.List.NonEmpty qualified as NE
import Control.Monad (forM, (>=>), foldM)
import Language.PureScript.Errors
    ( MultipleErrors, errorMessage', SimpleErrorMessage(..))
import Debug.Trace (traceM)
import Language.PureScript.CoreFn.Pretty ( ppType, renderExprStr )
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
      getValueMeta,
      importToCoreFn,
      properToIdent,
      purusTy,
      reExportsToCoreFn,
      showIdent',
      ssA,
      toReExportRef,
      wrapTrace,
      desugarConstraintTypes,
      M, unwrapRecord, withInstantiatedFunType, desugarConstraintsInDecl, analyzeCtor, instantiate, ctorArgs, instantiatePolyType, lookupDictType, desugarCasesEverywhere
      )
import Text.Pretty.Simple (pShow)
import Data.Text.Lazy qualified as LT
import Data.Set qualified as S
import Data.Either (lefts)

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
moduleToCoreFn (A.Module modSS coms mn _decls (Just exps)) = do
  setModuleName
  desugarConstraintTypes
  decls <- traverse desugarCasesEverywhere $ desugarConstraintsInDecl <$> _decls
  let importHelper ds = fmap (ssAnn modSS,) (findQualModules ds)
      imports = dedupeImports $ mapMaybe importToCoreFn decls ++ importHelper decls
      exps' = ordNub $ concatMap exportToCoreFn exps
      reExps = M.map ordNub $ M.unionsWith (++) (mapMaybe (fmap reExportsToCoreFn . toReExportRef) exps)
      externs = ordNub $ mapMaybe externToCoreFn decls
  decls' <- concat <$> traverse (declToCoreFn mn) decls
  let dataDecls = mkDataDecls decls
  pure $ Module modSS coms mn (spanName modSS) imports exps' reExps externs decls' dataDecls
 where
   setModuleName = modify $ \cs ->
     cs {checkCurrentModule = Just mn}

{- Turns out we need the data type declarations in order to reconstruct the SOP in PIR

   TODO/REVIEW/FIXME: This won't pull in data declarations from imports, we'll have to handle that in the linker.

-}

type DeclMapElem =  (DataDeclType,[(T.Text, Maybe SourceType)], [A.DataConstructorDeclaration])

mkDataDecls :: [A.Declaration] -> M.Map (ProperName 'TypeName) DeclMapElem
mkDataDecls [] = M.empty
mkDataDecls (d:ds) = case go d of
  Nothing -> mkDataDecls ds
  Just kv -> uncurry M.insert kv $ mkDataDecls ds
 where
 go :: A.Declaration -> Maybe (ProperName 'TypeName,DeclMapElem)
 go = \case
   A.DataDeclaration _ ddTy nm args ctors -> Just (nm,(ddTy,args,ctors))
   _ -> Nothing

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
  case M.lookup (Qualified (BySourcePos sp) tn) (names env) of
    Nothing -> case M.lookup (mkQualified tn mn) (names env) of
      Nothing -> error $ "No type found for " <> show tn
      Just (ty,_,nv) -> do
        traceM $ "lookupType: " <> showIdent' tn <> " :: " <> ppType 10 ty
        pure (ty,nv)
    Just (ty,_,nv) -> do
      traceM $ "lookupType: " <> showIdent' tn <> " :: " <> ppType 10 ty
      pure (ty,nv)

getInnerArrayTy :: Type a -> Maybe (Type a)
getInnerArrayTy (ArrayT arr) = Just arr
getInnerArrayTy (ForAll _ _ _ _ ty _) = getInnerArrayTy ty
getInnerArrayTy _ = Nothing

{-| Extracts inner type of an object if it is behind foralls
-}
getInnerObjectTy :: Type a -> Maybe (Type a)
getInnerObjectTy (RecordT row) = Just row
getInnerObjectTy (ForAll _ _ _ _ ty _) = getInnerObjectTy ty
getInnerObjectTy _ = Nothing

objectToCoreFn :: forall m. M m => ModuleName -> SourceSpan -> SourceType -> SourceType -> [(PSString, A.Expr)] -> m (Expr Ann)
objectToCoreFn mn ss recTy row objFields = do
  traceM $ "ObjLitTy: " <> show row
  let (tyFields,_) = rowToList row
      tyMap = M.fromList $ (\x -> (runLabel (rowListLabel x),x)) <$> tyFields
  resolvedFields <- foldM (go tyMap) [] objFields
  pure $ Literal (ss,[],Nothing) recTy (ObjectLiteral resolvedFields)
 where
   go :: M.Map PSString (RowListItem SourceAnn) -> [(PSString, Expr Ann)] -> (PSString, A.Expr) -> m [(PSString, Expr Ann)]
   go tyMap acc (lbl,expr) = case M.lookup lbl tyMap of
     Just rowListItem -> do
       let fieldTy = rowListType rowListItem
       expr' <- exprToCoreFn mn ss (Just fieldTy) expr
       pure $ (lbl,expr'):acc
     Nothing -> do -- error $ "row type missing field " <> T.unpack (prettyPrintString lbl)
       expr' <- exprToCoreFn mn ss Nothing expr
       pure $ (lbl,expr') : acc

{- Converts declarations from their AST to CoreFn representation, deducing types when possible & inferring them when not possible.

   TODO: The module name can be retrieved from the monadic context and doesn't need to be passed around
-}

-- newtype T = T Foo turns into T :: Foo -> Foo
declToCoreFn :: forall m. M m => ModuleName -> A.Declaration -> m [Bind Ann]
declToCoreFn _ (A.DataDeclaration (ss, com) Newtype name _ [ctor]) = wrapTrace ("decltoCoreFn NEWTYPE " <> show name) $ case A.dataCtorFields ctor of
  [(_,wrappedTy)] -> do
    -- traceM (show ctor)
    let innerFunTy = quantify $ function wrappedTy wrappedTy
    pure [NonRec (ss, [], declMeta) (properToIdent $ A.dataCtorName ctor) $
      Abs (ss, com, Just IsNewtype) innerFunTy (Ident "x") (Var (ssAnn ss) (purusTy wrappedTy) $ Qualified ByNullSourcePos (Ident "x"))]
  _ -> error "Found newtype with multiple fields"
  where
  declMeta = isDictTypeName (A.dataCtorName ctor) `orEmpty` IsTypeClassConstructor
-- Reject newtypes w/ multiple constructors
declToCoreFn _ d@(A.DataDeclaration _ Newtype _ _ _) =
  error $ "Found newtype with multiple constructors: " ++ show d
-- Data declarations get turned into value declarations for the constructor(s)
declToCoreFn  mn (A.DataDeclaration (ss, com) Data tyName _ ctors) = wrapTrace ("declToCoreFn DATADEC " <>  T.unpack (runProperName tyName)) $ traverse go ctors
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
  traceM $ renderValue 100 e
  (valDeclTy,nv) <- lookupType (spanStart ss) name
  traceM (ppType 100 valDeclTy)
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
   extractTypeAndPrepareBind (((_,_),ident),_,_) = error $ "Top level declaration " <> showIdent' ident <> " should have a type annotation, but does not"

   goRecBindings :: (A.Expr, (SourceSpan,Ident,SourceType,NameVisibility)) -> m ((Ann, Ident), Expr Ann)
   goRecBindings (expr,(ss',ident,ty,_)) = do
     expr' <- exprToCoreFn mn ss' (Just ty) expr
     pure ((ssA ss',ident), expr')
-- TODO: Avoid catchall case
declToCoreFn _ _ = pure []

-- Desugars expressions from AST to typed CoreFn representation.
exprToCoreFn :: forall m. M m => ModuleName -> SourceSpan -> Maybe SourceType -> A.Expr -> m (Expr Ann)
-- Array & Object literals can contain non-literal expressions. Both of these types should always be tagged
-- (i.e. returned as an AST.TypedValue) after the initial typechecking phase, so we expect the type to be passed in
exprToCoreFn mn ss (Just arrT) astlit@(A.Literal _ (ArrayLiteral ts))
  | Just ty <- getInnerArrayTy arrT
  = wrapTrace ("exprToCoreFn ARRAYLIT " <> renderValue 100 astlit) $ do
      traceM $ ppType 100 arrT
      arr <- ArrayLiteral <$> traverse (exprToCoreFn mn ss (Just ty)) ts
      pure $ Literal (ss,[],Nothing) arrT  arr

exprToCoreFn _ _ Nothing astlit@(A.Literal _ (ArrayLiteral _)) =
  internalError $ "Error while desugaring Array Literal. No type provided for literal:\n" <> renderValue 100 astlit

exprToCoreFn mn ss (Just recTy) (A.Literal _ (ObjectLiteral objFields))
  | Just row <- getInnerObjectTy recTy
  = objectToCoreFn mn ss recTy row objFields

exprToCoreFn _ _ (Just ty) (A.Literal _ (ObjectLiteral _)) =
  internalError $ "Error while desugaring Object Literal. Unexpected type:\n" <> show ty

exprToCoreFn _ _ Nothing astlit@(A.Literal _ (ObjectLiteral _)) =
  internalError $ "Error while desugaring Object Literal. No type provided for literal:\n" <> renderValue 100 astlit

-- Literals that aren't objects or arrays have deterministic types
exprToCoreFn _ _ _ (A.Literal ss (NumericLiteral (Left int))) =
  pure $ Literal (ss,[],Nothing) tyInt (NumericLiteral (Left int))
exprToCoreFn _ _ _ (A.Literal ss (NumericLiteral (Right number))) =
  pure $ Literal (ss,[],Nothing) tyNumber (NumericLiteral (Right number))
exprToCoreFn _ _ _ (A.Literal ss (CharLiteral char)) =
  pure $ Literal (ss,[],Nothing) tyChar (CharLiteral char)
exprToCoreFn _ _ _ (A.Literal ss (BooleanLiteral boolean)) =
  pure $ Literal (ss,[],Nothing) tyBoolean (BooleanLiteral boolean)
exprToCoreFn _ _ _ (A.Literal ss (StringLiteral string)) =
  pure $ Literal (ss,[],Nothing) tyString (StringLiteral string)

-- Accessor case is straightforward (these should always be typed explicitly)
exprToCoreFn mn ss (Just accT) accessor@(A.Accessor name v) = wrapTrace ("exprToCoreFn ACCESSOR " <> renderValue 100 accessor) $ do
  v'  <- exprToCoreFn mn ss Nothing v -- v should always have a type assigned during typechecking (i.e. it will be a TypedValue that will be unwrapped)
  pure $ Accessor (ssA ss) accT name v'
exprToCoreFn mn ss Nothing accessor@(A.Accessor name v) = do
  v' <- exprToCoreFn mn ss Nothing v
  let vTy = exprType v'
  env <- getEnv
  case analyzeCtor vTy of
    Nothing -> internalError $ "(1) Error while desugaring record accessor."
                  <> " No type provided for expression: \n" <> renderValue 100 accessor
                  <> "\nRecord type: " <> ppType 1000 vTy
                  <> "\nsynonyms: " <> show (runProperName . disqualify <$> M.keys env.types)
    Just (TypeConstructor _ tyNm,_) -> case M.lookup (tyNameToCtorName tyNm) env.dataConstructors of
      Nothing -> internalError $ "(2) Error while desugaring record accessor."
                  <> " No type provided for expression: \n" <> renderValue 100 accessor
      Just (_,_,ty,_) -> case stripQuantifiers ty of
        (_,RecordT inner :-> _) -> do
          let tyMap = M.fromList $ (\x -> (runLabel (rowListLabel x),x)) <$> (fst $ rowToList inner)
          case M.lookup name tyMap of
            Just (rowListType -> resTy) -> pure $ Accessor (ssA ss) resTy name v'
            Nothing -> internalError $ "(3) Error while desugaring record accessor."
                  <> " No type provided for expression: \n" <> renderValue 100 accessor
        (_,other) -> internalError $ "****DEBUG:\n" <> ppType 100 other
 where
   tyNameToCtorName :: Qualified (ProperName 'TypeName) -> Qualified (ProperName 'ConstructorName)
   tyNameToCtorName (Qualified qb tNm) = Qualified qb (coerceProperName tNm)

exprToCoreFn mn ss (Just recT) objUpd@(A.ObjectUpdate obj vs) = wrapTrace ("exprToCoreFn OBJ UPDATE " <> renderValue 100 objUpd) $ do
  obj' <- exprToCoreFn mn ss Nothing obj
  vs' <- traverse (\(lbl,val) -> exprToCoreFn mn ss Nothing val >>= \val' -> pure (lbl,val')) vs
  pure $
    ObjectUpdate
      (ssA ss)
      recT
      obj'
      (unchangedRecordFields (fmap fst vs) recT)
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
exprToCoreFn _ _ Nothing objUpd@(A.ObjectUpdate _ _) =
  internalError $ "Error while desugaring object update. No type provided for expression:\n" <> renderValue 100 objUpd

-- Lambda abstraction. See the comments on `instantiatePolyType` above for an explanation of the strategy here.
exprToCoreFn mn _ (Just t) (A.Abs (A.VarBinder ssb name) v) = wrapTrace ("exprToCoreFn " <> showIdent' name) $
  withInstantiatedFunType mn t $ \a b -> do
      body <-  bindLocalVariables [(ssb,name,a,Defined)] $ exprToCoreFn mn ssb (Just b) v
      pure $ Abs (ssA ssb) (function a b) name body
-- By the time we receive the AST, only Lambdas w/ a VarBinder should remain
-- TODO: Better failure message if we pass in 'Nothing' as the (Maybe Type) arg for an Abstraction
exprToCoreFn _  _ t lam@(A.Abs _ _) =
  internalError $ "Abs with Binder argument was not desugared before exprToCoreFn: \n"
                  <> renderValue 100  lam
                  <> "\n\n"
                  <> show (ppType 100 <$>  t)
                  <> "\n"
                  <> show lam

{- The App case is substantially complicated by our need to correctly type
   expressions that contain type class dictionary constructors, specifically expressions like:

   ```
   (C$Dict :: forall x. {method :: x -> (...)}) -> {method :: x -> (..)}) ({method: f})
   ````

   Because the dictionary ctor and record of methods it is being applied to
   are untouched by the PS typechecker, we have to instantiate the
   quantified variables to conform with the supplied type.
-}
exprToCoreFn mn ss mTy app@(A.App fun arg)
  | isDictCtor fun = wrapTrace "exprToCoreFn APP DICT " $ do
      traceM $ "APP Dict type" <> show (ppType 100 <$> mTy)
      traceM $ "APP Dict expr:\n" <> renderValue 100 app
      let analyzed = mTy >>= analyzeCtor
          prettyAnalyzed = bimap (ppType 100) (fmap (ppType 100)) <$> analyzed
      traceM $ "APP DICT analyzed:\n" <> show prettyAnalyzed
      case mTy of
        Just iTy ->
          case analyzed of
            -- Branch for a "normal" (i.e. non-empty) typeclass dictionary application
            Just (TypeConstructor _ (Qualified qb nm), args) -> do
              traceM $ "APP Dict name: " <> T.unpack (runProperName nm)
              env <- getEnv
              case M.lookup (Qualified qb $ coerceProperName nm) (dataConstructors env) of
                Just (_, _, ty, _) -> do
                    traceM $ "APP Dict original type:\n" <> ppType 100 ty
                    case instantiate ty args  of
                      iFun@(iArg :-> iRes) -> do
                        traceM $ "APP Dict iArg:\n" <> ppType 100 iArg
                        traceM $ "APP Dict iRes:\n" <> ppType 100 iRes
                        fun' <- exprToCoreFn mn ss (Just iFun) fun
                        arg' <- exprToCoreFn mn ss (Just iArg) arg
                        pure $ App (ss,[],Nothing) fun' arg'
                      _ -> error "dict ctor has to have a function type"
                _ -> throwError . errorMessage' ss . UnknownName . fmap DctorName $ Qualified qb (coerceProperName nm)
            -- This should actually be impossible here, so long as we desugared all the constrained types properly
            Just (other,_) -> error $  "APP Dict not a constructor type (impossible here?): \n" <> ppType 100 other
            -- Case for handling empty dictionaries (with no methods)
            Nothing ->  wrapTrace "APP DICT 3" $ do
              -- REVIEW: This might be the one place where `kindType` in instantiatePolyType is wrong, check the kinds in the output
              -- REVIEW: We might want to match more specifically on both/either the expression and type level to
              --         ensure that we are working only with empty dictionaries here. (Though anything else should be caught be the previous case)
              let (inner,g,act) = instantiatePolyType mn iTy
              act (exprToCoreFn mn ss (Just inner) app) >>= \case
                 App ann'  e1 e2 -> pure . g $ App ann' e1 e2
                 _ -> error "An application desguared to something else. This should not be possible."
        Nothing ->  error $ "APP Dict w/o type passed in (impossible to infer):\n" <> renderValue 100 app

  | otherwise =  wrapTrace "exprToCoreFn APP" $ do
      traceM $ renderValue 100 app
      fun' <- exprToCoreFn mn ss Nothing fun
      let funTy = exprType fun'
      traceM $ "app fun:\n" <> ppType 100  funTy <> "\n" <> renderExprStr fun'
      arg' <- exprToCoreFn mn ss Nothing arg -- We want to keep the original "concrete" arg type
      traceM $ "app arg:\n" <> ppType 100 (exprType arg') <> "\n" <> renderExprStr arg'
      pure $ App (ss, [], Nothing)  fun' arg'
  where
  isDictCtor = \case
    A.Constructor _ (Qualified _ name) -> isDictTypeName name
    A.TypedValue _ e _ -> isDictCtor e
    _ -> False
-- Dunno what to do here. Haven't encountered an Unused so far, will need to see one to figure out how to handle them
exprToCoreFn _ _  _ (A.Unused _) = -- ????? need to figure out what this _is_
  error "Don't know what to do w/ exprToCoreFn A.Unused"

exprToCoreFn _ _ (Just ty) (A.Var ss ident@(Qualified _ (GenIdent _ _))) = wrapTrace ("exprToCoreFn VAR (gen) " <> show ident) $
  gets checkEnv >>= \env ->
     pure $ Var (ss, [], getValueMeta env ident) (purusTy ty) ident
-- Non-generated Variables should *always* be bound & typed in the Environment before we encounter them.
-- NOTE: Not sure if we should ignore a type passed in? Generally we shouldn't *pass* types here, but bind variables
exprToCoreFn _ _ _ (A.Var ss ident) = wrapTrace ("exprToCoreFn VAR " <> show ident) $
  gets checkEnv >>= \env -> case lookupValue env ident of
    Just (ty,_,_) -> pure $ Var (ss, [], getValueMeta env ident) (purusTy ty) ident
    Nothing -> lookupDictType ident >>= \case
      Just ty -> pure $  Var (ss, [], getValueMeta env ident) (purusTy ty) ident
      Nothing -> internalError $ "No known type for identifier " <> show ident

exprToCoreFn _ _ mty expr@(A.Var ss ident) = internalError
  $ "Internal compiler error (exprToCoreFn var fail): Cannot synthesize type for var "
    <> show expr
    <> "\nSupplied type: "
    <> show (ppType 100 <$> mty)

-- If-Then-Else Turns into a case expression
exprToCoreFn mn ss (Just resT) (A.IfThenElse cond th el) = wrapTrace "exprToCoreFn IFTE" $ do
  condE <- exprToCoreFn mn ss (Just tyBoolean) cond
  thE <- exprToCoreFn mn ss (Just resT) th
  elE <- exprToCoreFn mn ss (Just resT) el
  pure $ Case (ss, [], Nothing) resT [condE]
    [ CaseAlternative [LiteralBinder (ssAnn ss) $ BooleanLiteral True]
                      (Right thE)
    , CaseAlternative [NullBinder (ssAnn ss)]
                      (Right elE) ]
exprToCoreFn _ _ Nothing ifte@(A.IfThenElse _ _ _) =
  internalError $ "Error while desugaring If-then-else expression. No type provided for:\n " <> renderValue 100 ifte

-- Constructor case is straightforward, we should already have all of the type info
exprToCoreFn _ _  (Just ctorTy) (A.Constructor ss name) = wrapTrace ("exprToCoreFn CTOR " <> show name) $ do
  ctorMeta <- flip getConstructorMeta name <$> getEnv
  pure $ Var (ss, [], Just ctorMeta) (purusTy ctorTy) $ fmap properToIdent name
exprToCoreFn _ _ Nothing ctor@(A.Constructor _ _) =
  internalError $ "Error while desugaring Constructor expression. No type provided for:\n" <> renderValue 100 ctor

{- Case Expressions

   For ordinary case expressions (i.e. those not generated by the compiler during guard desugaring),
   the type can be determined by the type of a top-level declaration or
   explicit type annotation (which may be either supplied by the user or inferred by the
   PS typechecker) and therefore should be passed explicitly.

   For compiler-generated case expressions (specifically: those generated during case/guard desugaring -
   some get generated during typeclass desugaring but we have special machinery to ensure that
   those types can be synthesized), we cannot be sure that the whole case expression has an explicit
   type annotation, so we try to deduce the type from the types of the alternative branches.

   NOTE: This is kind of a hack to let us reuse (rather than rewrite) the existing case/guard
         desugaring machinery. In order to correctly type the generated `let` bindings
         (see Language.PureScript.Sugar.CaseDeclarations), we must manually construct a polymorphic
         type that the PS typechecker cannot infer or deduce. We cannot construct such a type without
         the initial PS typechecker pass. We could write two nearly-identical versions of the
         case desugaring machinery and try to run the typechecker twice, but that adds a lot of
         complexity (machinery is complicated) and would not be good for performance (typechecking
         and inference have bad complexity).
-}
exprToCoreFn mn ss (Just caseTy) astCase@(A.Case vs alts) = wrapTrace "exprToCoreFn CASE" $ do
  traceM $ "CASE:\n" <> renderValue 100 astCase
  traceM $ "CASE TY:\n" <> show (ppType 100 caseTy)
  (vs',ts) <- unzip <$> traverse (exprToCoreFn mn ss Nothing >=> (\ e -> pure (e, exprType e))) vs -- extract type information for the *scrutinees*
  alts' <- traverse (altToCoreFn mn ss caseTy ts) alts -- see explanation in altToCoreFn. We pass in the types of the scrutinee(s)
  pure $ Case (ssA ss) (purusTy caseTy) vs' alts'
exprToCoreFn mn ss Nothing  astCase@(A.Case vs alts@(alt:_)) = wrapTrace "exprToCoreFn CASE (no type)" $ do
  case alt of
    A.CaseAlternative _ (A.GuardedExpr _ body1:_) -> do
      caseTy <- exprType <$> exprToCoreFn mn ss Nothing body1
      traceM $ "CASE:\n" <> renderValue 100 astCase
      traceM $ "CASE TY:\n" <> show (ppType 100 caseTy)
      (vs',ts) <- unzip <$> traverse (exprToCoreFn mn ss Nothing >=> (\ e -> pure (e, exprType e))) vs -- extract type information for the *scrutinees*
      alts' <- traverse (altToCoreFn mn ss caseTy ts) alts -- see explanation in altToCoreFn. We pass in the types of the scrutinee(s)
      pure $ Case (ssA ss) (purusTy caseTy) vs' alts'
    _ -> internalError $ "Error while desugaring Case expression. Could not synthesize type of: " <> renderValue 100 astCase
exprToCoreFn _ _ Nothing astCase@(A.Case _ _) =
  internalError $ "Error while desugaring Case expression. No type provided for:\n" <> renderValue 100 astCase

-- We prioritize the supplied type over the inferred type, since a type should only ever be passed when known to be correct.
exprToCoreFn  mn ss  (Just ty) (A.TypedValue _ v _) = wrapTrace "exprToCoreFn TV1" $
  exprToCoreFn mn ss (Just ty) v
-- If we encounter a TypedValue w/o a supplied type, we use the annotated type
exprToCoreFn mn ss Nothing (A.TypedValue _ v ty) = wrapTrace "exprToCoreFn TV2" $
  exprToCoreFn mn ss (Just ty) v

-- Complicated. See `transformLetBindings`
exprToCoreFn  mn ss _ (A.Let w ds v) = wrapTrace "exprToCoreFn LET" $ case NE.nonEmpty ds of
  Nothing -> error "declarations in a let binding can't be empty"
  Just _ -> do
    (decls,expr) <- transformLetBindings mn ss [] ds v
    pure $ Let (ss, [], getLetMeta w)  decls expr

-- Pretty sure we should prefer the positioned SourceSpan
exprToCoreFn  mn _ ty (A.PositionedValue ss _ v) = wrapTrace "exprToCoreFn POSVAL" $
  exprToCoreFn mn ss ty v
-- Function should never reach this case, but there are a lot of AST Expressions that shouldn't ever appear here, so
-- we use a catchall case.
exprToCoreFn _ ss   _ e =
  internalError
    $ "Unexpected value in exprToCoreFn:\n"
      <> renderValue 100 e
      <> "at position:\n"
      <> show ss

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


transformLetBindings :: forall m. M m => ModuleName -> SourceSpan -> [Bind Ann] -> [A.Declaration] -> A.Expr -> m ([Bind Ann], Expr Ann)
transformLetBindings mn ss seen [] ret = (seen,) <$> withBindingGroupVisible (exprToCoreFn mn ss Nothing ret)
transformLetBindings mn _ss seen ((A.ValueDecl sa@(ss,_) ident nameKind [] [A.MkUnguarded (A.TypedValue checkType val ty)]) : rest) ret =
  wrapTrace ("transformLetBindings VALDEC TYPED " <> showIdent' ident <> " :: " <> ppType 100 ty ) $
  bindNames (M.singleton (Qualified (BySourcePos $ spanStart ss) ident) (ty, nameKind, Defined)) $ do
    thisDecl <- declToCoreFn mn (A.ValueDecl sa ident nameKind [] [A.MkUnguarded (A.TypedValue checkType val ty)])
    let seen' = seen ++ thisDecl
    transformLetBindings mn _ss seen' rest ret
transformLetBindings mn _ss seen (A.ValueDecl (ss,_) ident nameKind [] [A.MkUnguarded val] : rest) ret = wrapTrace ("transformLetBindings VALDEC " <> showIdent' ident <> " = " <> renderValue 100 val) $ do
  e <- exprToCoreFn mn ss Nothing val
  let ty = exprType e
  if not (containsUnknowns ty) -- TODO: Don't need this anymore (shouldn't ever contain unknowns)
    then bindNames (M.singleton (Qualified (BySourcePos $ spanStart ss) ident) (ty, nameKind, Defined)) $ do
      let thisDecl = [NonRec (ssA ss) ident e]
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
  -- All of the types in the binding group should be TypedValues (after my modifications to the typechecker)
  -- NOTE: We re-implement part of TypeChecker.Types.typeDictionaryForBindingGroup here because it *could* try to do
  --       type checking/inference, which we want to avoid (because it mangles our types)
  let types = go <$> NEL.toList ((\(i, _, v) -> (i, v)) <$> ds)
  case sequence types  of
    Right typed ->  do
      let ds' =  flip map typed $ \((sann,iden),(expr,ty)) -> A.ValueDecl sann iden Private [] [A.MkUnguarded (A.TypedValue False expr ty)]
          dict = M.fromList $ flip map typed $ \(((ss,_),ident),(_,ty)) -> (Qualified (BySourcePos $ spanStart ss) ident, (ty, Private, Undefined))
      bindNames dict $ do
        makeBindingGroupVisible
        thisDecl <- concat <$> traverse (declToCoreFn mn) ds'
        let seen' = seen ++ thisDecl
        transformLetBindings mn _ss seen' rest ret
    -- Because this has already been through the typechecker once, every value in the binding group should have an explicit type. I hope.
    Left _ -> error
         $ "untyped binding group element in mutually recursive LET binding group after initial typechecker pass: \n"
         <> LT.unpack (pShow $ lefts types)
 where
  go :: ((SourceAnn, Ident), A.Expr) -> Either ((SourceAnn,Ident), A.Expr) ((SourceAnn, Ident), (A.Expr, SourceType))
  go  (annName,A.TypedValue _ expr ty)  = Right (annName,(expr,ty))
  go  (annName,other) = Left (annName,other)
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
inferBinder' _ (A.LiteralBinder _ (StringLiteral _)) = wrapTrace "inferBinder' STRLIT" $ return M.empty
inferBinder' _ (A.LiteralBinder _ (CharLiteral _)) = wrapTrace "inferBinder' CHARLIT" $  return M.empty
inferBinder' _ (A.LiteralBinder _ (NumericLiteral (Left _))) = wrapTrace "inferBinder' LITINT" $ return M.empty
inferBinder' _ (A.LiteralBinder _ (NumericLiteral (Right _))) = wrapTrace "inferBinder' NUMBERLIT" $ return M.empty
inferBinder' _ (A.LiteralBinder _ (BooleanLiteral _)) = wrapTrace "inferBinder' BOOLLIT" $ return M.empty
inferBinder' val (A.VarBinder ss name) = wrapTrace ("inferBinder' VAR " <> T.unpack (runIdent name))  $ return $ M.singleton name (ss, val)
inferBinder' val (A.ConstructorBinder ss ctor binders) = wrapTrace ("inferBinder' CTOR: " <> show ctor) $ do
  traceM $ "InferBinder VAL:\n" <>  ppType 100 val
  env <- getEnv
  let cArgs = ctorArgs val
  traceM $ "InferBinder CTOR ARGS:\n" <> concatMap (\x -> ppType 100 x <> "\n") cArgs
  case M.lookup ctor (dataConstructors env) of
    Just (_, _, _ty, _) -> do
      let ty = instantiate _ty cArgs
      traceM $ "InferBinder CTOR TY:\n" <> ppType 100 ty
      let (args, _) = peelArgs ty
      traceM $ "InferBinder ARGS:\n" <> concatMap (\x -> ppType 100 x <> "\n") args
      M.unions <$> zipWithM inferBinder' (reverse args) binders
    _ -> throwError . errorMessage' ss . UnknownName . fmap DctorName $ ctor
  where
  peelArgs :: Type a -> ([Type a], Type a)
  peelArgs = go []
    where
    go args (TypeApp _ (TypeApp _ fn arg) ret) | eqType fn tyFunction = go (arg : args) ret
    go args ret = (args, ret)
inferBinder' val (A.LiteralBinder _ (ObjectLiteral props)) = wrapTrace "inferBinder' OBJECTLIT" $ do
  traceM $ ppType 100 val
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
        then deduceRowProperties (M.fromList rowItems) props'
        else error $ "Error. Object literal in a pattern match is missing fields: " <> show diff
  where
  deduceRowProperties :: M.Map PSString SourceType -> [(PSString,A.Binder)] -> m (M.Map Ident (SourceSpan,SourceType))
  deduceRowProperties _ [] = pure M.empty
  deduceRowProperties types ((lbl,bndr):rest) = case M.lookup lbl types of
    Nothing -> error $ "Cannot deduce type information for record with label " <> show lbl -- should be impossible after typechecking
    Just ty -> do
      x <- inferBinder' ty bndr
      xs <- deduceRowProperties types rest
      pure $ M.union x xs
-- TODO: Remove ArrayT pattern synonym
inferBinder' (ArrayT val) (A.LiteralBinder _ (ArrayLiteral binders)) = wrapTrace "inferBinder' ARRAYLIT" $  M.unions <$> traverse (inferBinder' val) binders
inferBinder' _ (A.LiteralBinder _ (ArrayLiteral _)) = internalError "bad type in array binder "
inferBinder' val (A.NamedBinder ss name binder) = wrapTrace ("inferBinder' NAMEDBINDER " <> T.unpack (runIdent name)) $
  warnAndRethrowWithPositionTC ss $ do
    m <- inferBinder' val binder
    return $ M.insert name (ss, val) m
inferBinder' val (A.PositionedBinder pos _ binder) = wrapTrace "inferBinder' POSITIONEDBINDER" $
  warnAndRethrowWithPositionTC pos $ inferBinder' val binder
inferBinder' _ (A.TypedBinder ty binder) = wrapTrace "inferBinder' TYPEDBINDER" $ do
  (elabTy, _) <- kindOf ty
  inferBinder' elabTy binder
inferBinder' _ A.OpBinder{} =
  internalError "OpBinder should have been desugared before inferBinder'"
inferBinder' _ A.BinaryNoParensBinder{} =
  internalError "BinaryNoParensBinder should have been desugared before inferBinder'"
inferBinder' _ A.ParensInBinder{} =
  internalError "ParensInBinder should have been desugared before inferBinder'"

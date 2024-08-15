{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use camelCase" #-}
module Language.PureScript.CoreFn.Convert.DesugarCore where

import Prelude

import Bound (Var (..), abstract)
import Bound.Scope (Scope, fromScope, toScope)
import Control.Lens hiding (Context)
import Control.Lens.IndexedPlated (icosmos)
import Control.Monad (foldM, join, void)
import Control.Monad.Error.Class (MonadError (throwError))
import Control.Monad.State (StateT, gets, modify', runStateT)
import Control.Monad.Trans (lift)
import Data.Foldable (Foldable (foldl'), foldrM, traverse_)
import Data.List (find, sortOn)
import Data.Map qualified as M
import Data.Maybe (fromJust, mapMaybe)
import Data.Text qualified as T
import Data.Void (Void)
import Language.PureScript (runIdent)
import Language.PureScript.AST.Literals (Literal (..))
import Language.PureScript.AST.SourcePos (spanStart)
import Language.PureScript.Constants.Prim qualified as C
import Language.PureScript.CoreFn.Ann (Ann, annSS, nullAnn)
import Language.PureScript.CoreFn.Binders (Binder (..))
import Language.PureScript.CoreFn.Convert.IR (
  Alt (..),
  BVar (..),
  BindE (..),
  Exp (..),
  FVar (..),
  FuncType (..),
  Lit (CharL, IntL, ObjectL, StringL),
  Pat (..),
  XAccessor,
  XObjectLiteral,
  XObjectUpdate,
  analyzeApp,
  expTy,
  expTy',
  mkBindings,
  ppExp,
  unsafeAnalyzeApp,
 )
import Language.PureScript.CoreFn.Desugar.Utils (properToIdent, showIdent', wrapTrace)
import Language.PureScript.CoreFn.Expr (
  Bind (NonRec, Rec),
  CaseAlternative (CaseAlternative),
  Expr (..),
  PurusType,
  _Var,
 )
import Language.PureScript.CoreFn.Module (Module (..))
import Language.PureScript.CoreFn.Pretty (prettyAsStr, prettyTypeStr, renderExprStr)
import Language.PureScript.CoreFn.Utils (Context, exprType)
import Language.PureScript.Environment (mkCtorTy, mkTupleTyName)
import Language.PureScript.Names (
  Ident (..),
  ModuleName (ModuleName),
  ProperName (ProperName),
  Qualified (Qualified),
  QualifiedBy (ByModuleName, BySourcePos),
  coerceProperName,
  disqualify,
 )
import Language.PureScript.Types (Type (..))

import Data.Bifunctor (Bifunctor (first))
import Data.Char (isUpper)
import Data.Text (Text)
import Language.PureScript.CoreFn.Convert.Debug
import Language.PureScript.CoreFn.Pretty.Common qualified as PC -- jfc move this somewhere more sensible
import Language.PureScript.CoreFn.TypeLike (TypeLike (..))
import Prettyprinter (Pretty (..), vcat)
import Language.PureScript.CoreFn.Convert.IR.Utils


-- Need the map to keep track of whether a variable has already been used in the scope (e.g. for shadowing)
type DS = StateT (Int, M.Map Ident Int) (Either String)

liftErr :: Either String a -> DS a
liftErr = \case
  Left err -> lift (Left err)
  Right x -> pure x

freshly :: DS a -> DS a
freshly act = modify' (set _2 M.empty) >> act

fresh :: DS Int
fresh = do
  i <- gets (view _1)
  modify' $ over _1 (+ 1)
  pure i

bind :: Ident -> DS Int
bind ident = do
  i <- fresh
  modify' $ over _2 (M.insert ident i)
  doTraceM "bind" ("IDENT: " <> T.unpack (runIdent ident) <> "\n\nINDEX: " <> prettyAsStr i)
  pure i

forceBind :: Ident -> Int -> DS ()
forceBind i indx   = modify' $ over _2 (M.insert i indx)

getVarIx :: Ident -> DS Int
getVarIx ident =
  gets (preview (_2 . ix ident)) >>= \case
    Nothing -> error $ "getVarIx: Free variable " <> showIdent' ident
    Just indx -> pure indx

isBuiltinOrPrim :: Exp x t (Vars t) -> Bool
isBuiltinOrPrim = \case
  V (F (FVar _ (Qualified (ByModuleName (ModuleName "Prim")) _))) -> True
  V (F (FVar _ (Qualified _ (Ident i)))) -> isUpper (T.head i)
  _ -> False

{- We don't bind anything b/c the type level isn't `Bound` -}
tyAbs :: forall x t. Text -> KindOf t -> Exp x t (Vars t) -> DS (Exp x t (Vars t))
tyAbs nm k exp
  | not (isBuiltinOrPrim exp) = do
      u <- fresh
      pure $ TyAbs (BVar u k (Ident nm)) exp
  | otherwise = pure exp

tyAbsMany :: forall x t. [(Text, KindOf t)] -> Exp x t (Vars t) -> DS (Exp x t (Vars t))
tyAbsMany vars expr = foldrM (uncurry tyAbs) expr vars

desugarCoreModule :: Module (Bind Ann) PurusType PurusType Ann -> Either String (Module IR_Decl PurusType PurusType Ann, (Int, M.Map Ident Int))
desugarCoreModule m = case runStateT (desugarCoreModule' m) (0, M.empty) of
  Left err -> Left err
  Right res -> pure res

desugarCoreModule' :: Module (Bind Ann) PurusType PurusType Ann -> DS (Module IR_Decl PurusType PurusType Ann)
desugarCoreModule' Module {..} = do
  decls' <- traverse (freshly . desugarCoreDecl) moduleDecls
  decls  <- bindLocalTopLevelDeclarations decls'
  pure $ Module {moduleDecls = decls, ..}
 where
   {- Need to do this to ensure that all  -}
   bindLocalTopLevelDeclarations :: [BindE PurusType (Exp WithObjects PurusType) (Vars PurusType)]
                                 -> DS [BindE PurusType (Exp WithObjects PurusType) (Vars PurusType)]
   bindLocalTopLevelDeclarations ds = do
     let topLevelIdents = foldBinds (\acc nm _ -> nm:acc ) [] ds
     traverse_ (uncurry forceBind) topLevelIdents
     s <- gets (view _2)
     doTraceM "bindLocalTopLevelDeclarations" $ prettify [ "Input (Module Decls):\n" <> prettify (prettyAsStr <$> ds)
                                                         , "Top level binds:\n" <> prettyAsStr (M.toList s)
                                                         , "Top level idents:\n" <> prettify (prettyAsStr <$> topLevelIdents)
                                                         ]
     -- this is only safe because every locally-scoped (i.e. inside a decl) variable should be
     -- bound by this point
     let upd = \case
               V (B bv) -> V $ B bv
               V (F fv@(FVar t (Qualified _ ident))) -> case M.lookup ident s of
                 Nothing -> V $ F fv
                 Just indx -> V $ B $ BVar indx t ident
               other -> other 
     pure $ mapBind (const $ viaExp (transform upd))  <$> ds

desugarCoreDecl ::
  Bind Ann ->
  DS (BindE PurusType (Exp WithObjects PurusType) (Vars PurusType))
desugarCoreDecl = \case
  NonRec _ ident expr -> wrapTrace ("desugarCoreDecl: " <> showIdent' ident) $ do
    bvix <- bind ident
    s <- gets (view _2)
    let abstr = abstract (matchLet s) . runEtaReduce
    desugared <- desugarCore expr
    -- let boundVars = freeTypeVariables (expTy id desugared)
    -- scoped <-  abstr  <$> tyAbsMany boundVars desugared
    let scoped = abstr desugared
    pure $ NonRecursive ident bvix scoped
  Rec xs -> do
    let inMsg =
          concatMap
            ( \((_, i), x) ->
                prettyAsStr i
                  <> " :: "
                  <> prettyTypeStr (exprType x)
                  <> "\n"
                  <> prettyAsStr i
                  <> " = "
                  <> renderExprStr x
                  <> "\n\n"
            )
            xs
    doTraceM "desugarCoreDecl" inMsg
    first_pass <- traverse (\((_, ident), e) -> bind ident >>= \u -> pure ((ident, u), e)) xs
    s <- gets (view _2)
    let abstr = abstract (matchLet s) . runEtaReduce
    second_pass <-
      traverse
        ( \((ident, bvix), expr) -> do
            wrapTrace ("desugarCoreDecl: " <> showIdent' ident) $ do
              desugared <- desugarCore expr
              -- let boundVars = freeTypeVariables (expTy id desugared)
              -- scoped <- abstr <$> tyAbsMany boundVars desugared
              let scoped = abstr desugared
              pure ((ident, bvix), scoped)
        )
        first_pass
    doTraceM "desugarCoreDecl" ("RESULT (RECURSIVE):\n" <> prettyAsStr (fmap fromScope <$> second_pass))
    pure $ Recursive second_pass

{- | Turns a list of expressions into an n-ary
     tuple, where n = the length of the list.

     Throws an error on empty lists. Should only be used
     in contexts that must be nonempty (e.g. case expression
     scrutinees and case alternative patterns)
-}
tuplify :: [Expr Ann] -> Expr Ann
tuplify [] = error "tuplify called on empty list of expressions"
tuplify es = foldl' (App nullAnn) tupCtor es
  where
    n = length es
    tupName = Qualified (ByModuleName C.M_Prim) (ProperName $ "Tuple" <> T.pack (show n))
    tupCtorType = mkCtorTy tupName n

    tupCtor :: Expr Ann
    tupCtor = Var nullAnn tupCtorType (properToIdent <$> tupName)

desugarCore :: Expr Ann -> DS (Exp WithObjects PurusType (Vars PurusType))
desugarCore e = do
  let ty = exprType e
  result <- desugarCore' e
  let msg =
        prettify
          [ "INPUT:\n" <> renderExprStr e
          , "INPUT TY:\n" <> prettyTypeStr ty
          , "OUTPUT:\n" <> prettyAsStr result
          , "OUTPUT TY:\n" <> prettyAsStr (expTy id result)
          ]
  doTraceM "desugarCore" msg
  pure result

desugarCore' :: Expr Ann -> DS (Exp WithObjects PurusType (Vars PurusType))
desugarCore' (Literal _ann ty lit) = LitE ty <$> desugarLit lit
desugarCore' lam@(Abs _ann ty ident expr) = do
  bvIx <- bind ident
  s <- gets (view _2)
  expr' <- desugarCore' expr
  let !ty' = functionArgumentIfFunction $ snd (stripQuantifiers ty)
      (vars', _) = stripQuantifiers ty
      vars = (\(_, b, c) -> (b, c)) <$> vars'
      scopedExpr = abstract (matchLet s) expr'
  result <- tyAbsMany vars $ LamE (BVar bvIx ty' ident) scopedExpr
  let msg =
        prettify
          [ "ANNOTATED LAM TY:\n" <> prettyAsStr ty
          , "BOUND VAR TY:\n" <> prettyAsStr ty'
          , "BODY TY:\n" <> prettyAsStr (exprType expr)
          , "INPUT EXPR:\n" <> renderExprStr lam
          , "RESULT EXPR:\n" <> prettyAsStr result
          , "RESULT EXPR TY:\n" <> prettyAsStr (expTy id result)
          ]
  doTraceM "desugarCoreLam" msg
  pure result
desugarCore' appE@(App {}) = case fromJust $ PC.analyzeApp appE of
  (f, args) -> do
    f' <- desugarCore f
    args' <- traverse desugarCore args
    pure $ foldl' AppE f' args'
desugarCore' (Var _ann ty qi) = pure $ V . F $ FVar ty qi
desugarCore' (Let _ann binds cont) = do
  -- afaict their mutual recursion sorter doesn't work properly in let exprs (maybe it's implicit that they're all mutually recursive?)
  -- traverse_ bindAllNames binds
  bindEs <- traverse rebindInScope =<< traverse desugarCoreDecl binds
  s <- gets (view _2)
  cont' <- desugarCore cont
  let abstr = abstract (matchLet s)
  pure $ LetE M.empty bindEs $ abstr cont'
desugarCore' (Accessor _ann ty label expr) = do
  expr' <- desugarCore expr
  pure $ AccessorE () ty label expr'
desugarCore' (ObjectUpdate _ann ty expr toCopy toUpdate) = do
  expr' <- desugarCore expr
  toUpdate' <- desugarObjectMembers toUpdate
  pure $ ObjectUpdateE () ty expr' toCopy toUpdate'
-- NOTE: We do not tuple single scrutinees b/c that's just a performance hit w/ no point
desugarCore' (Case _ann ty [scrutinee] alts) = do
  scrutinee' <- desugarCore scrutinee
  alts' <- traverse desugarAlt alts
  pure $ CaseE ty scrutinee' alts'
desugarCore' (Case _ann ty scrutinees alts) = do
  scrutinees' <- desugarCore $ tuplify scrutinees
  alts' <- traverse desugarAlt alts
  pure $ CaseE ty scrutinees' alts'

bindAllNames :: Bind Ann -> DS ()
bindAllNames = \case
  NonRec _ ident _ -> void $ bind ident
  Rec xs -> (traverse_ ((void . bind) . snd . fst) xs)

rebindInScope ::
  BindE PurusType (Exp WithObjects PurusType) (Vars PurusType) ->
  DS (BindE PurusType (Exp WithObjects PurusType) (Vars PurusType))
rebindInScope b = do
  s <- gets (view _2)
  let f scoped = abstract (matchLet s) . fmap join . fromScope $ scoped
  case b of
    NonRecursive i x scoped -> pure $ NonRecursive i x (f scoped)
    Recursive xs -> pure . Recursive $ (\(nm, body) -> (nm, f body)) <$> xs
desugarAlt ::
  CaseAlternative Ann ->
  DS (Alt WithObjects PurusType (Exp WithObjects PurusType) (Vars PurusType))
desugarAlt (CaseAlternative [binder] result) = case result of
  Left exs ->
    throwError $
      "internal error: `desugarAlt` guarded alt not expected at this stage: " <> show exs
  Right ex -> do
    pat <- toPat binder
    s <- gets (view _2)
    let abstrE = abstract (matchLet s)
    re' <- desugarCore ex
    pure $ UnguardedAlt M.empty pat (abstrE re')
desugarAlt (CaseAlternative binders result) = do
  pats <- traverse toPat binders
  s <- gets (view _2)
  let abstrE = abstract (matchLet s)
      n = length binders
      tupTyName = mkTupleTyName n
      tupCtorName = coerceProperName <$> tupTyName
      pat = ConP tupTyName tupCtorName pats
  case result of
    Left exs -> do
      throwError $ "internal error: `desugarAlt` guarded alt not expected at this stage: " <> show exs
    Right ex -> do
      re' <- desugarCore ex
      pure $ UnguardedAlt M.empty pat (abstrE re')

toPat :: Binder ann -> DS (Pat x PurusType (Exp x ty) (Vars ty))
toPat = \case
  NullBinder _ -> pure WildP
  VarBinder _ i ty -> do
    n <- bind i
    pure $ VarP i n ty
  ConstructorBinder _ tn cn bs -> ConP tn cn <$> traverse toPat bs
  NamedBinder _ nm _ ->
    error $
      "found namedBinder: "
        <> T.unpack (runIdent nm)
        <> "TODO: remove NamedBinder/AsP everywhere (parser, cst, AST)"
  LiteralBinder _ lp -> case lp of
    NumericLiteral (Left i) -> pure . LitP $ IntL i
    NumericLiteral (Right _) -> error "numeric literals not supported (yet)"
    StringLiteral pss -> pure . LitP $ StringL pss
    CharLiteral c -> pure . LitP $ CharL c
    BooleanLiteral _ -> error "boolean literal patterns shouldn't exist anymore"
    ArrayLiteral _ -> error "array literal patterns shouldn't exist anymore"
    ObjectLiteral fs' -> do
      -- REVIEW/FIXME:
      -- this isn't right, we need to make sure the positions of the binders are correct,
      -- since (I think?) you can use an Obj binder w/o using all of the fields
      let fs = sortOn fst fs'
          len = length fs
          tupTyName = mkTupleTyName len
          tupCtorName = coerceProperName <$> tupTyName
      ConP tupTyName tupCtorName <$> traverse (toPat . snd) fs

getBoundVar :: forall ann. (Show ann) => Either [(Expr ann, Expr ann)] (Expr ann) -> Binder ann -> [Vars PurusType]
getBoundVar body binder = case binder of
  ConstructorBinder _ _ _ binders -> concatMap (getBoundVar body) binders
  LiteralBinder _ (ArrayLiteral arrBinders) -> concatMap (getBoundVar body) arrBinders
  LiteralBinder _ (ObjectLiteral objBinders) -> concatMap (getBoundVar body . snd) objBinders
  VarBinder _ ident _ -> case body of
    Right expr -> case findBoundVar ident expr of
      Nothing -> [] -- probably should trace or warn at least
      Just (ty, qi) -> [F $ FVar ty qi]
    Left fml -> do
      let allResults = concatMap (\(x, y) -> [x, y]) fml
          matchingVar = mapMaybe (findBoundVar ident) allResults
      case matchingVar of
        ((ty, qi) : _) -> [F $ FVar ty qi]
        _ -> []
  _ -> []

findBoundVar :: forall ann. (Show ann) => Ident -> Expr ann -> Maybe (PurusType, Qualified Ident)
findBoundVar nm ex = find (goFind . snd) (allVars ex)
  where
    goFind = \case
      Qualified (ByModuleName _) _ -> False
      Qualified (BySourcePos _) nm' -> nm == nm'

allVars :: forall ann. (Show ann) => Expr ann -> [(PurusType, Qualified Ident)]
allVars ex = ex ^.. icosmos @Context @(Expr ann) M.empty . _Var . to (\(_, b, c) -> (b, c))

qualifySS :: Ann -> Ident -> Qualified Ident
qualifySS ann i = Qualified (BySourcePos $ spanStart (annSS ann)) i

-- Stolen from DesugarObjects
desugarBinds :: [Bind Ann] -> DS [[((Vars PurusType, Int), Scope (BVar PurusType) (Exp WithObjects PurusType) (Vars PurusType))]]
desugarBinds [] = pure []
desugarBinds (b : bs) = case b of
  NonRec _ann ident expr -> do
    bvIx <- bind ident
    let qualifiedIdent = qualifySS _ann ident
    e' <- desugarCore expr
    let scoped = abstract (matchLet $ M.singleton ident bvIx) e'
    rest <- desugarBinds bs
    pure $ [((F $ FVar (exprType expr) qualifiedIdent, bvIx), scoped)] : rest
  -- TODO: Fix this to preserve recursivity (requires modifying the *LET* ctor of Exp)
  Rec xs -> do
    traverse_ (\((_, nm), _) -> bind nm) xs
    recRes <- traverse handleRecBind xs
    rest <- desugarBinds bs
    pure $ recRes : rest
  where
    handleRecBind ::
      ((Ann, Ident), Expr Ann) ->
      DS ((Vars PurusType, Int), Scope (BVar PurusType) (Exp WithObjects PurusType) (Vars PurusType))
    handleRecBind ((ann, ident), expr) = do
      u <- getVarIx ident
      s <- gets (view _2)
      expr' <- desugarCore expr
      let scoped = abstract (matchLet s) expr'
          fv = F $ FVar (expTy' id scoped) (qualifySS ann ident)
      pure ((fv, u), scoped)

desugarLit :: Literal (Expr Ann) -> DS (Lit WithObjects (Exp WithObjects PurusType (Vars PurusType)))
desugarLit (NumericLiteral (Left int)) = pure $ IntL int
desugarLit (NumericLiteral (Right _)) = error "TODO: Remove Number lits from all preceding ASTs" -- pure $ NumL number
desugarLit (StringLiteral string) = pure $ StringL string
desugarLit (CharLiteral char) = pure $ CharL char
desugarLit (BooleanLiteral _) = error "TODO: Remove BooleanLiteral from all preceding ASTs"
desugarLit (ArrayLiteral _) = error "TODO: Remove ArrayLiteral from IR AST" -- ArrayL <$> traverse desugarCore arr
desugarLit (ObjectLiteral object) = ObjectL () <$> desugarObjectMembers object

-- this looks like existing monadic combinator but I couldn't find it
desugarObjectMembers :: [(field, Expr Ann)] -> DS [(field, Exp WithObjects PurusType (Vars PurusType))]
desugarObjectMembers = traverse (\(memberName, val) -> (memberName,) <$> desugarCore val)

pattern (:~>) :: PurusType -> PurusType -> PurusType
pattern a :~> b <-
  ( TypeApp
      _
      ( TypeApp
          _
          (TypeConstructor _ (Qualified (ByModuleName (ModuleName "Prim")) (ProperName "Function")))
          a
        )
      b
    )
infixr 0 :~>

functionArgumentIfFunction :: PurusType -> PurusType
functionArgumentIfFunction (arg :~> _) = arg
functionArgumentIfFunction t = t

-- | For usage with `Bound`, use only with lambda
matchVarLamAbs :: Ident -> Int -> FVar ty -> Maybe (BVar ty)
matchVarLamAbs nm bvix (FVar ty n')
  | nm == disqualify n' = Just (BVar bvix ty nm)
  | otherwise = Nothing

matchLet :: (Pretty ty) => M.Map Ident Int -> Vars ty -> Maybe (BVar ty)
matchLet _ (B bv) = Just bv
matchLet binds fv@(F (FVar ty n')) = case result of
  Nothing -> Nothing
  Just _ -> doTrace "matchLet" msg result
  where
    msg =
      "INPUT:\n"
        <> prettyAsStr fv
        <> "\n\nOUTPUT:\n"
        <> prettyAsStr result
    result = do
      let nm = disqualify n'
      bvix <- M.lookup nm binds
      pure $ BVar bvix ty nm

-- TODO (t4ccer): Move somehwere, but cycilc imports are annoying
instance FuncType PurusType where
  headArg = functionArgumentIfFunction

-- REVIEW: This *MIGHT* not be right. I'm not 1000% sure what the PS criteria for a mutually rec group are
--         First arg threads the FVars that correspond to the already-processed binds
--         through the rest of the conversion. I think that's right - earlier bindings
--         should be available to later bindings
assembleBindEs :: (Eq ty) => [[((FVar ty, Int), Scope (BVar ty) (Exp x ty) (FVar ty))]] -> DS [BindE ty (Exp x ty) (FVar ty)]
assembleBindEs [] = pure []
assembleBindEs ([] : rest) = assembleBindEs rest -- shouldn't happen but w/e
assembleBindEs ([((FVar _tx idnt, bix), e)] : rest) = do
  (NonRecursive (disqualify idnt) bix e :) <$> assembleBindEs rest
assembleBindEs (xsRec : rest) = do
  let recBinds = flip map xsRec $ \((FVar _tx idnt, bvix), e) -> ((disqualify idnt, bvix), e)
  rest' <- assembleBindEs rest
  pure $ Recursive recBinds : rest'

runEtaReduce ::
  forall x t.
  (Eq (Exp x t (Var (BVar t) (FVar t))), Pretty t, Pretty (KindOf t), TypeLike t) =>
  Exp x t (Var (BVar t) (FVar t)) ->
  Exp x t (Var (BVar t) (FVar t))
runEtaReduce e = doTrace "runEtaReduce" msg result
  where
    result = transform etaReduce e
    msg =
      "INPUT:\n"
        <> prettyAsStr e
        <> "\n\nOUTPUT:\n"
        <> prettyAsStr result

etaReduce ::
  forall x t.
  (Eq (Exp x t (Var (BVar t) (FVar t))), Pretty t, Pretty (KindOf t), TypeLike t) =>
  Exp x t (Var (BVar t) (FVar t)) ->
  Exp x t (Var (BVar t) (FVar t))
etaReduce input = case partitionLam input of
  Just (boundVars, fun, args) ->
    let result = if boundVars == args then fun else input
        msg =
          "INPUT:\n"
            <> prettyAsStr input
            <> "\n\nBOUND VARS:\n"
            <> show (vcat $ pretty <$> boundVars)
            <> "\n\nARGS:\n"
            <> show (vcat $ pretty <$> args)
            <> "\n\nFUN:\n"
            <> prettyAsStr fun
            <> "\n\nFUN TY:\n"
            <> prettyAsStr (expTy id fun)
            <> "\n\nARG TYS:\n"
            <> prettyAsStr (expTy id <$> args)
     in doTrace "etaReduce" msg result
  Nothing -> input
  where
    partitionLam ::
      Exp x t (Var (BVar t) (FVar t)) ->
      Maybe ([Exp x t (Var (BVar t) (FVar t))], Exp x t (Var (BVar t) (FVar t)), [Exp x t (Var (BVar t) (FVar t))])
    partitionLam e = do
      let (bvars, inner) = stripLambdas e
      (f, args) <- analyzeApp inner
      pure $ (V . B <$> bvars, f, args)

    stripLambdas = \case
      LamE bv body -> first (bv :) $ stripLambdas (join <$> fromScope body)
      TyInstE _ inner -> stripLambdas inner
      other -> ([], other)

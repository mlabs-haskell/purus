{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use camelCase" #-}
module Language.Purus.Pipeline.DesugarCore (desugarCoreModule, matchVarLamAbs) where

import Prelude

import Data.Map qualified as M

import Data.Text (Text)
import Data.Text qualified as T

import Data.Char (isUpper)
import Data.Foldable (Foldable (foldl'), foldrM, traverse_)
import Data.List (sort, sortOn)
import Data.Maybe (fromJust, isJust)

import Data.Bifunctor (Bifunctor (first,second))

import Control.Monad.Reader ( join, unless, MonadReader(local) )
import Control.Monad.State (get, modify)

import Language.PureScript.Names
    ( runIdent,
      Ident(..),
      ModuleName(ModuleName),
      ProperName(ProperName),
      Qualified(Qualified),
      QualifiedBy(ByModuleName),
      coerceProperName,
      disqualify )
import Language.PureScript.AST.Literals (Literal (..))
import Language.PureScript.Constants.Prim qualified as C
import Language.PureScript.CoreFn.Ann (Ann, nullAnn)
import Language.PureScript.CoreFn.Binders (Binder (..))
import Language.PureScript.CoreFn.Desugar.Utils (properToIdent, showIdent', wrapTrace)
import Language.PureScript.CoreFn.Expr (
  Bind (NonRec, Rec),
  CaseAlternative (CaseAlternative),
  Expr (..),
  PurusType,
 )
import Language.PureScript.CoreFn.Module (Datatypes, Module (..))
import Language.PureScript.CoreFn.TypeLike (TypeLike (..))
import Language.PureScript.CoreFn.Utils (exprType)
import Language.PureScript.Environment (mkCtorTy, mkTupleTyName)
import Language.PureScript.Types (Type (..))

import Language.Purus.Debug (
  doTrace,
  doTraceM,
  prettify,
 )
import Language.Purus.IR (
  Alt (..),
  BVar (..),
  BindE (..),
  Exp (..),
  FVar (..),
  FuncType (..),
  Lit (CharL, IntL, ObjectL, StringL),
  Pat (..),
  expTy,
 )
import Language.Purus.IR.Utils
    ( foldBinds, WithObjects, Vars, IR_Decl, mapBind, viaExp )
import Language.Purus.Pipeline.Monad
    ( globalScope,
      DesugarContext(DesugarContext),
      DesugarCore,
      localScope,
      MonadCounter(next) )
import Language.Purus.Pretty (prettyStr, prettyTypeStr, renderExprStr)
import Language.Purus.Pretty.Common qualified as PC

import Bound (Var (..), abstract)
import Bound.Scope (fromScope)

import Control.Lens (
  At (at),
  Ixed (ix),
  folded,
  over,
  set,
  transform,
  view,
  (.=),
  (^?),
 )

import Debug.Trace (traceM)
import Prettyprinter (Pretty (..))

{- This runs the computation in an empty *local* context. The globals (i.e. top level declarations and
   imports are still in scope.)

   In general, we ignore locally-scoped top-level declarations until we've desugared
   everything to core, then we abstract those variables after we've assigned enough
   indices.
-}
freshly :: DesugarCore a -> DesugarCore a
freshly act = local (set localScope M.empty) act

bindLocal :: Ident -> DesugarCore Int
bindLocal ident = do
  i <- next
  modify $ over localScope (M.insert ident i) -- localScope . at ident .= i
  s <- view localScope
  doTraceM "bind" ("IDENT: " <> T.unpack (runIdent ident) <> "\n\nINDEX: " <> prettyStr i <> "\n\nSCOPE:\n" <> prettyStr (M.toList s))
  pure i

forceBindGlobal :: ModuleName -> Ident -> Int -> DesugarCore ()
forceBindGlobal mn i indx = do
  DesugarContext globals _ <- get
  let modMapExists = isJust (M.lookup mn globals)
  unless modMapExists $
    modify $
      over globalScope (M.insert mn M.empty)
  modify $ over (globalScope . ix mn) (M.insert i indx)

isBuiltinOrPrim :: Exp x t (Vars t) -> Bool
isBuiltinOrPrim = \case
  V (F (FVar _ (Qualified (ByModuleName (ModuleName "Prim")) _))) -> True
  V (F (FVar _ (Qualified _ (Ident i)))) -> isUpper (T.head i)
  _ -> False

{- We don't bind anything b/c the type level isn't `Bound` -}
tyAbs :: forall x t. Text -> KindOf t -> Exp x t (Vars t) -> DesugarCore (Exp x t (Vars t))
tyAbs nm k exp'
  | not (isBuiltinOrPrim exp') = do
      u <- next
      pure $ TyAbs (BVar u k (Ident nm)) exp'
  | otherwise = pure exp'

tyAbsMany :: forall x t. [(Text, KindOf t)] -> Exp x t (Vars t) -> DesugarCore (Exp x t (Vars t))
tyAbsMany vars expr = foldrM (uncurry tyAbs) expr vars

{- NOTE: We need access to the declarations for all imports here in order to correctly abstract (i.e. bind)
         all variables that should be represented as BVars.

         For all subsequent compiler passes, a free variable (an IR.FVar) indicates a variable which is
         *absolutely free* until the final code generation pass, i.e., a variable that either *cannot* be bound
         at all, or one which can only be sensibly bound by some PIR-specific construct.

         In practice these variables fall into three catogories:

           - Constructors, which acquire binders in PIR datatype declarations. Those declarations cannot be
             created until (at least) the object desugaring pass, which necessarily occurs somewhat late in
             the compilation pipeline. Only the winnowed IR, which we create in that pass, has types which are
             guaranteed to be representable in PIR.

           - Actual PLC (well, PLC DefaultFun) Builtins. These are true primitives and are replaced with the
             PIR AST `Builtin` construct during code generation.

           - "Morally" Builtin functions that have declarations which are baked into the Purus compiler.
             We have to provide some of these for (e.g.) serializing and deserializing ledger API types.

        Note that variables which represent the identifiers of imported declarations do not fall into
        one of those categories! We have to bind those declarations *here*, so we need to collect those
        declarations from this modules dependencies and pass them as an argument here.
-}

desugarCoreModule ::
  Datatypes PurusType PurusType ->
  [IR_Decl] ->
  Module (Bind Ann) PurusType PurusType Ann ->
  DesugarCore (Module IR_Decl PurusType PurusType Ann)
desugarCoreModule inScope imports Module {..} = do
  globalScope . at moduleName .= Just M.empty
  decls' <- traverse (freshly . desugarCoreDecl . doEtaReduce) moduleDecls
  decls <- bindLocalTopLevelDeclarations decls'
  let allDatatypes = moduleDataTypes <> inScope
  s <- get
  traceM $ "DesugarContext for " <> prettyStr moduleName <> "\n" <> prettyStr s
  let result = Module {moduleDecls = decls <> imports, moduleDataTypes = allDatatypes, ..}
  -- traceM $ "Desugar Coure output for " <> prettyStr moduleName <> "\n" <> docString (prettyModule result)
  pure result
  where
    doEtaReduce = \case
      NonRec a nm body -> NonRec a nm (runEtaReduce body)
      Rec xs -> Rec $ fmap runEtaReduce <$> xs
    {- Need to do this to ensure that all  -}
    bindLocalTopLevelDeclarations ::
      [BindE PurusType (Exp WithObjects PurusType) (Vars PurusType)] ->
      DesugarCore [BindE PurusType (Exp WithObjects PurusType) (Vars PurusType)]
    bindLocalTopLevelDeclarations ds = do
      let topLevelIdents = foldBinds (\acc nm _ -> nm : acc) [] ds
      traverse_ (uncurry (forceBindGlobal moduleName)) topLevelIdents
      s <- get
      doTraceM "bindLocalTopLevelDeclarations" $
        prettify
          [ -- "Input (Module Decls):\n" <> prettify (prettyStr <$> ds)
            "Top level idents:\n" <> prettify (prettyStr <$> topLevelIdents)
          , "State:\n" <> prettyStr s
          ]
      -- this is only safe because every locally-scoped (i.e. inside a decl) variable should be
      -- bound by this point
      let upd = \case
            V (B bv) -> V $ B bv
            V (F fv@(FVar t (Qualified (ByModuleName mn) ident))) ->
              case s ^? globalScope . at mn . folded . at ident . folded of
                Just indx -> V $ B $ BVar indx t ident
                Nothing -> V $ F fv
            other -> other
      pure $ mapBind (const $ viaExp (transform upd)) <$> ds

desugarCoreDecl ::
  Bind Ann ->
  DesugarCore (BindE PurusType (Exp WithObjects PurusType) (Vars PurusType))
desugarCoreDecl = \case
  NonRec _ ident expr -> wrapTrace ("desugarCoreDecl: " <> showIdent' ident) $ do
    bvix <- bindLocal ident
    s <- view localScope
    let abstr = abstract (matchLet s)
    desugared <- desugarCore expr
    let scoped = abstr desugared
    pure $ NonRecursive ident bvix scoped
  Rec xs -> do
    let inMsg =
          concatMap
            ( \((_, i), x) ->
                prettyStr i
                  <> " :: "
                  <> prettyTypeStr (exprType x)
                  <> "\n"
                  <> prettyStr i
                  <> " = "
                  <> renderExprStr x
                  <> "\n\n"
            )
            xs
    doTraceM "desugarCoreDecl" inMsg
    first_pass <- traverse (\((_, ident), e) -> bindLocal ident >>= \u -> pure ((ident, u), e)) xs
    s <- view localScope
    let abstr = abstract (matchLet s)
    second_pass <-
      traverse
        ( \((ident, bvix), expr) -> do
            wrapTrace ("desugarCoreDecl: " <> showIdent' ident) $ do
              desugared <- desugarCore expr
              let scoped = abstr desugared
              pure ((ident, bvix), scoped)
        )
        first_pass
    doTraceM "desugarCoreDecl" ("RESULT (RECURSIVE):\n" <> prettyStr (fmap fromScope <$> second_pass))
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

desugarCore :: Expr Ann -> DesugarCore (Exp WithObjects PurusType (Vars PurusType))
desugarCore e = do
  let ty = exprType e
  result <- desugarCore' e
  let msg =
        prettify
          [ "INPUT:\n" <> renderExprStr e
          , "INPUT TY:\n" <> prettyTypeStr ty
          , "OUTPUT:\n" <> prettyStr result
          , "OUTPUT TY:\n" <> prettyStr (expTy id result)
          ]
  doTraceM "desugarCore" msg
  pure result

desugarCore' :: Expr Ann -> DesugarCore (Exp WithObjects PurusType (Vars PurusType))
desugarCore' (Literal _ann ty lit) = LitE ty <$> desugarLit lit
desugarCore' lam@(Abs _ann ty ident expr) = do
  bvIx <- bindLocal ident
  s <- view localScope
  expr' <- desugarCore' expr
  let !ty' = functionArgumentIfFunction $ snd (stripQuantifiers ty)
      (vars', _) = stripQuantifiers ty
      vars = (\(_, b, c) -> (b, c)) <$> vars'
      scopedExpr = abstract (matchLet s) expr'
  result <- tyAbsMany vars $ LamE (BVar bvIx ty' ident) scopedExpr
  let msg =
        prettify
          [ "ANNOTATED LAM TY:\n" <> prettyStr ty
          , "BOUND VAR TY:\n" <> prettyStr ty'
          , "BOUND VAR INDEX: " <> prettyStr bvIx
          , "BOUND VAR IDENT: " <> prettyStr ident
          , "BODY TY:\n" <> prettyStr (exprType expr)
          , "INPUT EXPR:\n" <> renderExprStr lam
          , "RESULT EXPR:\n" <> prettyStr result
          , "RESULT EXPR TY:\n" <> prettyStr (expTy id result)
          , "LOCAL SCOPE:\n" <> prettyStr (M.toList s)
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
  bindEs <- traverse rebindInScope =<< traverse desugarCoreDecl binds
  s <- view localScope
  cont' <- desugarCore cont
  let abstr = abstract (matchLet s)
  pure $ LetE bindEs $ abstr cont'
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

rebindInScope ::
  BindE PurusType (Exp WithObjects PurusType) (Vars PurusType) ->
  DesugarCore (BindE PurusType (Exp WithObjects PurusType) (Vars PurusType))
rebindInScope b = do
  s <- view localScope
  let f scoped = abstract (matchLet s) . fmap join . fromScope $ scoped
  case b of
    NonRecursive i x scoped -> pure $ NonRecursive i x (f scoped)
    Recursive xs -> pure . Recursive $ second f <$> xs
desugarAlt ::
  CaseAlternative Ann ->
  DesugarCore (Alt WithObjects PurusType (Exp WithObjects PurusType) (Vars PurusType))
desugarAlt (CaseAlternative [binder] result) = case result of
  Left exs ->
    error $
      "internal error: `desugarAlt` guarded alt not expected at this stage: " <> show exs
  Right ex -> do
    pat <- toPat binder
    s <- view localScope
    re' <- desugarCore ex
    let abstrE = abstract (matchLet s)
    pure $ UnguardedAlt pat (abstrE re')
desugarAlt (CaseAlternative binders result) = do
  pats <- traverse toPat binders
  s <- view localScope -- NOTE: Maybe this should be moved into the case block?
  let abstrE = abstract (matchLet s)
      n = length binders
      tupTyName = mkTupleTyName n
      tupCtorName = coerceProperName <$> tupTyName
      pat = ConP tupTyName tupCtorName pats
  case result of
    Left exs -> do
      error $ "internal error: `desugarAlt` guarded alt not expected at this stage: " <> show exs
    Right ex -> do
      re' <- desugarCore ex
      pure $ UnguardedAlt pat (abstrE re')

toPat :: Binder ann -> DesugarCore (Pat x PurusType (Exp x ty) (Vars ty))
toPat = \case
  NullBinder _ -> pure WildP
  VarBinder _ i ty -> do
    n <- bindLocal i
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


desugarLit :: Literal (Expr Ann) -> DesugarCore (Lit WithObjects (Exp WithObjects PurusType (Vars PurusType)))
desugarLit (NumericLiteral (Left int)) = pure $ IntL int
desugarLit (NumericLiteral (Right _)) = error "TODO: Remove Number lits from all preceding ASTs" -- pure $ NumL number
desugarLit (StringLiteral string) = pure $ StringL string
desugarLit (CharLiteral char) = pure $ CharL char
desugarLit (BooleanLiteral _) = error "TODO: Remove BooleanLiteral from all preceding ASTs"
desugarLit (ArrayLiteral _) = error "TODO: Remove ArrayLiteral from IR AST" -- ArrayL <$> traverse desugarCore arr
desugarLit (ObjectLiteral object) = ObjectL () <$> desugarObjectMembers object

-- this looks like existing monadic combinator but I couldn't find it
desugarObjectMembers :: [(field, Expr Ann)] -> DesugarCore [(field, Exp WithObjects PurusType (Vars PurusType))]
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
        <> prettyStr fv
        <> "\n\nOUTPUT:\n"
        <> prettyStr result
    result = do
      let nm = disqualify n'
      bvix <- M.lookup nm binds
      pure $ BVar bvix ty nm

-- TODO (t4ccer): Move somehwere, but cycilc imports are annoying
instance FuncType PurusType where
  headArg = functionArgumentIfFunction

runEtaReduce :: Expr ann -> Expr ann
runEtaReduce = transform etaReduce

etaReduce ::
  forall ann.
  Expr ann ->
  Expr ann
etaReduce input = case partitionLam input of
  Just (boundVars, fun, args) ->
    if sort boundVars == sort args then fun else input
  Nothing -> input
  where
    partitionLam ::
      Expr ann ->
      Maybe ([Ident], Expr ann, [Ident])
    partitionLam e = do
      let (bvars, inner) = stripLambdas e
      (f, args) <- analyzeAppCfn inner
      argBVars <- traverse argToBVar args
      pure $ (bvars, f, argBVars)

    argToBVar :: Expr ann -> Maybe Ident
    argToBVar = \case
      Var _ _ qualified -> Just . disqualify $ qualified
      _ -> Nothing

    stripLambdas = \case
      Abs _ _ bv body -> first (bv :) $ stripLambdas body
      other -> ([], other)

analyzeAppCfn :: Expr ann -> Maybe (Expr ann, [Expr ann])
analyzeAppCfn e = (,appArgs e) <$> appFun e
  where
    appArgs :: Expr ann -> [Expr ann]
    appArgs (App _ t1 t2) = appArgs t1 <> [t2]
    appArgs _ = []

    appFun :: Expr ann -> Maybe (Expr ann)
    appFun (App _ t1 _) = go t1
      where
        go (App _ tx _) = case appFun tx of
          Nothing -> Just tx
          Just tx' -> Just tx'
        go other = Just other
    appFun _ = Nothing

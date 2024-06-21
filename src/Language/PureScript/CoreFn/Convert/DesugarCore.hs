{-# OPTIONS_GHC  -Wno-orphans #-}
{-# LANGUAGE TypeApplications #-}
module Language.PureScript.CoreFn.Convert.DesugarCore where

import Prelude

import Language.PureScript.Types (Type(..))
import Language.PureScript.Names (Ident, Qualified(Qualified),
                                  QualifiedBy (ByModuleName, BySourcePos),
                                  pattern ByNullSourcePos,
                                  ModuleName (ModuleName), ProperName (ProperName), disqualify, coerceProperName)
import Language.PureScript.CoreFn.Expr (Expr(..), PurusType, Bind (NonRec, Rec),
                                        CaseAlternative(CaseAlternative), _Var)
import Language.PureScript.CoreFn.Ann (Ann, annSS, nullAnn)
import Language.PureScript.CoreFn.Convert.IR
    ( Exp(..),
      XObjectLiteral,
      XObjectUpdate,
      XAccessor,
      BindE(..),
      Alt(..),
      Pat(..),
      Lit(ObjectL, IntL, NumL, StringL, CharL,  ArrayL),
      FVar(..),
      BVar(..),
      FuncType(..),
      ppExp,
      assembleBindEs,
      mkBindings,
      abstractMany )
import Data.Map qualified as M
import Language.PureScript.AST.Literals (Literal (..))
import Bound (abstract, Var (..))
import Control.Monad (join, forM)
import Control.Monad.State ( join, StateT, modify', put, gets, get, evalState, evalStateT, runStateT )
import Data.List (find, sortOn)
import Language.PureScript.CoreFn.Utils (exprType, Context)
import Language.PureScript.CoreFn.Binders ( Binder(..) )
import Data.Maybe (mapMaybe)
import Control.Lens.Operators ((^..))
import Control.Lens.IndexedPlated (icosmos)
import Control.Lens.Combinators (to, ix, preview)
import Control.Monad.Error.Class (MonadError(throwError))
import Language.PureScript.AST.SourcePos (spanStart)
import Language.PureScript.CoreFn.Module (Module(..))
import Language.PureScript.CoreFn.Pretty (renderExprStr, prettyAsStr)
import Debug.Trace (traceM)
import Language.PureScript.CoreFn.Desugar.Utils (wrapTrace, showIdent', properToIdent)
import Data.Void (Void)
import Language.PureScript.Constants.Prim qualified as C
import Data.Text qualified as T
import Data.Foldable (Foldable(foldl'), traverse_)
import Language.PureScript.Environment (mkCtorTy, mkTupleTyName)
import Control.Lens hiding (Context)
import Control.Monad.Trans (lift)


-- Need the map to keep track of whether a variable has already been used in the scope (e.g. for shadowing)
type DS = StateT (Int,M.Map Ident Int) (Either String)

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
  pure i

getVarIx :: Ident -> DS Int
getVarIx ident = gets (preview (_2 . ix ident)) >>= \case
  Nothing -> error $ "getVarIx: Free variable " <> showIdent' ident
  Just indx -> pure indx

data WithObjects

type instance XAccessor WithObjects = ()
type instance XObjectUpdate WithObjects = ()
type instance XObjectLiteral WithObjects = ()

data WithoutObjects

type instance XAccessor WithoutObjects = Void
type instance XObjectUpdate WithoutObjects = Void
type instance XObjectLiteral WithoutObjects = Void

type IR_Decl = BindE PurusType (Exp WithObjects PurusType) (FVar PurusType)

desugarCoreModule :: Module (Bind Ann) PurusType PurusType Ann -> Either String (Module IR_Decl PurusType PurusType Ann,(Int,M.Map Ident Int))
desugarCoreModule m = case runStateT (desugarCoreModule' m) (0,M.empty) of
  Left err -> Left err
  Right res -> do
    traceM ("desugarCoreModule decls OUTPUT:\n" <> prettyAsStr (moduleDecls $ fst res))
    pure res

desugarCoreModule' :: Module (Bind Ann) PurusType PurusType Ann -> DS (Module IR_Decl PurusType PurusType Ann)
desugarCoreModule' Module{..} = do
  decls <- traverse (freshly . desugarCoreDecl) moduleDecls
  pure $ Module {moduleDecls = decls,..}

desugarCoreDecl :: Bind Ann
                -> DS (BindE PurusType (Exp WithObjects PurusType) (FVar PurusType))
desugarCoreDecl = \case
  NonRec _ ident expr -> wrapTrace ("desugarCoreDecl: " <> showIdent' ident) $
    NonRecursive ident  <$> desugarCore' expr
  Rec xs -> Recursive
          <$> traverse (\((_,ident),expr) ->
                          wrapTrace ("desugarCoreDecl: " <> showIdent' ident) $
                          (ident,)
                          <$> desugarCore' expr) xs

desugarCore' :: Expr Ann -> DS (Exp WithObjects PurusType (FVar PurusType))
desugarCore' e = do
  traceM $ "desugarCore INPUT: " <> renderExprStr e
  res <- desugarCore e
  traceM $ "desugarCore OUTPUT: " <> ppExp res
  pure res

{- | Turns a list of expressions into an n-ary
     tuple, where n = the length of the list.

     Throws an error on empty lists. Should only be used
     in contexts that must be nonempty (e.g. case expression
     scrutinees and case alternative patterns)
-}
tuplify :: [Expr Ann] -> Expr Ann
tuplify [] = error  "tuplify called on empty list of expressions"
tuplify es = foldl' (App nullAnn) tupCtor es
  where
    n = length es
    tupName = Qualified (ByModuleName C.M_Prim) (ProperName $ "Tuple" <> T.pack (show n))
    tupCtorType = mkCtorTy tupName n

    tupCtor :: Expr Ann
    tupCtor = Var nullAnn tupCtorType (properToIdent <$> tupName)

desugarCore :: Expr Ann -> DS (Exp WithObjects PurusType (FVar PurusType))
desugarCore (Literal _ann ty lit) = LitE ty <$> desugarLit lit
desugarCore (Abs _ann ty ident expr) = do
  bvIx  <- bind ident
  expr' <- desugarCore expr
  let !ty' = functionArgumentIfFunction ty
  let scopedExpr = abstract (matchVarLamAbs ident bvIx) expr'
  pure $ LamE ty (BVar bvIx ty' ident) scopedExpr
desugarCore (App _ann expr1 expr2) = do
  expr1' <- desugarCore expr1
  expr2' <- desugarCore expr2
  pure $ AppE expr1' expr2'
desugarCore (Var _ann ty qi) = pure $ V $ FVar ty qi
desugarCore (Let _ann binds cont) = do
  binds' <- desugarBinds binds
  let allBoundIdents = fst <$> join binds'
  traverse_ (bind . (\(FVar _ nm) -> disqualify nm)) allBoundIdents
  s <- gets (view _2)
  cont' <- desugarCore cont
  let abstr = abstract (matchLet s)
      bindEs = assembleBindEs [] binds'
      bindings = mkBindings allBoundIdents
  pure $ LetE bindings bindEs $ abstr cont'
desugarCore (Accessor _ann ty label expr) = do
  expr' <- desugarCore expr
  pure $ AccessorE () ty label expr'
desugarCore (ObjectUpdate _ann ty expr toCopy toUpdate) = do
  expr' <- desugarCore expr
  toUpdate' <- desugarObjectMembers toUpdate
  pure $ ObjectUpdateE () ty expr' toCopy toUpdate'
-- NOTE: We do not tuple single scrutinees b/c that's just a performance hit w/ no point
desugarCore (Case _ann ty [scrutinee] alts) = do
  scrutinee' <- desugarCore  scrutinee
  alts' <- traverse desugarAlt alts
  pure $ CaseE ty scrutinee' alts'
desugarCore (Case _ann ty scrutinees alts) = do
  scrutinees' <- desugarCore $ tuplify scrutinees
  alts' <- traverse desugarAlt alts
  pure $ CaseE ty scrutinees' alts'

desugarAlt :: CaseAlternative Ann -> DS (Alt WithObjects PurusType (Exp WithObjects PurusType) (FVar PurusType))
desugarAlt (CaseAlternative [binder] result) = case result of
  Left exs -> throwError $ "internal error: `desugarAlt` guarded alt not expected at this stage: " <> show exs
  Right ex -> do
    pat <- toPat binder
    s <- gets (view _2)
    let abstrE = abstract (matchLet s)
    re <- desugarCore ex
    pure $ UnguardedAlt (M.empty) pat (abstrE re)
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
      re <- desugarCore ex
      pure $ UnguardedAlt (M.empty) pat (abstrE re)

toPat :: Binder ann -> DS (Pat x (Exp x ty) (FVar ty))
toPat = \case
  NullBinder _ -> pure WildP
  VarBinder _ i  ->  do
    n <- bind i
    pure $ VarP i n
  ConstructorBinder _ tn cn bs -> ConP tn cn <$> traverse toPat bs
  NamedBinder _ nm b ->  do
    n <- bind nm
    AsP (nm,n) <$> toPat b
  LiteralBinder _ lp -> case lp of
    NumericLiteral (Left i) -> pure . LitP $ IntL i
    NumericLiteral (Right d) -> pure . LitP $ NumL d
    StringLiteral pss -> pure . LitP $ StringL pss
    CharLiteral c -> pure . LitP $ CharL c
    BooleanLiteral _ -> error "boolean literals shouldn't exist anymore"
    ArrayLiteral as ->  LitP . ArrayL <$> traverse toPat as
    ObjectLiteral fs' -> do
      -- REVIEW/FIXME:
      -- this isn't right, we need to make sure the positions of the binders are correct,
      -- since (I think?) you can use an Obj binder w/o using all of the fields
      let fs          = sortOn fst fs'
          len         = length fs
          tupTyName   = mkTupleTyName len
          tupCtorName = coerceProperName <$> tupTyName
          inner = map (toPat . snd) fs
      ConP tupTyName tupCtorName <$> traverse (toPat . snd) fs




getBoundVar :: forall ann. Show ann => Either [(Expr ann, Expr ann)] (Expr ann) -> Binder ann -> [FVar PurusType]
getBoundVar body binder = case binder of
  ConstructorBinder _ _ _ binders -> concatMap (getBoundVar body) binders
  LiteralBinder _ (ArrayLiteral arrBinders) -> concatMap (getBoundVar body) arrBinders
  LiteralBinder _ (ObjectLiteral objBinders) -> concatMap (getBoundVar body . snd) objBinders
  VarBinder _ ident -> case body of
    Right expr -> case findBoundVar ident expr of
      Nothing -> [] -- probably should trace or warn at least
      Just (ty, qi) -> [FVar ty qi]
    Left fml -> do
      let allResults = concatMap (\(x,y) -> [x,y]) fml
          matchingVar = mapMaybe (findBoundVar ident) allResults
      case matchingVar of
        ((ty, qi) : _) -> [FVar ty qi]
        _ -> []
  _ -> []

findBoundVar :: forall ann. Show ann => Ident -> Expr ann -> Maybe (PurusType, Qualified Ident)
findBoundVar nm ex = find (goFind . snd) (allVars ex)
  where
    goFind = \case
      Qualified (ByModuleName _) _ -> False
      Qualified (BySourcePos _) nm' -> nm == nm'

allVars :: forall ann. Show ann => Expr ann -> [(PurusType, Qualified Ident)]
allVars ex = ex ^.. icosmos @Context @(Expr ann) M.empty . _Var . to (\(_,b,c) -> (b,c))

qualifySS :: Ann -> Ident -> Qualified Ident
qualifySS ann i = Qualified (BySourcePos $ spanStart (annSS ann)) i

-- Stolen from DesugarObjects
desugarBinds :: [Bind Ann] -> DS [[(FVar PurusType, Exp WithObjects PurusType (FVar PurusType))]]
desugarBinds [] = pure []
desugarBinds (b:bs) = case b of
  NonRec _ann ident expr -> do
    let qualifiedIdent = qualifySS _ann ident
    e' <- desugarCore expr
    rest <- desugarBinds bs
    pure $ [(FVar (exprType expr) qualifiedIdent,e')] : rest
  -- TODO: Fix this to preserve recursivity (requires modifying the *LET* ctor of Exp)
  Rec xs -> do
    let xs' = map (\((ann,nm),e) -> NonRec ann nm e) xs
    xs'' <- desugarBinds xs'
    rest <- desugarBinds bs
    pure $ xs'' <> rest

desugarLit :: Literal (Expr Ann) -> DS (Lit WithObjects (Exp WithObjects PurusType (FVar PurusType)))
desugarLit (NumericLiteral (Left int)) = pure $ IntL int
desugarLit (NumericLiteral (Right number)) = pure $ NumL number
desugarLit (StringLiteral string) = pure $ StringL string
desugarLit (CharLiteral char) = pure $ CharL char
desugarLit (BooleanLiteral _) = error "TODO: Remove BooleanLiteral from all preceding ASTs"
desugarLit (ArrayLiteral arr) = ArrayL <$> traverse desugarCore arr
desugarLit (ObjectLiteral object) = ObjectL () <$> desugarObjectMembers object

-- this looks like existing monadic combinator but I couldn't find it
desugarObjectMembers :: [(field, Expr Ann)] -> DS [(field, Exp WithObjects PurusType (FVar PurusType))]
desugarObjectMembers = traverse (\(memberName, val) -> (memberName,) <$> desugarCore val)

pattern (:~>) :: PurusType -> PurusType -> PurusType
pattern a :~> b <-
  (TypeApp _
   (TypeApp _
    (TypeConstructor _ (Qualified (ByModuleName (ModuleName "Prim")) (ProperName "Function"))) a) b)
infixr 0 :~>

functionArgumentIfFunction :: PurusType -> PurusType
functionArgumentIfFunction (arg :~> _) = arg
functionArgumentIfFunction t = t

-- | For usage with `Bound`, use only with lambda
matchVarLamAbs :: Ident -> Int -> FVar ty -> Maybe (BVar ty)
matchVarLamAbs nm bvix (FVar ty n')
  | nm == disqualify n' =  Just (BVar bvix ty nm)
  | otherwise = Nothing

matchLet :: M.Map Ident Int -> FVar ty -> Maybe (BVar ty)
matchLet binds (FVar ty n') = do
  let nm = disqualify n'
  bvix <- M.lookup nm binds
  pure $ BVar bvix ty nm

-- TODO (t4ccer): Move somehwere, but cycilc imports are annoying
instance FuncType PurusType where
  headArg = functionArgumentIfFunction

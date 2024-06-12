{-# OPTIONS_GHC -Werror -Wno-orphans #-}
{-# LANGUAGE TypeApplications #-}
module Language.PureScript.CoreFn.Convert.DesugarCore (WithObjects, WithoutObjects, desugarCore, desugarCoreModule) where

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
      Pat(ConP),
      Lit(ObjectL, IntL, NumL, StringL, CharL,  ArrayL),
      FVar(..),
      BVar(..),
      FuncType(..),
      ppExp,
      assembleBindEs,
      mkBindings,
      abstractMany,
      toPat )
import Data.Map qualified as M
import Language.PureScript.AST.Literals (Literal (..))
import Bound (abstract)
import Control.Monad (join)
import Data.List (find)
import Language.PureScript.CoreFn.Utils (exprType, Context)
import Language.PureScript.CoreFn.Binders ( Binder(..) )
import Data.Maybe (mapMaybe)
import Control.Lens.Operators ((^..))
import Control.Lens.IndexedPlated (icosmos)
import Control.Lens.Combinators (to)
import Control.Monad.Error.Class (MonadError(throwError))
import Language.PureScript.AST.SourcePos (spanStart)
import Language.PureScript.CoreFn.Module (Module(..))
import Language.PureScript.CoreFn.Pretty (renderExprStr)
import Debug.Trace (traceM)
import Language.PureScript.CoreFn.Desugar.Utils (wrapTrace, showIdent', properToIdent)
import Data.Void (Void)
import Language.PureScript.Constants.Prim qualified as C
import Data.Text qualified as T
import Data.Foldable (Foldable(foldl'))
import Language.PureScript.Environment (mkCtorTy, mkTupleTyName)

-- TODO: Something more reasonable
type DS = Either String

data WithObjects

type instance XAccessor WithObjects = ()
type instance XObjectUpdate WithObjects = ()
type instance XObjectLiteral WithObjects = ()

data WithoutObjects

type instance XAccessor WithoutObjects = Void
type instance XObjectUpdate WithoutObjects = Void
type instance XObjectLiteral WithoutObjects = Void

type IR_Decl = BindE PurusType (Exp WithObjects PurusType) (FVar PurusType)

desugarCoreModule :: Module (Bind Ann) PurusType PurusType Ann -> DS (Module IR_Decl PurusType PurusType Ann)
desugarCoreModule Module{..} = do
  decls <- traverse desugarCoreDecl moduleDecls
  pure $ Module {moduleDecls = decls,..}

desugarCoreDecl :: Bind Ann
                -> Either String (BindE PurusType (Exp WithObjects PurusType) (FVar PurusType))
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
  expr' <- desugarCore expr
  let !ty' = functionArgumentIfFunction ty
  let scopedExpr = abstract (matchVarLamAbs ty' ident) expr'
  pure $ LamE ty (BVar 0 ty' ident) scopedExpr
desugarCore (App _ann expr1 expr2) = do
  expr1' <- desugarCore expr1
  expr2' <- desugarCore expr2
  pure $ AppE expr1' expr2'
desugarCore (Var _ann ty qi) = pure $ V $ FVar ty qi
desugarCore (Let _ann binds cont) = do
  binds' <- desugarBinds binds
  cont' <- desugarCore cont
  let allBoundIdents = fst <$> join binds'
      abstr = abstract $ abstractMany allBoundIdents
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
    let boundVars = getBoundVar result binder
        pat = toPat binder
        abstrE = abstract (abstractMany boundVars)
    re <- desugarCore ex
    pure $ UnguardedAlt (mkBindings boundVars) pat (abstrE re)
desugarAlt (CaseAlternative binders result) = do
  let boundVars = concatMap (getBoundVar result) binders
      pats = map toPat binders
      abstrE = abstract (abstractMany boundVars)
      n = length binders
      tupTyName = mkTupleTyName n
      tupCtorName = coerceProperName <$> tupTyName
      pat = ConP tupTyName tupCtorName pats
  case result of
    Left exs -> do
      throwError $ "internal error: `desugarAlt` guarded alt not expected at this stage: " <> show exs
    Right ex -> do
      re <- desugarCore ex
      let
      pure $ UnguardedAlt (mkBindings boundVars) pat (abstrE re)

getBoundVar :: Either [(Expr ann, Expr ann)] (Expr ann) -> Binder ann -> [FVar PurusType]
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

findBoundVar :: Ident -> Expr ann -> Maybe (PurusType, Qualified Ident)
findBoundVar nm ex = find (goFind . snd) (allVars ex)
  where
    goFind = \case
      Qualified (ByModuleName _) _ -> False
      Qualified ByNullSourcePos _ -> False -- idk about this actually, guess we'll find out
      Qualified (BySourcePos _) nm' -> nm == nm'

allVars :: forall ann. Expr ann -> [(PurusType, Qualified Ident)]
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
desugarLit (BooleanLiteral b) = error "TODO: Remove BooleanLiteral from all ASTs"
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
matchVarLamAbs :: Eq ty => ty -> Ident -> FVar ty -> Maybe (BVar ty)
matchVarLamAbs t nm (FVar ty n')
  | ty == t && nm == disqualify n' =  Just (BVar 0 t nm)
  | otherwise = Nothing

-- TODO (t4ccer): Move somehwere, but cycilc imports are annoying
instance FuncType PurusType where
  headArg = functionArgumentIfFunction

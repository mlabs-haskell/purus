{-# OPTIONS_GHC  -Wno-orphans #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
module Language.PureScript.CoreFn.Convert.DesugarCore where

import Prelude

import Language.PureScript.Types (Type(..))
import Language.PureScript.Names (Ident(..), Qualified(Qualified),
                                  QualifiedBy (ByModuleName, BySourcePos),
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
      Lit(ObjectL, IntL,  StringL, CharL),
      FVar(..),
      BVar(..),
      FuncType(..),
      ppExp,
      mkBindings, expTy', unsafeAnalyzeApp, analyzeApp, expTy )
import Data.Map qualified as M
import Language.PureScript.AST.Literals (Literal (..))
import Bound (abstract, Var (..))
import Control.Monad (join, foldM, void)
import Control.Monad.State ( StateT, modify', gets, runStateT )
import Data.List (find, sortOn)
import Language.PureScript.CoreFn.Utils (exprType, Context)
import Language.PureScript.CoreFn.Binders ( Binder(..) )
import Data.Maybe (mapMaybe)
import Control.Lens.IndexedPlated (icosmos)
import Control.Monad.Error.Class (MonadError(throwError))
import Language.PureScript.AST.SourcePos (spanStart)
import Language.PureScript.CoreFn.Module (Module(..))
import Language.PureScript.CoreFn.Pretty (renderExprStr, prettyAsStr, prettyTypeStr)
import Language.PureScript.CoreFn.Desugar.Utils (wrapTrace, showIdent', properToIdent)
import Data.Void (Void)
import Language.PureScript.Constants.Prim qualified as C
import Data.Text qualified as T
import Data.Foldable (Foldable(foldl'), traverse_, foldrM)
import Language.PureScript.Environment (mkCtorTy, mkTupleTyName)
import Control.Lens hiding (Context)
import Control.Monad.Trans (lift)
import Language.PureScript (runIdent)
import Bound.Scope (toScope, Scope, fromScope)

import Language.PureScript.CoreFn.Convert.Debug
import Prettyprinter (Pretty (..), vcat)
import Language.PureScript.CoreFn.TypeLike (TypeLike(..))
import Data.Bifunctor (Bifunctor(first))
import Data.Text (Text)

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
  doTraceM "bind" ("IDENT: " <> T.unpack (runIdent ident) <> "\n\nINDEX: " <> prettyAsStr i)
  pure i

getVarIx :: Ident -> DS Int
getVarIx ident = gets (preview (_2 . ix ident)) >>= \case
  Nothing -> error $ "getVarIx: Free variable " <> showIdent' ident
  Just indx -> pure indx

{- We don't bind anything b/c the type level isn't `Bound` -}
tyAbs :: forall x t a.  Text -> KindOf t -> Exp x t a -> DS (Exp x t a)
tyAbs nm k exp = do
  u <- fresh
  pure $ TyAbs (BVar u k (Ident nm)) exp

tyAbsMany :: forall x t a. [(Text,KindOf t)] -> Exp x t a -> DS (Exp x t a)
tyAbsMany vars expr = foldrM (uncurry tyAbs) expr vars

data WithObjects

type instance XAccessor WithObjects = ()
type instance XObjectUpdate WithObjects = ()
type instance XObjectLiteral WithObjects = ()

data WithoutObjects

type instance XAccessor WithoutObjects = Void
type instance XObjectUpdate WithoutObjects = Void
type instance XObjectLiteral WithoutObjects = Void

type IR_Decl = BindE PurusType (Exp WithObjects PurusType) (Vars PurusType)

type Vars t = Var (BVar t) (FVar t)

desugarCoreModule :: Module (Bind Ann) PurusType PurusType Ann -> Either String (Module IR_Decl PurusType PurusType Ann,(Int,M.Map Ident Int))
desugarCoreModule m = case runStateT (desugarCoreModule' m) (0,M.empty) of
  Left err -> Left err
  Right res -> do
    doTraceM "desugarCoreModule" ("decls OUTPUT:\n" <> prettyAsStr (moduleDecls $ fst res))
    pure res

desugarCoreModule' :: Module (Bind Ann) PurusType PurusType Ann -> DS (Module IR_Decl PurusType PurusType Ann)
desugarCoreModule' Module{..} = do
  decls <- traverse (freshly . desugarCoreDecl) moduleDecls
  pure $ Module {moduleDecls = decls,..}

desugarCoreDecl :: Bind Ann
                -> DS (BindE PurusType (Exp WithObjects PurusType) (Vars PurusType))
desugarCoreDecl = \case
  NonRec _ ident expr -> wrapTrace ("desugarCoreDecl: " <> showIdent' ident) $ do
     bvix <- bind ident
     s <- gets (view _2)
     let abstr = abstract (matchLet s) . runEtaReduce
     desugared <- desugarCore' expr
     let boundVars = freeTypeVariables (expTy id desugared)
     scoped <-  abstr  <$> tyAbsMany boundVars  desugared
     pure $ NonRecursive ident bvix  scoped
  Rec xs ->  do
    let inMsg = concatMap (\((_,i),x) ->
                               prettyAsStr i <> " :: " <> prettyTypeStr (exprType x) <> "\n"
                               <> prettyAsStr i <> " = " <> renderExprStr x <> "\n\n") xs
    doTraceM "desugarCoreDecl" inMsg
    first_pass <- traverse (\((_,ident),e) -> bind ident >>= \u -> pure ((ident,u),e)) xs
    s <- gets (view _2)
    let abstr = abstract (matchLet s) . runEtaReduce 
    second_pass  <- traverse (\((ident,bvix),expr) -> do
                          wrapTrace ("desugarCoreDecl: " <> showIdent' ident) $ do
                            desugared <- desugarCore' expr
                            let boundVars = freeTypeVariables (expTy id desugared)
                            scoped <- abstr <$> tyAbsMany boundVars desugared
                            pure ((ident,bvix),scoped)) first_pass
    doTraceM "desugarCoreDecl" ("RESULT (RECURSIVE):\n" <> prettyAsStr (fmap fromScope <$> second_pass))
    pure $ Recursive second_pass


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

desugarCore :: Expr Ann -> DS (Exp WithObjects PurusType (Vars PurusType))
desugarCore e = do
  let ty = exprType e
  e' <- desugarCore' e
  let (vars',_) = stripQuantifiers ty
      vars = (\(a,b,c) -> (b,c)) <$> vars'
  result <- tyAbsMany vars e'
  let msg = prettify [ "INPUT: " <> renderExprStr e
                     , "OUTPUT: " <> prettyAsStr result]
  doTraceM "desugarCore" msg
  pure result 


desugarCore' :: Expr Ann -> DS (Exp WithObjects PurusType (Vars PurusType))
desugarCore' (Literal _ann ty lit) = LitE ty <$> desugarLit lit
desugarCore' lam@(Abs _ann ty ident expr) = do
  bvIx  <- bind ident
  s <- gets (view _2)
  expr' <- desugarCore expr
  let !ty' = functionArgumentIfFunction $ snd (stripQuantifiers ty)
      scopedExpr = abstract (matchLet s) expr'
      result =  LamE (BVar bvIx ty' ident) scopedExpr
      msg = "ANNOTATED LAM TY:\n" <> prettyAsStr ty
            <> "\n\nBOUND VAR TY:\n" <> prettyAsStr ty'
            <> "\n\nINPUT EXPR:\n" <> renderExprStr lam
            <> "\nRESULT EXPR:\n" <> prettyAsStr result
  doTraceM "desugarCoreLam" msg
  pure result 
desugarCore' (App _ann expr1 expr2) = do
  expr1' <- desugarCore expr1
  expr2' <- desugarCore expr2
  pure $ AppE expr1' expr2'
desugarCore' (Var _ann ty qi) = pure $ V . F $ FVar ty qi
desugarCore' (Let _ann binds cont) = do
  -- afaict their mutual recursion sorter doesn't work properly in let exprs (maybe it's implicit that they're all mutually recursive?)
  traverse_ bindAllNames binds
  bindEs <- traverse desugarCoreDecl binds
  s <-  gets (view _2)
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
  scrutinee' <- desugarCore  scrutinee
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

desugarAlt :: CaseAlternative Ann
           -> DS (Alt WithObjects PurusType (Exp WithObjects PurusType) (Vars PurusType))
desugarAlt (CaseAlternative [binder] result) = case result of
  Left exs ->
    throwError
    $ "internal error: `desugarAlt` guarded alt not expected at this stage: " <> show exs
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
  VarBinder _ i ty ->  do
    n <- bind i
    pure $ VarP i n ty
  ConstructorBinder _ tn cn bs -> ConP tn cn <$> traverse toPat bs
  NamedBinder _ nm _ ->  error
                         $ "found namedBinder: " <> T.unpack (runIdent nm)
                           <> "TODO: remove NamedBinder/AsP everywhere (parser, cst, AST)"
  LiteralBinder _ lp -> case lp of
    NumericLiteral (Left i) -> pure . LitP $ IntL i
    NumericLiteral (Right _) -> error "numeric literals not supported (yet)"
    StringLiteral pss -> pure . LitP $ StringL pss
    CharLiteral c -> pure . LitP $ CharL c
    BooleanLiteral _ -> error "boolean literal patterns shouldn't exist anymore"
    ArrayLiteral _ ->  error "array literal patterns shouldn't exist anymore"
    ObjectLiteral fs' -> do
      -- REVIEW/FIXME:
      -- this isn't right, we need to make sure the positions of the binders are correct,
      -- since (I think?) you can use an Obj binder w/o using all of the fields
      let fs          = sortOn fst fs'
          len         = length fs
          tupTyName   = mkTupleTyName len
          tupCtorName = coerceProperName <$> tupTyName
      ConP tupTyName tupCtorName <$> traverse (toPat . snd) fs

getBoundVar :: forall ann. Show ann => Either [(Expr ann, Expr ann)] (Expr ann) -> Binder ann -> [Vars PurusType]
getBoundVar body binder = case binder of
  ConstructorBinder _ _ _ binders -> concatMap (getBoundVar body) binders
  LiteralBinder _ (ArrayLiteral arrBinders) -> concatMap (getBoundVar body) arrBinders
  LiteralBinder _ (ObjectLiteral objBinders) -> concatMap (getBoundVar body . snd) objBinders
  VarBinder _ ident _ -> case body of
    Right expr -> case findBoundVar ident expr of
      Nothing -> [] -- probably should trace or warn at least
      Just (ty, qi) -> [F $ FVar ty qi]
    Left fml -> do
      let allResults = concatMap (\(x,y) -> [x,y]) fml
          matchingVar = mapMaybe (findBoundVar ident) allResults
      case matchingVar of
        ((ty, qi) : _) -> [F $ FVar ty qi]
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
desugarBinds :: [Bind Ann] -> DS [[((Vars PurusType,Int), Scope (BVar PurusType) (Exp WithObjects PurusType) (Vars PurusType))]]
desugarBinds []  = pure []
desugarBinds (b:bs) = case b of
  NonRec _ann ident expr ->  do
    bvIx <- bind ident
    let qualifiedIdent = qualifySS _ann ident
    e' <- desugarCore expr
    let scoped = abstract (matchLet $ M.singleton ident bvIx) e'
    rest <- desugarBinds bs
    pure $ [((F $ FVar (exprType expr) qualifiedIdent,bvIx),scoped)] : rest
  -- TODO: Fix this to preserve recursivity (requires modifying the *LET* ctor of Exp)
  Rec xs -> do
    traverse_ (\((_,nm),_) -> bind nm) xs
    recRes <- traverse handleRecBind xs
    rest <- desugarBinds bs
    pure $ recRes : rest
 where
   handleRecBind :: ((Ann,Ident), Expr Ann)
                 -> DS ((Vars PurusType,Int),Scope (BVar PurusType) (Exp WithObjects PurusType) (Vars PurusType))
   handleRecBind ((ann,ident),expr) = do
     u <- getVarIx ident
     s <- gets (view _2)
     expr' <- desugarCore expr
     let scoped = abstract (matchLet s) expr'
         fv = F $ FVar (expTy' id scoped) (qualifySS ann ident)
     pure ((fv,u),scoped)

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

matchLet :: Pretty ty => M.Map Ident Int -> Vars ty -> Maybe (BVar ty)
matchLet _ (B bv) = Just bv 
matchLet binds fv@(F (FVar ty n')) = case result of
    Nothing -> Nothing
    Just _ -> doTrace "matchLet" msg result
  where
    msg = "INPUT:\n" <> prettyAsStr fv
          <> "\n\nOUTPUT:\n" <> prettyAsStr result
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
assembleBindEs :: Eq ty => [[((FVar ty,Int) ,Scope (BVar ty) (Exp x ty) (FVar ty))]] -> DS [BindE ty (Exp x ty) (FVar ty)]
assembleBindEs  [] = pure []
assembleBindEs  ([]:rest) = assembleBindEs rest -- shouldn't happen but w/e
assembleBindEs ([((FVar _tx idnt,bix),e)]:rest) = do
  (NonRecursive (disqualify idnt) bix  e:) <$>  assembleBindEs rest
assembleBindEs  (xsRec:rest) = do
  let  recBinds = flip map xsRec $ \((FVar _tx idnt,bvix), e) -> ((disqualify idnt,bvix), e)
  rest' <- assembleBindEs rest
  pure $ Recursive recBinds : rest'

runEtaReduce :: forall x t
           . (Eq (Exp x t (Var (BVar t) (FVar t))), Pretty t, Pretty (KindOf t), TypeLike t)
          => Exp x t (Var (BVar t) (FVar t))
          -> Exp x t (Var (BVar t) (FVar t))
runEtaReduce e = doTrace "runEtaReduce" msg result
  where
    result = transform etaReduce e
    msg    = "INPUT:\n" <> prettyAsStr e
             <> "\n\nOUTPUT:\n" <> prettyAsStr result 

etaReduce :: forall x t
           . (Eq (Exp x t (Var (BVar t) (FVar t))), Pretty t, Pretty (KindOf t), TypeLike t)
          => Exp x t (Var (BVar t) (FVar t))
          -> Exp x t (Var (BVar t) (FVar t))
etaReduce input =  case partitionLam input  of
    Just (boundVars,fun,args) ->
      let result = if boundVars == args then fun else input
          msg = "INPUT:\n" <> prettyAsStr input
                <> "\n\nBOUND VARS:\n" <> show (vcat $ pretty <$> boundVars)
                <> "\n\nARGS:\n" <> show (vcat $ pretty <$> args)
                <> "\n\nFUN:\n" <> prettyAsStr fun
                <> "\n\nFUN TY:\n" <> prettyAsStr (expTy id fun)
                <> "\n\nARG TYS:\n" <> prettyAsStr (expTy id <$> args)
      in doTrace "etaReduce" msg result
    Nothing -> input 
 where
   partitionLam :: Exp x t (Var (BVar t) (FVar t))
                -> Maybe ([ Exp x t (Var (BVar t) (FVar t))], Exp x t (Var (BVar t) (FVar t)), [Exp x t (Var (BVar t) (FVar t))])
   partitionLam e = do
     let (bvars,inner) = stripLambdas e
     (f,args) <- analyzeApp inner
     pure $ (V . B <$> bvars,f,args)

   stripLambdas = \case
     LamE bv body -> first (bv:) $ stripLambdas (join <$> fromScope body)
     TyInstE _ inner -> stripLambdas inner
     other -> ([],other)

{- Useful for transform/rewrite/cosmos/etc -}
instance Plated (Exp x t (Vars t)) where
  plate = go
   where
     go :: forall f
         . ( Applicative f)
        => (Exp x t (Vars t) -> f  (Exp x t (Vars t)))
        -> Exp x t (Vars t)
        -> f (Exp x t (Vars t))
     go  tfun = \case
      LamE bv e ->  LamE  bv <$> scopeHelper e
      CaseE t es alts ->
        let goAlt ::  Alt x t (Exp x t) (Vars t) -> f (Alt x t (Exp x t) (Vars t))
            goAlt (UnguardedAlt bs pats scoped) =
              UnguardedAlt bs pats <$> scopeHelper scoped
        in CaseE t <$>  tfun es <*>  traverse goAlt alts
      LetE binds decls scoped ->
        let goDecls :: BindE t (Exp x t) (Vars t) -> f (BindE t (Exp x t) (Vars t))
            goDecls = \case
              NonRecursive ident bvix expr ->
                NonRecursive ident bvix <$> scopeHelper expr
              Recursive xs ->
                Recursive <$> traverse (\(i,x) -> (i,) <$> scopeHelper x) xs

        in LetE binds <$> traverse goDecls decls <*> scopeHelper scoped
      AppE e1 e2 -> AppE <$> tfun e1 <*> tfun e2
      AccessorE x t pss e -> AccessorE x t pss <$> tfun e
      ObjectUpdateE x t e cf fs -> (\e' fs' -> ObjectUpdateE x t e' cf fs')
                                   <$> tfun  e
                                   <*> traverse (\(nm,expr) -> (nm,) <$> tfun expr) fs
      LitE t lit -> LitE t <$> traverseLit lit
      V a -> pure (V a)
      TyAbs bv e -> TyAbs bv <$> tfun e
      TyInstE t e -> TyInstE t <$> tfun e
      where
        scopeHelper :: Scope (BVar t) (Exp x t) (Vars t)
                    -> f (Scope (BVar t) (Exp x t) (Vars t))
        scopeHelper scoped =
          let unscoped = join <$> fromScope scoped
              effed    = tfun unscoped
              abstr = abstract $ \case
               B bv -> Just bv
               _    -> Nothing
          in abstr <$> effed

        traverseLit  = \case
          IntL i -> pure $ IntL i
          -- NumL d -> pure $ NumL d
          StringL str -> pure $ StringL str
          CharL char -> pure $ CharL char
          -- ArrayL xs -> ArrayL <$> traverse tfun  xs
          -- ConstArrayL xs -> ConstArrayL <$> pure xs
          ObjectL x fs -> ObjectL x <$> traverse (\(str,e) -> (str,) <$> tfun e) fs

-- | This module contains functions for converting the CST into the core AST. It
-- is mostly boilerplate, and does the job of resolving ranges for all the nodes
-- and attaching comments.

module Language.PureScript.CST.Convert
  ( convertType
  , convertExpr
  , convertBinder
  , convertDeclaration
  , convertImportDecl
  , convertModule
  , sourcePos
  , sourceSpan
  , comment
  , comments
  , runConvert
  ) where

import Prelude hiding (take)

import Control.Monad.State
import Data.Bifunctor (bimap, first, second)
import Data.Char (toLower)
import Data.Foldable (foldl', foldrM, toList, traverse_)
import Data.Functor (($>))
import Data.List.NonEmpty qualified as NE
import Data.Maybe (isJust, fromJust, mapMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Map (Map)
import Data.Map qualified as M
import Language.PureScript.AST qualified as AST
import Language.PureScript.AST.Declarations.ChainId (mkChainId)
import Language.PureScript.AST.SourcePos qualified as Pos
import Language.PureScript.Comments qualified as C
import Language.PureScript.Crash (internalError)
import Language.PureScript.Environment qualified as Env
import Language.PureScript.Label qualified as L
import Language.PureScript.Names qualified as N
import Language.PureScript.PSString (mkString, prettyPrintStringJS)
import Language.PureScript.Types qualified as T
import Language.PureScript.CST.Positions
import Language.PureScript.CST.Print (printToken)
import Language.PureScript.CST.Types
import Data.Bitraversable (Bitraversable(..))
import Language.PureScript.Names (runProperName, coerceProperName)

import Debug.Trace (trace)
import Data.List (partition)

type ConvertM a = State (Map Text T.SourceType) a

runConvert :: ConvertM a -> a
runConvert ma = evalState ma M.empty

tvKind :: SourceToken -> Text -> ConvertM T.SourceType
tvKind srcTok nm = do
  cxt <- get
  case M.lookup nm cxt of
    Nothing -> internalError
               $ "Error: Missing kind annotation for TyVar " <> Text.unpack nm
                 <> "\n  at (or near): " <> prettyRange (srcTokenRange srcTok)
    Just t -> pure t
 where
   prettyRange (SourceRange start end) = goPos start <> "-" <> goPos end
   goPos (SourcePos line col) = show line <> ":" <> show col

bindTv :: Text -> T.SourceType -> ConvertM ()
bindTv nm ty = modify' (M.insert nm ty)

srcTokenRange :: SourceToken -> SourceRange
srcTokenRange = tokRange . tokAnn

{- Our new way of handling kinds introduces an annoying problem:

   We need to have the kinds of tyvars bound the decl kind signature or
   type signature in scope when we convert the declaration.

-}
groupSignaturesAndDeclarations :: Show a => [Declaration a] -> [[Declaration a]]
groupSignaturesAndDeclarations [] = []
groupSignaturesAndDeclarations decls = trace ("DECLARATIONS (grouping): \n" <> concatMap ((<> "\n\n") . show) decls)
  $ go kindSigs typeSigs decls'
  where
    ((kindSigs,typeSigs),decls') = foldr (\x acc -> case x of
        ksig@(DeclKindSignature _ _ (Labeled (nameValue -> nm) _ _)) -> first (first $ M.insert nm ksig) acc
        tsig@(DeclSignature _ (Labeled (nameValue -> nm) _ _)) -> first (second (M.insert nm tsig)) acc
        other -> second (other:) acc
      ) ((M.empty,M.empty),[]) decls

    go _ _ [] = []
    go ksigs tsigs (d:ds)  = case d of
      dataDecl@(DeclData _ (DataHead _ (nameValue -> nm) _ ) _) -> case M.lookup nm ksigs of
        Just sigDecl -> [sigDecl,dataDecl] : go ksigs tsigs ds
        Nothing -> [dataDecl] : go ksigs tsigs ds
      tyDecl@(DeclType _ (DataHead _ (nameValue -> nm) _) _ _) -> case M.lookup nm ksigs of
        Just sigDecl -> [sigDecl,tyDecl] : go ksigs tsigs ds
        Nothing -> [tyDecl] : go ksigs tsigs ds
      newtypeDecl@(DeclNewtype _ (DataHead _ (nameValue -> nm) _) _ _ _) -> case M.lookup nm ksigs of
        Just sigDecl -> [sigDecl,newtypeDecl] : go ksigs tsigs ds
        Nothing -> [newtypeDecl] : go ksigs tsigs ds
      classDecl@(DeclClass _ (clsName -> nm) _) -> case M.lookup (coerceProperName $ nameValue nm) ksigs of
        Just sigDecl -> [sigDecl,classDecl] : go ksigs tsigs ds
        Nothing -> [classDecl] : go ksigs tsigs ds
      valDecl@(DeclValue _ (valName -> nm)) ->
        let (valDecls',ds') = partition (valDecWithName nm) ds
            valDecls = valDecl : valDecls'
        in case M.lookup (nameValue nm) tsigs of
             Just sigDecl -> (sigDecl:valDecls) : go ksigs tsigs ds'
             Nothing -> valDecls : go ksigs tsigs ds'
      -- I don't think anything else can have a type/kind sig but I could be wrong...
      other -> [other] : go ksigs tsigs ds
     where
       valDecWithName :: Name Ident -> Declaration a -> Bool
       valDecWithName nm (DeclValue _ (valName -> nm')) = nameValue nm == nameValue nm'
       valDecWithName _ _ = False


comment :: Comment a -> Maybe C.Comment
comment = \case
  Comment t
    | "{-" `Text.isPrefixOf` t -> Just $ C.BlockComment $ Text.drop 2 $ Text.dropEnd 2 t
    | "--" `Text.isPrefixOf` t -> Just $ C.LineComment $ Text.drop 2 t
  _ -> Nothing

comments :: [Comment a] -> [C.Comment]
comments = mapMaybe comment

sourcePos :: SourcePos -> Pos.SourcePos
sourcePos (SourcePos line col) = Pos.SourcePos line col

sourceSpan :: String -> SourceRange -> Pos.SourceSpan
sourceSpan name (SourceRange start end) = Pos.SourceSpan name (sourcePos start) (sourcePos end)

widenLeft :: TokenAnn -> Pos.SourceAnn -> Pos.SourceAnn
widenLeft ann (sp, _) =
  ( Pos.widenSourceSpan (sourceSpan (Pos.spanName sp) $ tokRange ann) sp
  , comments $ tokLeadingComments ann
  )

sourceAnnCommented :: String -> SourceToken -> SourceToken -> Pos.SourceAnn
sourceAnnCommented fileName (SourceToken ann1 _) (SourceToken ann2 _) =
  ( Pos.SourceSpan fileName (sourcePos $ srcStart $ tokRange ann1) (sourcePos $ srcEnd $ tokRange ann2)
  , comments $ tokLeadingComments ann1
  )

sourceAnn :: String -> SourceToken -> SourceToken -> Pos.SourceAnn
sourceAnn fileName (SourceToken ann1 _) (SourceToken ann2 _) =
  ( Pos.SourceSpan fileName (sourcePos $ srcStart $ tokRange ann1) (sourcePos $ srcEnd $ tokRange ann2)
  , []
  )

sourceName :: String -> Name a -> Pos.SourceAnn
sourceName fileName a = sourceAnnCommented fileName (nameTok a) (nameTok a)

sourceQualName :: String -> QualifiedName a -> Pos.SourceAnn
sourceQualName fileName a = sourceAnnCommented fileName (qualTok a) (qualTok a)

moduleName :: Token -> Maybe N.ModuleName
moduleName = \case
  TokLowerName as _ -> go as
  TokUpperName as _ -> go as
  TokSymbolName as _ -> go as
  TokOperator as _ -> go as
  _ -> Nothing
  where
  go [] = Nothing
  go ns = Just $ N.ModuleName $ Text.intercalate "." ns

qualified :: QualifiedName a -> N.Qualified a
qualified q = N.Qualified qb (qualName q)
  where
  qb = maybe N.ByNullSourcePos N.ByModuleName $ qualModule q

ident :: Ident -> N.Ident
ident = N.Ident . getIdent

convertType :: String -> Type a -> ConvertM T.SourceType
convertType = convertType' False

convertVtaType :: String -> Type a -> ConvertM T.SourceType
convertVtaType = convertType' True

convertType' :: Bool -> String -> Type a -> ConvertM T.SourceType
convertType' withinVta fileName = go
  where
  goRow :: Row a -> SourceToken -> ConvertM T.SourceType
  goRow (Row labels tl) b = do
    let
      rowTail = case tl of
        Just (_, ty) -> go ty
        Nothing -> pure $ T.REmpty $ sourceAnnCommented fileName b b
      rowCons (Labeled a _ ty) c = do
        let ann = sourceAnnCommented fileName (lblTok a) (snd $ typeRange ty)
        ty' <- go ty
        pure $ T.RCons ann (L.Label $ lblName a) ty' c
    case labels of
      Just (Separated h t) -> do
        rtail <- rowTail
        rowCons h =<< foldrM (rowCons . snd) rtail t
      Nothing ->
        rowTail
  go :: forall a. Type a -> ConvertM T.SourceType
  go = \case
    TypeKinded _ (TypeVar _ a) _ kd -> do
      kd' <- go kd
      let nm = getIdent (nameValue a)
      bindTv nm kd'
      pure $ T.TypeVar (sourceName fileName a) (getIdent $ nameValue a) kd'
    TypeVar _ a  -> do
      kd <- tvKind (nameTok a) (getIdent $ nameValue a)
      pure $ T.TypeVar (sourceName fileName a) (getIdent $ nameValue a) kd
    TypeConstructor _ a ->
      pure $ T.TypeConstructor (sourceQualName fileName a) $ qualified a
    TypeWildcard _ a ->
      pure $ T.TypeWildcard (sourceAnnCommented fileName a a) $ if withinVta then T.IgnoredWildcard else T.UnnamedWildcard
    TypeHole _ a ->
      pure $ T.TypeWildcard (sourceName fileName a) . T.HoleWildcard . getIdent $ nameValue a
    TypeString _ a b ->
      pure $ T.TypeLevelString (sourceAnnCommented fileName a a) b
    TypeInt _ _ a b ->
      pure $ T.TypeLevelInt (sourceAnnCommented fileName a a) b
    TypeRow _ (Wrapped _ row b) ->
      goRow row b
    TypeRecord _ (Wrapped a row b) -> do
      let
        ann = sourceAnnCommented fileName a b
        annRec = sourceAnn fileName a a
      T.TypeApp ann (Env.tyRecord $> annRec) <$> goRow row b
    TypeForall _ kw bindings _ ty -> do
      -- TODO: Refactor this (if it works)
      let
        doBind (TypeVarKinded (Wrapped _ (Labeled (v, a) _ b) _)) = do
          let nm = getIdent (nameValue a)
          b' <- go b
          bindTv nm b'
        doBind (TypeVarName (v,a)) = internalError $  "Error: Universally quantified type variable without kind annotation: " <> (Text.unpack . getIdent . nameValue $ a) <> "\nat: " <> show v

        mkForAll a b v t = do
          let ann' = widenLeft (tokAnn $ nameTok a) $ T.getAnnForType t
          T.ForAll ann' (maybe T.TypeVarInvisible (const T.TypeVarVisible) v) (getIdent $ nameValue a) b t Nothing

        k (TypeVarKinded (Wrapped _ (Labeled (v, a) _ b) _)) t = do
          let nm = getIdent (nameValue a)
          b' <- go b
          bindTv nm b'
          pure $ mkForAll a b' v t
        -- TODO: Fix this better
        k (TypeVarName (v, a)) t = internalError $  "Error: Universally quantified type variable without kind annotation: " <> (Text.unpack . getIdent . nameValue $ a) <> "\nat: " <> show v
      traverse_ doBind bindings
      inner <- go ty
      ty' <- foldrM k inner bindings
      let ann = widenLeft (tokAnn kw) $ T.getAnnForType ty'
      pure $ T.setAnnForType ann ty'
    TypeKinded _ ty _ kd -> do
      ty' <- go ty
      kd' <- go kd
      let ann = Pos.widenSourceAnn (T.getAnnForType ty') (T.getAnnForType kd')
      pure $ T.KindedType ann ty' kd'
    TypeApp _ a b -> do
      a' <- go a
      b' <- go b
      let ann = Pos.widenSourceAnn (T.getAnnForType a') (T.getAnnForType b')
      pure $ T.TypeApp ann a' b'
    ty@(TypeOp {}) -> do
      let
        reassoc op b' a = do
          a' <- go a
          let
            op' = T.TypeOp (sourceQualName fileName op) $ qualified op
            ann = Pos.widenSourceAnn (T.getAnnForType a') (T.getAnnForType b')
          pure $ T.BinaryNoParensType ann op' a' b'
        loop :: (Type a -> ConvertM T.SourceType) -> Type a -> ConvertM T.SourceType
        loop k = \case
          TypeOp _ a op b -> do
            b' <- k b
            loop (reassoc op b') a
          expr' -> k expr'
      loop go ty
    TypeOpName _ op -> do
      let rng = qualRange op
      pure $ T.TypeOp (uncurry (sourceAnnCommented fileName) rng) (qualified op)
    TypeArr _ a arr b -> do
      a' <- go a
      b' <- go b
      let arr' = Env.tyFunction $> sourceAnnCommented fileName arr arr
          ann = Pos.widenSourceAnn (T.getAnnForType a') (T.getAnnForType b')
      pure $ T.TypeApp ann (T.TypeApp ann arr' a') b'
    TypeArrName _ a ->
      pure $ Env.tyFunction $> sourceAnnCommented fileName a a
    TypeConstrained _ a _ b -> do
      a' <- convertConstraint withinVta fileName a
      b' <- go b
      let ann = Pos.widenSourceAnn (T.constraintAnn a') (T.getAnnForType b')
      pure $ T.ConstrainedType ann a' b'
    TypeParens _ (Wrapped a ty b) -> do
      ty' <- go ty
      pure $ T.ParensInType (sourceAnnCommented fileName a b) ty'
    ty@(TypeUnaryRow _ _ a) -> do
      a' <- go a
      let rng = typeRange ty
          ann = uncurry (sourceAnnCommented fileName) rng
      pure $ T.setAnnForType ann $ Env.kindRow a'

convertConstraint :: Bool -> String -> Constraint a -> ConvertM T.SourceConstraint
convertConstraint withinVta fileName = go
  where
  go = \case
    cst@(Constraint _ name args) -> do
      let ann = uncurry (sourceAnnCommented fileName) $ constraintRange cst
      args' <- traverse (convertType' withinVta fileName) args
      pure $ T.Constraint ann (qualified name) [] args' Nothing
    ConstraintParens _ (Wrapped _ c _) -> go c

convertGuarded :: String -> Guarded a -> ConvertM [AST.GuardedExpr]
convertGuarded fileName = \case
  Unconditional _ x -> do
    where' <- convertWhere fileName x
    pure [AST.GuardedExpr [] where']
  Guarded gs -> traverse uh $ NE.toList gs
  where
  uh (GuardedExpr _ ps _ x) = do
    ps' <- traverse p (toList ps)
    where' <- convertWhere fileName x
    pure $ AST.GuardedExpr ps' where'
  go = convertExpr fileName
  p (PatternGuard Nothing x) = AST.ConditionGuard <$> go x
  p (PatternGuard (Just (b, _)) x) = AST.PatternGuard <$> convertBinder fileName b <*> go x

convertWhere :: String -> Where a -> ConvertM AST.Expr
convertWhere fileName = \case
  Where expr Nothing -> convertExpr fileName expr
  Where expr (Just (_, bs)) -> do
    let ann = uncurry (sourceAnnCommented fileName) $ exprRange expr
    letExp <- AST.Let AST.FromWhere <$> traverse (convertLetBinding fileName) (NE.toList bs)
    uncurry AST.PositionedValue ann . letExp <$> convertExpr fileName expr

convertLetBinding :: String -> LetBinding a -> ConvertM AST.Declaration
convertLetBinding fileName = \case
  LetBindingSignature _ lbl ->
    convertSignature fileName lbl
  binding@(LetBindingName _ fields) -> do
    let ann = uncurry (sourceAnnCommented fileName) $ letBindingRange binding
    convertValueBindingFields fileName ann fields
  binding@(LetBindingPattern _ a _ b) -> do
    let ann = uncurry (sourceAnnCommented fileName) $ letBindingRange binding
    binder' <- convertBinder fileName a
    where'  <- convertWhere fileName b
    pure $ AST.BoundValueDeclaration ann binder' where'

convertExpr :: forall a. String -> Expr a -> ConvertM AST.Expr
convertExpr fileName = go
  where
  positioned =
    uncurry AST.PositionedValue

  goDoStatement = \case
    stmt@(DoLet _ as) -> do
      let ann = uncurry (sourceAnnCommented fileName) $ doStatementRange stmt
      bindings <- traverse (convertLetBinding fileName) (NE.toList as)
      pure $ uncurry AST.PositionedDoNotationElement ann . AST.DoNotationLet $ bindings
    stmt@(DoDiscard a) -> do
      let ann = uncurry (sourceAnn fileName) $ doStatementRange stmt
      uncurry AST.PositionedDoNotationElement ann . AST.DoNotationValue <$> go a
    stmt@(DoBind a _ b) -> do
      let ann = uncurry (sourceAnn fileName) $ doStatementRange stmt
      a' <- convertBinder fileName a
      b' <- go b
      pure $ uncurry AST.PositionedDoNotationElement ann $ AST.DoNotationBind a' b'

  go :: Expr a -> ConvertM AST.Expr
  go = \case
    ExprHole _ a ->
      pure $ positioned (sourceName fileName a) . AST.Hole . getIdent $ nameValue a
    ExprSection _ a ->
      pure $ positioned (sourceAnnCommented fileName a a) AST.AnonymousArgument
    ExprIdent _ a -> do
      let ann = sourceQualName fileName a
      pure $ positioned ann . AST.Var (fst ann) . qualified $ fmap ident a
    ExprConstructor _ a -> do
      let ann = sourceQualName fileName a
      pure $ positioned ann . AST.Constructor (fst ann) $ qualified a
    ExprBoolean _ a b -> do
      let ann = sourceAnnCommented fileName a a
      pure $ positioned ann . AST.Literal (fst ann) $ AST.BooleanLiteral b
    ExprChar _ a b -> do
      let ann = sourceAnnCommented fileName a a
      pure $ positioned ann . AST.Literal (fst ann) $ AST.CharLiteral b
    ExprString _ a b -> do
      let ann = sourceAnnCommented fileName a a
      pure $ positioned ann . AST.Literal (fst ann) . AST.StringLiteral $ b
    ExprNumber _ a b -> do
      let ann = sourceAnnCommented fileName a a
      pure $ positioned ann . AST.Literal (fst ann) $ AST.NumericLiteral b
    ExprArray _ (Wrapped a bs c) -> do
      let
        ann = sourceAnnCommented fileName a c
        vals = case bs of
          Just (Separated x xs) -> do
            xs' <- traverse (go . snd) xs
            x' <- go x
            pure $ x' : xs'
          Nothing -> pure []
      positioned ann . AST.Literal (fst ann) . AST.ArrayLiteral <$> vals
    ExprRecord z (Wrapped a bs c) -> do
      let
        ann = sourceAnnCommented fileName a c
        lbl = \case
          RecordPun f -> do
            exp' <- go . ExprIdent z $ QualifiedName (nameTok f) Nothing (nameValue f)
            pure (mkString . getIdent $ nameValue f, exp')
          RecordField f _ v -> (lblName f,) <$>  go v
        vals = case bs of
          Just (Separated x xs) -> do
            lx <- lbl x
            lxs <- traverse (lbl . snd) xs
            pure $ lx : lxs
          Nothing -> pure []
      positioned ann . AST.Literal (fst ann) . AST.ObjectLiteral <$> vals
    ExprParens _ (Wrapped a b c) ->
      positioned (sourceAnnCommented fileName a c) . AST.Parens <$> go b
    expr@(ExprTyped _ a _ b) -> do
      a' <- go a
      b' <- convertType fileName b
      let ann = (sourceSpan fileName . toSourceRange $ exprRange expr, [])
      pure $ positioned ann $ AST.TypedValue True a' b'
    expr@(ExprInfix _ a (Wrapped _ b _) c) -> do
      let ann = (sourceSpan fileName . toSourceRange $ exprRange expr, [])
      a' <- go a
      b' <- go b
      c' <- go c
      pure $ positioned ann $ AST.BinaryNoParens b' a' c'
    expr@(ExprOp {}) -> do
      let
        ann = uncurry (sourceAnn fileName) $ exprRange expr
        reassoc op b a = do
          a' <- go a
          let op' = AST.Op (sourceSpan fileName . toSourceRange $ qualRange op) $ qualified op
          pure $ AST.BinaryNoParens op' a'  b
        loop k = \case
          ExprOp _ a op b -> do
            b' <- k b
            loop (reassoc op b') a
          expr' -> k expr'
      positioned ann <$> loop go expr
    ExprOpName _ op -> do
      let
        rng = qualRange op
        op' = AST.Op (sourceSpan fileName $ toSourceRange rng) $ qualified op
      pure $ positioned (uncurry (sourceAnnCommented fileName) rng) op'
    expr@(ExprNegate _ _ b) -> do
      let ann = uncurry (sourceAnnCommented fileName) $ exprRange expr
      positioned ann . AST.UnaryMinus (fst ann) <$> go b
    expr@(ExprRecordAccessor _ (RecordAccessor a _ (Separated h t))) -> do
      let
        ann = uncurry (sourceAnnCommented fileName) $ exprRange expr
        field x f = AST.Accessor (lblName f) x
      a' <- go a
      pure $ positioned ann $ foldl (\x (_, f) -> field x f) (field a' h) t
    expr@(ExprRecordUpdate _ a b) -> do
      let
        ann = uncurry (sourceAnnCommented fileName) $ exprRange expr
        k (RecordUpdateLeaf f _ x) = go x >>= \x' -> pure (lblName f, AST.Leaf  x')
        k (RecordUpdateBranch f xs) = toTree xs >>= \xs' -> pure (lblName f, AST.Branch xs')
        toTree (Wrapped _ xs _) = do
          xs' <- traverse k $ toList xs
          pure $ AST.PathTree . AST.AssocList $ xs'
      a' <- go a
      positioned ann . AST.ObjectUpdateNested a' <$> toTree b
    expr@(ExprApp _ a b) -> do
      let ann = uncurry (sourceAnn fileName) $ exprRange expr
      a' <- go a
      b' <- go b
      pure $ positioned ann $ AST.App a' b'
    expr@(ExprVisibleTypeApp _ a _ b) -> do
      let ann = uncurry (sourceAnn fileName) $ exprRange expr
      a' <- go a
      b' <- convertVtaType fileName b
      pure $ positioned ann $ AST.VisibleTypeApp a' b'
    expr@(ExprLambda _ (Lambda _ as _ b)) -> do
      let ann = uncurry (sourceAnnCommented fileName) $ exprRange expr
      a' <- convertBinder fileName (NE.head as)
      b' <- go b
      let goAbs = foldrM (\x acc -> do
                                  x' <- convertBinder fileName x
                                  pure $ AST.Abs x' acc)
      inner <- goAbs b' (NE.tail as)
      pure $ positioned ann
        . AST.Abs a'
        $ inner
    expr@(ExprIf _ (IfThenElse _ a _ b _ c)) -> do
      let ann = uncurry (sourceAnnCommented fileName) $ exprRange expr
      a' <- go a
      b' <- go b
      c' <- go c
      pure $ positioned ann $ AST.IfThenElse a' b' c'
    expr@(ExprCase _ (CaseOf _ as _ bs)) -> do
      let ann = uncurry (sourceAnnCommented fileName) $ exprRange expr
      as' <- traverse go $ toList as
      let bss = NE.toList bs
      bs' <- traverse (bitraverse (traverse (convertBinder fileName) . toList) (convertGuarded fileName)) bss
      let bss' = uncurry AST.CaseAlternative <$> bs'
      pure $ positioned ann $ AST.Case as' bss'

    expr@(ExprLet _ (LetIn _ as _ b)) -> do
      let ann = uncurry (sourceAnnCommented fileName) $ exprRange expr
      as' <- traverse (convertLetBinding fileName) $ NE.toList as
      b' <- go b
      pure $ positioned ann $ AST.Let AST.FromLet as' b'
    -- expr@(ExprWhere _ (Where a _ bs)) -> do
    --   let ann = uncurry (sourceAnnCommented fileName) $ exprRange expr
    --   positioned ann . AST.Let AST.FromWhere (goLetBinding <$> bs) $ go a
    expr@(ExprDo _ (DoBlock kw stmts)) -> do
      let ann = uncurry (sourceAnnCommented fileName) $ exprRange expr
      stmts' <- traverse goDoStatement (NE.toList stmts)
      pure $ positioned ann . AST.Do (moduleName $ tokValue kw) $ stmts'
    expr@(ExprAdo _ (AdoBlock kw stms _ a)) -> do
      let ann = uncurry (sourceAnnCommented fileName) $ exprRange expr
      stmts <- traverse goDoStatement stms
      a' <- go a
      pure $ positioned ann $ AST.Ado (moduleName $ tokValue kw) stmts a'

convertBinder :: String -> Binder a -> ConvertM AST.Binder
convertBinder fileName = go
  where
  positioned =
    uncurry AST.PositionedBinder

  go :: Binder a -> ConvertM AST.Binder
  go = \case
    BinderWildcard _ a ->
      pure $ positioned (sourceAnnCommented fileName a a) AST.NullBinder
    BinderVar _ a -> do
      let ann = sourceName fileName a
      pure $ positioned ann . AST.VarBinder (fst ann) . ident $ nameValue a
    binder@(BinderNamed _ a _ b) -> do
      let ann = uncurry (sourceAnnCommented fileName) $ binderRange binder
      positioned ann . AST.NamedBinder (fst ann) (ident $ nameValue a) <$> go b
    binder@(BinderConstructor _ a bs) -> do
      let ann = uncurry (sourceAnnCommented fileName) $ binderRange binder
      positioned ann . AST.ConstructorBinder (fst ann) (qualified a) <$> traverse go bs
    BinderBoolean _ a b -> do
      let ann = sourceAnnCommented fileName a a
      pure $ positioned ann . AST.LiteralBinder (fst ann) $ AST.BooleanLiteral b
    BinderChar _ a b -> do
      let ann = sourceAnnCommented fileName a a
      pure $ positioned ann . AST.LiteralBinder (fst ann) $ AST.CharLiteral b
    BinderString _ a b -> do
      let ann = sourceAnnCommented fileName a a
      pure $ positioned ann . AST.LiteralBinder (fst ann) . AST.StringLiteral $ b
    BinderNumber _ n a b -> do
      let
        ann = sourceAnnCommented fileName a a
        b'
          | isJust n = bimap negate negate b
          | otherwise = b
      pure $ positioned ann . AST.LiteralBinder (fst ann) $ AST.NumericLiteral b'
    BinderArray _ (Wrapped a bs c) -> do
      let
        ann = sourceAnnCommented fileName a c
        vals = case bs of
          Just (Separated x xs) -> do
            x' <- go x
            xs' <- traverse (go . snd) xs
            pure $ x' : xs'
          Nothing -> pure []
      positioned ann . AST.LiteralBinder (fst ann) . AST.ArrayLiteral <$> vals
    BinderRecord z (Wrapped a bs c) -> do
      let
        ann = sourceAnnCommented fileName a c
        lbl = \case
          RecordPun f -> (mkString . getIdent $ nameValue f,) <$> go (BinderVar z f)
          RecordField f _ v -> (lblName f,) <$>  go v
        vals = case bs of
          Just (Separated x xs) -> do
            x' <- lbl x
            xs' <- traverse (lbl . snd) xs
            pure $ x' : xs'
          Nothing -> pure []
      positioned ann . AST.LiteralBinder (fst ann) . AST.ObjectLiteral <$> vals
    BinderParens _ (Wrapped a b c) ->
      positioned (sourceAnnCommented fileName a c) . AST.ParensInBinder <$> go b
    binder@(BinderTyped _ a _ b) -> do
      a' <- go a
      b' <- convertType fileName b
      let ann = (sourceSpan fileName . toSourceRange $ binderRange binder, [])
      pure $ positioned ann $ AST.TypedBinder b' a'
    binder@(BinderOp {}) -> do
      let
        ann = uncurry (sourceAnn fileName) $ binderRange binder
        reassoc op b a = do
          let op' = AST.OpBinder (sourceSpan fileName . toSourceRange $ qualRange op) $ qualified op
          a' <- go a
          pure $ AST.BinaryNoParensBinder op' a' b
        loop k = \case
          BinderOp _ a op b -> do
            b' <- k b
            loop (reassoc op b') a
          binder' -> k binder'
      positioned ann <$> loop go binder

convertDeclaration :: String -> Declaration a -> ConvertM [AST.Declaration]
convertDeclaration fileName decl = case decl of
  DeclData _ (DataHead _ a vars) bd -> do
    vars' <- traverse goTypeVar vars
    let
      ctrs :: SourceToken -> DataCtor a -> [(SourceToken, DataCtor a)] -> ConvertM [AST.DataConstructorDeclaration]
      ctrs st (DataCtor _ name fields) tl = do
        fields' <- traverse (convertType fileName) fields
        case tl of
          [] ->
            pure [AST.DataConstructorDeclaration (sourceAnnCommented fileName st (nameTok name)) (nameValue name) (zip ctrFields fields')]
          (st',ctor) : tl' -> do
            rest <- ctrs st' ctor tl'
            pure $ AST.DataConstructorDeclaration (sourceAnnCommented fileName st (nameTok name)) (nameValue name) (zip ctrFields fields')
                   : rest

    ctorDecls <- maybe (pure []) (\(st, Separated hd tl) -> ctrs st hd tl) bd
    pure [AST.DataDeclaration ann Env.Data (nameValue a) vars' ctorDecls]
  DeclType _ (DataHead _ a vars) _ bd -> do
    vars' <- traverse goTypeVar vars
    bd'   <- convertType fileName bd
    pure . pure $ AST.TypeSynonymDeclaration ann
      (nameValue a)
      vars'
      bd'
  DeclNewtype _ (DataHead _ a vars) st x ys -> do
    vars' <- traverse goTypeVar vars
    ys' <- convertType fileName ys
    let ctrs = [AST.DataConstructorDeclaration (sourceAnnCommented fileName st (snd $ declRange decl)) (nameValue x) [(head ctrFields, ys')]]
    pure [AST.DataDeclaration ann Env.Newtype (nameValue a) vars' ctrs]
  DeclClass _ (ClassHead _ sup name vars fdeps) bd -> do
    let
      goTyVar (TypeVarKinded (Wrapped _ (Labeled (_, a) _ _) _)) = nameValue a
      goTyVar (TypeVarName (_, a)) = nameValue a
      vars' = zip (toList $ goTyVar <$> vars) [0..]
      goName = fromJust . flip lookup vars' . nameValue
      goFundep (FundepDetermined _ bs) = Env.FunctionalDependency [] (goName <$> NE.toList bs)
      goFundep (FundepDetermines as _ bs) = Env.FunctionalDependency (goName <$> NE.toList as) (goName <$> NE.toList bs)
      goSig (Labeled n _ ty) = do
        ty' <- convertType fileName ty
        let ann' = widenLeft (tokAnn $ nameTok n) $ T.getAnnForType ty'
        pure [AST.TypeDeclaration $ AST.TypeDeclarationData ann' (ident $ nameValue n) ty']
    argVars <- traverse goTypeVar vars
    cstrnt <- traverse (convertConstraint False fileName) $ maybe [] (toList . fst) sup
    sig' <- traverse goSig $ maybe [] (NE.toList . snd) bd
    pure . pure $ AST.TypeClassDeclaration ann
      (nameValue name)
      argVars
      cstrnt
      (goFundep <$> maybe [] (toList . snd) fdeps)
      (concat sig')
  DeclInstanceChain _ insts -> do
    let
      chainId = mkChainId fileName $ startSourcePos $ instKeyword $ instHead $ sepHead insts
      goInst ix inst@(Instance (InstanceHead _ _todo nameSep ctrs cls args) bd) = do
        let ann' = uncurry (sourceAnnCommented fileName) $ instanceRange inst
            clsAnn = findInstanceAnn cls args
        cstrnt <- traverse (convertConstraint False fileName) $  maybe [] (toList . fst) ctrs
        args'  <- traverse (convertType fileName) args
        instBinding <- traverse goInstanceBinding $  maybe [] (NE.toList . snd) bd
        pure $ AST.TypeInstanceDeclaration ann' clsAnn chainId ix
          (mkPartialInstanceName nameSep cls args)
          cstrnt
          (qualified cls)
          args'
          (AST.ExplicitInstance instBinding)
    traverse (uncurry goInst) $ zip [0..] (toList insts)
  DeclDerive _ _ new (InstanceHead kw _todo nameSep ctrs cls args) -> do
    let
      chainId = mkChainId fileName $ startSourcePos kw
      name' = mkPartialInstanceName nameSep cls args
      instTy
        | isJust new = AST.NewtypeInstance
        | otherwise = AST.DerivedInstance
      clsAnn = findInstanceAnn cls args
    cstrnt <- traverse (convertConstraint False fileName) $ maybe [] (toList . fst) ctrs
    args' <- traverse (convertType fileName) args
    pure [AST.TypeInstanceDeclaration ann clsAnn chainId 0 name'
      cstrnt
      (qualified cls)
      args'
      instTy]
  DeclKindSignature _ kw (Labeled name _ ty) -> do
    let nm =  runProperName (nameValue name)
    ty' <- convertType fileName ty
    bindTv nm ty'
    let
      kindFor = case tokValue kw of
        TokLowerName [] "data" -> AST.DataSig
        TokLowerName [] "newtype" -> AST.NewtypeSig
        TokLowerName [] "type" -> AST.TypeSynonymSig
        TokLowerName [] "class" -> AST.ClassSig
        tok -> internalError $ "Invalid kind signature keyword " <> Text.unpack (printToken tok)
    pure [AST.KindDeclaration ann kindFor (nameValue name) ty']
  DeclSignature _ lbl ->
    pure <$> convertSignature fileName lbl
  DeclValue _ fields ->
    pure <$> convertValueBindingFields fileName ann fields
  DeclFixity _ (FixityFields (_, kw) (_, prec) fxop) -> do
    let
      assoc =  case kw of
        Infix  -> AST.Infix
        Infixr -> AST.Infixr
        Infixl -> AST.Infixl
      fixity = AST.Fixity assoc prec
    pure . pure $ AST.FixityDeclaration ann $ case fxop of
      FixityValue name _ op -> Left $ AST.ValueFixity fixity (first ident <$> qualified name) (nameValue op)
      FixityType _ name _ op ->
        Right $ AST.TypeFixity fixity (qualified name) (nameValue op)
  DeclForeign _ _ _ frn ->
    pure <$> case frn of
      ForeignValue (Labeled a _ b) ->
        AST.ExternDeclaration ann (ident $ nameValue a) <$> convertType fileName b
      ForeignData _ (Labeled a _ b) ->
        AST.ExternDataDeclaration ann (nameValue a) <$> convertType fileName b
      ForeignKind _ a ->
        pure $ AST.DataDeclaration ann Env.Data (nameValue a) [] []
  DeclRole _ _ _ name roles ->
    pure . pure $ AST.RoleDeclaration $
      AST.RoleDeclarationData ann (nameValue name) (roleValue <$> NE.toList roles)
  where
  ann =
    uncurry (sourceAnnCommented fileName) $ declRange decl

  startSourcePos :: SourceToken -> Pos.SourcePos
  startSourcePos = sourcePos . srcStart . tokRange . tokAnn

  mkPartialInstanceName :: Maybe (Name Ident, SourceToken) -> QualifiedName (N.ProperName 'N.ClassName) -> [Type a] -> Either Text.Text N.Ident
  mkPartialInstanceName nameSep cls args =
    maybe (Left genName) (Right . ident . nameValue . fst) nameSep
    where
      -- truncate to 25 chars to reduce verbosity
      -- of name and still keep it readable
      -- name will be used to create a GenIdent
      -- in desugaring process
      genName :: Text.Text
      genName = Text.take 25 (className <> typeArgs)

      className :: Text.Text
      className
        = foldMap (uncurry Text.cons . first toLower)
        . Text.uncons
        . N.runProperName
        $ qualName cls

      typeArgs :: Text.Text
      typeArgs = foldMap argName args

      argName :: Type a -> Text.Text
      argName = \case
        -- These are only useful to disambiguate between overlapping instances
        -- but they’re disallowed outside of instance chains. Since we’re
        -- avoiding name collisions with unique identifiers anyway,
        -- we don't need to render this constructor.
        TypeVar{} -> ""
        TypeConstructor _ qn -> N.runProperName $ qualName qn
        TypeOpName _ qn -> N.runOpName $ qualName qn
        TypeString _ _ ps -> prettyPrintStringJS ps
        TypeInt _ _ _ nt -> Text.pack $ show nt

        -- Typed holes are disallowed in instance heads
        TypeHole{} -> ""
        TypeParens _ t -> argName $ wrpValue t
        TypeKinded _ t1 _ t2 -> argName t1 <> argName t2
        TypeRecord _ _ -> "Record"
        TypeRow _ _ -> "Row"
        TypeArrName _ _ -> "Function"
        TypeWildcard{} -> "_"

        -- Polytypes are disallowed in instance heads
        TypeForall{} -> ""
        TypeApp _ t1 t2 -> argName t1 <> argName t2
        TypeOp _ t1 op t2 ->
          argName t1 <> N.runOpName (qualName op) <> argName t2
        TypeArr _ t1 _ t2 -> argName t1 <> "Function" <> argName t2
        TypeConstrained{} -> ""
        TypeUnaryRow{} -> "Row"

  goTypeVar = \case
    TypeVarKinded (Wrapped _ (Labeled (_, x) _ y) _) -> do
      let nm = getIdent (nameValue x)
      k <- convertType fileName y
      bindTv nm k
      pure (nm,k)
    TypeVarName (_, x) -> do
      let nm = getIdent (nameValue x)
      ki <- tvKind (nameTok x) nm
      pure (nm,ki)

  goInstanceBinding = \case
    InstanceBindingSignature _ lbl ->
      convertSignature fileName lbl
    binding@(InstanceBindingName _ fields) -> do
      let ann' = uncurry (sourceAnnCommented fileName) $ instanceBindingRange binding
      convertValueBindingFields fileName ann' fields

  findInstanceAnn cls args = uncurry (sourceAnnCommented fileName) $
    if null args then
      qualRange cls
    else
      (fst $ qualRange cls, snd $ typeRange $ last args)

convertSignature :: String -> Labeled (Name Ident) (Type a) -> ConvertM AST.Declaration
convertSignature fileName (Labeled a _ b) = do
  b' <- convertType fileName b
  let ann = widenLeft (tokAnn $ nameTok a) $ T.getAnnForType b'
  pure $ AST.TypeDeclaration $ AST.TypeDeclarationData ann (ident $ nameValue a) b'

convertValueBindingFields :: String -> Pos.SourceAnn -> ValueBindingFields a -> ConvertM AST.Declaration
convertValueBindingFields fileName ann (ValueBindingFields a bs c) = do
  bs' <- traverse (convertBinder fileName) bs
  cs' <- convertGuarded fileName c
  pure $ AST.ValueDeclaration $ AST.ValueDeclarationData ann (ident $ nameValue a) Env.Public bs' cs'

convertImportDecl
  :: String
  -> ImportDecl a
  -> ConvertM (Pos.SourceAnn, N.ModuleName, AST.ImportDeclarationType, Maybe N.ModuleName)
convertImportDecl fileName decl@(ImportDecl _ _ modName mbNames mbQual) = do
  let
    ann = uncurry (sourceAnnCommented fileName) $ importDeclRange decl
  importTy <- case mbNames of
      Nothing -> pure AST.Implicit
      Just (hiding, Wrapped _ imps _) -> do
        imps' <- traverse (convertImport fileName) $ toList imps
        if isJust hiding
          then pure $ AST.Hiding imps'
          else pure $ AST.Explicit imps'
  pure (ann, nameValue modName, importTy, nameValue . snd <$> mbQual)

convertImport :: String -> Import a -> ConvertM AST.DeclarationRef
convertImport fileName imp = case imp of
  ImportValue _ a ->
    pure $ AST.ValueRef ann . ident $ nameValue a
  ImportOp _ a ->
    pure $ AST.ValueOpRef ann $ nameValue a
  ImportType _ a mb -> do
    let
      ctrs = case mb of
        Nothing -> Just []
        Just (DataAll _ _) -> Nothing
        Just (DataEnumerated _ (Wrapped _ Nothing _)) -> Just []
        Just (DataEnumerated _ (Wrapped _ (Just idents) _)) ->
          Just . map nameValue $ toList idents
    pure $ AST.TypeRef ann (nameValue a) ctrs
  ImportTypeOp _ _ a ->
    pure $ AST.TypeOpRef ann $ nameValue a
  ImportClass _ _ a ->
    pure $ AST.TypeClassRef ann $ nameValue a
  where
  ann = sourceSpan fileName . toSourceRange $ importRange imp

convertExport :: String -> Export a -> ConvertM AST.DeclarationRef
convertExport fileName export = case export of
  ExportValue _ a ->
    pure $ AST.ValueRef ann . ident $ nameValue a
  ExportOp _ a ->
    pure $ AST.ValueOpRef ann $ nameValue a
  ExportType _ a mb -> do
    let
      ctrs = case mb of
        Nothing -> Just []
        Just (DataAll _ _) -> Nothing
        Just (DataEnumerated _ (Wrapped _ Nothing _)) -> Just []
        Just (DataEnumerated _ (Wrapped _ (Just idents) _)) ->
          Just . map nameValue $ toList idents
    pure $ AST.TypeRef ann (nameValue a) ctrs
  ExportTypeOp _ _ a ->
    pure $ AST.TypeOpRef ann $ nameValue a
  ExportClass _ _ a ->
    pure $ AST.TypeClassRef ann $ nameValue a
  ExportModule _ _ a ->
    pure $ AST.ModuleRef ann (nameValue a)
  where
  ann = sourceSpan fileName . toSourceRange $ exportRange export

convertModule :: Show a => String -> Module a ->  AST.Module
convertModule fileName module'@(Module _ _ modName exps _ imps decls _) = do
  let
    groupedDecls = groupSignaturesAndDeclarations decls
    ann = uncurry (sourceAnnCommented fileName) $ moduleRange module'
    imps' = importCtr . runConvert . convertImportDecl fileName <$> imps
    decls' = concatMap (concat . runConvert . traverse (convertDeclaration fileName)) groupedDecls
    exps' = map (runConvert . convertExport fileName) . toList . wrpValue <$> exps
  uncurry AST.Module ann (nameValue modName) (imps' <> decls') exps'
  where
  importCtr (a, b, c, d) = AST.ImportDeclaration a b c d

ctrFields :: [N.Ident]
ctrFields = [N.Ident ("value" <> Text.pack (show (n :: Integer))) | n <- [0..]]

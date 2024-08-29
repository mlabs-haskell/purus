{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Language.Purus.Pipeline.DesugarObjects (desugarObjects, desugarObjectsInDatatypes) where

import Data.Bifunctor (Bifunctor (second))
import Data.List (elemIndex, foldl', sortOn)

import Data.Map qualified as M

import Data.Maybe (fromJust)

import Data.Text qualified as T

import Control.Monad (foldM)

import Language.PureScript.Constants.Prim qualified as C
import Language.PureScript.CoreFn.Desugar.Utils (properToIdent)
import Language.PureScript.CoreFn.Expr (
  PurusType,
 )
import Language.PureScript.CoreFn.FromJSON ()
import Language.PureScript.CoreFn.Module (
  Datatypes,
  bitraverseDatatypes,
 )
import Language.PureScript.CoreFn.TypeLike (TypeLike (..))
import Language.PureScript.Environment (kindType, mkTupleTyName, pattern RecordT, pattern (:->))
import Language.PureScript.Names (Ident (..), coerceProperName)
import Language.PureScript.PSString (PSString)
import Language.PureScript.Types (
  RowListItem (rowListType),
  SourceType,
  Type (..),
  eqType,
  rowToList,
  srcTypeApp,
  srcTypeConstructor,
 )
import Language.Purus.Debug (doTraceM)
import Language.Purus.IR (
  Alt (..),
  BVar (..),
  BindE (..),
  Exp (..),
  FVar (..),
  Kind (..),
  Lit (..),
  Pat (..),
  Ty (..),
  expTy,
  ppExp,
  pattern (:~>),
 )
import Language.Purus.IR.Utils (
  Vars,
  WithObjects,
  WithoutObjects,
  fromExp,
  toExp,
 )
import Language.Purus.Pipeline.Monad (
  Counter,
  MonadCounter (next),
 )
import Language.Purus.Pretty.Common (prettyStr)
import Language.Purus.Pretty.Types (prettyTypeStr)
import Language.Purus.Utils (mkFieldMap)
import Prelude

import Bound (Var (..))
import Bound.Scope (toScope)

import Control.Lens (ix, (&), (.~))

import Control.Monad.Except (liftEither, throwError)

tryConvertType :: SourceType -> Counter Ty
tryConvertType = go id
  where
    go :: (SourceType -> SourceType) -> SourceType -> Counter Ty
    go f t = case t of
      RecordT fs ->
        if isClosedRow fs
          then do
            let fields = rowListType <$> mkFieldMap fs
                arity = M.size fields
                fakeTName = mkTupleTyName arity
                types = M.elems fields
                ctorType = foldl' srcTypeApp (srcTypeConstructor fakeTName) types
            go f ctorType
          else throwError $ prettyTypeStr fs <> " is not a closed row. Last: " <> prettyTypeStr (rowLast fs)
      TypeVar _ txt k -> TyVar txt <$> tryConvertKind' f t k
      TypeConstructor _ tn -> pure $ TyCon tn
      TypeApp ann t1 t2 -> do
        t2' <- go (f . TypeApp ann t1) t2
        t1' <- go (f . (\x -> TypeApp ann x t2)) t1
        pure $ TyApp t1' t2'
      KindApp ann t1 t2 -> do
        t2' <- go (f . KindApp ann t1) t2
        t1' <- go (f . (\x -> KindApp ann x t2)) t1
        pure $ KApp t1' t2'
      ForAll ann vis var mbk inner skol -> do
        let khole = f . (\x -> ForAll ann vis var x inner skol)
            ihole = f . (\x -> ForAll ann vis var mbk x skol)
        k <- tryConvertKind' khole t mbk
        inner' <- go ihole inner
        pure $ Forall vis var k inner' skol
      KindedType ann t1 t2 -> do
        t2' <- go (f . KindedType ann t1) t2
        t1' <- go (f . (\x -> KindedType ann x t2)) t1
        pure $ KType t1' t2'
      other -> throwError $ "Unsupported type:\n  " <> show other

tryConvertKind' :: (SourceType -> SourceType) -> SourceType -> SourceType -> Counter Kind
tryConvertKind' f t = \case
  TypeConstructor _ C.Type -> pure KindType
  k1 :-> k2 -> do
    k1' <- tryConvertKind' f t k1
    k2' <- tryConvertKind' f t k2
    pure $ KindArrow k1' k2'
  other ->
    throwError $
      "Couldn't convert type: "
        <> prettyTypeStr other
        <> " to a Plutus Kind.\n"
        <> "Plutus only supports kinds * and (* -> Plutus Kind)"

isClosedRow :: SourceType -> Bool
isClosedRow t = case rowToList t of
  (_, REmpty {}) -> True
  (_, KindApp _ REmpty {} k) | eqType k kindType -> True
  _ -> False

rowLast :: SourceType -> SourceType
rowLast t = case rowToList t of
  (_, r) -> r

desugarObjects ::
  Exp WithObjects SourceType (Vars SourceType) ->
  Counter (Exp WithoutObjects Ty (Vars Ty))
desugarObjects __expr = do
  result <- go __expr
  let msg =
        "INPUT:\n"
          <> ppExp __expr
          <> "\n\nINPUT TY:\n"
          <> prettyStr (expTy id __expr)
          <> "\n\nRESULT:\n"
          <> ppExp result
          <> "\n\nRESULT TY:\n"
          <> prettyStr (expTy id result)
  doTraceM "desugarObjects" msg
  pure result
  where
    go ::
      Exp WithObjects SourceType (Vars SourceType) ->
      Counter (Exp WithoutObjects Ty (Vars Ty))
    go expression = case expression of
      LitE ty lit -> do
        ty' <- goType ty
        tryConvertLit lit >>= \case
          Left desObj -> pure desObj
          Right lit' -> pure $ LitE ty' lit'
      LamE bv e -> do
        bv' <- updateBV bv
        let unscoped = toExp e
        ex <- fromExp <$> desugarObjects unscoped
        pure $ LamE bv' ex
      AppE e1 e2 -> do
        e2' <- desugarObjects e2
        e1' <- desugarObjects e1
        pure $ AppE e1' e2'
      CaseE ty scrutinee alts -> do
        ty' <- goType ty
        scrutinees' <- desugarObjects scrutinee
        alts' <- traverse goAlt alts
        pure $ CaseE ty' scrutinees' alts'
      LetE bound e -> do
        bound' <- goBinds bound
        let unscoped = toExp e
        e' <- toScope <$> desugarObjects unscoped
        pure $ LetE bound' (F <$> e')
      AccessorE _ resTy lbl e -> desugarObjectAccessor resTy lbl e
      ObjectUpdateE _ ty orig copF updF -> desugarObjectUpdate ty orig copF updF
      V fvar -> case fvar of
        B (BVar i t nm) -> do
          t' <- goType t
          pure . V . B $ BVar i t' nm
        F (FVar t qi) -> do
          t' <- goType t
          pure . V . F $ FVar t' qi
      TyInstE t e -> TyInstE <$> goType t <*> go e
      TyAbs (BVar bvix bvty bvid) e -> do
        case purusTypeToKind bvty of
          Left err -> error err
          Right k -> do
            e' <- go e
            pure $ TyAbs (BVar bvix k bvid) e'
      where
        -- TODO: Error location w/ scope in alts
        goAlt ::
          Alt WithObjects SourceType (Exp WithObjects SourceType) (Vars SourceType) ->
          Counter (Alt WithoutObjects Ty (Exp WithoutObjects Ty) (Vars Ty))
        goAlt (UnguardedAlt pat e) = do
          pat' <- goPat pat
          let unscoped = toExp e
          e' <- toScope . fmap F <$> desugarObjects unscoped
          pure $ UnguardedAlt pat' e'

        goPat ::
          Pat WithObjects SourceType (Exp WithObjects SourceType) a ->
          Counter (Pat WithoutObjects Ty (Exp WithoutObjects Ty) (Vars Ty))
        goPat = \case
          VarP i n tyx -> VarP i n <$> goType tyx
          WildP -> pure WildP
          LitP lp ->
            tryConvertLitP lp >>= \case
              Left p -> pure p
              Right litp -> pure $ LitP litp
          ConP tn cn ps -> ConP tn cn <$> traverse goPat ps

        tryConvertLitP ::
          Lit WithObjects (Pat WithObjects SourceType (Exp WithObjects SourceType) a) ->
          Counter
            ( Either
                (Pat WithoutObjects Ty (Exp WithoutObjects Ty) (Vars Ty))
                (Lit WithoutObjects (Pat WithoutObjects Ty (Exp WithoutObjects Ty) (Vars Ty)))
            )
        tryConvertLitP = \case
          IntL i -> pure . pure $ IntL i
          -- NumL d -> pure . pure $ NumL d
          StringL s -> pure . pure $ StringL s
          CharL c -> pure . pure $ CharL c
          -- ArrayL ps -> pure . ArrayL <$> traverse goPat ps
          -- ConstArrayL lits -> pure . ConstArrayL <$> traverse tryConvertConstLitP lits
          ObjectL _ fs' -> do
            let fs = sortOn fst fs'
                len = length fs
                tupTyName = mkTupleTyName len
                tupCtorName = coerceProperName <$> tupTyName
                bareFields = snd <$> fs
            bareFields' <- traverse goPat bareFields
            pure . Left $ ConP tupTyName tupCtorName bareFields'

        goBinds ::
          [BindE SourceType (Exp WithObjects SourceType) (Vars SourceType)] ->
          Counter [BindE Ty (Exp WithoutObjects Ty) (Vars Ty)]
        goBinds [] = pure []
        goBinds (b : bs) = case b of
          NonRecursive ident bvix expr -> do
            let unscoped = toExp expr
            e' <- desugarObjects unscoped
            rest <- goBinds bs
            pure $ NonRecursive ident bvix (F <$> toScope e') : rest
          Recursive xs -> do
            -- TODO: Accurate error reporting
            let xsUnscoped = second (\e -> toExp e) <$> xs
            xs' <- traverse (traverse desugarObjects) xsUnscoped
            let xsRescoped = second (fmap F . toScope) <$> xs'
            rest <- goBinds bs
            pure $ Recursive xsRescoped : rest

        tryConvertLit ::
          Lit WithObjects (Exp WithObjects SourceType (Vars SourceType)) ->
          Counter
            ( Either
                (Exp WithoutObjects Ty (Vars Ty))
                (Lit WithoutObjects (Exp WithoutObjects Ty (Vars Ty)))
            )
        tryConvertLit = \case
          IntL i -> pure . pure $ IntL i
          -- NumL d -> pure . pure $ NumL d
          StringL psstr -> pure . pure $ StringL psstr
          CharL c -> pure . pure $ CharL c
          -- ArrayL nonLitArr ->  Right . ArrayL <$> traverse go  nonLitArr
          -- ConstArrayL lits -> Right . ConstArrayL <$> traverse constArrHelper lits
          ObjectL _ fs' -> Left <$> handleObjectLiteral fs'
          where
            handleObjectLiteral ::
              [(PSString, Exp WithObjects SourceType (Vars SourceType))] ->
              Counter (Exp WithoutObjects Ty (Vars Ty))
            handleObjectLiteral fs' = do
              let fs = sortOn fst fs'
                  len = length fs
                  tupTyName = mkTupleTyName len
                  bareFields = snd <$> fs
              bareFields' <- traverse desugarObjects bareFields
              let types' = expTy id <$> bareFields'
                  types = types' <> [foldl' applyType (TyCon tupTyName) types']
                  ctorType = foldr1 funTy types

                  ctorExp :: Exp WithoutObjects Ty (Vars Ty)
                  ctorExp = V . F $ FVar ctorType $ properToIdent <$> tupTyName
              assembleDesugaredObjectLit ctorExp ctorType bareFields'

        goType :: SourceType -> Counter Ty
        goType = tryConvertType

        updateBV :: BVar SourceType -> Counter (BVar Ty)
        updateBV (BVar bvIx bvTy bvNm) = do
          bvTy' <- goType bvTy
          pure $ BVar bvIx bvTy' bvNm

        -- TODO/FIXME: We need to ensure that we don't introduce conflicts in the bound variables. Not sure what the best way is ATM

        -- I'm not really sure what the point of the copy fields is? TODO: Figure out what the point of them is
        desugarObjectUpdate ::
          SourceType ->
          Exp WithObjects SourceType (Vars SourceType) ->
          Maybe [PSString] ->
          [(PSString, Exp WithObjects SourceType (Vars SourceType))] ->
          Counter (Exp WithoutObjects Ty (Vars Ty))
        desugarObjectUpdate _ e _ updateFields = do
          _fs <- case expTy id e of
            RecordT fs -> pure fs
            other ->
              error $
                "ERROR: Record expression:\n  "
                  <> prettyStr e
                  <> "\n  should have a Record type, but instead has type:\n  "
                  <> prettyStr other

          updateMap <- traverse desugarObjects $ M.fromList updateFields
          updateTypes <- traverse goType $ M.fromList $ second (expTy id) <$> updateFields
          origTypes <- traverse (goType . rowListType) (mkFieldMap _fs)
          let ts = updateTypes `M.union` origTypes
              len = M.size ts
              tupTyName = mkTupleTyName len
              tupCtorName = coerceProperName <$> tupTyName
              types' = M.elems ts
              types = types' <> [foldl' applyType (TyCon tupTyName) types']
              ctorType = foldr1 funTy types

              positioned = zip (M.keys ts) [0 ..]

          indices <- foldM (\acc nm -> do u <- next; pure $ M.insert nm u acc) M.empty (fst <$> positioned)

          let withPositioned :: forall x. (PSString -> Int -> Counter x) -> Counter [x]
              withPositioned f' = traverse (uncurry f') positioned

              argBndrTemplate :: Counter [Pat WithoutObjects Ty (Exp WithoutObjects Ty) (FVar Ty)]
              argBndrTemplate = withPositioned $ \lbl i -> case M.lookup lbl updateMap of
                Nothing -> do
                  let ident = Ident $ "<UPD_" <> T.pack (show i) <> ">"
                      n = indices M.! lbl
                  pure @Counter $ VarP ident n (ts M.! lbl)
                Just _ -> pure @Counter WildP

              resultTemplate = withPositioned $ \lbl i -> case M.lookup lbl updateMap of
                Nothing -> do
                  let nm = Ident $ "<UPD_" <> T.pack (show i) <> ">"
                      n = indices M.! lbl
                  pure $ V . B $ BVar n (origTypes M.! lbl) nm
                Just expr -> pure expr

              ctorExp = V . F $ FVar ctorType $ properToIdent <$> tupTyName

          ctorBndr <- ConP tupTyName tupCtorName <$> argBndrTemplate

          resultExpr <- assembleDesugaredObjectLit ctorExp ctorType =<< resultTemplate
          e' <- go e
          let scoped = toScope resultExpr
              altBranch = F <$> UnguardedAlt ctorBndr scoped
          pure $ CaseE ctorType e' [altBranch]

        desugarObjectAccessor ::
          SourceType ->
          PSString ->
          Exp WithObjects SourceType (Vars SourceType) ->
          Counter (Exp WithoutObjects Ty (Vars Ty))
        desugarObjectAccessor resTy lbl e = do
          _fs <- case expTy id e of
            RecordT fs -> pure fs
            other ->
              throwError $
                "ERROR: Record expression:\n  "
                  <> prettyStr e
                  <> "\n  should have a Record type, but instead has type:\n  "
                  <> prettyStr other
          fs <- traverse (traverse goType) $ M.toList (rowListType <$> mkFieldMap _fs)
          let len = length fs
              tupTyName = mkTupleTyName len
              tupCtorName = coerceProperName <$> tupTyName
              types' = snd <$> fs
              dummyNm = Ident "<ACCESSOR>"
              lblIx = fromJust $ elemIndex lbl (fst <$> fs) -- FIXME: fromJust
          let fieldTy = types' !! lblIx -- if it's not there *something* should have caught it by now
          n <- next
          let argBndrTemplate = replicate len WildP & ix lblIx .~ VarP dummyNm n fieldTy
          let ctorBndr = ConP tupTyName tupCtorName argBndrTemplate
              -- NOTE: `lblIx` is a placeholder for a better var ix
              rhs :: Exp WithoutObjects Ty (Vars Ty)
              rhs = V . B $ BVar n fieldTy dummyNm
              altBranch = F <$> UnguardedAlt ctorBndr (toScope rhs)
          e' <- desugarObjects e
          let result = CaseE fieldTy e' [altBranch]
              msg =
                "INPUT EXP:\n"
                  <> prettyStr e
                  <> "\n\nLABEL:\n"
                  <> prettyStr lbl
                  <> "\n\nSUPPLIED RESTYPE:\n"
                  <> prettyStr resTy
                  <> "\n\nFIELD TYPES:\n"
                  <> prettyStr types'
                  <> "\n\nRESULT RHS:\n"
                  <> prettyStr rhs
                  <> "\n\nOUTPUT RESULT:\n"
                  <> prettyStr result
          doTraceM "desugarObjectAccessor" msg
          pure $ CaseE fieldTy e' [altBranch]

assembleDesugaredObjectLit :: forall x a. Exp x Ty a -> Ty -> [Exp x Ty a] -> Counter (Exp x Ty a)
assembleDesugaredObjectLit expr (_ :~> b) (arg : args) = assembleDesugaredObjectLit (AppE expr arg) b args
assembleDesugaredObjectLit expr _ [] = pure expr -- TODO better error
assembleDesugaredObjectLit _ _ _ = error "something went wrong in assembleDesugaredObjectLit"

purusTypeToKind :: SourceType -> Either String Kind
purusTypeToKind _t =
  doTraceM "sourceTypeToKind" (prettyStr _t) >> case _t of
    TypeConstructor _ C.Type -> pure KindType
    t1 :-> t2 -> do
      t1' <- purusTypeToKind t1
      t2' <- purusTypeToKind t2
      pure $ KindArrow t1' t2'
    other -> Left $ "Error: PureScript type '" <> prettyTypeStr other <> " is not a valid Plutus Kind"

desugarObjectsInDatatypes ::
  Datatypes PurusType PurusType ->
  Counter (Datatypes Kind Ty)
desugarObjectsInDatatypes = bitraverseDatatypes (liftEither . purusTypeToKind) tryConvertType

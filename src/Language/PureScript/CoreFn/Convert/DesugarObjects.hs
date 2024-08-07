{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
-- has to be here (more or less)
{-# OPTIONS_GHC -Wno-orphans #-}

module Language.PureScript.CoreFn.Convert.DesugarObjects where

import Bound (Var (..))
import Control.Lens (ix)
import Control.Lens.Combinators (to)
import Control.Lens.IndexedPlated (icosmos)
import Control.Lens.Operators ((&), (.~), (<&>), (^.), (^..))
import Data.Bifunctor (Bifunctor (second))
import Data.List (elemIndex, foldl', sortOn)
import Data.Map (Map)
import Data.Map qualified as M
import Data.Maybe (fromJust)
import Data.Text (Text)
import Data.Text qualified as T
import Language.PureScript.Constants.Prim qualified as C
import Language.PureScript.CoreFn.Ann (Ann)
import Language.PureScript.CoreFn.Convert.DesugarCore (DS, IR_Decl, WithObjects, WithoutObjects, bind, desugarCoreModule, getVarIx, liftErr)
import Language.PureScript.CoreFn.Convert.IR (
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
import Language.PureScript.CoreFn.Convert.Monomorphize (
  instantiateAllConstructors,
  runMonomorphize,
 )
import Language.PureScript.CoreFn.Convert.Monomorphize.Utils (
  MonoError (MonoError),
  decodeModuleIO,
  findDeclBody,
  mkFieldMap,
 )
import Language.PureScript.CoreFn.Expr (
  Expr (..),
 )
import Language.PureScript.CoreFn.FromJSON ()
import Language.PureScript.CoreFn.Pretty (
  prettyTypeStr,
 )
import Language.PureScript.CoreFn.Utils (Context, exprType)
import Language.PureScript.Environment (DataDeclType (Data), kindType, mkTupleTyName, pattern RecordT, pattern (:->))
import Language.PureScript.Names (Ident (..), ProperName (..), ProperNameType (..), Qualified (..), QualifiedBy (..), coerceProperName, runModuleName, ModuleName (ModuleName))
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
import Prelude

import Bound.Scope
import Control.Monad (join)
import Control.Monad.State (evalStateT)
import GHC.IO (throwIO)
import Language.PureScript.CoreFn.Convert.Debug
import Language.PureScript.CoreFn.Desugar.Utils (properToIdent)
import Language.PureScript.CoreFn.Module (
  CtorDecl (CtorDecl),
  DataDecl (DataDecl),
  Datatypes (Datatypes),
  Module (..),
  bitraverseDatatypes,
  dDataTyName,
 )
import Language.PureScript.CoreFn.TypeLike (TypeLike (..))
import Prettyprinter (
  Pretty (pretty),
  defaultLayoutOptions,
  layoutPretty,
 )
import Prettyprinter.Render.Text (renderStrict)

prettyStr :: (Pretty a) => a -> String
prettyStr = T.unpack . renderStrict . layoutPretty defaultLayoutOptions . pretty

decodeModuleIR :: FilePath -> IO (Module IR_Decl SourceType SourceType Ann, (Int, M.Map Ident Int))
decodeModuleIR path = do
  myMod <- decodeModuleIO path
  case desugarCoreModule myMod of
    Left err -> throwIO $ userError err
    Right myModIR -> pure myModIR

test :: FilePath -> Text -> IO (Exp WithoutObjects Ty (Var (BVar Ty) (FVar Ty)))
test path decl = do
  (myMod, ds) <- decodeModuleIR path
  Just myDecl <- pure $ findDeclBody decl myMod
  case runMonomorphize myMod [] (join <$> fromScope myDecl) of
    Left (MonoError msg) -> throwIO $ userError $ "Couldn't monomorphize " <> T.unpack decl <> "\nReason:\n" <> msg
    Right body -> case evalStateT (tryConvertExpr body) ds of
      Left convertErr -> throwIO $ userError convertErr
      Right e -> do
        putStrLn (ppExp e)
        pure e

prepPIR ::
  FilePath ->
  Text ->
  IO (Exp WithoutObjects Ty (Var (BVar Ty) (FVar Ty)), Datatypes Kind Ty)
prepPIR path decl = do
  (myMod@Module {..}, ds) <- decodeModuleIR path

  desugaredExpr <- case findDeclBody decl myMod of
    Nothing -> throwIO $ userError "findDeclBody"
    Just expr -> pure expr
  case runMonomorphize myMod [] (join <$> fromScope desugaredExpr) of
    Left (MonoError msg) ->
      throwIO $
        userError $
          "Couldn't monomorphize "
            <> T.unpack (runModuleName moduleName <> ".main")
            <> "\nReason:\n"
            <> msg
    Right body -> do
      putStrLn (ppExp body)
      case evalStateT (tryConvertExpr body) ds of
        Left convertErr -> throwIO $ userError convertErr
        Right e -> do
          moduleDataTypes' <-
            either (throwIO . userError) pure $
              bitraverseDatatypes
                tryConvertKind
                tryConvertType
                moduleDataTypes
          putStrLn $ "tryConvertExpr result:\n" <> ppExp e <> "\n" <> replicate 20 '-'
          pure (e, moduleDataTypes')

-- This gives us a way to report the exact location of the error (which may no longer correspond *at all* to
-- the location given in the SourcePos annotations due to inlining and monomorphization)

tryConvertType :: SourceType -> Either String Ty
tryConvertType = go id
  where
    go :: (SourceType -> SourceType) -> SourceType -> Either String Ty
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
          else Left $ prettyTypeStr fs <> " is not a closed row. Last: " <> prettyTypeStr (rowLast fs)
      TypeVar _ txt k -> TyVar txt <$> tryConvertKind' f t k
      TypeConstructor _ tn -> Right $ TyCon tn
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
      other -> Left $ "Unsupported type:\n  " <> prettyTypeStr other

tryConvertKind :: SourceType -> Either String Kind
tryConvertKind t = tryConvertKind' id t t

tryConvertKind' :: (SourceType -> SourceType) -> SourceType -> SourceType -> Either String Kind
tryConvertKind' f t = \case
  TypeConstructor _ C.Type -> pure KindType
  k1 :-> k2 -> do
    k1' <- tryConvertKind' f t k1
    k2' <- tryConvertKind' f t k2
    pure $ KindArrow k1' k2'
  other ->
    Left $
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

allTypes :: Expr Ann -> [SourceType]
allTypes e = e ^.. icosmos @Context @(Expr Ann) M.empty . to exprType

tryConvertExpr ::
  Exp WithObjects SourceType (Var (BVar SourceType) (FVar SourceType)) ->
  DS (Exp WithoutObjects Ty (Var (BVar Ty) (FVar Ty)))
tryConvertExpr =
  {- TODO: We have to instantiate constructors here to keep the types correct for the later steps
           in the pipeline, but we really get this into a state where we can see all the transformations
           in one place (i.e. by unifying the disparate functionality of the several monads we have
           & representing the pipeline as a sequence of kleisli arrows)
  -}
  fmap instantiateAllConstructors . tryConvertExpr' id

tryConvertExpr' ::
  forall a.
  (Pretty a) =>
  (a -> Var (BVar SourceType) (FVar SourceType)) ->
  Exp WithObjects SourceType a ->
  DS (Exp WithoutObjects Ty (Var (BVar Ty) (FVar Ty)))
tryConvertExpr' toVar __expr = do
  result <- go __expr
  let msg =
        "INPUT:\n"
          <> ppExp __expr
          <> "\n\nINPUT TY:\n"
          <> prettyStr (expTy toVar __expr)
          <> "\n\nRESULT:\n"
          <> ppExp result
          <> "\n\nRESULT TY:\n"
          <> prettyStr (expTy id result)
  doTraceM "tryConvertExpr'" msg
  pure result
  where
    go :: Exp WithObjects SourceType a -> DS (Exp WithoutObjects Ty (Var (BVar Ty) (FVar Ty)))
    go expression = case expression of
      LitE ty lit -> do
        ty' <- goType ty
        tryConvertLit lit >>= \case
          Left desObj -> pure desObj
          Right lit' -> pure $ LitE ty' lit'
      LamE bv e -> do
        bv' <- updateBV bv
        let unscoped = join <$> fromScope (toVar <$> e)
        ex <- toScope <$> tryConvertExpr' id unscoped -- Exp x t (Var (BVar t) (FVar t))
        pure $ LamE bv' (F <$> ex)
      AppE e1 e2 -> do
        e2' <- tryConvertExpr' toVar e2
        e1' <- tryConvertExpr' toVar e1
        pure $ AppE e1' e2'
      CaseE ty scrutinee alts -> do
        ty' <- goType ty
        scrutinees' <- tryConvertExpr' toVar scrutinee
        alts' <- traverse goAlt alts
        pure $ CaseE ty' scrutinees' alts'
      LetE bindings_ bound e -> do
        bindings' <- traverse (traverse goType) bindings_
        bound' <- goBinds bound
        let unscoped = join <$> fromScope (toVar <$> e)
        e' <- toScope <$> tryConvertExpr' id unscoped
        pure $ LetE bindings' bound' (F <$> e')
      AccessorE _ resTy lbl e -> desugarObjectAccessor resTy lbl e
      ObjectUpdateE _ ty orig copF updF -> desugarObjectUpdate ty orig copF updF
      V fvar -> case toVar fvar of
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
          Alt WithObjects SourceType (Exp WithObjects SourceType) a ->
          DS (Alt WithoutObjects Ty (Exp WithoutObjects Ty) (Var (BVar Ty) (FVar Ty)))
        goAlt (UnguardedAlt binders pat e) = do
          binders' <- M.fromList <$> traverse (traverse (traverse goType)) (M.toList binders)
          pat' <- goPat pat
          let unscoped = join <$> fromScope (toVar <$> e)
          e' <- toScope . fmap F <$> tryConvertExpr' id unscoped
          pure $ UnguardedAlt binders' pat' e'

        goPat ::
          Pat WithObjects SourceType (Exp WithObjects SourceType) a ->
          DS (Pat WithoutObjects Ty (Exp WithoutObjects Ty) (Var (BVar Ty) (FVar Ty)))
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
          DS
            ( Either
                (Pat WithoutObjects Ty (Exp WithoutObjects Ty) (Var (BVar Ty) (FVar Ty)))
                (Lit WithoutObjects (Pat WithoutObjects Ty (Exp WithoutObjects Ty) (Var (BVar Ty) (FVar Ty))))
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
          [BindE SourceType (Exp WithObjects SourceType) a] ->
          DS [BindE Ty (Exp WithoutObjects Ty) (Var (BVar Ty) (FVar Ty))]
        goBinds [] = pure []
        goBinds (b : bs) = case b of
          NonRecursive ident bvix expr -> do
            let unscoped = join <$> fromScope (toVar <$> expr)
            e' <- tryConvertExpr' id unscoped
            rest <- goBinds bs
            pure $ NonRecursive ident bvix (F <$> toScope e') : rest
          Recursive xs -> do
            -- TODO: Accurate error reporting
            let xsUnscoped = second (\e -> join <$> fromScope (toVar <$> e)) <$> xs
            xs' <- traverse (traverse $ tryConvertExpr' id) xsUnscoped
            let xsRescoped = second (fmap F . toScope) <$> xs'
            rest <- goBinds bs
            pure $ Recursive xsRescoped : rest

        tryConvertLit ::
          Lit WithObjects (Exp WithObjects SourceType a) ->
          DS
            ( Either
                (Exp WithoutObjects Ty (Var (BVar Ty) (FVar Ty)))
                (Lit WithoutObjects (Exp WithoutObjects Ty (Var (BVar Ty) (FVar Ty))))
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
              [(PSString, Exp WithObjects SourceType a)] ->
              DS (Exp WithoutObjects Ty (Var (BVar Ty) (FVar Ty)))
            handleObjectLiteral fs' = do
              let fs = sortOn fst fs'
                  len = length fs
                  tupTyName = mkTupleTyName len
                  bareFields = fmap toVar . snd <$> fs
              bareFields' <- traverse (tryConvertExpr' id) bareFields
              let types' = expTy id <$> bareFields'
                  types = types' <> [foldl' applyType (TyCon tupTyName) types']
                  ctorType = foldr1 funTy types

                  ctorExp :: Exp WithoutObjects Ty (Var (BVar Ty) (FVar Ty))
                  ctorExp = V . F $ FVar ctorType $ properToIdent <$> tupTyName
              assembleDesugaredObjectLit ctorExp ctorType bareFields'

        goType :: SourceType -> DS Ty
        goType = liftErr . tryConvertType

        updateBV :: BVar SourceType -> DS (BVar Ty)
        updateBV (BVar bvIx bvTy bvNm) = do
          bvTy' <- goType bvTy
          pure $ BVar bvIx bvTy' bvNm

        -- TODO/FIXME: We need to ensure that we don't introduce conflicts in the bound variables. Not sure what the best way is ATM

        -- I'm not really sure what the point of the copy fields is? TODO: Figure out what the point of them is
        desugarObjectUpdate ::
          SourceType ->
          Exp WithObjects SourceType a ->
          Maybe [PSString] ->
          [(PSString, Exp WithObjects SourceType a)] ->
          DS (Exp WithoutObjects Ty (Var (BVar Ty) (FVar Ty)))
        desugarObjectUpdate _ e _ updateFields = do
          _fs <- case expTy toVar e of
            RecordT fs -> pure fs
            other ->
              error $
                "ERROR: Record expression:\n  "
                  <> prettyStr e
                  <> "\n  should have a Record type, but instead has type:\n  "
                  <> prettyStr other

          updateMap <- traverse (tryConvertExpr' toVar) $ M.fromList updateFields
          updateTypes <- traverse goType $ M.fromList $ second (expTy toVar) <$> updateFields
          origTypes <- traverse (goType . rowListType) (mkFieldMap _fs)
          let ts = updateTypes `M.union` origTypes
              len = M.size ts
              tupTyName = mkTupleTyName len
              tupCtorName = coerceProperName <$> tupTyName
              types' = M.elems ts
              types = types' <> [foldl' applyType (TyCon tupTyName) types']
              ctorType = foldr1 funTy types

              positioned = zip (M.keys ts) [0 ..]

              withPositioned :: forall x. (PSString -> Int -> DS x) -> DS [x]
              withPositioned f' = traverse (uncurry f') positioned

              argBndrTemplate :: DS [Pat WithoutObjects Ty (Exp WithoutObjects Ty) (FVar Ty)]
              argBndrTemplate = withPositioned $ \lbl i -> case M.lookup lbl updateMap of
                Nothing -> do
                  let ident = Ident $ "<UPD_" <> T.pack (show i) <> ">"
                  n <- bind ident
                  pure @DS $ VarP ident n (ts M.! lbl)
                Just _ -> pure @DS WildP

              resultTemplate = withPositioned $ \lbl i -> case M.lookup lbl updateMap of
                Nothing -> do
                  let nm = Ident $ "<UPD_" <> T.pack (show i) <> ">"
                  n <- getVarIx nm
                  pure $ V . B $ BVar n (origTypes M.! lbl) nm
                Just expr -> pure expr

              ctorExp = V . F $ FVar ctorType $ properToIdent <$> tupTyName

          ctorBndr <- ConP tupTyName tupCtorName <$> argBndrTemplate

          resultExpr <- assembleDesugaredObjectLit ctorExp ctorType =<< resultTemplate
          e' <- go e
          let scoped = toScope resultExpr
              -- TODO/FIXME/REVIEW/HACK: Either remove the bindings Map from UnguardedAlt or actually construct a real one
              --                         (I think we never use it so it should be OK to remove?)
              altBranch = F <$> UnguardedAlt M.empty ctorBndr scoped
          pure $ CaseE ctorType e' [altBranch]

        desugarObjectAccessor ::
          SourceType ->
          PSString ->
          Exp WithObjects SourceType a ->
          DS (Exp WithoutObjects Ty (Var (BVar Ty) (FVar Ty)))
        desugarObjectAccessor resTy lbl e = do
          _fs <- case expTy toVar e of
            RecordT fs -> pure fs
            other ->
              error $
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
          n <- bind dummyNm
          let argBndrTemplate = replicate len WildP & ix lblIx .~ VarP dummyNm n fieldTy
          let ctorBndr = ConP tupTyName tupCtorName argBndrTemplate
              -- NOTE: `lblIx` is a placeholder for a better var ix
              rhs :: Exp WithoutObjects Ty (Var (BVar Ty) (FVar Ty))
              rhs = V . B $ BVar n fieldTy dummyNm
              altBranch = F <$> UnguardedAlt M.empty ctorBndr (toScope rhs)
          e' <- tryConvertExpr' toVar e
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

assembleDesugaredObjectLit :: forall x a. Exp x Ty a -> Ty -> [Exp x Ty a] -> DS (Exp x Ty a)
assembleDesugaredObjectLit expr (_ :~> b) (arg : args) = assembleDesugaredObjectLit (AppE expr arg) b args
assembleDesugaredObjectLit expr _ [] = pure expr -- TODO better error
assembleDesugaredObjectLit _ _ _ = error "something went wrong in assembleDesugaredObjectLit"

-- TODO/FIXME: Adapt this for use w/ the PIR Data declaration machinery (i.e. don't manually construct SOPs)

pattern ArrayCons :: Qualified Ident
pattern ArrayCons = Qualified (ByModuleName C.M_Prim) (Ident "Cons")

pattern ArrayNil :: Qualified Ident
pattern ArrayNil = Qualified (ByModuleName C.M_Prim) (Ident "Nil")

mkProdFields :: [t] -> [(Ident, t)]
mkProdFields = map (UnusedIdent,)

primData :: Datatypes Kind Ty
primData = 
  tupleDatatypes <> 
  Datatypes tDict cDict <> 
  Datatypes ledgerApiTDict ledgerApiCDict
  where
    tDict :: Map (Qualified (ProperName 'TypeName)) (DataDecl Kind Ty)
    tDict =
      M.fromList $
        map
          (\x -> (x ^. dDataTyName, x))
          [ DataDecl
              Data
              C.Array
              [("a", KindType)]
              [ CtorDecl ArrayNil []
              , CtorDecl ArrayCons $ mkProdFields [TyVar "a" KindType, TyApp (TyCon C.Array) (TyVar "a" KindType)]
              ]
          , DataDecl
              Data
              C.Boolean
              []
              [ CtorDecl (properToIdent <$> C.C_False) []
              , CtorDecl (properToIdent <$> C.C_True) []
              ]
          ]

    cDict :: Map (Qualified Ident) (Qualified (ProperName 'TypeName))
    cDict =
      M.fromList
        [ (ArrayCons, C.Array)
        , (ArrayNil, C.Array)
        , (properToIdent <$> C.C_True, C.Boolean)
        , (properToIdent <$> C.C_False, C.Boolean)
        ]
    ledgerApiTDict :: Map (Qualified (ProperName 'TypeName)) (DataDecl Kind Ty)
    ledgerApiTDict = M.fromList [
        -- Ledger API (V2), as per https://github.com/IntersectMBO/plutus/blob/master/plutus-ledger-api/src/PlutusLedgerApi/V2.hs
        -- Context types
        (ledgerTyName "ScriptContext", 
         DataDecl Data (ledgerTyName "ScriptContext") [] (record "ScriptContext" [
            field "txInfo" "TxInfo",
            field "purpose" "ScriptPurpose"
            ]))
      , (ledgerTyName "ScriptPurpose", DataDecl Data (ledgerTyName "ScriptPurpose") [] [
          CtorDecl (ledgerIdent "Minting") [(UnusedIdent, TyCon . ledgerTyName $ "CurrencySymbol")],
          CtorDecl (ledgerIdent "Spending") [(UnusedIdent, TyCon . ledgerTyName $ "TxOutRef")],
          CtorDecl (ledgerIdent "Rewarding") [(UnusedIdent, TyCon . ledgerTyName $ "StakingCredential")],
          CtorDecl (ledgerIdent "Staking") [(UnusedIdent, TyCon . ledgerTyName $ "DCert")]
          ])
      , _
      ]
    ledgerApiCDict :: Map (Qualified Ident) (Qualified (ProperName 'TypeName))
    ledgerApiCDict = M.fromList [
        -- Ledger API (V2), as per https://github.com/IntersectMBO/plutus/blob/master/plutus-ledger-api/src/PlutusLedgerApi/V2.hs
        -- Context types
        homonym "ScriptContext"
      , (ledgerIdent "Minting", ledgerTyName "ScriptPurpose")
      , (ledgerIdent "Spending", ledgerTyName "ScriptPurpose")
      , (ledgerIdent "Rewarding", ledgerTyName "ScriptPurpose")
      , (ledgerIdent "Staking", ledgerTyName "ScriptPurpose")
      ]
    ledgerTyName :: Text -> Qualified (ProperName 'TypeName)
    ledgerTyName x = Qualified (ByModuleName (ModuleName "Prim")) (ProperName x)
    ledgerIdent :: Text -> Qualified Ident
    ledgerIdent x = Qualified (ByModuleName (ModuleName "Prim")) (Ident x)
    -- Used to mark a data constructor that matches the type name
    homonym :: Text -> (Qualified Ident, Qualified (ProperName 'TypeName))
    homonym x = (ledgerIdent x, ledgerTyName x)
    record :: _
    record recName fields = [CtorDecl (ledgerIdent recName) fields]
    field :: _
    field fieldName tyName = (Ident fieldName, TyCon . ledgerTyName $ tyName)

tupleDatatypes :: Datatypes Kind Ty
tupleDatatypes = Datatypes (M.fromList tupleTypes) (M.fromList tupleCtors)
  where
    tupleTypes = flip map [0 .. 10] $ \(n :: Int) ->
      let tyNm = mkTupleTyName n
          ctorNm = mkTupleCtorIdent n
          argKinds = mkTupleArgKinds n
          ctorTvArgs = mkTupleCtorTvArgs n
       in (tyNm, DataDecl Data tyNm argKinds [CtorDecl ctorNm ctorTvArgs])

    tupleCtors = [0 .. 10] <&> \x -> (mkTupleCtorIdent x, mkTupleTyName x)

    mkTupleCtorIdent :: Int -> Qualified Ident
    mkTupleCtorIdent n = properToIdent <$> mkTupleTyName n

    vars :: Int -> [Text]
    vars n = map (\x -> "t" <> T.pack (show x)) [1 .. n]

    mkTupleArgKinds = fmap (,KindType) . vars

    mkTupleCtorTvArgs = mkProdFields . map (flip TyVar KindType) . vars

purusTypeToKind :: SourceType -> Either String Kind
purusTypeToKind _t =
  doTraceM "sourceTypeToKind" (prettyStr _t) >> case _t of
    TypeConstructor _ C.Type -> pure KindType
    t1 :-> t2 -> do
      t1' <- purusTypeToKind t1
      t2' <- purusTypeToKind t2
      pure $ KindArrow t1' t2'
    other -> Left $ "Error: PureScript type '" <> prettyTypeStr other <> " is not a valid Plutus Kind"

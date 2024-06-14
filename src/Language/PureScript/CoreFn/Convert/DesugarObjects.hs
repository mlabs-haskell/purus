{-# OPTIONS_GHC -Wno-orphans #-} -- has to be here (more or less)
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Language.PureScript.CoreFn.Convert.DesugarObjects where


import Prelude
import Language.PureScript.CoreFn.Expr
    ( Expr(..) )
import Language.PureScript.Names (Ident(..), Qualified (..), QualifiedBy (..), pattern ByNullSourcePos, ProperNameType (..), ProperName(..), runModuleName, coerceProperName)
import Language.PureScript.Types
    ( SourceType, Type(..), srcTypeConstructor, srcTypeApp, RowListItem (rowListType), rowToList, eqType )
import Language.PureScript.Environment (pattern (:->), pattern RecordT, kindType, DataDeclType (Data), mkTupleTyName)
import Language.PureScript.CoreFn.Pretty
    ( prettyTypeStr )
import Language.PureScript.CoreFn.Ann (Ann)
import Language.PureScript.CoreFn.FromJSON ()
import Data.Text qualified as T
import Data.List ( elemIndex, sortOn, foldl' )
import Data.Map qualified as M
import Language.PureScript.PSString (PSString)
import Language.PureScript.AST.SourcePos
    ( pattern NullSourceAnn )
import Control.Lens.IndexedPlated ( icosmos )
import Control.Lens ( ix )
import Language.PureScript.CoreFn.Convert.Monomorphize.Utils
    ( transverseScopeViaExpX,
      mkFieldMap,
      findDeclBody,
      decodeModuleIO,
      MonoError(MonoError),
      IR_Decl )
import Language.PureScript.CoreFn.Convert.Monomorphize
    ( runMonomorphize )
import Data.Text (Text)
import Bound ( toScope, Var(..) )
import Data.Bifunctor (Bifunctor(first, second))
import Control.Lens.Combinators (to)
import Data.Maybe (fromJust)
import Control.Lens.Operators ( (<&>), (.~), (&), (^..), (^.) )
import Language.PureScript.CoreFn.Convert.IR
    ( pattern (:~>),
      expTy,
      ppExp,
      BindE(..),
      Alt(..),
      BVar(..),
      Pat(..),
      Kind(..),
      Lit(..),
      Exp(..),
      FVar(..),
      Ty(..) )
import Language.PureScript.CoreFn.Utils ( exprType, Context )
import Debug.Trace ( traceM )
import Data.Map (Map)
import Language.PureScript.Constants.Prim qualified as C
import Language.PureScript.CoreFn.Convert.DesugarCore (WithoutObjects, WithObjects, desugarCoreModule)
import Data.Void (Void, absurd)

import Unsafe.Coerce qualified as UNSAFE

import Prettyprinter
    ( defaultLayoutOptions, layoutPretty, Pretty(pretty) )
import Prettyprinter.Render.Text (renderStrict)
import Language.PureScript.CoreFn.TypeLike (TypeLike(..))
import Language.PureScript.CoreFn.Module
    ( Datatypes(Datatypes),
      CtorDecl(CtorDecl),
      DataDecl(DataDecl),
      dDataTyName,
      Module(..),
      bitraverseDatatypes )
import GHC.IO (throwIO)
import Language.PureScript.CoreFn.Desugar.Utils (properToIdent)

prettyStr :: Pretty a => a -> String
prettyStr = T.unpack . renderStrict . layoutPretty defaultLayoutOptions . pretty

decodeModuleIR :: FilePath -> IO (Module IR_Decl SourceType SourceType Ann)
decodeModuleIR path = do
  myMod <- decodeModuleIO path
  case desugarCoreModule myMod of
    Left err -> throwIO $ userError err
    Right myModIR -> pure myModIR

test :: FilePath -> Text -> IO (Exp WithoutObjects Ty (FVar Ty))
test path decl = do
  myMod <- decodeModuleIR path
  Just myDecl <- pure $ findDeclBody decl myMod
  case runMonomorphize myMod [] myDecl of
    Left (MonoError msg) -> throwIO $ userError $ "Couldn't monomorphize " <> T.unpack decl <> "\nReason:\n" <> msg
    Right body -> case tryConvertExpr body of
      Left convertErr -> throwIO $ userError convertErr
      Right e -> do
        putStrLn (ppExp e)
        pure e

prepPIR :: FilePath
        -> Text
        -> IO (Exp WithoutObjects Ty (FVar Ty), Datatypes Kind Ty)
prepPIR path  decl = do
  myMod@Module{..} <- decodeModuleIR path

  desugaredExpr <- case findDeclBody decl myMod of
    Nothing -> throwIO $ userError "findDeclBody"
    Just expr -> pure expr
  case runMonomorphize myMod [] desugaredExpr of
    Left (MonoError msg ) ->
      throwIO
      $ userError
      $ "Couldn't monomorphize "
        <> T.unpack (runModuleName moduleName <> ".main") <> "\nReason:\n" <> msg
    Right body -> do
      putStrLn (ppExp body)
      case tryConvertExpr body of
         Left convertErr -> throwIO $ userError convertErr
         Right e -> do
           moduleDataTypes' <- either (throwIO . userError . prettyErrorT) pure
                               $ bitraverseDatatypes
                                   tryConvertKind
                                   tryConvertType
                                   moduleDataTypes
           putStrLn $ "tryConvertExpr result:\n" <> (ppExp e) <> "\n" <> replicate 20 '-'
           pure (e,moduleDataTypes')

-- This gives us a way to report the exact location of the error (which may no longer correspond *at all* to
-- the location given in the SourcePos annotations due to inlining and monomorphization)

type ExpWithoutObjects = Exp WithoutObjects Ty (FVar Ty)
type ExpWithObjects    = Exp WithObjects SourceType (FVar SourceType)

type BindEWithObjects = BindE SourceType (Exp WithObjects SourceType) (FVar SourceType)
type BindEWithoutObjects = BindE Ty (Exp WithoutObjects Ty) (FVar Ty)

data TypeConvertError
 = TypeConvertError (SourceType -> SourceType ) SourceType String

type TyConvertM = Either TypeConvertError


-- unsafeCoerce would probably be safe here?
forgetConstantLiteral :: Lit x Void -> Lit x a
forgetConstantLiteral = UNSAFE.unsafeCoerce

tryConvertType :: SourceType -> Either TypeConvertError Ty
tryConvertType = go id
  where
    go :: (SourceType -> SourceType) -> SourceType -> Either TypeConvertError Ty
    go f t = case t of
      RecordT fs ->
        if isClosedRow fs
          then do
            let fields = rowListType <$> mkFieldMap fs
                arity  = M.size fields
                fakeTName = mkTupleTyName arity
                types = M.elems fields
                ctorType = foldl' srcTypeApp (srcTypeConstructor fakeTName) types
            go f ctorType
          else Left $ TypeConvertError f t $ prettyTypeStr fs <> " is not a closed row. Last: " <> prettyTypeStr (rowLast fs)
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

      other -> Left $ TypeConvertError f other $ "Unsupported type:\n  " <> prettyTypeStr other


tryConvertKind :: SourceType -> Either TypeConvertError Kind
tryConvertKind t = tryConvertKind' id t t

tryConvertKind' :: (SourceType -> SourceType) -> SourceType -> SourceType -> Either TypeConvertError Kind
tryConvertKind' f t = \case
  TypeConstructor _ C.Type -> pure KindType
  k1 :-> k2 -> do
    k1' <- tryConvertKind' f t k1
    k2' <- tryConvertKind' f t k2
    pure $ KindArrow k1' k2'
  other -> Left $ TypeConvertError f t
           $ "Couldn't convert type: "
             <> prettyTypeStr other
             <> " to a Plutus Kind.\n"
             <> "Plutus only supports kinds * and (* -> Plutus Kind)"

isClosedRow :: SourceType -> Bool
isClosedRow t = case rowToList t of
  (_,REmpty{}) -> True
  (_,KindApp _ REmpty{} k) | eqType k kindType -> True
  _ -> False

rowLast :: SourceType -> SourceType
rowLast t = case rowToList t of
  (_,r) -> r

allTypes :: Expr Ann -> [SourceType]
allTypes e = e ^.. icosmos @Context @(Expr Ann) M.empty . to exprType

data ExprConvertError
  = ExprConvertError (ExpWithObjects -> ExpWithObjects) ExpWithObjects (Maybe TypeConvertError) String

type ConvertM = Either ExprConvertError

prettyErrorT :: TypeConvertError -> String
prettyErrorT (TypeConvertError g t msg1)
  = "Error when converting types to final IR: " <> msg1
     <> "\nin type:\n  " <>  prettyTypeStr (g $ TypeVar NullSourceAnn "<ERROR HERE>" (TUnknown NullSourceAnn 0))
     <> "\nin type component:\n  " <> prettyTypeStr t


prettyError :: ExprConvertError -> String
prettyError = \case
  ExprConvertError f e Nothing msg -> "Error when converting expression: " <> msg <> "\n  "
                                  <> prettyStr (f $ fakeVar "<ERROR_HERE>")
                                  <> "\nin subexpression:\n  "
                                  <> prettyStr e
  ExprConvertError f e (Just (TypeConvertError g t msg1)) msg2 -> "Error when converting types: " <>  msg1 <> "\n" <> msg2 <> "\n  "
                                    <> prettyStr (f $ fakeVar "<ERROR_HERE>")
                                    <> "\nin subexpression:\n  "
                                    <> prettyStr e
                                    <> "\nin type:\n  "
                                    <> prettyTypeStr (g $ TypeVar NullSourceAnn "<ERROR HERE>" (TUnknown NullSourceAnn 0))
                                    <> "\nin type component:\n  "
                                    <> prettyTypeStr t
 where
   fakeVar :: Text -> ExpWithObjects
   fakeVar t = V $ FVar (srcTypeConstructor $ Qualified ByNullSourcePos (ProperName "ERROR!")) (Qualified ByNullSourcePos (Ident t))

tryConvertExprIO :: ExpWithObjects -> IO ()
tryConvertExprIO = putStrLn . either id ppExp . tryConvertExpr

tryConvertExpr :: Exp WithObjects SourceType (FVar SourceType) -> Either String (Exp WithoutObjects Ty (FVar Ty))
tryConvertExpr = first prettyError . tryConvertExpr'

tryConvertExpr' :: Exp WithObjects SourceType (FVar SourceType)
                -> Either ExprConvertError (Exp WithoutObjects Ty (FVar Ty))
tryConvertExpr' __expr =  do
    result <- go id __expr
    let spacer = replicate 20 '-'
    traceM (spacer <> "tryConvertExpr" <> spacer <> "\n  Expr:\n" <> ppExp __expr <> "\n  Result:\n" <> ppExp result <> "\n" <> spacer )
    pure result
  where
    go :: (ExpWithObjects -> ExpWithObjects) -> ExpWithObjects -> Either ExprConvertError (Exp WithoutObjects Ty (FVar Ty))
    go f expression =  case expression of
      LitE  ty lit -> do
        let lhole = f . LitE  ty . ArrayL . pure
        ty' <- goType ty
        tryConvertLit  lhole lit >>= \case
          Left desObj -> pure desObj
          Right lit' -> pure $ LitE ty' lit'
      LamE  ty bv  e -> do
        ty' <- goType ty
        bv' <- updateBV bv
        -- TODO: Figure out how to get a better error location ehre
        ex <- transverseScopeViaExpX tryConvertExpr'  updateBV e
        pure $ LamE ty' bv' ex
      AppE e1 e2 -> do
        e2' <- tryConvertExpr' e2
        e1' <- tryConvertExpr' e1
        pure $ AppE  e1' e2'
      CaseE ty scrutinee alts -> do
        ty' <- goType ty
        scrutinees' <- tryConvertExpr' scrutinee
        alts' <- traverse (goAlt (f . CaseE  ty scrutinee . pure)) alts
        pure $ CaseE ty' scrutinees' alts'
      LetE bindings bound e -> do
        bindings' <- traverse (traverse goType) bindings
        bound' <- goBinds (f . (\x -> LetE bindings [x] e)) bound
        e' <- transverseScopeViaExpX tryConvertExpr' updateBV  e
        pure $ LetE bindings' bound' e'
      AccessorE _ ty lbl e -> desugarObjectAccessor ty lbl e
      ObjectUpdateE _ ty orig copF updF -> desugarObjectUpdate ty orig copF updF
      V fvar -> V <$> traverse goType fvar
     where
       -- TODO: Error location w/ scope in alts
       goAlt :: (Alt WithObjects SourceType (Exp WithObjects SourceType) (FVar SourceType) -> ExpWithObjects)
             -> Alt WithObjects SourceType (Exp WithObjects SourceType) (FVar SourceType)
             -> Either ExprConvertError (Alt WithoutObjects Ty (Exp WithoutObjects Ty) (FVar Ty))
       goAlt _ (UnguardedAlt binders pat e) = do
         binders' <- fmap M.fromList . catchTE $ traverse (traverse (traverse tryConvertType)) (M.toList binders)
         pat' <- goPat pat
         e' <- transverseScopeViaExpX tryConvertExpr' updateBV e
         pure $ UnguardedAlt binders' pat' e'

       goPat :: Pat WithObjects (Exp WithObjects SourceType) (FVar SourceType)
                -> Either ExprConvertError (Pat WithoutObjects (Exp WithoutObjects Ty) (FVar Ty))
       goPat = \case
            VarP i -> pure $ VarP i
            WildP  -> pure WildP
            AsP i px -> AsP i <$> goPat px
            LitP lp -> tryConvertLitP lp >>= \case
              Left p -> pure p
              Right litp -> pure $ LitP litp
            ConP tn cn ps -> ConP tn cn  <$> traverse goPat ps

       tryConvertLitP :: Lit WithObjects (Pat WithObjects (Exp WithObjects SourceType) (FVar SourceType))
                      -> Either ExprConvertError
                                (Either
                                 (Pat WithoutObjects (Exp WithoutObjects Ty) (FVar Ty))
                                 (Lit WithoutObjects (Pat WithoutObjects (Exp WithoutObjects Ty) (FVar Ty))))
       tryConvertLitP = \case
         IntL i -> pure . pure $ IntL i
         NumL d -> pure . pure $ NumL d
         StringL s -> pure . pure $ StringL s
         CharL c -> pure . pure $ CharL c
         ArrayL ps -> pure . ArrayL <$> traverse goPat ps
         ConstArrayL lits -> pure . ConstArrayL <$> traverse tryConvertConstLitP lits
         ObjectL _ fs' -> do
           let fs          = sortOn fst fs'
               len         = length fs
               tupTyName   = mkTupleTyName len
               tupCtorName = coerceProperName <$> tupTyName
               bareFields = snd <$> fs
           bareFields' <- traverse goPat bareFields
           pure . Left $ ConP tupTyName tupCtorName  bareFields'

       -- FIXME: We can have empty objects here, so this function is partial, but I'm not sure what to do w/
       --        an empty Object in a pattern.
       tryConvertConstLitP :: Lit WithObjects Void -> Either ExprConvertError (Lit WithoutObjects Void)
       tryConvertConstLitP = \case
         IntL i -> pure $ IntL i
         NumL d -> pure $ NumL d
         StringL s -> pure $ StringL s
         CharL c -> pure $ CharL c
         ArrayL [] -> pure $ ConstArrayL []
         ConstArrayL lits ->  ConstArrayL <$> traverse tryConvertConstLitP lits
         ObjectL _ [] -> undefined
         _ -> error "impossible (?) pattern"

       goBinds :: (BindEWithObjects -> ExpWithObjects)
               -> [BindEWithObjects]
               -> Either ExprConvertError [BindEWithoutObjects]
       goBinds _ [] = pure []
       goBinds g (b:bs) = case b of
         NonRecursive ident expr -> do
           e' <- tryConvertExpr' expr
           rest <- goBinds g bs
           pure $ NonRecursive ident e'  : rest
         Recursive  xs -> do
           -- TODO: Accurate error reporting
           let g' = g . NonRecursive (Ident "HERE")
           xs' <- traverse (traverse tryConvertExpr') xs
           rest <- goBinds g bs
           pure $ Recursive xs' : rest

       tryConvertLit :: (ExpWithObjects -> ExpWithObjects)
                     -> Lit WithObjects ExpWithObjects
                     -> Either ExprConvertError (Either ExpWithoutObjects (Lit WithoutObjects ExpWithoutObjects))
       tryConvertLit  cb  = \case
         IntL i -> pure . pure $ IntL i
         NumL d -> pure . pure $ NumL d
         StringL psstr -> pure . pure $ StringL psstr
         CharL c       -> pure . pure $ CharL c
         ArrayL nonLitArr ->  Right . ArrayL <$> traverse (go cb)  nonLitArr
         ConstArrayL lits -> Right . ConstArrayL <$> traverse constArrHelper lits
         ObjectL _ fs'  ->  Left <$> handleObjectLiteral fs'
        where
         constArrHelper ::  Lit WithObjects Void
                        -> Either ExprConvertError (Lit WithoutObjects Void)
         constArrHelper = \case
              IntL i -> pure $ IntL i
              NumL d -> pure $ NumL d
              StringL s -> pure $ StringL s
              CharL c   -> pure $ CharL c
              ArrayL [] -> pure $ ConstArrayL []
              ArrayL (x:_) -> absurd x
              ObjectL _ [] -> error "Empty record inside ConstArrayL. We should forbid this somehow."
              ObjectL _ ((_,x):_) -> absurd x
              ConstArrayL lits -> do
                res <- traverse constArrHelper lits
                pure $ ConstArrayL res

         handleObjectLiteral :: [(PSString,ExpWithObjects)]
                             -> Either ExprConvertError ExpWithoutObjects
         handleObjectLiteral fs' = do
           let fs = sortOn fst fs'
               len = length fs
               tupTyName = mkTupleTyName len
               bareFields = snd <$> fs
           bareFields' <- traverse tryConvertExpr' bareFields
           let types' = expTy F <$> bareFields'
               types = types' <> [foldl' applyType (TyCon tupTyName) types']
               ctorType = foldr1 funTy types

               ctorExp :: ExpWithoutObjects
               ctorExp = V $ FVar ctorType $ properToIdent <$> tupTyName
           assembleDesugaredObjectLit ctorExp ctorType bareFields'


       goType :: SourceType -> Either ExprConvertError Ty
       goType = catchTE . tryConvertType

       updateBV ::  BVar SourceType -> Either ExprConvertError (BVar Ty)
       updateBV (BVar bvIx bvTy bvNm) = do
         bvTy' <- catchTE $ tryConvertType bvTy
         pure $ BVar bvIx bvTy' bvNm

       catchTE :: forall t. Either TypeConvertError t -> Either ExprConvertError t
       catchTE = first ((\x -> ExprConvertError f expression x "Failed to convert type") . Just)

       -- TODO/FIXME: We need to ensure that we don't introduce conflicts in the bound variables. Not sure what the best way is ATM

       -- I'm not really sure what the point of the copy fields is? TODO: Figure out what the point of them is
       desugarObjectUpdate :: SourceType
                           -> ExpWithObjects
                           -> Maybe [PSString]
                           -> [(PSString,ExpWithObjects)]
                           -> Either ExprConvertError ExpWithoutObjects
       desugarObjectUpdate _ e _ updateFields = do
         _fs <- case expTy F e of
                         RecordT fs -> pure fs
                         other -> error $ "ERROR: Record expression:\n  "
                                            <> prettyStr e
                                            <> "\n  should have a Record type, but instead has type:\n  "
                                            <> prettyStr other

         updateMap <- traverse tryConvertExpr' $ M.fromList updateFields
         updateTypes <-  traverse goType $ M.fromList $ second (expTy F) <$> updateFields
         origTypes <- traverse (goType . rowListType) (mkFieldMap _fs)
         let ts = updateTypes  `M.union` origTypes
             len = M.size ts
             tupTyName = mkTupleTyName len
             tupCtorName = coerceProperName <$> tupTyName
             types' = M.elems ts
             types = types' <> [foldl' applyType (TyCon tupTyName) types']
             ctorType = foldr1 funTy types

             positioned = zip (M.keys ts) [0..]

             withPositioned :: forall x. (PSString -> Int -> x) -> [x]
             withPositioned f' = uncurry f' <$> positioned

             argBndrTemplate = withPositioned $ \lbl i -> case M.lookup lbl updateMap of
               Nothing -> VarP . Ident $ "<UPD_" <> T.pack (show i) <> ">"
               Just _  -> WildP

             resultTemplate = withPositioned $ \lbl i -> case M.lookup lbl updateMap of
               Nothing ->
                 let nm = Ident $ "<UPD_" <> T.pack (show i) <> ">"
                 in V . B  $ BVar (M.findIndex lbl origTypes) (origTypes M.! lbl) nm
               Just expr -> F <$> expr

             ctorExp = V . F $  FVar ctorType $ properToIdent <$> tupTyName

             ctorBndr = ConP tupTyName tupCtorName argBndrTemplate

         resultExpr <- assembleDesugaredObjectLit ctorExp ctorType resultTemplate
         e' <- go f e
         let scoped = toScope resultExpr
             -- TODO/FIXME/REVIEW/HACK: Either remove the bindings Map from UnguardedAlt or actually construct a real one
             --                         (I think we never use it so it should be OK to remove?)
             altBranch = UnguardedAlt M.empty ctorBndr scoped
         pure $ CaseE ctorType e' [altBranch]

       desugarObjectAccessor :: SourceType -> PSString -> ExpWithObjects -> Either ExprConvertError ExpWithoutObjects
       desugarObjectAccessor _ lbl e = do
         traceM "desugarObjectAccesor"
         _fs <- case expTy F e of
                         RecordT fs -> pure fs
                         other -> error $ "ERROR: Record expression:\n  "
                                            <> prettyStr e
                                            <> "\n  should have a Record type, but instead has type:\n  "
                                            <> prettyStr other
         fs <- traverse (traverse goType) $  M.toList (rowListType <$> mkFieldMap _fs)
         let len = length fs
             tupTyName = mkTupleTyName len
             tupCtorName = coerceProperName <$> tupTyName
             types' = snd <$> fs
             dummyNm =  Ident "<ACCESSOR>"
             lblIx = fromJust $ elemIndex lbl (fst <$> fs) -- FIXME: fromJust
         traceM $ "TYPES: " <> show (prettyStr <$> types')

         let fieldTy = types' !! lblIx -- if it's not there *something* should have caught it by now
             argBndrTemplate = replicate len WildP & ix lblIx .~ VarP dummyNm
             ctorBndr = ConP tupTyName tupCtorName argBndrTemplate
             -- NOTE: `lblIx` is a placeholder for a better var ix
             rhs :: Exp WithoutObjects Ty (Var (BVar Ty) (FVar Ty))
             rhs = V . B $ BVar lblIx  fieldTy  dummyNm
             altBranch = UnguardedAlt M.empty ctorBndr (toScope rhs)
         e' <- tryConvertExpr' e
         pure $ CaseE fieldTy e' [altBranch]

assembleDesugaredObjectLit :: forall x a. Exp x Ty a  -> Ty -> [Exp x Ty a] -> Either ExprConvertError (Exp x Ty a)
assembleDesugaredObjectLit expr (_ :~> b) (arg:args) = assembleDesugaredObjectLit (AppE  expr arg) b args
assembleDesugaredObjectLit expr _ [] = pure expr -- TODO better error
assembleDesugaredObjectLit _ _ _ = error "something went wrong in assembleDesugaredObjectLit"

-- TODO/FIXME: Adapt this for use w/ the PIR Data declaration machinery (i.e. don't manually construct SOPs)

pattern ArrayCons :: Qualified Ident
pattern ArrayCons = Qualified (ByModuleName C.M_Prim) (Ident "Cons")

pattern ArrayNil :: Qualified Ident
pattern ArrayNil = Qualified (ByModuleName C.M_Prim) (Ident "Nil")

mkProdFields :: [t] -> [(Ident,t)]
mkProdFields = map (UnusedIdent,)

primData :: Datatypes Kind Ty
primData = tupleDatatypes <> Datatypes tDict cDict
  where
    tDict = M.fromList $ map (\x -> (x ^. dDataTyName,x))
        [ DataDecl Data C.Array [("a", KindType)] [
            CtorDecl ArrayNil [],
            CtorDecl ArrayCons $ mkProdFields [TyVar "a" KindType]
          ],

          DataDecl Data C.Boolean [] [
            CtorDecl (properToIdent <$> C.C_False) [],
            CtorDecl (properToIdent <$> C.C_True) []
          ]
        ]

    cDict :: Map (Qualified Ident) (Qualified (ProperName 'TypeName))
    cDict = M.fromList  [
        (ArrayCons, C.Array),
        (ArrayNil, C.Array),
        (properToIdent <$> C.C_True,C.Boolean),
        (properToIdent <$> C.C_False,C.Boolean)
      ]

tupleDatatypes :: Datatypes Kind Ty
tupleDatatypes = Datatypes (M.fromList tupleTypes) (M.fromList tupleCtors)
  where
    tupleTypes = flip map [1..100] $ \(n :: Int) ->
      let tyNm       = mkTupleTyName n
          ctorNm     = mkTupleCtorIdent n
          argKinds   = mkTupleArgKinds n
          ctorTvArgs = mkTupleCtorTvArgs n
      in (tyNm,DataDecl Data tyNm argKinds [CtorDecl ctorNm ctorTvArgs])

    tupleCtors = [1..100] <&> \x -> (mkTupleCtorIdent x, mkTupleTyName x)

    mkTupleCtorIdent :: Int -> Qualified Ident
    mkTupleCtorIdent n = properToIdent  <$> mkTupleTyName n

    vars :: Int -> [Text]
    vars n = map (\x ->  "t" <> T.pack (show x)) [1..n]

    mkTupleArgKinds = fmap (,KindType) .  vars

    mkTupleCtorTvArgs = mkProdFields . map (flip TyVar KindType) . vars

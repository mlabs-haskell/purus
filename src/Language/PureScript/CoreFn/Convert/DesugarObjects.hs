{-# OPTIONS_GHC -Wno-orphans #-} -- has to be here (more or less)
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Language.PureScript.CoreFn.Convert.DesugarObjects where

import Prelude
import Language.PureScript.CoreFn.Expr
    ( _Var,
      eType,
      exprType,
      Bind(..),
      CaseAlternative(CaseAlternative),
      Expr(..) )
import Language.PureScript.Names (Ident(..), Qualified (..), QualifiedBy (..), pattern ByNullSourcePos, ProperNameType (..), ProperName(..), moduleNameFromString, coerceProperName, disqualify, ModuleName (..), runModuleName)
import Language.PureScript.Types
    ( SourceType, Type(..), srcTypeConstructor, srcTypeApp, RowListItem (rowListType), rowToList, eqType )
import Language.PureScript.Environment (pattern (:->), pattern RecordT, function, kindType, DataDeclType (Data))
import Language.PureScript.CoreFn.Pretty
    ( prettyTypeStr, renderExprStr )
import Language.PureScript.CoreFn.Ann (Ann)
import Language.PureScript.CoreFn.FromJSON ()
import Data.Text qualified as T
import Data.List (find, elemIndex, sortOn, foldl')
import Language.PureScript.AST.Literals (Literal(..))
import Data.Map qualified as M
import Language.PureScript.PSString (PSString)
import Language.PureScript.AST.SourcePos
    ( pattern NullSourceAnn )
import Control.Lens.IndexedPlated
import Control.Lens ( ix )
import Language.PureScript.CoreFn.Convert.Monomorphize
    ( nullAnn, mkFieldMap, decodeModuleIO, MonoError (..), monomorphizeExpr )
import Data.Text (Text)
import Bound
import Data.Bifunctor (Bifunctor(bimap, first, second))
import Control.Lens.Combinators (to)
import Language.PureScript.CoreFn (Binder(..), Module (..))
import Data.Maybe (mapMaybe)
import Control.Lens.Operators
import Control.Lens (view)
import Control.Lens.Tuple
import Language.PureScript.CoreFn.Convert.IR hiding (stripQuantifiers)
import Language.PureScript.CoreFn.Convert.Plated
import Control.Exception (throwIO)
import Debug.Trace
import Data.List (findIndex)
import Control.Monad (join, foldM)
import Language.PureScript.AST.Declarations (DataConstructorDeclaration (..))
import Data.Map (Map)
import Language.PureScript.Constants.Prim qualified as C

test :: FilePath -> Text -> IO (Exp FVar)
test path decl = do
  myMod <- decodeModuleIO path
  case monomorphizeExpr myMod decl of
    Left (MonoError _ msg ) -> throwIO $ userError $ "Couldn't monomorphize " <> T.unpack decl <> "\nReason:\n" <> msg
    Right body -> case tryConvertExpr body of
      Left convertErr -> throwIO $ userError convertErr
      Right e -> do
        putStrLn (ppExp e)
        pure e

prepPIR :: FilePath
        -> Text
        -> IO (Exp FVar, Map (ProperName 'TypeName) (DataDeclType, [(Text, Maybe SourceType)], [DataConstructorDeclaration]))
prepPIR path  decl = do
  myMod@Module{..} <- decodeModuleIO path
  case monomorphizeExpr myMod decl of
    Left (MonoError _ msg ) ->
      throwIO
      $ userError
      $ "Couldn't monomorphize "
        <> T.unpack (runModuleName moduleName <> ".main") <> "\nReason:\n" <> msg
    Right body -> case tryConvertExpr body of
      Left convertErr -> throwIO $ userError convertErr
      Right e -> do
        putStrLn (ppExp e)
        pure (e,moduleDataTypes)

-- This gives us a way to report the exact location of the error (which may no longer correspond *at all* to
-- the location given in the SourcePos annotations due to inlining and monomorphization)
data TypeConvertError
 = TypeConvertError (SourceType -> SourceType ) SourceType String

type TyConvertM = Either TypeConvertError

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
                fakeTName = mkFakeTName arity
                types = M.elems fields
                -- types = types' <> [foldl' srcTypeApp (srcTypeConstructor fakeTName) types'] -- the types applied to the ctor
                ctorType = foldl' srcTypeApp (srcTypeConstructor fakeTName) types
            go f ctorType
          else Left $ TypeConvertError f t $ prettyTypeStr fs <> " is not a closed row. Last: " <> prettyTypeStr (rowLast fs)
      TypeVar _ txt -> Right $ TyVar txt
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
        let khole = f . (\x -> ForAll ann vis var (Just x) inner skol)
            ihole = f . (\x -> ForAll ann vis var mbk x skol)
        mbk' <- case mbk of
                  Nothing -> pure Nothing
                  Just k -> Just <$> go khole k
        inner' <- go ihole inner
        pure $ Forall vis var mbk' inner' skol
      KindedType ann t1 t2 -> do
        t2' <- go (f . KindedType ann t1) t2
        t1' <- go (f . (\x -> KindedType ann x t2)) t1
        pure $ KType t1' t2'

      other -> Left $ TypeConvertError f other $ "Unsupported type: " <> prettyTypeStr other

data ExprConvertError
  = ExprConvertError (Expr Ann -> Expr Ann) (Expr Ann) (Maybe TypeConvertError) String

type ConvertM = Either ExprConvertError

prettyErrorT :: TypeConvertError -> String
prettyErrorT (TypeConvertError g t msg1)
  = "Error when converting types to final IR: " <> msg1
     <> "\nin type:\n  " <>  prettyTypeStr (g $ TypeVar NullSourceAnn "<ERROR HERE>")
     <> "\nin type component:\n  " <> prettyTypeStr t

prettyError :: ExprConvertError -> String
prettyError = \case
  ExprConvertError f e Nothing msg -> "Error when converting expression: " <> msg <> "\n  "
                                  <> renderExprStr (f $ fakeVar "<ERROR_HERE>")
                                  <> "\nin subexpression:\n  "
                                  <> renderExprStr e
  ExprConvertError f e (Just (TypeConvertError g t msg1)) msg2 -> "Error when converting types: " <>  msg1 <> "\n" <> msg2 <> "\n  "
                                    <> renderExprStr (f $ fakeVar "<ERROR_HERE>")
                                    <> "\nin subexpression:\n  "
                                    <> renderExprStr e
                                    <> "\nin type:\n  "
                                    <> prettyTypeStr (g $ TypeVar NullSourceAnn "<ERROR HERE>")
                                    <> "\nin type component:\n  "
                                    <> prettyTypeStr t
 where
   fakeVar :: Text -> Expr Ann
   fakeVar t = Var nullAnn (srcTypeConstructor $ Qualified ByNullSourcePos (ProperName "ERROR!")) (Qualified ByNullSourcePos (Ident t))

tryConvertExprIO :: Expr Ann -> IO ()
tryConvertExprIO = putStrLn . either id ppExp . tryConvertExpr

tryConvertExpr :: Expr Ann -> Either String (Exp FVar)
tryConvertExpr = first prettyError . tryConvertExpr'

tryConvertExpr' :: Expr Ann -> Either ExprConvertError (Exp FVar)
tryConvertExpr' = go id
  where
    go :: (Expr Ann -> Expr Ann) -> Expr Ann -> Either ExprConvertError (Exp FVar)
    go f expression = case expression of
      Literal ann ty lit -> do
        let lhole = f . Literal ann ty . ArrayLiteral . pure
        ty' <- goType ty
        tryConvertLit lhole lit >>= \case
          Left desObj -> pure desObj
          Right lit' -> pure $ LitE ty' lit'
      Constructor _ ty tn cn fs -> do
        ty' <- goType ty
        pure $ CtorE ty' tn cn fs
      Abs ann ty ident e -> do
        ty' <- goType ty
        ex <- go (f . Abs ann ty ident) e
        let expr = abstract (matchVar (headArg ty') ident) ex
        pure $ LamE ty' (BVar 0 (headArg ty') ident) expr
      App ann ty e1 e2 -> do
        ty' <- goType ty
        e2' <- go (f . App ann ty e1) e2
        e1' <- go (f . (\x -> App ann ty x e2)) e1
        pure $ AppE ty' e1' e2'
      Case ann ty scrutinees alts -> do
        ty' <- goType ty
        scrutinees' <- goList (f . (\x -> Case ann ty [x] alts)) scrutinees
        alts' <- traverse (goAlt (f . Case ann ty scrutinees . pure)) alts
        pure $ CaseE ty' scrutinees' alts'
      Let ann ty binds e -> do
        ty' <- goType ty
        rawBinds <- goBinds (f . (\x -> Let ann ty [x] e)) binds
        e' <- go (f . Let ann ty binds) e
        let allBoundIdents = fst <$> join rawBinds
            abstr = abstract (abstractMany allBoundIdents)
            bindEs = assembleBindEs [] rawBinds
            bindings = mkBindings allBoundIdents
        pure $ LetE ty' bindings  bindEs (abstr e')
      xp@(Accessor _ ty lbl e) -> case desugarObjectAccessor ty lbl e of
        Nothing -> Left $ ExprConvertError f xp Nothing "Failed to desugar Accessor"
        Just desugE -> go f desugE
      upd@(ObjectUpdate _ ty orig copF updF) -> case desugarObjectUpdate ty orig copF updF of
        Nothing -> Left $ ExprConvertError f upd Nothing "Failed to desugar ObjectUpdate"
        Just desugE -> go f desugE
      Var _ ty (Qualified _ nm) -> do
        ty' <- goType ty
        pure . V $ FVar ty' nm
     where
       -- REVIEW: This *MIGHT* not be right. I'm not 1000% sure what the PS criteria for a mutually rec group are
       --         First arg threads the FVars that correspond to the already-processed binds
       --         through the rest of the conversion. I think that's right - earlier bindings
       --         should be available to later bindings
       assembleBindEs :: [FVar] -> [[(FVar,Exp FVar)]] -> [BindE Exp FVar]
       assembleBindEs _ [] = []
       assembleBindEs dict ([]:rest) = assembleBindEs dict rest -- shouldn't happen but w/e
       assembleBindEs dict ([(fv@(FVar tx ix),e)]:rest) =
         let dict' = fv:dict
             abstr = abstract (abstractMany dict')
         in NonRecursive ix (abstr e) : assembleBindEs dict' rest
       assembleBindEs dict (xsRec:rest) = case assembleRec dict xsRec of
         (dict',recb) -> recb : assembleBindEs dict' rest

       assembleRec :: [FVar] -> [(FVar, Exp FVar)] -> ([FVar], BindE Exp FVar)
       assembleRec dict xs =
         let dict' = dict <> (fst <$> xs)
             abstr = abstract (abstractMany dict')
             recBind = Recursive
                       . map (uncurry $ \(FVar _ ixx) xp -> (ixx,abstr xp))
                       $ xs
         in (dict',recBind)


       mkBindings :: [FVar] -> Bindings
       mkBindings = M.fromList . zip [0..]

       abstractMany :: [FVar] -> FVar -> Maybe BVar
       abstractMany xs (FVar t i) =
         (\indX -> BVar indX t i)
         <$> findIndex (\(FVar t' i') -> t == t' && i == i') xs

       goAlt :: (CaseAlternative Ann -> Expr Ann) -> CaseAlternative Ann -> Either ExprConvertError (Alt Exp FVar)
       goAlt g (CaseAlternative binders result) = do
         boundVars <- concat <$> traverse (getBoundVar result) binders
         pats <- traverse toPat  binders
         let resultF = g . CaseAlternative binders
             abstrE = abstract (abstractMany boundVars)
         goResult resultF result >>= \case
           Left ges -> pure $ GuardedAlt (mkBindings boundVars) pats (bimap abstrE abstrE <$> ges)
           Right re -> pure $ UnguardedAlt (mkBindings boundVars) pats (abstrE re)
        where
          getBoundVar :: Either [(Expr Ann, Expr Ann)] (Expr Ann) -> Binder Ann -> ConvertM [FVar]
          getBoundVar body b = case b of
            ConstructorBinder _ _ _ bs -> concat <$> traverse (getBoundVar body) bs
            LiteralBinder _ (ArrayLiteral xs) -> concat <$> traverse (getBoundVar body) xs
            LiteralBinder _ (ObjectLiteral fs) -> concat <$> traverse (getBoundVar body . snd) fs
            VarBinder _ nm -> case body of
              Right exbody -> case findBoundVar nm exbody of
                Nothing -> pure [] -- probably should trace or warn at least
                Just (t,_) -> do
                  ty' <- goType t
                  pure  [FVar ty' nm]
              Left fml -> do
                let allResults = concatMap (\(x,y) -> [x,y]) fml
                    matchingVar = mapMaybe (findBoundVar nm) allResults
                case matchingVar of
                  ((t,_):_) -> do
                    ty' <- goType t
                    pure [FVar ty' nm]
                  _ -> pure []
            _ -> pure []

          toPat :: Binder Ann -> ConvertM (Pat Exp FVar)
          toPat = \case
            NullBinder _ -> pure WildP
            VarBinder _ i  ->  pure $ VarP i
            ConstructorBinder _ tn cn bs -> ConP tn cn <$> traverse toPat bs
            NamedBinder _ nm b ->  AsP nm <$> toPat b
            LiteralBinder _ lp -> case lp of
              NumericLiteral (Left i) -> pure . LitP .  IntL $ i
              NumericLiteral (Right d) -> pure . LitP .  NumL $ d
              StringLiteral pss -> pure . LitP . StringL $ pss
              CharLiteral c -> pure . LitP . CharL $ c
              BooleanLiteral b -> pure . LitP . BoolL $ b
              ArrayLiteral as ->  LitP . ArrayL <$>  traverse  toPat as
              ObjectLiteral fs' -> do
                -- this isn't right, we need to make sure the positions of the binders are correct,
                -- since (I think?) you can use an Obj binder w/o using all of the fields
                let fs = sortOn fst fs'
                    len = length fs
                    fakeCName = mkFakeCName len
                    fakeTName = mkFakeTName len
                inner <- traverse (toPat . snd) fs
                pure $ ConP fakeTName fakeCName inner

          goResult :: (Either [(Expr Ann, Expr Ann)] (Expr Ann) -> Expr Ann)
                   -> Either [(Expr Ann, Expr Ann)] (Expr Ann)
                   -> Either ExprConvertError (Either [(Exp FVar, Exp FVar)] (Exp FVar))
          goResult h = \case
            Left exs -> do
              exs' <- traverse (goGuarded (h . Left)) exs
              pure (Left exs')
            Right ex -> do
              ex' <- go (h . Right) ex
              pure (Right ex')
           where
             goGuarded cb (e1,e2) = do
               e1' <- go (\x -> cb [(x,e2)]) e1
               e2' <- go (\x -> cb [(e1,x)]) e2
               pure (e1',e2')

       goBinds :: (Bind Ann -> Expr Ann) -> [Bind Ann] -> Either ExprConvertError [[(FVar, Exp FVar)]]
       goBinds _ [] = pure []
       goBinds g (b:bs) = case b of
         NonRec ann ident expr -> do
           ty' <- goType (exprType expr)
           e' <- go (g . NonRec ann ident ) expr
           rest <- goBinds g bs
           pure $ [(FVar ty' ident,e')] : rest
         -- TODO: Fix this to preserve recursivity (requires modifying the *LET* ctor of Exp)
         Rec _xs -> do
           let xs = map (\((ann,nm),e) -> NonRec ann nm e) _xs
           xs' <- goBinds g xs
           rest <- goBinds g bs
           pure $ xs' <> rest

       allVars :: Expr Ann -> [(SourceType,Qualified Ident)]
       allVars ex = ex ^.. icosmos @Context @(Expr Ann) M.empty . _Var . to (\(_,b,c) -> (b,c))

       findBoundVar :: Ident -> Expr Ann -> Maybe (SourceType, Qualified Ident)
       findBoundVar nm ex = find (goFind . snd) (allVars ex)
         where
           goFind = \case
             Qualified (ByModuleName _) _ -> False
             Qualified ByNullSourcePos _ -> False -- idk about this actually, guess we'll find out
             Qualified (BySourcePos _) nm' -> nm == nm'

       goList :: (Expr Ann -> Expr Ann) -> [Expr Ann] -> Either ExprConvertError [Exp FVar]
       goList _ [] = pure []
       goList g (ex:exs) = do
         e' <- go g ex
         es' <- goList g exs
         pure $ e' : es'

       -- ONLY USE THIS IN LAMBDA ABSTRACTIONS!
       matchVar :: Ty -> Ident -> FVar -> Maybe BVar
       matchVar t nm (FVar ty n')
         | ty == t && nm == n' =  Just (BVar 0 t nm)
         | otherwise = Nothing

       tryConvertLit :: (Expr Ann -> Expr Ann) -> Literal (Expr Ann) -> Either ExprConvertError (Either (Exp FVar) (Lit (Exp FVar)))
       tryConvertLit cb = \case
         NumericLiteral (Left i) -> pure . Right $ IntL i
         NumericLiteral (Right d) -> pure . Right $ NumL d
         StringLiteral pss -> pure . Right $ StringL pss
         CharLiteral c     -> pure . Right $ CharL c
         BooleanLiteral b  -> pure . Right $ BoolL b
         ArrayLiteral xs   -> Right . ArrayL <$> traverse (go cb) xs
         ObjectLiteral fs'  -> do -- TODO Maybe check for empty? I think that's a legal expr?
           let fs = sortOn fst fs'
               len = length fs
               fakeCName = mkFakeCName len
               fakeTName = mkFakeTName len
               bareFields = snd <$> fs
               types' = exprType <$> bareFields
               types = types' <> [foldl' srcTypeApp (srcTypeConstructor fakeTName) types']
               ctorType = foldr1 function types
               ctorExp = Constructor nullAnn ctorType (disqualify fakeTName) (disqualify fakeCName) []
           ctor <-  assembleDesugaredObjectLit ctorExp ctorType bareFields
           Left <$> go cb ctor

       goType :: SourceType -> Either ExprConvertError Ty
       goType = catchTE . tryConvertType

       catchTE :: forall t. Either TypeConvertError t -> Either ExprConvertError t
       catchTE = first ((\x -> ExprConvertError f expression x "Failed to convert type") . Just)

assembleDesugaredObjectLit :: Expr Ann -> SourceType -> [Expr Ann] -> Either ExprConvertError (Expr Ann)
assembleDesugaredObjectLit expr (_ :-> b) (arg:args) = assembleDesugaredObjectLit (App nullAnn b expr arg) b args
assembleDesugaredObjectLit expr _ [] = pure expr -- TODO better error
assembleDesugaredObjectLit _ _ _ = error "something went wrong in assembleDesugaredObjectLit"

desugarObjectAccessor :: SourceType -> PSString -> Expr Ann -> Maybe (Expr Ann)
desugarObjectAccessor _ lbl e = do
  traceM "desugarObjectAccesor"
  RecordT _fs <- pure $ exprType e -- TODO check that it's actually a closed record?
  let fs = M.toList (rowListType <$> mkFieldMap _fs)
      len = length fs
      fakeCName = mkFakeCName len
      fakeTName = mkFakeTName len
      types' = snd <$> fs
      dummyNm =  Ident "<ACCESSOR>"
  lblIx <-  elemIndex lbl (fst <$> fs)
  traceM $ "TYPES: " <> show (prettyTypeStr <$> types')
  let fieldTy = types' !! lblIx -- if it's not there *something* should have caught it by now
  let argBndrTemplate = replicate len (NullBinder nullAnn) & ix lblIx .~ VarBinder nullAnn dummyNm
      ctorBndr = ConstructorBinder nullAnn fakeTName fakeCName argBndrTemplate
      rhs = Var nullAnn fieldTy (Qualified ByNullSourcePos dummyNm)
      altBranch = CaseAlternative [ctorBndr] $ Right rhs
  -- the actual expression should get desugared after this (i hope?)
  pure $ Case nullAnn fieldTy [e] [altBranch]
      -- ctorBndr = ConstructorBinder

-- I'm not really sure what the point of the copy fields is? TODO: Figure out what the point of them is
desugarObjectUpdate :: SourceType -> Expr Ann -> Maybe [PSString] -> [(PSString,Expr Ann)] -> Maybe (Expr Ann)
desugarObjectUpdate _ e _ updateFields = do
  RecordT _fs <- pure $ exprType e
  let updateMap = M.fromList updateFields
      updateTypes = M.fromList $ second exprType <$> updateFields
      origTypes = rowListType <$> mkFieldMap _fs
      ts = updateTypes  `M.union` origTypes
      len = M.size ts
      fakeCName = mkFakeCName len
      fakeTName = mkFakeTName len
      types' = M.elems ts
      types = types' <> [foldl' srcTypeApp (srcTypeConstructor fakeTName) types']
      ctorType = foldr1 function types

      positioned = zip (M.keys ts) [0..]

      withPositioned :: forall x. (PSString -> Int -> x) -> [x]
      withPositioned f = uncurry f <$> positioned

      argBndrTemplate = withPositioned $ \lbl i -> case M.lookup lbl updateMap of
        Nothing -> VarBinder nullAnn . Ident $ "<UPD_" <> T.pack (show i) <> ">"
        Just _  -> NullBinder nullAnn

      resultTemplate = withPositioned $ \lbl i -> case M.lookup lbl updateMap of
        Nothing ->
          let nm = Ident $ "<UPD_" <> T.pack (show i) <> ">"
          in Var nullAnn (origTypes M.! lbl) (Qualified ByNullSourcePos nm)
        Just expr -> expr

      ctorExp = Constructor nullAnn ctorType (disqualify fakeTName) (disqualify fakeCName) []

      resultExpr = assembleDesugaredObjectLit ctorExp ctorType resultTemplate

      ctorBndr = ConstructorBinder nullAnn fakeTName fakeCName argBndrTemplate
  case resultExpr of
    Left err -> error $ prettyError err
    Right res -> do
      let altBranch = CaseAlternative [ctorBndr] $ Right res
      pure $ Case nullAnn ctorType [e] [altBranch]

isClosedRow :: SourceType -> Bool
isClosedRow t = case rowToList t of
  (_,REmpty{}) -> True
  (_,KindApp _ REmpty{} k) | eqType k kindType -> True
  _ -> False

rowLast :: SourceType -> SourceType
rowLast t = case rowToList t of
  (_,r) -> r

mkFakeCName :: Int -> Qualified (ProperName 'ConstructorName)
mkFakeCName x = Qualified (ByModuleName $ moduleNameFromString "$GEN") (ProperName $ "~TUPLE_" <> T.pack (show x))

mkFakeTName :: Int -> Qualified (ProperName 'TypeName)
mkFakeTName x = case mkFakeCName x of
  Qualified qb n -> Qualified qb $ coerceProperName @_ @'TypeName n

allTypes :: Expr Ann -> [SourceType]
allTypes e = e ^.. icosmos @Context @(Expr Ann) M.empty . eType

-- TODO: Fuck these tuples, make real data types when more time

mkTupleCtorData :: Int -> (ProperName 'ConstructorName,(ProperName 'TypeName,Int,[Ty]))
mkTupleCtorData n | n <= 0 = error "Don't try to make a 0-tuple"
mkTupleCtorData n = (cn,(tn,n,tys))
  where
    cn = disqualify . mkFakeCName $ n
    tn = disqualify . mkFakeTName $ n
    tys = TyVar . ("~TUPLE_ARG_" <>) . T.pack . show <$> [1..n]

_100TupleCtors :: CtorDict
_100TupleCtors = M.fromList $ mkTupleCtorData <$> [1..100]

-- Don't normally like type syns in contexts like this but hlint will probably make this unreadable w/o them
type CtorDict = Map (ProperName 'ConstructorName) (ProperName 'TypeName,Int,[Ty])
mkConstructorMap :: Map (ProperName 'TypeName) (DataDeclType,[(Text, Maybe SourceType)],[DataConstructorDeclaration])
                 -> TyConvertM CtorDict
mkConstructorMap decls = M.union _100TupleCtors <$>  foldM go M.empty (M.toList decls)
  where
    go :: Map (ProperName 'ConstructorName) (ProperName 'TypeName, Int, [Ty])
          -> (ProperName 'TypeName, (DataDeclType, [(Text, Maybe SourceType)],[DataConstructorDeclaration]))
          -> TyConvertM (Map (ProperName 'ConstructorName) (ProperName 'TypeName, Int, [Ty]))
    go acc (tyNm,(declTy,tyArgs,ctorDatas)) = do
      ctors <- traverse extractCTorData ctorDatas
      let indexedCTors = mkIndex <$> ctors
      pure $ foldl' (\acc' (a,b,c,d) -> M.insert a (b,c,d) acc') acc indexedCTors
     where
       extractCTorData :: DataConstructorDeclaration -> TyConvertM (ProperName 'ConstructorName,ProperName 'TypeName,[Ty])
       extractCTorData (DataConstructorDeclaration _ ctorNm ctorFields) = do
         fields' <- traverse (tryConvertType . snd) ctorFields
         pure $ (ctorNm,tyNm,fields')
       mkIndex :: (ProperName 'ConstructorName, ProperName 'TypeName, [Ty])
               -> (ProperName 'ConstructorName, ProperName 'TypeName, Int,[Ty])
       mkIndex (cn,tn,fs) = case findIndex (\DataConstructorDeclaration{..} -> dataCtorName == cn) ctorDatas of
         Nothing -> error "couldn't find ctor name (impossible)"
         Just i  ->  (cn,tn,i,fs)

lookupSOP :: ProperName 'TypeName -> TyConDict -> Maybe [(Int,[Ty])]
lookupSOP nm dict = view _3 <$> M.lookup nm dict

lookupArgs :: ProperName 'TypeName -> TyConDict -> Maybe [(Text,Maybe Ty)]
lookupArgs nm dict = view _2 <$> M.lookup nm dict

mkTupleTyConData :: Int -> (ProperName 'TypeName,(DataDeclType,[(Text,Maybe Ty)],[(Int,[Ty])]))
mkTupleTyConData n = (tn,(Data,args,indices))
  where
    tn = disqualify $ mkFakeTName n
    vars = [1..n] <&> \x -> "~TUPLE_ARG_" <> T.pack (show x)
    args = zip vars (replicate n (Just (TyCon C.Type)))
    qualifier = Qualified (ByModuleName $ ModuleName "$GEN") . ProperName
    indices = [(0,TyCon . qualifier  <$> vars)]

_100TupleTyCons :: TyConDict
_100TupleTyCons = M.fromList $ mkTupleTyConData <$> [1..100]

type TyConDict = (Map (ProperName 'TypeName) (DataDeclType,[(Text,Maybe Ty)],[(Int,[Ty])]))
mkTyConMap :: Map (ProperName 'TypeName) (DataDeclType,[(Text, Maybe SourceType)],[DataConstructorDeclaration])
           -> TyConvertM (Map (ProperName 'TypeName) (DataDeclType,[(Text,Maybe Ty)],[(Int,[Ty])]))
mkTyConMap decls = M.union _100TupleTyCons <$> foldM go M.empty (M.toList decls)
  where
    go :: Map (ProperName 'TypeName) (DataDeclType,[(Text,Maybe Ty)],[(Int,[Ty])])
       -> (ProperName 'TypeName, (DataDeclType, [(Text, Maybe SourceType)],[DataConstructorDeclaration]))
       -> TyConvertM (Map (ProperName 'TypeName) (DataDeclType,[(Text,Maybe Ty)],[(Int,[Ty])]))
    go acc (tn,(declTy,tyArgs,ctorDatas)) = do
      tyArgs' <- (traverse . traverse . traverse) tryConvertType tyArgs
      indexedProducts <- zip [0..] <$> traverse (traverse (tryConvertType . snd) . dataCtorFields) ctorDatas
      pure $ M.insert tn (declTy,tyArgs',indexedProducts) acc

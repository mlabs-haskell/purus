{-# LANGUAGE GADTs, PolyKinds, TemplateHaskell #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Language.PureScript.CoreFn.Convert.Datatypes where

import Prelude

import Language.PureScript.CoreFn.Convert.IR
    ( ppTy,
      Alt(..),
      BVar(BVar),
      Exp(..),
      FVar(FVar),
      Ty(..), Pat (..), Lit (..), pattern (:~>), getPat, expTy, expTy', BindE (..), unsafeAnalyzeApp )
import Language.PureScript.CoreFn.Convert.IR qualified as IR
import PlutusIR
    ( Type(TyBuiltin, TyForall),
      VarDecl(VarDecl),
      TyVarDecl(TyVarDecl),
      TyName,
      Name(Name) )
import PlutusIR qualified as PIR
import PlutusCore qualified as PLC

import Control.Lens
    ( (^.), (^?), ix, to, cosmos, (^..), over, view, makeLenses, _1 )
import Language.PureScript.Names
    ( ProperNameType(..),
      Qualified(..),
      ProperName(..),
      Ident (..),
      pattern ByThisModuleName,
      showQualified,
      showIdent,
      disqualify, runIdent, QualifiedBy (ByModuleName) )
import Bound.Var ( Var )


import Data.Text (Text)
import Data.Text qualified as T
import Data.Map (Map)
import Data.Map qualified as M
import Data.Set qualified as S

import Language.PureScript.CoreFn.Module
    ( cdCtorName,
      cdCtorFields,
      dDataCtors,
      dDataArgs,
      lookupDataDecl,
      CtorDecl (..),
      Datatypes, tyDict, getAllConstructorDecls, getConstructorIndexAndDecl, lookupCtorType )
import Language.PureScript.Environment ( pattern (:->), mkTupleTyName )
import Language.PureScript.Types
    ( Type(TypeConstructor), SourceType, TypeVarVisibility (TypeVarVisible) )
import Language.PureScript.Constants.Prim qualified as C
import Language.PureScript.Constants.Purus qualified as C
import Language.PureScript.CoreFn.Convert.DesugarCore
    ( WithoutObjects, matchVarLamAbs, Vars )
import Language.PureScript.CoreFn.Convert.Monomorphize.Utils
    ( unsafeApply, isConstructor )
import Language.PureScript.CoreFn.Pretty (prettyTypeStr)
import Data.Kind qualified as GHC
import Bound.Scope
    ( bindings,
      instantiate,
      toScope,
      fromScope,
      abstract,
      mapBound,
      Scope )
import Control.Monad.State ( gets, runState, modify, State )
import Control.Monad.Except
    ( MonadError(throwError), runExceptT, ExceptT, liftEither )
import Data.Foldable (traverse_, foldl',foldrM)
import Language.PureScript.CoreFn.TypeLike
import Language.PureScript.CoreFn.Convert.DesugarObjects
import Data.Maybe (fromJust, isJust)
import Debug.Trace
import Bound (Var(..))
import Prettyprinter (Pretty (..))
import Control.Monad (join)
import Control.Lens.Plated (transformM)
import PlutusCore.Name (Unique(Unique))
import Data.List (sortOn)
import Data.Bifunctor (second)
import Language.PureScript.CoreFn.Convert.Debug
import System.Random (mkStdGen,randomR)
import Control.Lens.Combinators (transform, preview)
import Data.Traversable (for)


foldr1Err :: Foldable t => String -> (a -> a -> a) -> t a -> a
foldr1Err msg f ta
 | null ta = error msg
 | otherwise = foldr1 f ta 

{- Monad for performing operations with datatypes. Supports limited TyVar binding
   operations for operating on type variables in scoped datatype declarations or
   types.
-}
type DatatypeM  = ExceptT String (State DatatypeDictionary)

type PIRDatatype = PIR.Datatype
                     PIR.TyName
                     PIR.Name
                     PLC.DefaultUni
                     ()

type PIRType = PIR.Type PIR.TyName PLC.DefaultUni ()

type PIRTerm = PIR.Term PIR.TyName PIR.Name PLC.DefaultUni PLC.DefaultFun ()

-- TODO/FIXME: We need a state field for Type Variable bindings
--             which is distinct from tyNames in order to ensure
--             correct Unique assignments (& kinds) for locally-scoped TyVars
--             in datatype declarations
--
-- TODO: Rename this since we use it in PIR conversion too
data DatatypeDictionary = DatatypeDictionary {
    -- | The datatype declarations & their corresponding PS type name
    _pirDatatypes :: Map (Qualified (ProperName 'TypeName)) PIRDatatype,
    -- | Map from PS Constructors (free variables) to PLC Names (w/ a unique) & constructor indices
    _constrNames  :: Map (Qualified Ident) (PIR.Name,Int),
    -- | Map from PS Type names to PLC Type Names (w/ a unique)
    _tyNames      :: Map (Qualified (ProperName 'TypeName)) PIR.TyName,
    -- | Locally bound type variables, to be used with `withLocalTyVars`
    _tyVars       :: Map Text PIR.TyName,
    -- | Locally bound variables. This is only used when we need to introduce new variables during conversion
    _vars         :: Map Text PIR.Name,
    -- | Map from a PS type name to the name of the case destructor
    _destructors  :: Map (Qualified (ProperName 'TypeName)) PIR.Name,
    -- | A counter. We have to assign uniques to PS names and this lets us ensure we do so correctly & w/o clashes
    _counter      :: Int
}

-- jfc why didn't i makeLenses everywhere
makeLenses ''DatatypeDictionary

pseudoRandomChar :: Int -> Char
pseudoRandomChar i = fst $ randomR ('a','z') (mkStdGen i)

next :: DatatypeM Int
next = do
  n <- gets (view counter)
  modify $ over counter (+ 1)
  pure n

mkTyName :: Qualified (ProperName 'TypeName) -> DatatypeM PIR.TyName
mkTyName qn = doTraceM "mkTyName" (prettyQPN qn) >> gets (view tyNames) >>= \tnames -> case M.lookup qn tnames of
  Just tyname -> pure tyname
  Nothing     -> do
    uniq <- next
    let tyname = PIR.TyName $ Name (runProperName . disqualify $ qn) $ PLC.Unique uniq
    modify $ over tyNames (M.insert qn tyname)
    pure tyname

mkConstrName :: Qualified Ident -> Int -> DatatypeM PIR.Name
mkConstrName qi cix = doTraceM "mkConstrName" (prettyQI qi) >> gets (view constrNames) >>= \cnames -> case M.lookup qi cnames of
  Just cname -> pure $ fst cname
  Nothing -> do
    uniq <- next
    let nm = Name (showIdent . disqualify $ qi) $ PLC.Unique uniq
    modify $ over constrNames (M.insert qi (nm,cix))
    pure nm

-- | Only gives you a TyName, doesn't insert anything into the context
mkNewTyVar :: Text -> DatatypeM TyName
mkNewTyVar nm = doTraceM "mkNewTyVar" (T.unpack nm) >> do
  uniq <- next
  pure .  PIR.TyName $ PIR.Name nm $ PLC.Unique uniq

-- | Only gives you a Name, doesn't insert anything into the context
mkNewVar :: Text -> DatatypeM PIR.Name
mkNewVar nm = doTraceM "mkNewVar" (T.unpack nm) >> PIR.Name nm . PLC.Unique <$> next

freshName :: DatatypeM PIR.Name
freshName = do
  uniq <- next
  let c = pseudoRandomChar uniq
  let nm = T.pack (c : '#' : show uniq)
  pure $ PIR.Name nm (PLC.Unique uniq)

mkVar :: Text -> DatatypeM PIR.Name
mkVar nm = doTraceM "mkVar" (T.unpack nm) >> gets (view vars) >>= \names -> case M.lookup nm names of
  Nothing -> do
     var <- mkNewVar nm
     modify $ over vars (M.insert nm var)
     pure var
  Just var -> pure var

getBoundTyVarName :: Text -> DatatypeM PIR.TyName
getBoundTyVarName nm = doTraceM "mkBoundTyVarName" (T.unpack nm) >>  do
  boundTyVars <- gets _tyVars
  case M.lookup nm boundTyVars of
    Just tyName -> pure  tyName
    Nothing -> error $ "Free type variable in IR: " <> T.unpack nm

-- Sometimes (e.g. when typing lambdas) we have to branch on whether the tv is already bound
lookupTyVar :: Text -> DatatypeM (Maybe PIR.TyName)
lookupTyVar nm = gets (preview (tyVars . ix nm))



bindTV :: Text -> PIR.TyName -> DatatypeM ()
bindTV txt nm = modify $ over tyVars (M.insert txt nm)


insertMany :: forall k v. Ord k => [(k,v)] -> Map k v -> Map k v
insertMany new acc = foldl' (flip $ uncurry M.insert) acc new

deleteMany :: forall k v. Ord k => [k] -> Map k v -> Map k v
deleteMany xs acc = foldl' (flip M.delete) acc xs

note :: String -> Maybe a -> DatatypeM a
note msg = maybe (throwError msg) pure

mkTypeBindDict :: Datatypes IR.Kind Ty
               -> Exp WithoutObjects Ty (Var (BVar Ty) (FVar Ty))
               -> Either String DatatypeDictionary
mkTypeBindDict _datatypes main = doTraceM "mkTypeBindDict" (prettyStr main) >>  case runState act initState of
   (Left err,_) -> Left err
   (Right _,res) -> pure res
  where
    datatypes = primData <> _datatypes
    tuples = S.fromList $ map mkTupleTyName [1..10]
    act = runExceptT $ mkPIRDatatypes datatypes (allTypeConstructors main <> tuples)
    initState = DatatypeDictionary M.empty M.empty M.empty M.empty M.empty M.empty maxIx
    maxIx = maxBV main

    allTypeConstructors :: Exp WithoutObjects Ty (Var (BVar Ty) (FVar Ty)) -> S.Set (Qualified (ProperName 'TypeName))
    allTypeConstructors _ = datatypes ^. tyDict . to M.keys . to S.fromList
                            {- FIXME: Something in the plated instance is broken -_-
                            let res = S.fromList $  e ^.. cosmos . to (expTy id) . cosmos . _TyCon
                            in doTrace "allTypeConstructors" (show $ S.map prettyQPN res) $ res
                            -}
    maxBV :: Exp WithoutObjects t (Var (BVar t) (FVar t)) -> Int
    maxBV e = foldr go 0 $ e ^.. cosmos
      where
        bvIx (BVar n _ _ ) = n
        go :: Exp WithoutObjects t (Var (BVar t) (FVar t)) -> Int -> Int
        go x acc = case x of
          V (B bv) -> max (bvIx bv) acc
          LamE (BVar n _ _) _ -> max n acc
          LetE _ _ scope ->  foldr (max . bvIx) acc (bindings scope)
          CaseE _ _ alts -> maxAlt acc alts
          AppE e1 e2     -> max (go e1 acc) (go e2 acc)
          _ -> acc
         where
           maxAlt :: Int -> [Alt WithoutObjects t (Exp WithoutObjects t) (Var (BVar t) (FVar t))] -> Int
           maxAlt n [] = n
           maxAlt n (UnguardedAlt _ _ scope:rest) =
             let thisMax = foldr (max . bvIx) n (bindings scope)
             in maxAlt thisMax rest

isTupleCtor :: Qualified (ProperName a) -> Bool
isTupleCtor (Qualified (ByModuleName C.M_Prim) (ProperName xs)) = T.isPrefixOf "Tuple" xs
isTupleCtor _ = False

mkPIRDatatypes :: Datatypes IR.Kind Ty
               -> S.Set (Qualified (ProperName 'TypeName))
               -> DatatypeM ()
mkPIRDatatypes datatypes tyConsInExp = doTraceM "mkPIRDatatypes" (show $ S.map prettyQPN tyConsInExp ) >>
  traverse_ go tyConsInExp
  where
    -- these things don't have datatype definitions anywhere
    truePrimitives = S.fromList [C.Function, C.Int, C.Char, C.String]


    go :: Qualified (ProperName 'TypeName)
       -> DatatypeM ()
    go qn | qn `S.member` truePrimitives  = pure ()
    go qn@(Qualified _ (ProperName tnm)) = doTraceM "mkPIRDatatypes" ("go: " <> prettyQPN qn) >> case lookupDataDecl qn datatypes of
      Nothing -> throwError $ "Error when translating data types to PIR: "
                        <> "Couldn't find a data type declaration for "
                        <> T.unpack (showQualified runProperName qn)
      Just dDecl -> do -- TODO: newtypes should probably be newtype-ey
        let declArgs = fst <$> dDecl ^. dDataArgs
            declKind = mkDeclKind $ mkKind . snd <$> dDecl ^. dDataArgs
        doTraceM "mkPIRDatatypes" $  "Decl " <> prettyStr dDecl
        doTraceM "mkPIRDatatypes" $ "decl args: "  <> show declArgs
        tyName <- mkTyName qn
        let typeNameDecl = TyVarDecl () tyName declKind
            dataArgs = dDecl ^. dDataArgs
        argDecls <-  traverse mkArgDecl (dDecl ^. dDataArgs)
        uniq <- next
        let destructorName = PIR.Name ("match_" <> tnm) $ PLC.Unique uniq
        modify $ over destructors (M.insert qn destructorName)
        ctors <- traverse (mkCtorDecl qn dataArgs) $ zip [0..] (dDecl ^. dDataCtors)
        let this = PIR.Datatype () typeNameDecl argDecls destructorName ctors
        modify $ over pirDatatypes (M.insert qn this)

    mkCtorDecl :: Qualified (ProperName 'TypeName)
               -> [(Text,IR.Kind)]
               -> (Int,CtorDecl Ty)
               -> DatatypeM (PIR.VarDecl PIR.TyName PIR.Name PLC.DefaultUni ())
    mkCtorDecl qTyName dataArgs (cix,ctorDecl) =  doTraceM "mkCtorDecl" (prettyQPN qTyName) >> do
        let ctorFields = snd <$> ctorDecl ^. cdCtorFields
            resultTy' = foldl' TyApp (TyCon qTyName) (uncurry TyVar <$> dataArgs)
            ctorFunTy :: Ty
            ctorFunTy = foldr1Err "mkCtorDecl" funTy (ctorFields <> [resultTy'])
        ctorName <- mkConstrName (ctorDecl ^. cdCtorName) cix
        ctorFunTyPIR <- toPIRType  ctorFunTy
        pure $ VarDecl () ctorName ctorFunTyPIR

    mkDeclKind :: [PIR.Kind ()] -> PIR.Kind ()
    mkDeclKind = \case
      [] -> PIR.Type ()
      xs ->  foldr1Err "mkDeclKind" (PIR.KindArrow ()) (xs <> [PIR.Type ()])

    -- the arguments to the *type*, i.e., they're all tyvars
    -- NOTE: We should really make changes such that `Maybe SourceType` is `SourceType`
    mkArgDecl :: (Text, IR.Kind) -> DatatypeM (PIR.TyVarDecl PIR.TyName ())
    mkArgDecl (varNm,ki) = do
      tyVarNm <- mkNewTyVar varNm
      bindTV varNm tyVarNm
      pure $ TyVarDecl () tyVarNm (mkKind ki)

toPIRType :: Ty -> DatatypeM PIRType
toPIRType _ty =  case _ty of
  IR.TyVar txt _ -> PIR.TyVar () <$> getBoundTyVarName txt
  TyCon qtn@(Qualified qb _) -> case qb of
    ByThisModuleName "Builtin" -> either throwError pure $  handleBuiltinTy qtn
    ByThisModuleName "Prim" | isJust (handlePrimTy qtn) ->   pure . fromJust $ handlePrimTy qtn
    _ -> do
      tyName <- mkTyName qtn
      let result =  PIR.TyVar () tyName
      doTraceM "toPIRType" ("\nINPUT:\n" <> prettyStr _ty <> "\n\nRESULT:\n" <> prettyStr result)
      pure result
  IR.TyApp t1 t2 -> do
    result <- goTypeApp t1 t2
    doTraceM "toPIRType" ("\nINPUT:\n" <> prettyStr _ty <> "\n\nRESULT:\n" <> prettyStr result)
    pure result
  Forall _ v k ty _ -> do
    vTyName <- mkNewTyVar v
    bindTV v vTyName
    ty' <- toPIRType ty
    let result =  TyForall () vTyName (mkKind k) ty'
    doTraceM "toPIRType" ("\nINPUT:\n" <> prettyStr _ty <> "\n\nRESULT:\n" <> prettyStr result)
    pure result
  other -> error $ "Upon reflection, other types like " <> ppTy other <> " shouldn't be allowed in the Ty ast"
 where
   goTypeApp (IR.TyApp (TyCon C.Function) a) b = do
         a' <- toPIRType a
         b' <- toPIRType b
         pure $ PIR.TyFun () a' b'
   goTypeApp a b = PIR.TyApp () <$> toPIRType a <*> toPIRType b

handleBuiltinTy :: Qualified (ProperName 'TypeName) -> Either String (PIR.Type tyname PLC.DefaultUni ())
handleBuiltinTy = \case
     C.BuiltinData -> pure $ TyBuiltin () (PLC.SomeTypeIn PLC.DefaultUniData)
     C.BuiltinPair -> pure $ TyBuiltin () (PLC.SomeTypeIn PLC.DefaultUniProtoPair)
     C.BuiltinList -> pure $ TyBuiltin () (PLC.SomeTypeIn PLC.DefaultUniProtoList)
     C.BuiltinByteString -> pure $ TyBuiltin () (PLC.SomeTypeIn PLC.DefaultUniByteString)
     other -> Left $   "Error when translating to PIR types: unsupported builtin type: " <> show other

handlePrimTy :: Qualified (ProperName 'TypeName) -> Maybe (PLC.Type tyname PLC.DefaultUni ())
handlePrimTy = \case
     C.String   -> pure $ TyBuiltin () (PLC.SomeTypeIn PLC.DefaultUniString)
     C.Char     -> pure $ TyBuiltin () (PLC.SomeTypeIn PLC.DefaultUniInteger)
     C.Int      -> pure $ TyBuiltin () (PLC.SomeTypeIn PLC.DefaultUniInteger)
     _          -> Nothing

mkKind ::  IR.Kind -> PIR.Kind ()
mkKind = \case
  IR.KindType -> PIR.Type ()
  IR.KindArrow k1 k2 -> PIR.KindArrow () (mkKind k1) (mkKind k2)

sourceTypeToKind :: SourceType  -> Either String (PIR.Kind ())
sourceTypeToKind _t = doTraceM "sourceTypeToKind" (prettyStr _t) >> case _t of
    TypeConstructor _ C.Type -> pure $ PIR.Type ()
    t1 :-> t2 -> do
      t1' <- sourceTypeToKind t1
      t2' <- sourceTypeToKind t2
      pure $ PIR.KindArrow () t1' t2'
    other -> Left $ "Error: PureScript type '" <> prettyTypeStr other <> " is not a valid Plutus Kind"

getDestructorTy :: Qualified (ProperName 'TypeName) -> DatatypeM PLC.Name
getDestructorTy qn = do
  dctors <- gets (view destructors)
  case M.lookup qn dctors of
    Nothing -> throwError $ "No destructor defined for datatype "
                          <> T.unpack (showQualified runProperName qn)
                          <> ". This indicates a compiler bug (datatype declaration not generated)"
    Just dctor -> pure dctor

getConstructorName :: Qualified Ident -> DatatypeM (Maybe PLC.Name)
getConstructorName qi = doTraceM "getConstructorName" (show qi) >> do
  ctors <- gets (view constrNames)
  traceM $ show ctors
  pure $ ctors ^? ix qi . _1
 

prettyQPN :: Qualified (ProperName 'TypeName) -> String
prettyQPN = T.unpack . showQualified runProperName

instance Pretty (Qualified (ProperName 'TypeName)) where
  pretty = pretty . prettyQPN

prettyQI :: Qualified Ident -> String
prettyQI = T.unpack . showQualified runIdent

instance Pretty (Qualified Ident) where
  pretty = pretty . prettyQI

-- Utilities for PIR conversion (move somewhere else)
pattern (:@) :: PIR.Type tyName PLC.DefaultUni () -> PIR.Type tyName PLC.DefaultUni () -> PIR.Type tyName PLC.DefaultUni ()
pattern f :@ e = PIR.TyApp () f e

infixl 9 :@

pattern PlcPair :: PIR.Type tyName PLC.DefaultUni () -> PIR.Type tyName PLC.DefaultUni () -> PIR.Type tyName PLC.DefaultUni ()
pattern PlcPair a b =  TyBuiltin () (PLC.SomeTypeIn PLC.DefaultUniProtoPair) :@ a :@ b

pattern PlcList :: PIR.Type tyName PLC.DefaultUni () -> PIR.Type tyName PLC.DefaultUni ()
pattern PlcList a =  TyBuiltin () (PLC.SomeTypeIn PLC.DefaultUniProtoList) :@ a

pattern PlcData :: PIR.Type tyName PLC.DefaultUni ()
pattern PlcData = TyBuiltin () (PLC.SomeTypeIn PLC.DefaultUniData)

pattern PlcInt :: PIR.Type tyName PLC.DefaultUni ()
pattern PlcInt = TyBuiltin () (PLC.SomeTypeIn PLC.DefaultUniInteger)

pattern PlcBool :: PIR.Type tyName PLC.DefaultUni ()
pattern PlcBool = TyBuiltin () (PLC.SomeTypeIn PLC.DefaultUniBool)

pattern PlcString :: PIR.Type tyName PLC.DefaultUni ()
pattern PlcString = TyBuiltin () (PLC.SomeTypeIn PLC.DefaultUniString)

pattern PlcByteString :: PIR.Type tyName PLC.DefaultUni ()
pattern PlcByteString = TyBuiltin () (PLC.SomeTypeIn PLC.DefaultUniByteString)

-- the kind annotation on `a` is necessary here
data SomeUni :: GHC.Type where
  SomeUni :: forall (a :: GHC.Type).  PLC.DefaultUni (PLC.Esc a) -> SomeUni

extractUni :: Show tyName => PIR.Type tyName PLC.DefaultUni () -> DatatypeM SomeUni
extractUni = liftEither . extractUni'

extractUni' :: Show tyName => PIR.Type tyName PLC.DefaultUni () -> Either String SomeUni
extractUni' = \case
  PlcInt -> pure $ SomeUni PLC.DefaultUniInteger
  PlcBool -> pure $ SomeUni PLC.DefaultUniBool
  PlcString -> pure $ SomeUni PLC.DefaultUniString
  PlcByteString -> pure $ SomeUni PLC.DefaultUniByteString
  PlcPair a b -> do
    SomeUni a' <- extractUni' a
    SomeUni b' <- extractUni' b
    pure . SomeUni $ PLC.DefaultUniPair a' b'
  PlcList a -> do
    SomeUni a' <- extractUni' a
    pure . SomeUni $ PLC.DefaultUniList a'
  other -> Left $ "Not a PLC constant-able type:\n " <> show other

mkDestructorFunTy :: Datatypes IR.Kind Ty
                  -> Qualified (ProperName 'TypeName)
                  -> DatatypeM (Bool,Ty) -- (Is it a nullary TyCon,Destructor fun ty )
mkDestructorFunTy _datatypes tn = do
  let datatypes = primData <> _datatypes
  case datatypes ^? tyDict . ix tn of
    Nothing -> throwError $ "mkDestructorFunTy: No type info for " <> prettyQPN tn
    Just dDecl -> do
      let tyArgs = dDecl ^. dDataArgs
          tyAppliedToArgs = foldl' (\acc (t,k) -> TyApp acc (TyVar t k)) (TyCon tn) tyArgs
      let funTyLHS' out = foldr (\(txt,k) acc -> Forall TypeVarVisible txt k acc Nothing) out (dDecl ^. dDataArgs)
      n <- T.pack . show <$> next
      let outVarNm = "out" <> n
          outVar   = TyVar outVarNm IR.KindType
      let funTyLHS inner = funTyLHS' $ Forall TypeVarVisible ("out" <> n) IR.KindType inner Nothing
          ctorfs = map snd . view cdCtorFields <$> dDecl ^. dDataCtors
      let funTyRHS = tyAppliedToArgs :~> mkFunTyRHS outVar ctorfs  -- foldr funTy outVar funTyCtorArgs
      let result = funTyLHS funTyRHS
      doTraceM "mkDestructorFunTy" ("TYPE NAME:\n" <> prettyStr tn
                                    <> "\n\nTY CTOR FIELDS:\n" <> prettyStr ctorfs
                                    <> "\n\nTY RHS:\n" <> prettyStr funTyRHS
                                    <> "\n\nRESULT:\n" <> prettyStr result)
      pure (null tyArgs,result)
 where
   mkFunTyRHS outVar [] =  outVar
   mkFunTyRHS outVar ([]:fss) = outVar :~> mkFunTyRHS outVar fss
   mkFunTyRHS outVar (fs:fss) =
     let fs' = foldr1Err "mkFunTyRHS" (:~>) fs
     in (fs' :~> outVar) :~> mkFunTyRHS outVar fss

bvTy :: BVar ty -> ty
bvTy (BVar _ t _ ) = t


eliminateCaseExpressionsTrace :: Datatypes IR.Kind Ty
                         -> Exp WithoutObjects Ty (Var (BVar Ty) (FVar Ty))
                         -> DatatypeM (Exp WithoutObjects Ty (Var (BVar Ty) (FVar Ty)))
eliminateCaseExpressionsTrace _datatypes _exp = do
  let datatypes = _datatypes <> primData
  res <- eliminateCaseExpressions datatypes _exp
  doTraceM "eliminateCaseExpressions" ("INPUT:\n" <> prettyStr _exp <> "\n\nOUTPUT:\n" <> prettyStr res)
  pure
    . instantiateNullaryWithAnnotatedType datatypes
    . instantiateCtors datatypes
    $ res

eliminateCaseExpressions :: Datatypes IR.Kind Ty
                         -> Exp WithoutObjects Ty (Var (BVar Ty) (FVar Ty))
                         -> DatatypeM (Exp WithoutObjects Ty (Var (BVar Ty) (FVar Ty)))
eliminateCaseExpressions _datatypes = \case
  V x -> pure $ V x
  LitE t lit -> pure $ LitE t lit
  LamE  bv scoped -> do
    let unscoped = join <$> fromScope scoped
    rescoped <- eliminateCaseExpressions datatypes unscoped
    pure
     . LamE bv
     . toScope
     . fmap F
     $ rescoped
  AppE e1 e2 -> do
    e1' <- eliminateCaseExpressions datatypes e1
    e2' <- eliminateCaseExpressions datatypes e2
    pure $ AppE e1' e2'
  ce@CaseE{} ->
    case ezMonomorphize $  monomorphizePatterns datatypes ce of
      CaseE resTy _scrut _alts -> do
        let retTy  = case head _alts of {UnguardedAlt _ _ e -> expTy' id e}
            msg = prettify ["ANN RES TY:\n " <> prettyStr resTy
                           , "SCRUTINEE:\n" <> prettyStr _scrut
                           , "ALTS:\n" <> prettyStr _alts
                           ]
        doTraceM "eliminateCaseExpressions" msg 
        scrut <- eliminateCaseExpressions datatypes _scrut
        alts  <- traverse eliminateCasesInAlt _alts
        desugarConstructorPattern datatypes retTy (CaseE resTy scrut alts)
      other -> error ("eliminateCaseExpressions: IMPOSSIBLE:\n" <> prettyStr other)
  LetE bindingsMap _bindEs _scoped -> do
    let unscoped = join <$> fromScope _scoped
    scoped <- toScope . fmap F <$> eliminateCaseExpressions datatypes unscoped
    bindEs <- traverse eliminateCasesInBind _bindEs
    pure $ LetE bindingsMap bindEs scoped
  TyInstE ty inner -> TyInstE ty <$> eliminateCaseExpressions datatypes inner
  TyAbs bv inner -> TyAbs bv <$> eliminateCaseExpressions datatypes inner
 where
   datatypes = primData <> _datatypes

   eliminateCasesInAlt :: Alt WithoutObjects Ty (Exp WithoutObjects Ty) (Var (BVar Ty) (FVar Ty))
                       -> DatatypeM (Alt WithoutObjects Ty (Exp WithoutObjects Ty) (Var (BVar Ty) (FVar Ty)))
   eliminateCasesInAlt (UnguardedAlt bs pat inner) = do
     let unscoped = join <$> fromScope inner
     inner' <- toScope . fmap F <$> eliminateCaseExpressions datatypes unscoped
     pure $ UnguardedAlt bs pat inner'

   eliminateCasesInBind = \case
     NonRecursive i bvix e ->
       let e' = join <$> fromScope e
       in NonRecursive i bvix . toScope . fmap F
                         <$> eliminateCaseExpressions datatypes e'
     Recursive xs ->
       let deScope e = join <$> fromScope e
           rescope = fmap F . toScope
           xs' = traverse (traverse (eliminateCaseExpressions datatypes) . second deScope) xs
       in Recursive . map (second rescope) <$> xs'
{-
eliminateCaseExpressions' :: Datatypes IR.Kind Ty
                          -> Exp WithoutObjects Ty (Var (BVar Ty) (FVar Ty))
                          -> DatatypeM (Exp WithoutObjects Ty (Var (BVar Ty) (FVar Ty)))
eliminateCaseExpressions' datatypes
  = transformM (
        desugarLiteralPatterns
    >=> desugarIrrefutables
    >=> desugarConstructorPatterns datatypes
    >=> (pure . monomorphizePatterns datatypes)
        )
-}
desugarLiteralPatterns :: Exp WithoutObjects Ty (Var (BVar Ty) (FVar Ty)) -> DatatypeM (Exp WithoutObjects Ty (Var (BVar Ty) (FVar Ty)))
desugarLiteralPatterns = transformM desugarLiteralPattern

desugarIrrefutables :: Exp WithoutObjects Ty (Var (BVar Ty) (FVar Ty)) -> DatatypeM (Exp WithoutObjects Ty (Var (BVar Ty) (FVar Ty)))
desugarIrrefutables = transformM desugarIrrefutable

{-
desugarConstructorPatterns :: Datatypes IR.Kind Ty
                           -> Exp WithoutObjects Ty (Var (BVar Ty) (FVar Ty))
                           -> DatatypeM (Exp WithoutObjects Ty (Var (BVar Ty) (FVar Ty)))
desugarConstructorPatterns datatypes = transformM (desugarConstructorPattern datatypes)
-}

desugarLiteralPattern :: Exp WithoutObjects Ty (Var (BVar Ty) (FVar Ty))
                      -> DatatypeM (Exp WithoutObjects Ty (Var (BVar Ty) (FVar Ty)))
desugarLiteralPattern  = \case
  CaseE resTy scrut (UnguardedAlt _ (LitP patLit) rhs:alts) -> do
    let eqTest = mkEqTestFun  scrut patLit
        trueP  = ConP C.Boolean C.C_True []
        falseP = ConP C.Boolean C.C_False []
    rest <- fmap F . toScope <$> desugarLiteralPattern  (CaseE resTy scrut alts)
    pure $ CaseE resTy eqTest [UnguardedAlt M.empty trueP rhs,
                               UnguardedAlt M.empty falseP rest
                              ]
  CaseE _ _ (UnguardedAlt _ WildP rhs:_) -> pure $ join <$> fromScope rhs -- FIXME: Wrong! Need to do the same
                                                                          -- catchall stuff we do in the ctor
                                                                          -- case eliminator
  CaseE _ scrut (UnguardedAlt _ (VarP bvId bvIx _) rhs:_) -> pure $ flip instantiate rhs $ \case
    bv@(BVar bvIx' _ bvId') ->
      if bvIx == bvIx' && bvId == bvId'
      then  scrut
      else V . B $ bv
  other -> pure other
 where
   eqInt = V . F $ FVar
             (TyCon C.Int :~> TyCon C.Int :~> TyCon C.Boolean)
             C.I_equalsInteger

   eqChar = eqInt
   eqString = V . F $ FVar
             (TyCon C.Int :~> TyCon C.Int :~> TyCon C.Boolean)
             C.I_equalsString

   mkEqTestFun :: Exp WithoutObjects Ty (Var (BVar Ty) (FVar Ty))
               -> Lit WithoutObjects (Pat WithoutObjects Ty (Exp WithoutObjects Ty) (Var (BVar Ty) (FVar Ty)))
               -> Exp WithoutObjects Ty (Var (BVar Ty) (FVar Ty))
   mkEqTestFun scrut = \case
     IntL i -> eqInt `AppE` LitE (TyCon C.Int) (IntL i) `AppE` scrut
     CharL c -> eqChar `AppE` LitE (TyCon C.Char) (CharL c) `AppE` scrut
     StringL s -> eqString `AppE` LitE (TyCon C.String) (StringL s) `AppE` scrut


{-

We can't easily do this in the monomorphizer itself

-}
monomorphizePatterns :: Datatypes IR.Kind Ty
                     -> Exp WithoutObjects Ty (Var (BVar Ty) (FVar Ty))
                     -> Exp WithoutObjects Ty (Var (BVar Ty) (FVar Ty))
monomorphizePatterns _datatypes _e' =  case _e' of
  CaseE resTy scrut alts ->
      let scrutTy = expTy id scrut
          alts'   = goAlt scrutTy <$> alts
          res =  CaseE resTy scrut $ goAlt scrutTy <$> alts'
      in doTrace "monomorphizePatterns" ("INPUT:\n" <> prettyStr _e' <> "\n\nRESULT:\n" <> prettyStr res)  res
  other -> other
 where
   monomorphPat :: Ty
                -> Pat WithoutObjects Ty (Exp WithoutObjects Ty) (Var (BVar Ty) (FVar Ty))
                -> Pat WithoutObjects Ty (Exp WithoutObjects Ty) (Var (BVar Ty) (FVar Ty))
   monomorphPat t = \case
     VarP idnt indx _ -> VarP idnt indx t
     ConP tn cn ps ->
       let monoFields = snd $ monoCtorFields tn cn t datatypes
           ps'        = zipWith monomorphPat monoFields ps
       in ConP tn cn ps'
     other -> other

   rebindPat :: Pat WithoutObjects Ty (Exp WithoutObjects Ty) (Var (BVar Ty) (FVar Ty))
             -> Scope (BVar Ty) (Exp WithoutObjects Ty) (Var (BVar Ty) (FVar Ty))
             -> Scope (BVar Ty) (Exp WithoutObjects Ty) (Var (BVar Ty) (FVar Ty))
   rebindPat p e =
     let upd idnt indx t =
           mapBound (\bv@(BVar bvix _ bvidnt) -> if idnt == bvidnt && indx == bvix then BVar bvix t bvidnt else bv)
           . fmap (\case
                      bv@(B (BVar bvix _ bvidnt)) -> if idnt == bvidnt && indx == bvix then B (BVar bvix t bvidnt) else bv
                      other -> other
                  )
     in case p of
       VarP vpId vpIx vpTy -> upd vpId vpIx vpTy e
       ConP _ _  ps' -> foldl' (flip rebindPat) e ps'
       _  -> e

   datatypes = _datatypes <> primData
   goAlt :: Ty
         -> Alt WithoutObjects Ty (Exp WithoutObjects Ty) (Var (BVar Ty) (FVar Ty))
         -> Alt WithoutObjects Ty (Exp WithoutObjects Ty) (Var (BVar Ty) (FVar Ty))
   goAlt scrutTy (UnguardedAlt a p e) =
     let p' = monomorphPat scrutTy p
         e' = rebindPat p' e
     in UnguardedAlt a p' e'

-- This is for case expressions where the first alternative contains an irrefutable pattern (WildP, VarP)
-- (we need this b/c the other two won't catch and eliminate those expressions)
desugarIrrefutable :: Exp WithoutObjects Ty (Var (BVar Ty) (FVar Ty))
                   -> DatatypeM (Exp WithoutObjects Ty (Var (BVar Ty) (FVar Ty)))
desugarIrrefutable = \case
  CaseE _ _ (UnguardedAlt _ WildP rhs:_) -> pure $ join <$> fromScope rhs
  CaseE _ scrut (UnguardedAlt _ (VarP bvId bvIx _) rhs:_) -> pure $ flip instantiate rhs $ \case
    bv@(BVar bvIx' _ bvId') ->
      if bvIx == bvIx' && bvId == bvId'
      then  scrut
      else V . B $ bv
  other -> pure other


data CtorCase = CtorCase {
    irrefutableRHS :: Exp WithoutObjects Ty (Var (BVar Ty) (FVar Ty)),
    indexedMatchArgs :: M.Map Int (Exp WithoutObjects Ty (Var (BVar Ty) (FVar Ty))),
    -- The destructor fun initially, then the application of that fun to its args
    acc :: Exp WithoutObjects Ty (Var (BVar Ty) (FVar Ty)),
    scrutType :: Ty
  }


ezMonomorphize :: Exp WithoutObjects Ty (Var (BVar Ty) (FVar Ty))
               -> Exp WithoutObjects Ty (Var (BVar Ty) (FVar Ty))
ezMonomorphize = transform go
  where
    go expr = case expr of
      AppE fe ae -> case unsafeAnalyzeApp (AppE fe ae) of
        (f,args) -> case expTy id f of
          ft@Forall{} -> case getAllInstantiations ft (expTy id <$> args) of
                 [] -> expr
                 instantiations' ->
                   let instantiations = reverse (snd <$> instantiations')
                       f' = foldr TyInstE f instantiations
                       result = unsafeApply id f' args
                       msg = "INPUT:\n" <> prettyStr expr
                             <> "\n\nOUTPUT:\n" <> prettyStr result
                   in doTrace "ezMonomorphize" msg result
          ft -> let msg = "NO CHANGE (NOT A FORALL):\n\n"
                          <> "FUN TY:\n" <> prettyStr ft
                          <> "\n\nARG TYPES:\n" <> prettyStr (expTy id <$> args)
                          <> "\n\nORIGINAL EXPR:\n" <> prettyStr expr
               in doTrace "ezMonomorphize" msg   expr
      _ -> expr -- doTrace "ezMonomorphize" ("" <> prettyStr expr) expr

desugarConstructorPattern :: Datatypes IR.Kind Ty
                          -> Ty
                          -> Exp WithoutObjects Ty (Var (BVar Ty) (FVar Ty))
                          -> DatatypeM (Exp WithoutObjects Ty (Var (BVar Ty) (FVar Ty)))
desugarConstructorPattern datatypes altBodyTy _e = let _eTy = expTy id _e in case _e of
  CaseE _resTy scrut alts@(UnguardedAlt _ (ConP tn _ _) _:_) -> do
    let isConP alt =  case getPat alt of {ConP{} -> True; _ -> False}
        conPatAlts = takeWhile isConP alts
        scrutTy = expTy id scrut
    indexedBranches <- sortOn fst  <$> traverse (mkIndexedBranch scrutTy) conPatAlts
    let branchTy  = expTy id . snd . head $ indexedBranches
        branchSplit = splitFunTyParts branchTy
        branchRetTy =  last . splitFunTyParts $ branchTy
        allCtors =  zip [0..] $ getAllConstructorDecls tn datatypes
    (Name dcTor (Unique dctorIx)) <- getDestructorTy tn
    (isNullaryTyCon,dctorTy) <- mkDestructorFunTy datatypes tn
    let destructorRaw = V . B $ BVar dctorIx dctorTy (Ident dcTor)
        instantiateTyCon :: Exp WithoutObjects Ty (Var (BVar Ty) (FVar Ty)) -> Exp WithoutObjects Ty (Var (BVar Ty) (FVar Ty))
        instantiateTyCon
          | isNullaryTyCon = id
          | otherwise = mkInstantiateTyCon (expTy id scrut)

        retTy' = mkInstantiateResTy scrutTy altBodyTy
    -- NOTE: We'll need more sophisticated "pattern sorting" with as patterns
    case dropWhile isConP alts of
      [] -> do -- In this branch we know we have exhaustive constructor patterns in the alts
        let destructor = TyInstE altBodyTy (AppE (instantiateTyCon destructorRaw) scrut)
            result = foldl' AppE destructor (snd <$> indexedBranches)
            msg =  "INPUT TY:\n" <> prettyStr _eTy
                  --  <> "\n\nINPUT:\n" <> prettyStr _e
                   <> "\n\nRESULT TY:\n" <> prettyStr (expTy id result)
                   <> "\n\nDESTRUCTOR TY:\n" <> prettyStr (expTy id destructor)
                   <> "\n\nORIGINAL CASE RES TY:\n" <> prettyStr _resTy
                   <> "\n\nDEDUCED BRANCH RES TY:\n" <> prettyStr branchRetTy
                   <> "\n\nSPLIT BRANCH TY:\n" <> prettyStr branchSplit
                   <> "\n\nFULL BRANCH TY:\n" <> prettyStr branchTy
                   <> "\n\nSCRUT TY:\n" <> prettyStr scrutTy
                   <> "\n\nSCRUT EXPR:\n" <> prettyStr scrut
                   <> "\n\nRESULT:\n" <> prettyStr result
                   <> "\n\nALT BODY TY:\n" <> prettyStr altBodyTy
                   <> "\n\nINSTANTIATED ALT BODY TY:\n" <> prettyStr retTy'
        doTraceM "desugarConstructorPattern" msg
        pure result
      irrefutables -> do
        let destructor = TyInstE altBodyTy (AppE (instantiateTyCon destructorRaw) scrut)
            {- This is confusing and I keep making mistakes, so what's going on with 'irrefutables' is:

               This only concerns the "catchall" case that we expect when we encounter an
               incomplete enumeration of constructor patterns in a set of case alternatives.

               If we have a WildP, we only care about the RHS b/c no variables are bound.

               If we have a VarP, it binds a variable. But we have to be careful here. If we have (pardon the stupid example)
                  ```
                     case (mb :: Maybe Int) of
                       Nothing -> 0
                       other  -> fromJust other
                  ```

               `other` is a VarBinder for a value of type `Maybe Int`.

               The `match` functions we're forced to use don't expect function arguments where the lambda binds
               a variable of the scrutinee type. E.g. (assuming Nothing is the first ctor)

               ```
                 match_Maybe :: forall t out. Maybe t -> out -> (t -> out) -> out
               ```

               So the translation for the above example is going to look like (pay attention to the types!)

               match_Maybe @Int mb 0 (\(_n :: Int) -> (\(other :: Maybe Int) -> fromJust other) mb)

               That's unnecessarily verbose and we can avoid creating a new lambda entirely by substituting the
               scrutinee into `other` to perform a reduction step, a la:

               match_Maybe @Int mb 0 (\(_n :: Int) -> fromJust mb)

               If we had
               ```
                  case (mb :: Maybe Int) of
                    Nothing -> 0
                    _       -> 1
               ```

              The translation would be:

               ```
                 match_Maybe @Int mb 0 (\(_n :: Int) -> 1)
               ```

              Anyway, the idea is that `irrefutable` here is always going to be a self-contained RHS for a lambda that we will attach unused binders to
              during assembly so as to make the types line up w/ what the destructor fn expects

              TODO: We should let- bind the scrutinee because it will almost always occur in multiple places
            -}
            irrefutable = case head irrefutables of
                            UnguardedAlt _ WildP irrRHS -> join <$> fromScope irrRHS
                            UnguardedAlt _ (VarP bvId bvIx _) irrRHS -> flip instantiate irrRHS $ \case
                              bv@(BVar bvIx' _ bvId') ->
                                if bvIx == bvIx' && bvId == bvId'
                                then scrut
                                else V . B $ bv
                            other -> error $ "Expected an irrefutable alt but got: " <> prettyStr other
        result <- assemblePartialCtorCase (CtorCase irrefutable  (M.fromList indexedBranches) destructor scrutTy) allCtors
        let  msg = "INPUT TY:\n" <> prettyStr _eTy
                   <> "\n\nINPUT:\n" <> prettyStr _e
                   <> "\n\nRESULT TY:\n" <> prettyStr (expTy id result)
                   <> "\n\n DESTRUCTOR TY:\n" <> prettyStr (expTy id destructor)
                   <> "\n\nORIGINAL CASE RES TY:\n" <> prettyStr _resTy
                   <> "\n\nDEDUCED BRANCH RES TY:\n" <> prettyStr branchRetTy
                   <> "\n\nSPLIT BRANCH TY:\n" <> prettyStr branchSplit
                   <> "\n\nFULL BRANCH TY:\n" <> prettyStr branchTy
                   <> "\n\nSCRUT TY:\n" <> prettyStr scrutTy
                   <> "\n\nSCRUT EXPR:\n" <> prettyStr scrut
                   <> "\n\nRESULT:\n" <> prettyStr result
                   <> "\n\nALT BODY TY:\n" <> prettyStr altBodyTy
                   <> "\n\nINSTANTIATED ALT BODY TY:\n" <> prettyStr retTy'
        doTraceM "desugarConstructorPattern" msg
        pure result
  other -> pure other
 where
   assemblePartialCtorCase :: CtorCase -> [(Int,CtorDecl Ty)] -> DatatypeM (Exp WithoutObjects Ty (Var (BVar Ty) (FVar Ty)))
   assemblePartialCtorCase CtorCase{..} [] = pure acc
   assemblePartialCtorCase CtorCase{..} ((ctorIx,ctorDecl):rest) = case M.lookup ctorIx indexedMatchArgs of
     Nothing -> do
       let cn = ctorDecl ^. cdCtorName
           tn = fromJust $ lookupCtorType cn datatypes
           cnProper = ProperName . runIdent <$> cn
           monoFieldTypes = snd $ monoCtorFields tn cnProper scrutType datatypes

       lhsVars <- for monoFieldTypes $ \fldTy -> do
                         Name nm (Unique u) <- freshName
                         pure $  BVar u fldTy (Ident nm)


       let irrefutableLam = mkLHSBinder lhsVars irrefutableRHS
           acc' = AppE acc irrefutableLam
       assemblePartialCtorCase (CtorCase irrefutableRHS indexedMatchArgs acc' scrutType) rest
     Just iFun -> do
       let acc' = AppE acc iFun
       assemblePartialCtorCase (CtorCase irrefutableRHS indexedMatchArgs acc' scrutType) rest
    where
      defAbstr = abstract $ \case
                  B bv -> Just bv
                  _    -> Nothing
      mkLHSBinder :: [BVar Ty]
                  -> Exp WithoutObjects Ty (Var (BVar Ty) (FVar Ty))
                  -> Exp WithoutObjects Ty (Var (BVar Ty) (FVar Ty))
      mkLHSBinder [] = id
      mkLHSBinder (bv:bvs) = \inner ->
        let mkRHS = mkLHSBinder bvs
            rhs   = mkRHS inner
        in  LamE bv (defAbstr rhs)

   mkInstantiateTyCon :: Ty
                      -> Exp WithoutObjects Ty (Var (BVar Ty) (FVar Ty))
                      -> Exp WithoutObjects Ty (Var (BVar Ty) (FVar Ty))
   mkInstantiateTyCon t e  = doTrace "instantiateTyCon" msg result
     where
       result = case analyzeTyApp t of
            Just (_,tyArgs) -> foldr TyInstE  e (reverse tyArgs)
            Nothing -> e

       resTy = expTy id result

       msg = "INPUT TY:\n" <> prettyStr t
             <> "\n\nINPUT EXPR:\n" <> prettyStr e
             <> "\n\nINPUT EXPR TY:\n" <> prettyStr (expTy id e)
             <> "\n\nOUTPUT TY:\n" <> prettyStr resTy
             <> "\n\nOUTPUT:\n" <> prettyStr result
   {- This is a bit weird. If the alt body type is already quantified then we don't want to
      do any instantiations. TODO: Explain why (kind of complicated)
   -}
   mkInstantiateResTy :: Ty -> Ty -> Ty
   mkInstantiateResTy _ altT@(Forall{}) = doTrace "instantiateResTy" ("UNCHANGED:\n" <> prettyStr altT) altT
   mkInstantiateResTy scrutT altT = doTrace "instantiateResTy" msg result
     where
       result = case analyzeTyApp scrutT of
            Just (_,tyArgs) -> foldr instTy (quantify altT) (reverse tyArgs)
            Nothing -> altT
       msg = "INPUT SCRUT TY:\n" <> prettyStr scrutT
             <> "\n\nINPUT TARG TY:\n" <> prettyStr altT
             <> "\n\nOUTPUT TY:\n" <> prettyStr result

   mkIndexedBranch :: Ty
                   -> Alt WithoutObjects Ty (Exp WithoutObjects Ty) (Var (BVar Ty) (FVar Ty))
                   -> DatatypeM (Int, Exp WithoutObjects Ty (Var (BVar Ty) (FVar Ty)))
   mkIndexedBranch scrutTy alte@(UnguardedAlt _ (ConP tn cn binders) rhs) = do
     doTraceM "mkIndexedBranch" ("INPUT:\n" <> prettyStr alte)
     let go (x,t) acc = case x of
           VarP bvId bvIx bvTy' -> do
             let lamBV = BVar bvIx bvTy' bvId
             pure $  LamE  lamBV
                 . abstract (\case {F fv -> matchVarLamAbs bvId bvIx fv; B _ -> Nothing})
                 . acc
           WildP -> do
             freshIx <- next
             let newName = "_t" <> T.pack (show freshIx)
                 lamBv = BVar freshIx t (Ident newName)
             pure $ LamE lamBv . toScope . fmap F . acc
           other -> error $ "Unexpected pattern in alternative: Expected a VarP but got " <> show other
         monoFields = snd $ monoCtorFields tn cn scrutTy datatypes
     doTraceM "mkIndexedBranch" ("MONO FIELDS:\n" <> prettyStr monoFields)
     lambdaLHS <- foldrM go id (zip binders monoFields)
     let indx = case fst <$> getConstructorIndexAndDecl cn datatypes of
                          Left _ -> error $ "No constructor data for ctor " <> show cn
                          Right i ->  i
         rhsUnscoped = join <$> fromScope rhs
         result = lambdaLHS rhsUnscoped
     doTraceM "mkIndexedBranch" ("RESULT:\n" <> prettyStr result)
     pure (indx,result)
   mkIndexedBranch _ (UnguardedAlt _ otherP _) = error $  "mkIndexedBranch: Expected constructor pattern but got " <> prettyStr otherP

instantiateCtors :: Datatypes IR.Kind Ty -> Exp WithoutObjects Ty (Var (BVar Ty) (FVar Ty)) -> Exp WithoutObjects Ty (Var (BVar Ty) (FVar Ty))
instantiateCtors dt = transform (instantiateCtor dt)

instantiateCtor :: Datatypes IR.Kind Ty
                -> Exp WithoutObjects Ty (Var (BVar Ty) (FVar Ty))
                -> Exp WithoutObjects Ty (Var (BVar Ty) (FVar Ty))
instantiateCtor datatypes expr = case expr of
  AppE fe ae -> case unsafeAnalyzeApp (AppE fe ae) of
        (V (F (FVar t n)),args) | isConstructor n ->
          let ctorNm :: Qualified (ProperName 'ConstructorName)
              ctorNm = ProperName . runIdent <$> n

              tyNm   = case lookupCtorType n datatypes of
                         Nothing -> error
                                    $ "instantiateCtor: No type information for constructor: "
                                    <> prettyQI n
                         Just tn -> tn
              monoFields =  monoCtorInst tyNm ctorNm (funResultTy t) datatypes
              fe' = foldr TyInstE fe monoFields
              result = unsafeApply id fe' args
              msg =     "NAME:" <> T.unpack (showQualified runIdent n)
                     <> "\n\nMONO TYPE:\n" <> prettyStr t
                     <> "\n\nINPUT:\n" <> prettyStr expr
                     <> "\n\nRESULT:\n" <> prettyStr result
                     <> "\n\nMONO FIELDS:\n" <> prettyStr monoFields
                     <> "\n\nINSTANTIATED FUN:\n" <> prettyStr fe'
          in doTrace "instantiateCtor" msg
              $ unsafeApply id fe' args
        _ -> expr
  _  -> expr

funResultTy :: TypeLike t => t -> t
funResultTy = last . splitFunTyParts

analyzeTyApp :: Ty -> Maybe (Ty,[Ty])
analyzeTyApp t = (,tyAppArgs t) <$> tyAppFun t
  where
    tyAppArgs (IR.TyApp t1 t2) = tyAppArgs t1 <> [t2]
    tyAppArgs _ = []

    tyAppFun (IR.TyApp tF _) = go tF
      where
        go (IR.TyApp tX _) = case tyAppFun tX of
          Nothing -> Just tX
          Just tX' -> Just tX'
        go other = Just other
    tyAppFun _ = Nothing

instantiateNullaryWithAnnotatedType :: forall x
                                     . Datatypes IR.Kind Ty
                                    -> Exp x Ty (Vars Ty)
                                    -> Exp x Ty (Vars Ty)
instantiateNullaryWithAnnotatedType datatypes _e = doTrace "instantiateNullaryWithAnnotatedType" msg result
  where
    msg = "INPUT:\n" <> prettyStr _e
          <> "\n\nOUTPUT:\n" <> prettyStr result
    result = transform go _e
    go :: Exp x Ty (Vars Ty)
       -> Exp x Ty (Vars Ty)
    go expr = case expr of
      V (F (FVar ty nm)) | isConstructor nm ->
        let cnm = ProperName . runIdent <$> nm
            ctorDecl = either error snd $ getConstructorIndexAndDecl cnm datatypes
        in case ctorDecl ^. cdCtorFields of
            [] -> case analyzeTyApp ty of
              Just (_,xs@(_:_)) -> foldr TyInstE expr (reverse xs)
              _ -> expr
            _ -> expr
      _ -> expr

monoCtorInst
  :: Qualified (ProperName 'TypeName)
  -> Qualified (ProperName 'ConstructorName)
  -> Ty -- the type of the scrutinee
  -> Datatypes IR.Kind Ty
  -> [Ty] -- Constructor index & list of field types
monoCtorInst tn cn t datatypes = doTrace "monoCtorInst" msg $ snd <$> reverse instantiations
 where
   msg = "TYPE NAME:" <> T.unpack (showQualified runProperName tn)
       <> "\n\nCTOR NAME:\n" <> T.unpack (showQualified runProperName cn)
       <> "\n\nMONO IN TYPE:\n" <> prettyStr t
       <> "\n\nCTOR DECL ARGS:\n" <> prettyStr ctorArgs
       <> "\n\nPOLY TY:\n" <> prettyStr polyTy
       <> "\n\nINSTANTIATIONS:\n" <> prettyStr instantiations
   (_,thisCtorDecl) =  either error id $ getConstructorIndexAndDecl cn datatypes
   ctorArgs = snd <$> thisCtorDecl ^. cdCtorFields
   thisDataDecl = fromJust $ lookupDataDecl tn datatypes
   declArgVars = uncurry IR.TyVar <$> thisDataDecl ^. dDataArgs
   dataTyCon    = TyCon tn
   polyTy    = foldl' applyType dataTyCon declArgVars

   instantiations = getInstantiations t polyTy

monoCtorFields
  :: Qualified (ProperName 'TypeName)
  -> Qualified (ProperName 'ConstructorName)
  -> Ty -- the type of the scrutinee
  -> Datatypes IR.Kind Ty
  -> (Int,[Ty]) -- Constructor index & list of field types
monoCtorFields tn cn t datatypes = doTrace "monoCtorFields" msg (thisCtorIx,monoCtorArgs)
 where
   msg = "TYPE NAME:" <> T.unpack (showQualified runProperName tn)
       <> "\n\nCTOR NAME:\n" <> T.unpack (showQualified runProperName cn)
       <> "\n\nMONO IN TYPE:\n" <> prettyStr t
       <> "\n\nCTOR DECL ARGS:\n" <> prettyStr ctorArgs
       <> "\n\nPOLY TY:\n" <> prettyStr polyTy
       <> "\n\nRESULT TYS:\n" <> prettyStr monoCtorArgs
       <> "\n\nINSTANTIATIONS:\n" <> prettyStr instantiations
   (thisCtorIx,thisCtorDecl) =  either error id $ getConstructorIndexAndDecl cn datatypes
   ctorArgs = snd <$> thisCtorDecl ^. cdCtorFields
   thisDataDecl = fromJust $ lookupDataDecl tn datatypes
   declArgVars = uncurry IR.TyVar <$> thisDataDecl ^. dDataArgs
   dataTyCon    = TyCon tn
   polyTy    = foldl' applyType dataTyCon declArgVars
   polyCtorTy = foldr funTy polyTy ctorArgs
   instantiations = getInstantiations t polyTy
   -- This might not be exactly correct, it assumes that NO tyvars remain in the input type.
   -- If any remain, they might not line up with the var names in the declaration (probably won't
   -- since I think I generated the decl vars). Might require tweaking "instantiates" & co
   monoCtorTy = replaceAllTypeVars instantiations polyCtorTy
   -- if it's a unary constructor, we want to skip the error from funArgTypes being partial
   monoCtorArgs = safeFunArgTypes monoCtorTy

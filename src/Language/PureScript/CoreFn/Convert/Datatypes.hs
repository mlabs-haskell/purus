{-# LANGUAGE GADTs, PolyKinds, TemplateHaskell #-}
{-# LANGUAGE StandaloneKindSignatures #-}
module Language.PureScript.CoreFn.Convert.Datatypes where

import Prelude

import Language.PureScript.CoreFn.Convert.IR
    ( ppTy,
      _TyCon,
      expTy,
      Alt(..),
      BVar(BVar),
      Exp(CaseE, LamE, LetE, AppE),
      FVar,
      Ty(Forall, TyCon) )
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
    ( ProperNameType(TypeName),
      Qualified(..),
      ProperName(..),
      Ident,
      pattern ByThisModuleName,
      showQualified,
      showIdent,
      disqualify, runIdent )
import Bound.Var ( Var(F) )


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
      CtorDecl,
      Datatypes )
import Language.PureScript.Environment ( pattern (:->) )
import Language.PureScript.Types
    ( Type(TypeConstructor), SourceType )
import Language.PureScript.Constants.Prim qualified as C
import Language.PureScript.Constants.Purus qualified as C
import Language.PureScript.CoreFn.Convert.DesugarCore
    ( WithoutObjects )
import Language.PureScript.CoreFn.Convert.Monomorphize.Utils ()
import Language.PureScript.CoreFn.Pretty (prettyTypeStr)
import Data.Kind qualified as GHC
import Bound.Scope (bindings)
import Control.Monad.State ( gets, runState, modify, State )
import Control.Monad.Except
    ( MonadError(throwError), runExceptT, ExceptT, liftEither )
import Data.Foldable (traverse_, foldl')
import Language.PureScript.CoreFn.TypeLike ( TypeLike(funTy) )
import Language.PureScript.CoreFn.Convert.DesugarObjects
import Data.Maybe (mapMaybe, fromJust, isJust)

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

next :: DatatypeM Int
next = do
  n <- gets (view counter)
  modify $ over counter (+ 1)
  pure n

mkTyName :: Qualified (ProperName 'TypeName) -> DatatypeM PIR.TyName
mkTyName qn = gets (view tyNames) >>= \tnames -> case M.lookup qn tnames of
  Just tyname -> pure tyname
  Nothing     -> do
    uniq <- next
    let tyname = PIR.TyName $ Name (runProperName . disqualify $ qn) $ PLC.Unique uniq
    modify $ over tyNames (M.insert qn tyname)
    pure tyname

mkConstrName :: Qualified Ident -> Int -> DatatypeM PIR.Name
mkConstrName qi cix = gets (view constrNames) >>= \cnames -> case M.lookup qi cnames of
  Just cname -> pure $ fst cname
  Nothing -> do
    uniq <- next
    let nm = Name (showIdent . disqualify $ qi) $ PLC.Unique uniq
    modify $ over constrNames (M.insert qi (nm,cix))
    pure nm

-- | Only gives you a TyName, doesn't insert anything into the context
mkNewTyVar :: Text -> DatatypeM TyName
mkNewTyVar nm = do
  uniq <- next
  pure .  PIR.TyName $ PIR.Name nm $ PLC.Unique uniq

-- | Only gives you a Name, doesn't insert anything into the context
mkNewVar :: Text -> DatatypeM PIR.Name
mkNewVar nm = PIR.Name nm . PLC.Unique <$> next

freshName :: DatatypeM PIR.Name
freshName = do
  uniq <- next
  let nm = "a" <> T.pack (show uniq)
  pure $ PIR.Name nm (PLC.Unique uniq)

mkVar :: Text -> DatatypeM PIR.Name
mkVar nm = gets (view vars) >>= \names -> case M.lookup nm names of
  Nothing -> do
     var <- mkNewVar nm
     modify $ over vars (M.insert nm var)
     pure var
  Just var -> pure var

mkBoundTyVarName :: Text -> DatatypeM PIR.TyName
mkBoundTyVarName nm = do
  boundTyVars <- gets _tyVars
  case M.lookup nm boundTyVars of
    Just tyName -> pure  tyName
    Nothing -> error $ "Free type variable in IR: " <> T.unpack nm

addDatatype :: Qualified (ProperName 'TypeName) -> PIRDatatype -> DatatypeM ()
addDatatype qn = modify . over pirDatatypes . M.insert qn

withLocalVars :: [Text] -> DatatypeM a -> DatatypeM a
withLocalVars names act = do
  oldVars <- gets (view vars)
  let shadowed = mapMaybe (\x -> sequence (x,M.lookup x oldVars)) names
  pirNames <- traverse (\x -> (x,) <$> mkNewVar x) names
  modify $ over vars (insertMany pirNames)
  res <- act
  modify $ over vars (insertMany shadowed . deleteMany names)
  pure res

-- | This overwrites
withLocalTyVars :: [Text] ->  DatatypeM a -> DatatypeM a
withLocalTyVars nms act = do
  oldTVs <- gets (view tyVars)
  let shadowed = mapMaybe (\x -> sequence (x,M.lookup x oldTVs)) nms
  tNames <- traverse (\x -> (x,) <$> mkNewTyVar x) nms
  modify $ over tyVars (insertMany tNames)
  res <- act
  modify $ over tyVars (insertMany shadowed . deleteMany nms)
  pure res

insertMany :: forall k v. Ord k => [(k,v)] -> Map k v -> Map k v
insertMany new acc = foldl' (flip $ uncurry M.insert) acc new

deleteMany :: forall k v. Ord k => [k] -> Map k v -> Map k v
deleteMany xs acc = foldl' (flip M.delete) acc xs

note :: String -> Maybe a -> DatatypeM a
note msg = maybe (throwError msg) pure

mkTypeBindDict :: Datatypes IR.Kind Ty
               -> Exp WithoutObjects Ty (FVar Ty)
               -> Either String DatatypeDictionary
mkTypeBindDict _datatypes main = case runState act initState of
   (Left err,_) -> Left err
   (Right _,res) -> pure res
  where
    datatypes = primData <> _datatypes
    act = runExceptT $ mkPIRDatatypes datatypes (allTypeConstructors main)
    initState = DatatypeDictionary M.empty M.empty M.empty M.empty M.empty M.empty maxIx
    maxIx = maxBV main

    allTypeConstructors :: Exp WithoutObjects Ty (FVar Ty) -> S.Set (Qualified (ProperName 'TypeName))
    allTypeConstructors e = S.fromList $  e ^.. cosmos . to (expTy F) . cosmos . _TyCon

    maxBV :: Exp WithoutObjects t (FVar t) -> Int
    maxBV e = foldr go 0 $ e ^.. cosmos
      where
        bvIx (BVar n _ _ ) = n
        go :: Exp WithoutObjects t (FVar t) -> Int -> Int
        go x acc = case x of
          LamE _ (BVar n _ _) _ -> max n acc
          LetE _ _ scope ->  foldr (max . bvIx) acc (bindings scope)
          CaseE _ _ alts -> maxAlt acc alts
          AppE e1 e2     -> max (go e1 acc) (go e2 acc)
          _ -> acc
         where
           maxAlt :: Int -> [Alt WithoutObjects t (Exp WithoutObjects t) (FVar t)] -> Int
           maxAlt n [] = n
           maxAlt n (UnguardedAlt _ _ scope:rest) =
             let thisMax = foldr (max . bvIx) n (bindings scope)
             in maxAlt thisMax rest

mkPIRDatatypes :: Datatypes IR.Kind Ty
               -> S.Set (Qualified (ProperName 'TypeName))
               -> DatatypeM ()
mkPIRDatatypes datatypes tyConsInExp = traverse_ go tyConsInExp
  where
    -- these things don't have datatype definitions anywhere
    truePrimitives = S.fromList [C.Function, C.Int, C.Char, C.String]

    go :: Qualified (ProperName 'TypeName)
       -> DatatypeM ()
    go qn | qn `S.member` truePrimitives = pure ()
    go qn@(Qualified _ (ProperName tnm)) = case lookupDataDecl qn datatypes of
      Nothing -> throwError $ "Error when translating data types to PIR: "
                        <> "Couldn't find a data type declaration for "
                        <> T.unpack (showQualified runProperName qn)
      Just dDecl -> do -- TODO: newtypes should probably be newtype-ey
        let declKind = mkDeclKind $ mkKind . snd <$> (dDecl ^. dDataArgs)
        tyName <- mkTyName qn
        let typeNameDecl = TyVarDecl () tyName declKind
        withLocalTyVars (fst <$> dDecl ^. dDataArgs) $ do
          argDecls <-  traverse mkArgDecl (dDecl ^. dDataArgs)
          uniq <- next
          let destructorName = PIR.Name ("match_" <> tnm) $ PLC.Unique uniq
          modify $ over destructors (M.insert qn destructorName)
          ctors <- traverse (mkCtorDecl qn) $ zip [0..] (dDecl ^. dDataCtors)
          let this = PIR.Datatype () typeNameDecl argDecls destructorName ctors
          modify $ over pirDatatypes (M.insert qn this)

    mkCtorDecl :: Qualified (ProperName 'TypeName)
               -> (Int,CtorDecl Ty)
               -> DatatypeM (PIR.VarDecl PIR.TyName PIR.Name PLC.DefaultUni ())
    mkCtorDecl qTyName (cix,ctorDecl) =  do
        let ctorFields = snd <$> ctorDecl ^. cdCtorFields
            ctorFunTy :: Ty
            ctorFunTy = foldr1 funTy (ctorFields <> [TyCon qTyName])
        ctorName <- mkConstrName (ctorDecl ^. cdCtorName) cix
        ctorFunTyPIR <- toPIRType  ctorFunTy
        pure $ VarDecl () ctorName ctorFunTyPIR

    mkDeclKind :: [PIR.Kind ()] -> PIR.Kind ()
    mkDeclKind = \case
      [] -> PIR.Type ()
      xs ->  foldr1 (PIR.KindArrow ()) (xs <> [PIR.Type ()])

    -- the arguments to the *type*, i.e., they're all tyvars
    -- NOTE: We should really make changes such that `Maybe SourceType` is `SourceType`
    mkArgDecl :: (Text, IR.Kind) -> DatatypeM (PIR.TyVarDecl PIR.TyName ())
    mkArgDecl (varNm,ki) = do
      tyVarNm <- mkBoundTyVarName varNm
      pure $ TyVarDecl () tyVarNm (mkKind ki)

toPIRType :: Ty -> DatatypeM PIRType
toPIRType = \case
  IR.TyVar txt _ -> PIR.TyVar () <$> mkBoundTyVarName txt
  TyCon qtn@(Qualified qb _) -> case qb of
    ByThisModuleName "Builtin" -> either throwError pure $  handleBuiltinTy qtn
    ByThisModuleName "Prim" | isJust (handlePrimTy qtn)->   pure . fromJust $ handlePrimTy qtn
    _ -> do
      tyName <- mkTyName qtn
      pure $ PIR.TyVar () tyName
  IR.TyApp t1 t2 -> goTypeApp t1 t2
  Forall _ v k ty _ -> do
    vTyName <- mkNewTyVar v
    withLocalTyVars [v] $ do
      ty' <- toPIRType ty
      pure $ TyForall () vTyName (mkKind k) ty'
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
     other      -> Nothing

mkKind ::  IR.Kind -> PIR.Kind ()
mkKind = \case
  IR.KindType -> PIR.Type ()
  IR.KindArrow k1 k2 -> PIR.KindArrow () (mkKind k1) (mkKind k2)

sourceTypeToKind :: SourceType  -> Either String (PIR.Kind ())
sourceTypeToKind = \case
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
getConstructorName qi = do
  ctors <- gets (view constrNames)
  pure $ ctors ^? ix qi . _1


prettyQPN :: Qualified (ProperName 'TypeName) -> String
prettyQPN = T.unpack . showQualified runProperName

prettyQI :: Qualified Ident -> String
prettyQI = T.unpack . showQualified runIdent

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

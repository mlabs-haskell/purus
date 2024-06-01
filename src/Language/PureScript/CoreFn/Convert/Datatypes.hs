{-# LANGUAGE GADTs, PolyKinds, TemplateHaskell #-}
{-# LANGUAGE StandaloneKindSignatures #-}
module Language.PureScript.CoreFn.Convert.Datatypes where

import Prelude

import Language.PureScript.CoreFn.Convert.IR
import Language.PureScript.CoreFn.Convert.IR qualified as IR
import Language.PureScript.CoreFn.Convert.DesugarObjects
import PlutusIR
import PlutusIR qualified as PIR
import PlutusCore qualified as PLC

import Language.PureScript.CoreFn.Convert.Monomorphize.Utils
import Control.Lens hiding (locally)
import Language.PureScript.Names
import Bound.Var

import Control.Monad (foldM)

import Data.Text (Text)
import Data.Text qualified as T
import Data.Map (Map)
import Data.Map qualified as M
import Data.Set qualified as S

import Language.PureScript.CoreFn.Module
import Language.PureScript.Environment
import Language.PureScript.Types
import Language.PureScript.AST.Declarations (DataConstructorDeclaration(..))
import Language.PureScript.Constants.Prim qualified as C
import Language.PureScript.Constants.Purus qualified as C
import Language.PureScript.CoreFn.Desugar.Utils (properToIdent, showIdent')
import Language.PureScript.CoreFn.Convert.DesugarObjects (tryConvertType, prettyErrorT)
import Language.PureScript.CoreFn.Expr (PurusType)
import Language.PureScript.CoreFn.Convert.DesugarCore
import Language.PureScript.CoreFn.Pretty (prettyTypeStr)
import Data.Kind qualified as GHC
import Type.Reflection (Typeable)
import Bound.Scope (bindings)
import Control.Monad.State
import Control.Monad.Except
import Data.Foldable (traverse_)
import Language.PureScript.CoreFn.TypeLike

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
data DatatypeDictionary = DatatypeDictionary {
    _pirDatatypes :: [PIRDatatype],
    _constrNames  :: Map (Qualified Ident) PIR.Name,
    _tyNames      :: Map (Qualified (ProperName 'TypeName)) PIR.TyName,
    _counter      :: Int
}

-- jfc why didn't i makeLenses everywhere
makeLenses ''DatatypeDictionary

type DatatypeM  = ExceptT String (State DatatypeDictionary)

next :: DatatypeM Int
next = do
  n <- view counter
  modify $ over counter (+ 1)
  pure n

mkTyName :: Qualified (ProperName 'TypeName) -> DatatypeM PIR.TyName
mkTyName qn = view tyNames >>= \tnames -> case M.lookup qn tnames of
  Just tyname -> pure tyname
  Nothing     -> do
    uniq <- next
    let tyname = PIR.TyName $ Name (runProperName . disqualify $ qn) $ PLC.Unique uniq
    modify $ over tyNames (M.insert qn tyname)
    pure tyname

mkConstrName :: Qualified Ident -> DatatypeM PIR.Name
mkConstrName qi = view constrNames >>= \cnames -> case M.lookup qi cnames of
  Just cname -> pure cname
  Nothing -> do
    uniq <- next
    let nm = Name (showIdent . disqualify $ qi) $ PLC.Unique uniq
    modify $ over constrNames (M.insert qi nm)
    pure nm

addDatatype :: PIRDatatype -> ConvertM ()
addDatatype = modify . over pirDatatypes . (:)

-- "local" but preserves the counter so we don't reuse uniques across scopes (b/c i can't figure out
-- how uniques work in the PIR compiler -_-)
locally :: DatatypeM a -> DatatypeM a
locally act = do
  s <- get
  case runState (runExceptT act) s of
    (Left err,_) -> throwError err
    (Right a,s') -> do
      modify $ set counter (s' ^. counter)
      pure a

note :: String -> Maybe a -> DatatypeM a
note msg = maybe (throwError msg) pure


mkTypeBindDict :: Datatypes IR.Kind Ty
               -> Exp WithoutObjects Ty (FVar Ty)
               -> Either String DatatypeDictionary
mkTypeBindDict datatypes main = case runState act initState of
   (Left err,_) -> Left err
   (Right _,res) -> pure res
  where
    act = runExceptT $ mkPIRDatatypes datatypes (allTypeConstructors main)
    initState = DatatypeDictionary [] M.empty M.empty maxIx
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
          _ -> acc
         where
           maxAlt :: Int -> [Alt WithoutObjects t (Exp WithoutObjects t) (FVar t)] -> Int
           maxAlt n [] = n
           maxAlt n (UnguardedAlt _ _ scope:rest) =
             let thisMax = foldr (max . bvIx) n (bindings scope )
             in maxAlt thisMax rest


mkPIRDatatypes :: Datatypes IR.Kind Ty
               -> S.Set (Qualified (ProperName 'TypeName))
               -> DatatypeM ()
mkPIRDatatypes datatypes tyConsInExp = traverse_ go tyConsInExp
  where
    go :: Qualified (ProperName 'TypeName)
       -> DatatypeM ()
    go acc qn@(Qualified qb (ProperName tnm)) = case lookupDataDecl qn datatypes of
      Nothing -> throwError $ "Error when translating data types to PIR: "
                        <> "Couldn't find a data type declaration for "
                        <> T.unpack (showQualified runProperName qn)
      Just dDecl -> do -- TODO: newtypes should probably be newtype-ey
        let declKind = mkDeclKind $ mkKind . snd <$> (dDecl ^. dDataArgs)
        tyName <- mkTyName qn
        let typeNameDecl = TyVarDecl () tyName declKind
        argDecls <-  traverse (mkArgDecl) (dDecl ^. dDataArgs)
        let deconstructorName = Ident $ "match_" <> tnm
        ctors <- traverse (mkCtorDecl qn) $ dDecl ^. dDataCtors
        let this = PIR.Datatype () typeNameDecl argDecls deconstructorName ctors
        pure $ M.insert qn this acc

    mkCtorDecl :: Qualified (ProperName 'TypeName)
               -> CtorDecl Ty
               -> DatatypeM (PIR.VarDecl PIR.TyName PIR.Name PLC.DefaultUni ())
    mkCtorDecl qTyName ctorDecl =  do
        let ctorFields = ctorDecl ^. cdCtorFields . _2
            ctorFunTy :: Ty
            ctorFunTy = foldr1 funTy (ctorFields <> [TyCon qTyName])
        ctorName <- mkConstrName (ctorDecl ^. cdCtorName)
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
      -- Here, as elsewhere, a sourcePos qualified ident represents a bound var
      let qVarNm = qualifyNull (ProperName varNm)
      tyVarNm <- mkTyName
      pure $ TyVarDecl () qVarNm (mkKind ki)



toPIRType :: Ty -> DatatypeM PIRType
toPIRType = \case
  IR.TyVar txt k ->  pure $ PIR.TyVar () $ qualifyNull (ProperName txt)
  TyCon qtn@(Qualified qb _) -> case qb of
    ByThisModuleName "Builtin" -> either throwError pure $  handleBuiltinTy qtn
    ByThisModuleName "Prim" ->  either throwError pure $ handlePrimTy qtn
    _ -> do
      tyName <- mkTyName qtn
      pure $ PIR.TyVar () tyName
  -- TODO: Special handling for function types (-> is a TyCon in PS but a Ctor of the Type ADT in plc )
  IR.TyApp t1 t2 -> goTypeApp t1 t2
  Forall _ v k ty _ -> do
    ty' <- toPIRType ty
    pure $ TyForall () (qualifyNull $ ProperName v) (mkKind k) ty'
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

handlePrimTy :: Qualified (ProperName 'TypeName) -> Either String (PLC.Type tyname PLC.DefaultUni ())
handlePrimTy = \case
     C.Function -> Left  "function types should be caught in apps"
     C.Array    -> pure $ TyBuiltin () (PLC.SomeTypeIn PLC.DefaultUniProtoList) -- is this wrong? do we want a SOP list? too tired to know
     C.String   -> pure $ TyBuiltin () (PLC.SomeTypeIn PLC.DefaultUniString)
     C.Char     -> pure $ TyBuiltin () (PLC.SomeTypeIn PLC.DefaultUniInteger)
     C.Int      -> pure $ TyBuiltin () (PLC.SomeTypeIn PLC.DefaultUniInteger)
     C.Boolean  -> pure $ TyBuiltin () (PLC.SomeTypeIn PLC.DefaultUniBool)
     other      -> Left  $ "unsupported prim tycon: " <> show other

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

extractUni :: Show tyName => PIR.Type tyName PLC.DefaultUni () -> Either String SomeUni
extractUni = \case
  PlcInt -> pure $ SomeUni PLC.DefaultUniInteger
  PlcBool -> pure $ SomeUni PLC.DefaultUniBool
  PlcString -> pure $ SomeUni PLC.DefaultUniString
  PlcByteString -> pure $ SomeUni PLC.DefaultUniByteString
  PlcPair a b -> do
    SomeUni a' <- extractUni a
    SomeUni b' <- extractUni b
    pure . SomeUni $ PLC.DefaultUniPair a' b'
  PlcList a -> do
    SomeUni a' <- extractUni a
    pure . SomeUni $ PLC.DefaultUniList a'
  other -> Left $ "Not a PLC constant-able type:\n " <> show other

module Language.PureScript.CoreFn.Convert.Datatypes where

import Prelude

import Language.PureScript.CoreFn.Convert.IR
import Language.PureScript.CoreFn.Convert.IR qualified as IR
import Language.PureScript.CoreFn.Convert.DesugarObjects
import PlutusIR
import PlutusIR qualified as PIR
import PlutusCore qualified as PLC

import Language.PureScript.CoreFn.Convert.Monomorphize.Utils
import Control.Lens
import Language.PureScript.Names
import Bound.Var

import Control.Monad (foldM)

import Data.Text (Text)
import Data.Text qualified as T
import Data.Map (Map)
import Data.Map qualified as M
import Data.Set qualified as S

import Language.PureScript.CoreFn.Module (Module(..))
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
 

mkTypeBindDict :: Module IR_Decl a
               -> Exp WithoutObjects Ty (FVar Ty)
               -> Either String (Map PSTypeName PIRDatatypeWithPSNames)
mkTypeBindDict Module{..} main = mkPIRDatatypes moduleDecls moduleDataTypes' (allTypeConstructors main)
  where
    -- HACK: we'll eventually need to pass a container of all of the decls in scope for the module being compiled
    moduleDataTypes' = M.mapKeys (Qualified (ByModuleName moduleName)) moduleDataTypes

allTypeConstructors :: Exp WithoutObjects Ty (FVar Ty) -> S.Set (Qualified (ProperName 'TypeName))
allTypeConstructors e = S.fromList $  e ^.. cosmos . to (expTy F) . cosmos . _TyCon

-- NOTE: In the current (single-module) compilation mode the data types aren't
--       qualified, but eventually (i.e. when we have a linker) the map
--       that contains then should have qualified keys
type ModuleDataTypes = Map
                      (Qualified (ProperName 'TypeName))
                      (DataDeclType,[(Text, SourceType)],[DataConstructorDeclaration])

-- Maybe this should be the WithObjects / PurusType version? I don't think we ever explicitly convert all of the declarations
-- (and it'd be better to JUST convert the constructor functions that we need)
-- actually i think we just care about the types?
type ModuleDeclarations = [BindE PurusType (Exp WithObjects PurusType) (FVar PurusType)]

type PIRDatatypeWithPSNames = PIR.Datatype
                              (Qualified (ProperName 'TypeName))
                              Ident
                              PLC.DefaultUni
                              ()

type PIRDatatype = PIR.Datatype
                     PIR.TyName
                     PIR.Name
                     PLC.DefaultUni
                     ()

type PSTypeName = Qualified (ProperName 'TypeName)


mkPIRDatatypes :: ModuleDeclarations -- We already translate the constructor functions and it's easier to just *look* for them vs doing it all again
               -> ModuleDataTypes    -- /\ NOTE: We might have to make sure we don't inline constructor functions?
               -> S.Set PSTypeName
               -> Either String (Map PSTypeName PIRDatatypeWithPSNames)
mkPIRDatatypes moduleDecls moduleADTs tyConsInExp = foldM go M.empty tyConsInExp
  where
    go :: Map (Qualified (ProperName 'TypeName)) PIRDatatypeWithPSNames
       -> Qualified (ProperName 'TypeName)
       -> Either String (Map (Qualified (ProperName 'TypeName)) PIRDatatypeWithPSNames)
    go acc qn@(Qualified qb (ProperName tnm)) = case M.lookup qn moduleADTs of
      Nothing -> Left $ "Error when translating data types to PIR: "
                        <> "Couldn't find a data type declaration for "
                        <> T.unpack (showQualified runProperName qn)
      Just (declTy,args,ctorDecls) -> do -- TODO: newtypes should probably be newtype-ey
        declKind <- mkDeclKind args
        let typeNameDecl = TyVarDecl () qn declKind
        argDecls <- traverse mkArgDecl args
        let deconstructorName = Ident $ "match_" <> tnm
        ctors <- traverse mkCtorDecl ctorDecls
        let this :: PIRDatatypeWithPSNames
            this = PIR.Datatype () typeNameDecl argDecls deconstructorName ctors
        pure $ M.insert qn this acc

    mkCtorDecl :: DataConstructorDeclaration -> Either String (VarDecl (Qualified (ProperName 'TypeName)) Ident  PLC.DefaultUni ())
    mkCtorDecl DataConstructorDeclaration{..} = do
      let ctorIdent = properToIdent dataCtorName
      case findDeclBody' ctorIdent moduleDecls of
        Nothing -> Left $ "Error when translating Ctors. No constructor function expr found for: "
                          <> showIdent' ctorIdent
        Just ctorFun -> do
          let ctorFunTy = expTy F ctorFun
          ctorFunTyIR <- either (Left . prettyErrorT) pure $ tryConvertType ctorFunTy
          -- REVIEW: Do we need to construct PIRDatatypes for all the ctor args? Not sure. Guess we'll find out.
          ctorFunTyPIR <- toPIRType ctorFunTyIR
          pure $ VarDecl () ctorIdent ctorFunTyPIR

    mkDeclKind :: [(Text,  SourceType)] -> Either String (PIR.Kind ())
    mkDeclKind kargs = traverse (sourceTypeToKind . snd) kargs >>= \case
      [] -> pure $ PIR.Type ()
      xs -> pure $ foldr1 (PIR.KindArrow ()) (xs <> [PIR.Type ()])

    -- the arguments to the *type*, i.e., they're all tyvars
    -- NOTE: We should really make changes such that `Maybe SourceType` is `SourceType`
    mkArgDecl :: (Text, SourceType) -> Either String (TyVarDecl (Qualified (ProperName 'TypeName)) ())
    mkArgDecl (varNm,ki) = do
      kind <- sourceTypeToKind ki
      -- Here, as elsewhere, a sourcePos qualified ident represents a bound var
      let qVarNm = qualifyNull (ProperName varNm)
      pure $ TyVarDecl () qVarNm kind

type PIRTypeWithPSNames = PIR.Type (Qualified (ProperName 'TypeName)) PLC.DefaultUni ()

toPIRType :: Ty -> Either String PIRTypeWithPSNames
toPIRType = \case
  IR.TyVar txt k ->  pure $ PIR.TyVar () $ qualifyNull (ProperName txt)
  TyCon qtn@(Qualified qb tn) -> case qb of
    ByThisModuleName "Builtin" -> handleBuiltinTy qtn
    ByThisModuleName "Prim" ->  handlePrimTy qtn
    _ -> pure $ PIR.TyVar () qtn
  -- TODO: Special handling for function types (-> is a TyCon in PS but a Ctor of the Type ADT in plc )
  IR.TyApp t1 t2 -> goTypeApp t1 t2
  Forall _ v k ty _ -> do
    ty' <- toPIRType ty
    pure $ TyForall () (qualifyNull $ ProperName v) (mkKind k) ty'
  other -> error $ "Upon reflection, other types like " <> ppTy other <> " shouldn't be allowed in the Ty ast"
 where
   goTypeApp (IR.TyApp f a) b
     | f == TyCon C.Function = do
         a' <- toPIRType a
         b' <- toPIRType b
         pure $ PIR.TyFun () a' b'
     | otherwise = PIR.TyApp () <$> toPIRType a <*> toPIRType b

handleBuiltinTy :: Qualified (ProperName 'TypeName) -> Either String (PIR.Type tyname PLC.DefaultUni ())
handleBuiltinTy = \case
     C.BuiltinData -> pure $ TyBuiltin () (PLC.SomeTypeIn PLC.DefaultUniData)
     C.BuiltinPair -> pure $ TyBuiltin () (PLC.SomeTypeIn PLC.DefaultUniProtoPair)
     C.BuiltinList -> pure $ TyBuiltin () (PLC.SomeTypeIn PLC.DefaultUniProtoList)
     C.BuiltinByteString -> pure $ TyBuiltin () (PLC.SomeTypeIn PLC.DefaultUniByteString)
     other -> Left $   "Error when translating to PIR types: unsupported builtin type: " <> show other

handlePrimTy :: Qualified (ProperName 'TypeName) -> Either String (PLC.Type tyname PLC.DefaultUni ())
handlePrimTy = \case
     C.Function -> Left $  "function types should be caught in apps"
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

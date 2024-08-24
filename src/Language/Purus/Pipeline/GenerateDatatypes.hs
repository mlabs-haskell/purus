{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Language.Purus.Pipeline.GenerateDatatypes where

import Prelude

import Data.Map qualified as M
import Data.Set qualified as S
import Data.Text (Text)
import Data.Text qualified as T

import Control.Monad.State (modify)
import Data.Foldable (foldl', traverse_)
import Data.Maybe (fromJust, isJust)

import Language.PureScript.Constants.Prim qualified as C
import Language.PureScript.Constants.Purus qualified as C
import Language.PureScript.Names (
  ProperName (..),
  ProperNameType (..),
  Qualified (..),
  showQualified,
  pattern ByThisModuleName,
 )
import Language.PureScript.CoreFn.Module (
  CtorDecl (..),
  Datatypes,
  cdCtorFields,
  cdCtorName,
  dDataArgs,
  dDataCtors,
  lookupDataDecl,
  tyDict,
 )
import Language.PureScript.CoreFn.TypeLike
import Language.PureScript.Environment (pattern (:->))
import Language.PureScript.Types (
  SourceType,
  Type (TypeConstructor),
 )

import Language.Purus.IR (
  FVar,
  BVar,
  Exp (..),
  Ty (..),
  ppTy,
 )
import Language.Purus.IR qualified as IR
import Language.Purus.Debug ( doTraceM )
import Language.Purus.Pretty.Common ( prettyStr )
import Language.Purus.Pretty.Types ( prettyTypeStr )
import Language.Purus.IR.Utils ( WithoutObjects )
import Language.Purus.Pipeline.Monad
    ( MonadCounter(next), PlutusContext )
import Language.Purus.Types ( destructors, pirDatatypes, PIRType )
import Language.Purus.Pipeline.GenerateDatatypes.Utils
    ( foldr1Err,
      mkTyName,
      mkConstrName,
      mkNewTyVar,
      getBoundTyVarName,
      bindTV,
      prettyQPN )

import PlutusCore qualified as PLC
import PlutusIR (
  TyVarDecl (TyVarDecl),
  Type (TyBuiltin, TyForall),
  VarDecl (VarDecl),
 )
import PlutusIR qualified as PIR

import Bound.Var (Var)
import Control.Lens (
  over,
  to,
  (^.),
 )
import Control.Monad.Except (
  MonadError (throwError),
 )

generateDatatypes ::
  Datatypes IR.Kind Ty ->
  Exp WithoutObjects Ty (Var (BVar Ty) (FVar Ty)) ->
  PlutusContext ()
generateDatatypes datatypes main =  mkPIRDatatypes datatypes (allTypeConstructors main)
  where
    allTypeConstructors :: Exp WithoutObjects Ty (Var (BVar Ty) (FVar Ty)) -> S.Set (Qualified (ProperName 'TypeName))
    allTypeConstructors _ = datatypes ^. tyDict . to M.keys . to S.fromList

mkPIRDatatypes ::
  Datatypes IR.Kind Ty ->
  S.Set (Qualified (ProperName 'TypeName)) ->
  PlutusContext ()
mkPIRDatatypes datatypes tyConsInExp =
  doTraceM "mkPIRDatatypes" (show $ S.map prettyQPN tyConsInExp)
    >> traverse_ go tyConsInExp
  where
    -- these things don't have datatype definitions anywhere
    truePrimitives = S.fromList [C.Function, C.Int, C.Char, C.String]

    go ::
      Qualified (ProperName 'TypeName) ->
      PlutusContext ()
    go qn | qn `S.member` truePrimitives = pure ()
    go qn@(Qualified _ (ProperName tnm)) =
      doTraceM "mkPIRDatatypes" ("go: " <> prettyQPN qn) >> case lookupDataDecl qn datatypes of
        Nothing ->
          throwError $
            "Error when translating data types to PIR: "
              <> "Couldn't find a data type declaration for "
              <> T.unpack (showQualified runProperName qn)
        Just dDecl -> do
          -- TODO: newtypes should probably be newtype-ey
          let declArgs = fst <$> dDecl ^. dDataArgs
              declKind = mkDeclKind $ mkKind . snd <$> dDecl ^. dDataArgs
          doTraceM "mkPIRDatatypes" $ "Decl " <> prettyStr dDecl
          doTraceM "mkPIRDatatypes" $ "decl args: " <> show declArgs
          tyName <- mkTyName qn
          let typeNameDecl = TyVarDecl () tyName declKind
              dataArgs = dDecl ^. dDataArgs
          argDecls <- traverse mkArgDecl (dDecl ^. dDataArgs)
          uniq <- next
          let destructorName = PIR.Name ("match_" <> tnm) $ PLC.Unique uniq
          modify $ over destructors (M.insert qn destructorName)
          ctors <- traverse (mkCtorDecl qn dataArgs) $ zip [0 ..] (dDecl ^. dDataCtors)
          let this = PIR.Datatype () typeNameDecl argDecls destructorName ctors
          modify $ over pirDatatypes (M.insert qn this)

    mkCtorDecl ::
      Qualified (ProperName 'TypeName) ->
      [(Text, IR.Kind)] ->
      (Int, CtorDecl Ty) ->
      PlutusContext (PIR.VarDecl PIR.TyName PIR.Name PLC.DefaultUni ())
    mkCtorDecl qTyName dataArgs (cix, ctorDecl) =
      doTraceM "mkCtorDecl" (prettyQPN qTyName) >> do
        let ctorFields = snd <$> ctorDecl ^. cdCtorFields
            resultTy' = foldl' TyApp (TyCon qTyName) (uncurry TyVar <$> dataArgs)
            ctorFunTy :: Ty
            ctorFunTy = foldr1Err "mkCtorDecl" funTy (ctorFields <> [resultTy'])
        ctorName <- mkConstrName (ctorDecl ^. cdCtorName) cix
        ctorFunTyPIR <- toPIRType ctorFunTy
        pure $ VarDecl () ctorName ctorFunTyPIR

    mkDeclKind :: [PIR.Kind ()] -> PIR.Kind ()
    mkDeclKind = \case
      [] -> PIR.Type ()
      xs -> foldr1Err "mkDeclKind" (PIR.KindArrow ()) (xs <> [PIR.Type ()])

    -- the arguments to the *type*, i.e., they're all tyvars
    -- NOTE: We should really make changes such that `Maybe SourceType` is `SourceType`
    mkArgDecl :: (Text, IR.Kind) -> PlutusContext (PIR.TyVarDecl PIR.TyName ())
    mkArgDecl (varNm, ki) = do
      tyVarNm <- mkNewTyVar varNm
      bindTV varNm tyVarNm
      pure $ TyVarDecl () tyVarNm (mkKind ki)

toPIRType :: Ty -> PlutusContext PIRType
toPIRType _ty = case _ty of
  IR.TyVar txt _ -> PIR.TyVar () <$> getBoundTyVarName txt
  TyCon qtn@(Qualified qb _) -> case qb of
    ByThisModuleName "Builtin" -> either throwError pure $ handleBuiltinTy qtn
    ByThisModuleName "Prim" | isJust (handlePrimTy qtn) -> pure . fromJust $ handlePrimTy qtn
    _ -> do
      tyName <- mkTyName qtn
      let result = PIR.TyVar () tyName
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
    let result = TyForall () vTyName (mkKind k) ty'
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
  other -> Left $ "Error when translating to PIR types: unsupported builtin type: " <> show other

handlePrimTy :: Qualified (ProperName 'TypeName) -> Maybe (PLC.Type tyname PLC.DefaultUni ())
handlePrimTy = \case
  C.String -> pure $ TyBuiltin () (PLC.SomeTypeIn PLC.DefaultUniString)
  C.Char -> pure $ TyBuiltin () (PLC.SomeTypeIn PLC.DefaultUniInteger)
  C.Int -> pure $ TyBuiltin () (PLC.SomeTypeIn PLC.DefaultUniInteger)
  _ -> Nothing

mkKind :: IR.Kind -> PIR.Kind ()
mkKind = \case
  IR.KindType -> PIR.Type ()
  IR.KindArrow k1 k2 -> PIR.KindArrow () (mkKind k1) (mkKind k2)

sourceTypeToKind :: SourceType -> Either String (PIR.Kind ())
sourceTypeToKind _t =
  doTraceM "sourceTypeToKind" (prettyStr _t) >> case _t of
    TypeConstructor _ C.Type -> pure $ PIR.Type ()
    t1 :-> t2 -> do
      t1' <- sourceTypeToKind t1
      t2' <- sourceTypeToKind t2
      pure $ PIR.KindArrow () t1' t2'
    other -> Left $ "Error: PureScript type '" <> prettyTypeStr other <> " is not a valid Plutus Kind"




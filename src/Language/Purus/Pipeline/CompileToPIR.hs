{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Language.Purus.Pipeline.CompileToPIR (compileToPIR) where

import Prelude

import Data.List.NonEmpty qualified as NE
import Data.Map qualified as M
import Data.Text qualified as T

import Control.Monad (
  foldM,
 )
import Control.Monad.Except (MonadError (..))
import Data.Bifunctor (Bifunctor (..))

import Language.PureScript.Constants.PLC (defaultFunMap)
import Language.PureScript.Constants.Prim qualified as C
import Language.PureScript.CoreFn.FromJSON ()
import Language.PureScript.CoreFn.Module (
  Datatypes,
 )
import Language.PureScript.Names (
  Ident (..),
  Qualified (..),
  QualifiedBy (ByModuleName),
  runIdent,
 )
import Language.PureScript.PSString (prettyPrintString)

import Language.Purus.IR (
  BVar (..),
  BindE (..),
  Exp (..),
  FVar (..),
  Lit (CharL, IntL, StringL),
  Ty,
  expTy
 )
import Language.Purus.IR qualified as IR
import Language.Purus.IR.Utils (Vars, WithoutObjects, toExp)
import Language.Purus.Pipeline.GenerateDatatypes (
  mkKind,
  toPIRType,
 )
import Language.Purus.Pipeline.GenerateDatatypes.Utils (
  bindTV,
  getConstructorName,
 )
import Language.Purus.Pipeline.Monad (PlutusContext)
import Language.Purus.Pretty.Common (prettyStr)
import Language.Purus.Types (PIRTerm, pirDatatypes)
import PlutusCore.Default (
  DefaultFun,
  DefaultUni,
 )

import PlutusCore (Unique (..))
import PlutusCore qualified as PLC
import PlutusIR (Binding (TermBind), Name (Name), Strictness (..), Term (Builtin), VarDecl (VarDecl))
import PlutusIR qualified as PIR
import PlutusIR.MkPir (mkConstant)

import Bound (Var (..))
import Control.Lens (view)
import Language.Purus.Pipeline.CompileToPIR.Utils (builtinSubstitutions)

type PIRTermBind = Binding PLC.TyName Name DefaultUni DefaultFun ()

pattern Unit :: FVar t
pattern Unit <- FVar _ (Qualified (ByModuleName C.M_Prim) (Ident "unit"))

compileToPIR ::
  Datatypes IR.Kind Ty ->
  Exp WithoutObjects Ty (Vars Ty) ->
  PlutusContext PIRTerm
compileToPIR _datatypes _exp = do
  resBody <- compileToPIR' _datatypes _exp
  datatypes <- view pirDatatypes
  let binds = NE.fromList $ map (PIR.DatatypeBind ()) . M.elems $ datatypes
  pure $ PIR.Let () PIR.Rec binds resBody

compileToPIR' ::
  Datatypes IR.Kind Ty ->
  Exp WithoutObjects Ty (Vars Ty) ->
  PlutusContext PIRTerm
compileToPIR' datatypes _exp =
  case _exp of
    V x -> case x of
      F Unit -> pure $ mkConstant () ()
      F (FVar _ ident@(Qualified _ (runIdent -> nm))) ->
        case M.lookup (T.unpack nm) defaultFunMap of
          Just aBuiltinFun -> case M.lookup aBuiltinFun builtinSubstitutions of
            Nothing -> pure $ Builtin () aBuiltinFun
            Just substBuiltin -> substBuiltin
          Nothing -> do
            getConstructorName ident >>= \case
              Just aCtorNm -> pure $ PIR.Var () aCtorNm
              Nothing ->
                throwError $
                  T.unpack nm
                    <> " isn't a builtin, and it shouldn't be possible to have a"
                    <> " free variable that's anything but a builtin. Please "
                    <> "report this bug to the Purus authors. "
      B (BVar bvix _ (runIdent -> nm)) -> pure $ PIR.Var () (Name nm $ Unique bvix)
    LitE _ lit -> compileToPIRLit lit
    (LamE (BVar bvIx bvT bvNm) body) -> do
      ty' <- toPIRType bvT
      let nm = Name (runIdent bvNm) $ Unique bvIx
          body' = toExp body
      body'' <- compileToPIR' datatypes body'
      pure $ PIR.LamAbs () nm ty' body''
    AppE e1 e2 -> do
      e1' <- compileToPIR' datatypes e1
      e2' <- compileToPIR' datatypes e2
      pure $ PIR.Apply () e1' e2'
    LetE binds body -> do
      boundTerms <- foldM convertBind [] binds
      body' <- compileToPIR' datatypes $ toExp body
      case NE.nonEmpty boundTerms of
        -- REVIEW: For simplicity we assume here that all let bindings are mutually recursive.
        --         This might not be great for performance (depends on what the PIR compiler does)
        Just boundTerms' -> pure $ PIR.Let () PIR.Rec boundTerms' body'
        Nothing -> error "empty bindings"
    ce@CaseE {} -> error $ "Case expressions should be eliminated by now, but found:\n\n" <> prettyStr ce
    TyInstE t e -> do
      t' <- toPIRType t
      e' <- compileToPIR' datatypes e
      pure $ PIR.TyInst () e' t'
    TyAbs (BVar bvIx bvT bvNm) e -> do
      let bvKind = mkKind bvT
          bvNmTxt = runIdent bvNm
          tNm = PIR.TyName $ PIR.Name (runIdent bvNm) (Unique bvIx)
      bindTV bvNmTxt tNm
      e' <- compileToPIR' datatypes e
      pure $ PIR.TyAbs () tNm bvKind e'
  where
    convertBind ::
      [PIRTermBind] ->
      BindE Ty (Exp WithoutObjects Ty) (Vars Ty) ->
      PlutusContext [PIRTermBind]
    convertBind acc = \case
      NonRecursive ident bvix expr -> do
        let unscoped = toExp expr
        nonRec <- goBind (ident, bvix) unscoped
        pure $ nonRec : acc
      Recursive xs -> do
        xs' <- traverse (uncurry goBind . second toExp) xs
        pure $ xs' <> acc
      where
        goBind :: (Ident, Int) -> Exp WithoutObjects Ty (Var (BVar Ty) (FVar Ty)) -> PlutusContext PIRTermBind
        goBind (ident, i) expr = do
          let nm = Name (runIdent ident) $ Unique i
          ty <- toPIRType (expTy id expr)
          expr' <- compileToPIR' datatypes expr
          -- REVIEW: Not sure if this should always be strict?
          pure $ TermBind () Strict (VarDecl () nm ty) expr'

    compileToPIRLit ::
      Lit WithoutObjects (Exp WithoutObjects Ty (Vars Ty)) ->
      PlutusContext PIRTerm
    compileToPIRLit = \case
      IntL i -> pure $ mkConstant () i
      StringL str ->
        pure $ mkConstant () $ prettyPrintString str
      CharL c ->
        pure
          . mkConstant ()
          . toInteger
          . fromEnum
          $ c

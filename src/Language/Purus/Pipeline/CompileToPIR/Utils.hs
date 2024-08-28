{-# LANGUAGE GADTs #-}

module Language.Purus.Pipeline.CompileToPIR.Utils where

import Prelude

import Data.Kind qualified as GHC
import Data.List (foldl')
import Data.List.NonEmpty qualified as NE

import Language.Purus.IR (Ty)
import Language.Purus.Pipeline.GenerateDatatypes (toPIRType)
import Language.Purus.Pipeline.GenerateDatatypes.Utils (
  freshName,
 )
import Language.Purus.Pipeline.Monad (PlutusContext)
import Language.Purus.Types (PIRTerm, PIRType)

import PlutusCore qualified as PLC
import PlutusIR (
  Binding (TermBind),
  Recursivity (NonRec),
  Strictness (..),
  Type (TyBuiltin),
 )
import PlutusIR qualified as PIR
import PlutusIR.MkPir (mkConstant)

import Control.Monad.Except (
  liftEither,
 )

-- Utilities for PIR type conversion
pattern (:@) :: PIR.Type tyName PLC.DefaultUni () -> PIR.Type tyName PLC.DefaultUni () -> PIR.Type tyName PLC.DefaultUni ()
pattern f :@ e = PIR.TyApp () f e

infixl 9 :@

pattern PlcPair :: PIR.Type tyName PLC.DefaultUni () -> PIR.Type tyName PLC.DefaultUni () -> PIR.Type tyName PLC.DefaultUni ()
pattern PlcPair a b = TyBuiltin () (PLC.SomeTypeIn PLC.DefaultUniProtoPair) :@ a :@ b

pattern PlcList :: PIR.Type tyName PLC.DefaultUni () -> PIR.Type tyName PLC.DefaultUni ()
pattern PlcList a = TyBuiltin () (PLC.SomeTypeIn PLC.DefaultUniProtoList) :@ a

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
  SomeUni :: forall (a :: GHC.Type). PLC.DefaultUni (PLC.Esc a) -> SomeUni

extractUni :: (Show tyName) => PIR.Type tyName PLC.DefaultUni () -> PlutusContext SomeUni
extractUni = liftEither . extractUni'

extractUni' :: (Show tyName) => PIR.Type tyName PLC.DefaultUni () -> Either String SomeUni
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

{-
    PIR Constants, Term Builders, Helpers, Etc
-}

tyBuiltinBool :: PIRType
tyBuiltinBool = PLC.TyBuiltin () (PLC.SomeTypeIn PLC.DefaultUniBool)

(#) :: PIRTerm -> PIRTerm -> PIRTerm
e1 # e2 = PIR.Apply () e1 e2

-- I think this is the right fixity? TODO: Check plutarch
infixl 9 #

pirIfThen :: PIRType -> PIRTerm -> PIRTerm -> PIRTerm -> PlutusContext PIRTerm
pirIfThen resTy cond troo fawlse = do
  troo' <- pirDelay troo
  fawlse' <- pirDelay fawlse
  pure . pirForce $ pirTyInst (PIR.TyFun () sopUnit resTy) (PIR.Builtin () PLC.IfThenElse) # cond # troo' # fawlse'

-- utility for constructing LamAbs w/ a fresh variable name. We do this a lot in the case analysis stuff
freshLam ::
  Ty -> -- type of the fresh var being created
  (PIRType -> PIRTerm -> PlutusContext PIRTerm) -> -- fn from that fresh var to a term
  PlutusContext PIRTerm
freshLam t f = do
  name <- freshName
  t' <- toPIRType t
  PIR.LamAbs () name t' <$> f t' (PIR.Var () name)

sopUnit :: PIRType
sopUnit = PIR.TySOP () [[]]

pirTyInst :: PIRType -> PIRTerm -> PIRTerm
pirTyInst ty term = PIR.TyInst () term ty

tyInstMany :: PIRTerm -> [PIRType] -> PIRTerm
tyInstMany = foldl' (flip pirTyInst)

sopUnitTerm :: PIRTerm
sopUnitTerm = PIR.Constr () sopUnit 0 []

pirDelay :: PIRTerm -> PlutusContext PIRTerm
pirDelay term = do
  nm <- freshName
  pure $ PIR.LamAbs () nm sopUnit term

pirForce :: PIRTerm -> PIRTerm
pirForce term = PIR.Apply () term sopUnitTerm

pirEqInt :: PIRTerm -> PIRTerm -> PIRTerm
pirEqInt i1 i2 =
  let pirEq = PIR.Builtin () PLC.EqualsInteger
   in pirEq # i1 # i2

pirEqString :: PIRTerm -> PIRTerm -> PIRTerm
pirEqString s1 s2 =
  let pirEq = PIR.Builtin () PLC.EqualsString
   in pirEq # s1 # s2

-- delayed (which is literally always what we want). Not sure if need to force here?
pirError :: PIRType -> PlutusContext PIRTerm
pirError t = pirForce <$> pirDelay (PIR.Error () t)

-- for builtin booleans jfc why don't they have thiiiisss
pirAnd :: PIRTerm -> PIRTerm -> PlutusContext PIRTerm
pirAnd t1 t2 = do
  tBranch <- pirIfThen tyBuiltinBool t2 (mkConstant () True) (mkConstant () False)
  pirIfThen tyBuiltinBool t1 tBranch (mkConstant () False)

pirLetNonRec ::
  PIRType -> -- type of the expression we're let- binding
  PIRTerm ->
  (PIRTerm -> PlutusContext PIRTerm) ->
  PlutusContext PIRTerm
pirLetNonRec ty toLet f = do
  nm <- freshName
  let myvar = PIR.Var () nm
      varDecl = PIR.VarDecl () nm ty
      binding = TermBind () NonStrict varDecl toLet
  PIR.Let () NonRec (NE.singleton binding) <$> f myvar

{- Contains utilities for PIR CodeGen, mainly helper functions which run in the PlutusContext
   Monad to perform tasks that require access to the datatype context or counter.
-}
{-# LANGUAGE GADTs #-}
module Language.Purus.Pipeline.CompileToPIR.Utils (builtinSubstitutions) where

import Prelude

import Data.Map (Map)
import Data.Map qualified as M

import Data.Kind qualified as GHC
import Data.List (foldl')
import Data.List.NonEmpty qualified as NE

import Language.Purus.IR (Ty (..))
import Language.Purus.Pipeline.GenerateDatatypes (toPIRType)
import Language.Purus.Pipeline.GenerateDatatypes.Utils (
  freshName, getConstructorName, note, getDestructorTy,
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
import Language.Purus.Prim.Utils ( properToIdent )

import Control.Monad.Except (
  liftEither,
 )
import Language.PureScript.Constants.Prim qualified as C
import Language.PureScript.Constants.Purus qualified as C

{-
    PIR Constants, Term Builders, Helpers, Etc
-}

tyBuiltinBool :: PIRType
tyBuiltinBool = PLC.TyBuiltin () (PLC.SomeTypeIn PLC.DefaultUniBool)

unit :: PIRType
unit = PLC.TyBuiltin () (PLC.SomeTypeIn PLC.DefaultUniUnit)

unitTerm :: PIRTerm
unitTerm = mkConstant () ()

(#) :: PIRTerm -> PIRTerm -> PIRTerm
e1 # e2 = PIR.Apply () e1 e2
-- I think this is the right fixity? TODO: Check plutarch
infixl 9 #

-- :: con bool -> Boolean
pirBoolToBoolean :: PIRTerm -> PlutusContext PIRTerm
pirBoolToBoolean conBoolTerm = do
  tyConBool  <- toPIRType tyBool
  trueNm   <- note "True not defined" =<< getConstructorName (properToIdent <$> C.C_True)
  falseNm  <- note "False not defined" =<< getConstructorName (properToIdent <$> C.C_False)
  let true = PIR.Var () trueNm
      false = PIR.Var () falseNm
  pirIfThen tyConBool conBoolTerm true false

-- Boolean -> con bool
pirBooleanToBool :: PIRTerm -> PlutusContext PIRTerm
pirBooleanToBool psBool = do
  boolDctor <- PIR.Var () <$> getDestructorTy C.Boolean
  pure $ PIR.TyInst () (boolDctor # psBool #  mkConstant () True # mkConstant () False) tyBuiltinBool

{- This is the *lazy*
-}
pirIfThen :: PIRType -> PIRTerm -> PIRTerm -> PIRTerm -> PlutusContext PIRTerm
pirIfThen resTy cond troo fawlse = do
  troo' <- pirDelay troo
  fawlse' <- pirDelay fawlse
  pure . pirForce $ pirTyInst (PIR.TyFun () unit resTy) (PIR.Builtin () PLC.IfThenElse) # cond # troo' # fawlse'

-- utility for constructing LamAbs w/ a fresh variable name. We do this a lot in the case analysis stuff
freshLam ::
  Ty -> -- type of the fresh var being created
  (PIRType -> PIRTerm -> PlutusContext PIRTerm) -> -- fn from that fresh var to a term
  PlutusContext PIRTerm
freshLam t f = do
  name <- freshName
  t' <- toPIRType t
  PIR.LamAbs () name t' <$> f t' (PIR.Var () name)

-- Variant of the above function but accepts a PIR Type (useful in a few contexts)
freshLam' ::
  PIRType -> -- type of the fresh var being created
  (PIRType -> PIRTerm -> PlutusContext PIRTerm) -> -- fn from that fresh var to a term
  PlutusContext PIRTerm
freshLam' t f = do
  name <- freshName
  PIR.LamAbs () name t <$> f t (PIR.Var () name)



pirTyInst :: PIRType -> PIRTerm -> PIRTerm
pirTyInst ty term = PIR.TyInst () term ty

tyInstMany :: PIRTerm -> [PIRType] -> PIRTerm
tyInstMany = foldl' (flip pirTyInst)



pirDelay :: PIRTerm -> PlutusContext PIRTerm
pirDelay term = do
  nm <- freshName
  pure $ PIR.LamAbs () nm unit term

pirForce :: PIRTerm -> PIRTerm
pirForce term = PIR.Apply () term unitTerm

-- Assumes the kind is *
pirTyAbs :: (PIRType -> PlutusContext PIRTerm) -> PlutusContext PIRTerm
pirTyAbs f = do
  tName <- PIR.TyName <$> freshName
  let kindType = PIR.Type ()
  body <- f (PIR.TyVar () tName)
  pure $ PIR.TyAbs () tName kindType body 


{- REVIEW: Is this right? Is that what we *want*?
-}
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


{- Builtin function substitutions. Each builtin function with a Purus type that contains
   a `Boolean` is a lie. We use algebraic datatype Booleans, not the Plutus builtin. (This
   makes case expression desugaring much easier.)

   Consequently, we need to construct variants of those builtins which convert between `con bool`
   and the ADT Boolean. The simplicity is worth the minor performance penalty.

   TODO: Introduce machinery to let- bind these in the outer scope of the expression being compiled.
         There shouldn't be any need to duplicate them.
-}
builtinSubstitutions :: Map PLC.DefaultFun (PlutusContext PIRTerm)
builtinSubstitutions
  = M.fromList [ (PLC.EqualsInteger,pirEqInt)
               , (PLC.EqualsString,pirEqString)
               , (PLC.LessThanInteger,pirLessThanInteger)
               , (PLC.LessThanEqualsInteger,pirLessThanEqualsInteger)
               , (PLC.EqualsByteString,pirEqualsByteString)
               , (PLC.LessThanByteString,pirLessThanByteString)
               , (PLC.LessThanEqualsByteString,pirLessThanEqualsByteString)
               , (PLC.VerifyEd25519Signature,pirVerifyEd25519Signature)
               , (PLC.VerifyEcdsaSecp256k1Signature,pirVerifyEcdsaSecp256k1Signature)
               , (PLC.EqualsData,pirEqualsData)
               , (PLC.IfThenElse,pirIfThenElse)
               , (PLC.NullList,pirNullList)
               ]

tyInt, tyBool, tyByteString, tyData, tyString :: Ty
tyInt = TyCon C.Int
tyBool = TyCon C.Boolean
tyByteString =  TyCon C.BuiltinByteString
tyData = TyCon C.BuiltinData
tyString = TyCon C.String

wrapBoolToBoolean2 :: Ty -> PLC.DefaultFun -> PlutusContext PIRTerm
wrapBoolToBoolean2 t f = freshLam t $ \_ x1 -> freshLam t $ \_ x2 -> do
  let fun = PIR.Builtin () f
  pirBoolToBoolean $ fun # x1 # x2

wrapBoolToBoolean3 :: Ty -> PLC.DefaultFun -> PlutusContext PIRTerm
wrapBoolToBoolean3 t f = freshLam t $ \_ x1 -> freshLam t $ \_ x2 -> freshLam t $ \_ x3 -> do
  let fun = PIR.Builtin () f
  pirBoolToBoolean $ fun # x1 # x2 # x3


-- Int -> Int -> Bool
pirEqInt :: PlutusContext PIRTerm
pirEqInt  = wrapBoolToBoolean2 tyInt PLC.EqualsInteger

-- String -> String -> Bool
pirEqString :: PlutusContext PIRTerm
pirEqString  = wrapBoolToBoolean2 tyString PLC.EqualsString

-- Int -> Int -> Bool
pirLessThanInteger ::  PlutusContext PIRTerm
pirLessThanInteger = wrapBoolToBoolean2 tyInt PLC.LessThanInteger

-- Int -> Int -> Bool
pirLessThanEqualsInteger ::  PlutusContext PIRTerm
pirLessThanEqualsInteger = wrapBoolToBoolean2 tyInt PLC.LessThanEqualsInteger

-- Bytestring -> ByteString -> Bool
pirEqualsByteString :: PlutusContext PIRTerm
pirEqualsByteString =  wrapBoolToBoolean2 tyByteString PLC.EqualsByteString

-- Bytestring -> ByteString -> Bool
pirLessThanByteString :: PlutusContext PIRTerm
pirLessThanByteString = wrapBoolToBoolean2 tyByteString PLC.LessThanByteString

-- Bytestring -> ByteString -> Bool
pirLessThanEqualsByteString :: PlutusContext PIRTerm
pirLessThanEqualsByteString = wrapBoolToBoolean2 tyByteString PLC.LessThanEqualsByteString

-- Bytestring -> ByteString -> ByteString -> Bool
pirVerifyEd25519Signature :: PlutusContext PIRTerm
pirVerifyEd25519Signature = wrapBoolToBoolean3 tyByteString PLC.VerifyEd25519Signature

-- Bytestring -> ByteString -> ByteString -> Bool
pirVerifyEcdsaSecp256k1Signature :: PlutusContext PIRTerm
pirVerifyEcdsaSecp256k1Signature = wrapBoolToBoolean3 tyByteString PLC.VerifyEcdsaSecp256k1Signature

-- BuiltinData -> BuiltinData -> Bool
pirEqualsData :: PlutusContext PIRTerm
pirEqualsData = wrapBoolToBoolean2 tyData PLC.EqualsData

-- N.B. This differs from the one above in that
--      this assumed that the condition
--      will be represented as an ADT Prim.Boolean
--      so we need to turn it into a con bool
--      (also we're not adding force/delay here)
-- forall x. Bool -> x -> x ->
pirIfThenElse :: PlutusContext PIRTerm
pirIfThenElse
  = pirTyAbs $ \tv ->
    freshLam tyBool $ \_ cond ->
    freshLam' tv $ \_ trueVar ->
    freshLam' tv $ \_ falseVar -> do
      pirIfiedCond <- pirBooleanToBool cond
      let pirIFTE = PIR.Builtin () PLC.IfThenElse
      pure $ pirIFTE # pirIfiedCond # trueVar # falseVar

-- forall x. BuiltinList x -> Bool
pirNullList :: PlutusContext PIRTerm
pirNullList
  = pirTyAbs $ \tv -> do
      let listAppliedTy = PIR.TyApp ()
                              (TyBuiltin () (PLC.SomeTypeIn PLC.DefaultUniProtoList))
                              tv
      freshLam' listAppliedTy $ \_ arg -> do
        let nullListFun = PIR.Builtin () PLC.NullList
        pirBoolToBoolean (pirTyInst tv nullListFun # arg)

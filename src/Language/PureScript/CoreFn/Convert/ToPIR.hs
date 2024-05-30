{-# OPTIONS_GHC -Wno-orphans #-} -- has to be here (more or less)
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DeriveTraversable, DeriveAnyClass  #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Language.PureScript.CoreFn.Convert.ToPIR  where

import Prelude
import Language.PureScript.Names (Qualified (..), ProperName(..), runIdent, pattern ByThisModuleName, disqualify, ProperNameType (..))
import Language.PureScript.CoreFn.FromJSON ()
import Data.Text qualified as T
import Language.PureScript.PSString (prettyPrintString)
import Data.Text (Text)
import Bound
import Control.Monad
import Data.Bifunctor (Bifunctor(first))
import Data.List.NonEmpty qualified as NE
import Language.PureScript.Constants.Prim qualified as C
import Data.Map (Map)
 -- mainly for the module (we might need it for constructors? idk)
import Language.PureScript.CoreFn.Convert.IR
import PlutusIR
import PlutusIR qualified as PIR
import PlutusCore.Default
import Control.Monad.State
import PlutusCore (Unique (..), getDefTypeCheckConfig)
import Language.PureScript.Constants.PLC
import Data.Map qualified as M
import Language.PureScript.CoreFn.Desugar.Utils (showIdent')
import PlutusIR.MkPir hiding (error)
import qualified Language.PureScript.CoreFn.Convert.IR as IR
import Language.PureScript.CoreFn.Convert.DesugarObjects
import Bound.Scope (instantiateEither)
import Language.PureScript.Constants.Purus qualified as C
import PlutusIR.Core.Instance.Pretty.Readable (prettyPirReadable)
import PlutusIR.Compiler ( compileToReadable, Compiling, toDefaultCompilationCtx, CompilationCtx, compileProgram )
import PlutusCore.Version
import PlutusIR.Compiler.Provenance
import PlutusCore.Quote
import Control.Monad.Trans.Except (ExceptT, runExceptT)
import PlutusIR.Error
import Control.Monad.Reader
import PlutusCore qualified as PLC
import Control.Exception
import Data.List (sortOn)
import Control.Lens ((&),(.~),ix)
import PlutusCore.Evaluation.Machine.Ck
import PlutusCore.Evaluation.Machine.ExBudgetingDefaults qualified as PLC
import Test.Tasty
import Test.Tasty.HUnit
import Language.PureScript.CoreFn.Convert.DesugarCore
import Data.Set qualified as S
import Language.PureScript.CoreFn.Convert.Datatypes


type PLCProgram uni fun a = PLC.Program PLC.TyName PLC.Name uni fun (Provenance a)

type PIRTerm tyName = PIR.Term tyName PLC.Name DefaultUni DefaultFun ()


{- In the first pass, we convert IR to PIR terms
   but do not convert PS type names into PIR type names
-}
firstPass :: forall a
           . (a -> Var (BVar Ty) (FVar Ty))
          -> Exp WithoutObjects Ty a
          -> PIR.Term (Qualified (ProperName 'TypeName)) PLC.Name DefaultUni DefaultFun ()
firstPass f = \case
  V x -> case f x of
    F (FVar _ ident) ->
      let nm = runIdent $ disqualify ident
      in case M.lookup (T.unpack nm) defaultFunMap of
        Just aBuiltinFun ->  Builtin () aBuiltinFun
        Nothing -> error
         $ T.unpack nm <> " isn't a builtin, and it shouldn't be possible to have a free variable that's anything but a builtin"
    B (BVar bvix bvty (runIdent -> nm)) -> PIR.Var () (Name nm $ Unique bvix)
  LitE _ lit -> undefined
  CtorE{} -> undefined -- TODO, requires context
  LamE ty (BVar bvIx bvTy bvNm) body -> undefined

{-
fuckThisMonadStack ::
      forall e m c  b.
      (e ~ Error DefaultUni DefaultFun (Provenance ())
      , c ~ CompilationCtx DefaultUni DefaultFun ()
      , m ~ ExceptT e (ExceptT e (QuoteT (Reader c)))
      )
      => (Compiling m e DefaultUni DefaultFun ()) => m b -> Either String b
fuckThisMonadStack x  =
      let
        res :: Either e b
        res = do
            plcConfig <- getDefTypeCheckConfig (Original ())
            let ctx = toDefaultCompilationCtx plcConfig
            join $ flip runReader ctx $ runQuoteT $ runExceptT $ runExceptT x
      in first show res

runPLCProgram :: PLCProgram DefaultUni DefaultFun () -> (EvaluationResult (PLC.Term TyName Name DefaultUni DefaultFun ()),[Text])
runPLCProgram (PLC.Program _ _ c) = unsafeEvaluateCk PLC.defaultBuiltinsRuntime $ void c

runPLCProgramTest :: String
                  -> (EvaluationResult (PLC.Term TyName Name DefaultUni DefaultFun ()),[Text])
                  -> FilePath
                  -> Text
                  -> TestTree
runPLCProgramTest testName expected path decl  = testCase testName $ do
  prog <- declToUPLC path decl
  let out = runPLCProgram prog
  assertEqual "program output matches expected "  expected out

declToUPLC :: FilePath
           -> Text
           -> IO (PLCProgram  DefaultUni DefaultFun ())
declToUPLC path decl = prepPIR path decl >>= \case
  (mainExpr,dict) -> do
    tcMap <- rethrowIO $ mkTyConMap dict
    ctorMap <- rethrowIO $ mkConstructorMap dict
    let initialState = ConvertState 0 M.empty M.empty tcMap ctorMap
        result = evalState (toPIR F mainExpr) initialState
    putStrLn $ replicate 20 '-' <> "PIR" <> replicate 20 '-'
    print $ prettyPirReadable result
    putStrLn $ replicate 43 '-' <> "\n"
    let input = Program (Original ()) latestVersion (Original <$> result)
    readable <- aghhhh . fuckThisMonadStack $ compileToReadable input
    aghhhh . fuckThisMonadStack $ compileProgram (void readable)

 where
   aghhhh = either (throwIO . userError) pure
   rethrowIO = \case
     Left te@(TypeConvertError _ _ _ ) -> error (prettyErrorT te)
     Right x -> pure x

-- can't figure out if this is exported in w/e version of plutus-core we're on here
splitFunTyParts :: Type tyname uni a -> NE.NonEmpty (Type tyname uni a)
splitFunTyParts = \case
    TyFun _ t1 t2 -> t1 NE.<| splitFunTyParts t2
    t             -> pure t
-}

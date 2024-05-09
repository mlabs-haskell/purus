{-# OPTIONS_GHC -Wno-orphans #-} -- has to be here (more or less)
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DeriveTraversable, DeriveAnyClass  #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Language.PureScript.CoreFn.Convert.ToPIR (runPLCProgramTest, declToUPLC) where

import Prelude
import Language.PureScript.Names (Qualified (..), ProperName(..), runIdent, pattern ByThisModuleName, disqualify)
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

type PLCProgram uni fun a = PLC.Program PLC.TyName PLC.Name uni fun (Provenance a)

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
      in  first  show res

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

data PIRConvertError = PIRConvertError String -- TODO: Refine later

type PIRTerm = Term TyName Name DefaultUni DefaultFun ()

type PIRType = Type TyName DefaultUni ()

type PIRBindings = Binding TyName Name DefaultUni DefaultFun ()

{- SOP (just so i have an easy reference example)

data Maybe a = Just a | Nothing

type Maybe a = [[a],[]]

-}

mkTyName :: Text -> State ConvertState TyName
mkTyName t  = do
  tvMap <- gets tyVarMap
  case M.lookup t tvMap of
    Nothing -> do
      i <- getCounter
      let nm = TyName $ Name t (Unique i)
      modify $ \(ConvertState ix tvs vs tcd ctd) -> ConvertState ix (M.insert t i tvs) vs tcd ctd
      pure nm
    Just i -> pure $ TyName $ Name t (Unique i)

mkTermName :: Text -> State ConvertState Name
mkTermName t  = do
  tvMap <- gets varMap
  case M.lookup t tvMap of
    Nothing -> do
      i <- getCounter
      let nm = Name t (Unique i)
      modify $ \(ConvertState ix tvs vs tcd ctd) -> ConvertState ix tvs (M.insert t i vs) tcd ctd
      pure nm
    Just i ->  pure $ Name t (Unique i)

getCounter :: State ConvertState Int
getCounter = do
  i <- gets counter
  modify' $ \(ConvertState _ tvs vs  tcd ctd) -> ConvertState (i + 1) tvs vs tcd ctd
  pure i

genFreshName :: State ConvertState Name
genFreshName = do
  i <- getCounter
  pure $ Name ("~" <> T.pack (show i)) (Unique i)

genFreshTyName :: State ConvertState TyName
genFreshTyName = TyName <$> genFreshName

-- N.B. We use the counter for type names (b/c we don't have Bound ADT)
data ConvertState = ConvertState {
       counter   :: Int,
       tyVarMap  :: Map Text Int,
       varMap    :: Map Text Int,
       tyConDict :: TyConDict,
       ctorDict  :: CtorDict
     }

-- To avoid capturing quantified type variables in different scopes
locally :: State ConvertState a -> State ConvertState a
locally m = do
  s <- get
  let (a,ConvertState{..}) = runState m s
  modify' $ \cs -> cs {counter = counter}
  pure a

-- REVIEW/TODO/FIXME: We need to ensure that type variables are also well scoped and unique.
--                    Or at least I think we do? It's hard to tell whether that can still be
--                    a problem after monomorphization.
toPIRType :: Ty -> State ConvertState PIRType
toPIRType = \case
  IR.TyVar txt -> do
    tyName <- mkTyName txt
    pure $ PIR.TyVar () tyName
  TyCon qtn@(Qualified qb tn) -> case qb of
    ByThisModuleName "Builtin" -> pure $ handleBuiltinTy qtn
    ByThisModuleName "Prim" -> pure $ handlePrimTy qtn
    -- ByThisModuleName "$GEN"    -> handleGenTuple tn don't think we need this b/c magic tuples?
    _ -> gets tyConDict >>= \tcDict -> case lookupSOP tn tcDict of
      Nothing -> error $ "TyCon not found in context " <> T.unpack (runProperName tn)
      Just sop -> do
        let strippedIndices :: [[Ty]]
            strippedIndices = snd <$> sop
        PIR.TySOP () <$> (traverse . traverse) toPIRType strippedIndices
  -- TODO: Special handling for function types (-> is a TyCon in PS but a Ctor of the Type ADT in plc )
  IR.TyApp t1 t2 -> goTypeApp t1 t2
  Forall _ v mbk ty _ -> do
    let k  = mkKind mbk
    tyName <- mkTyName v
    ty' <- toPIRType ty
    pure $ TyForall () tyName k ty'
  other -> error $ "Upon reflection, other types like " <> ppTy other <> " shouldn't be allowed in the Ty ast"
 where
   goTypeApp (IR.TyApp f a) b
     | f == TyCon C.Function = do
         a' <- toPIRType a
         b' <- toPIRType b
         pure $ PIR.TyFun () a' b'

   handleBuiltinTy = \case
     C.BuiltinData -> TyBuiltin () (SomeTypeIn DefaultUniData)
     C.BuiltinPair -> TyBuiltin () (SomeTypeIn DefaultUniProtoPair)
     C.BuiltinList -> TyBuiltin () (SomeTypeIn DefaultUniProtoList)
     C.BuiltinByteString -> TyBuiltin () (SomeTypeIn DefaultUniByteString)
     other -> error $ "error: unsupported builtin type: " <> show other

   handlePrimTy = \case
     C.Function -> error "function types should be caught in apps"
     C.Array    -> TyBuiltin () (SomeTypeIn DefaultUniProtoList) -- is this wrong? do we want a SOP list? too tired to know
     C.String   -> TyBuiltin () (SomeTypeIn DefaultUniString)
     C.Char     -> TyBuiltin () (SomeTypeIn DefaultUniInteger)
     C.Int      -> TyBuiltin () (SomeTypeIn DefaultUniInteger)
     C.Boolean  -> TyBuiltin () (SomeTypeIn DefaultUniBool)
     other      -> error $ "unsupported prim tycon: " <> show other


 -- TODO: Actually I should catch non- */*->Kind kinds way earlier than this and enforce that in the structure of Ty
mkKind :: Maybe IR.Ty -> PIR.Kind ()
mkKind Nothing = error "not a type"
mkKind (Just t) = foldr1 (PIR.KindArrow ()) (collect t)
  where
    collect :: IR.Ty -> [PIR.Kind ()]
    collect = \case
      IR.TyCon C.Type -> [PIR.Type ()]
      IR.TyCon C.Type :~> b -> PIR.Type () : collect b
      other -> error $ "Not a thing of kind type " <> ppTy other

-- TODO: Real monad stack w/ errors and shit
toPIR :: forall a. (a -> Var (BVar Ty) (FVar Ty)) -> Exp WithoutObjects Ty a -> State  ConvertState PIRTerm
toPIR f = \case
  V (f -> F (FVar _ ident)) -> do
    let nm = showIdent' ident
    case M.lookup (showIdent' ident) defaultFunMap of
      Just aBuiltin -> pure $ Builtin () aBuiltin
      Nothing -> do error $ showIdent' ident <> " isn't a builtin, and it shouldn't be possible to have a free variable that's anything but a builtin"
  V (f -> B (BVar n t i)) -> PIR.Var () <$> mkTermName (runIdent i)
  LitE _ lit -> litToPIR f lit
  CtorE ty _ cn  _ -> gets ctorDict >>= \dict -> do
        case M.lookup cn dict of
          Nothing -> error $ "Unknown Constructor " <> T.unpack (runProperName cn)
          Just (_,ctorIx,_) -> locally $ do
            -- TODO: We need to "walk under" quantifiers here a la `instantiatePolyType` from the CoreFn conversion
            ctorFunTy  <- NE.toList . splitFunTyParts <$> toPIRType ty
            mkConstructorFun [] ctorIx ctorFunTy
  LamE ty (BVar i vTy ident) body -> do
    ty' <- toPIRType (argTy ty)
    nm <- mkTermName (runIdent ident)
    let iBody = instantiateEither (either (IR.V . B) (IR.V . F)) body
    body' <- toPIR (>>= f) iBody
    pure $ PIR.LamAbs () nm ty' body'
  AppE e1 e2 -> do
    e1' <- toPIR f e1
    e2' <- toPIR f e2
    pure $ PIR.Apply () e1' e2'
  {- Too fucking complicated to do multiple scrutinee cases, will do later.

     I'm so beat so idk how great of a plan this is, but I'm gonna try to:

       - a) Transform the alts and the scrutinne into an expression :: scrutinee -> e' in Case _ ty e' [altBodies] using the binders
       - b) Transform each alt body into a lambda. Somehow.
       - c) Apply a to b. Hopefully!
  -}
  CaseE ty [scrutinee] alts -> do
    scrutinee' <- toPIR f scrutinee
    assembleScrutinee scrutinee' ty alts
  -- TODO: I'm just going to mark all binding groups as recursive for now and do
  --       the sophisticated thing later. so tired.
  LetE cxtmap binds exp -> do
    let flatBinds = concatMap flattenBind binds
    named <- traverse (\(i,e) -> (,e) <$> mkTermName (runIdent i)) flatBinds
    -- REVIEW: should they all be strict?
    bindings <- traverse mkBinding named
    case NE.nonEmpty bindings of
      Just bindings' -> do
        exp' <- toPIR (>>= f)  $ instantiateEither (either (IR.V . B) (IR.V . F)) exp
        pure $ PIR.Let () PIR.Rec  bindings' exp'
      Nothing -> error "non empty bindings"
 where
   mkBinding :: (Name, Scope (BVar Ty) (Exp WithoutObjects Ty) a) -> State ConvertState (Binding TyName Name DefaultUni DefaultFun ())
   mkBinding (nm,scopedExp) = do
     let ty = eTy' f scopedExp
     ty' <- toPIRType ty
     exp' <- toPIR (>>= f) $ instantiateEither (either (IR.V . B) (IR.V . F)) scopedExp
     pure $ TermBind () Strict (VarDecl () nm ty') exp'

   assembleScrutinee :: PIRTerm -> Ty ->  [Alt WithoutObjects Ty (Exp WithoutObjects Ty) a] -> State ConvertState (Term TyName Name DefaultUni DefaultFun ())
   assembleScrutinee scrut tx  alts = do
     let _binders = IR.getPat <$> alts
     alted <-  unzip <$> traverse (locally . goAlt tx) alts
     let sopSchema = head . fst $ alted
         ctorNumberedBranches = snd alted
     failBranches <- traverse mkFailBranch sopSchema
     tx' <- toPIRType tx
     let resolvedBranches = foldr (\(i,x) acc -> acc & ix i .~ x) failBranches ctorNumberedBranches
     pure $ Case () tx' scrut resolvedBranches
      where
       boolT = TyBuiltin () (SomeTypeIn DefaultUniBool)
       mkFailBranch :: [Ty] -> State ConvertState PIRTerm
       mkFailBranch [] = pure $ mkConstant () False
       mkFailBranch [t] = do
            lamName <- genFreshName
            t' <- toPIRType t
            pure $ PIR.LamAbs () lamName (PIR.TyFun () t' boolT) (mkConstant () False)
       mkFailBranch (t:ts) = do
            lamName <- genFreshName
            t' <- toPIRType t
            rest <- mkFailBranch ts
            pure $ PIR.LamAbs () lamName (PIR.TyFun () t' boolT) rest

       goAlt :: Ty -> Alt WithoutObjects Ty (Exp WithoutObjects Ty) a -> State ConvertState ([[Ty]],(Int,PIRTerm))
       goAlt t  (UnguardedAlt _ [pat] body) = do
         body' <- toPIR (>>= f) $ instantiateEither (either (IR.V . B) (IR.V . F)) body
         patToBoolFunc body' t pat
         where

          patToBoolFunc :: PIRTerm -> Ty -> Pat WithoutObjects (Exp WithoutObjects Ty) a -> State ConvertState ([[Ty]],(Int,PIRTerm))
          patToBoolFunc res tx = \case
            ConP tn cn ips -> do
              ctordict <- gets ctorDict
              tcdict   <- gets tyConDict
              tx'      <- toPIRType tx
              let cn' = disqualify cn
                  tn' = disqualify tn
              case (M.lookup cn' ctordict, M.lookup tn' tcdict) of
                (Just (_,ctorix,tys), Just (_,_,ctors)) -> do
                  let ctorTypes = map snd . sortOn fst $ ctors
                  ibfs <- goCtorArgs (zip tys ips)
                  pure (ctorTypes,(ctorix,ibfs))
           where
             goCtorArgs :: [(Ty,Pat WithoutObjects (Exp WithoutObjects Ty) a)] -> State ConvertState PIRTerm
             goCtorArgs [] = pure res
             goCtorArgs [(t,VarP nm)] = do
               nm' <- mkTermName (runIdent nm)
               t' <- toPIRType t
               pure $ LamAbs () nm' t' res
             goCtorArgs ((t,VarP nm):rest) = do
               nm' <- mkTermName (runIdent nm)
               t' <- toPIRType t
               rest' <- goCtorArgs rest
               pure $ LamAbs () nm' t' rest'

       -- NOTE: We don't have force/delay in PIR so I think we have to use type abstraction/instantiation
       -- force ((\cond -> IfThenElse cond (delay caseT) (delay caseF)) cond)
       -- TyInst _
       -- This is probably wrong and we need quantifiers (see https://github.com/IntersectMBO/plutus/blob/381172295c0b0a8f17450b8377ee5905f03d294b/plutus-core/plutus-ir/src/PlutusIR/Transform/NonStrict.hs#L82-L95)
       -- TyInst ann (Var ann name) (TyForall ann a (Type ann) (TyVar ann a))
       -- TermBind x Strict (VarDecl x' name (TyForall ann a (Type ann) ty)) (TyAbs ann a (Type ann) rhs)
       pIfTe :: PIRTerm -> PIRTerm -> PIRTerm ->  State ConvertState PIRTerm
       pIfTe cond troo fawlse = do
         scrutineeNm <- genFreshName
         let scrutineeTNm = TyName scrutineeNm
         let boolT = TyBuiltin () (SomeTypeIn DefaultUniBool)
             builtinIfTe = Builtin () IfThenElse
             ifte c t f = PIR.Apply () (PIR.Apply () (PIR.Apply () builtinIfTe c) t) f
             sVar = (PIR.Var () scrutineeNm)
             tBranch = (PIR.TyAbs () scrutineeTNm (PIR.Type ()) troo)
             fBranch = (PIR.TyAbs () scrutineeTNm (PIR.Type ()) fawlse)
             body = PIR.LamAbs () scrutineeNm boolT
                      $ ifte sVar tBranch fBranch
             forced = TyInst () body (TyForall () scrutineeTNm (PIR.Type ()) (PIR.TyVar () scrutineeTNm))
         pure $ PIR.Apply () forced cond


argTy :: Ty -> Ty
argTy (a :~> b) = a
argTy other = other

mkConstructorFun :: [Name] -> Int -> [PIRType] -> State ConvertState PIRTerm
mkConstructorFun acc ctorIx [ty] = do
  let argsInOrder = reverse acc
      ctorIx'     = fromIntegral ctorIx
  pure $ PIR.Constr () ty ctorIx' (PIR.Var () <$> argsInOrder)
mkConstructorFun acc cix (t:ts) = do
  newName <- genFreshName
  -- \/ NOTE: Not sure if abstractions here have the type of the bound var or the type of the abstraction as a whole.
  --         If not working look here first
  let lamTy = foldr1 (TyFun ()) (t:ts)
  rest <- mkConstructorFun (newName:acc) cix ts
  pure $ LamAbs () newName lamTy rest
mkConstructorFun _ _ _ = error "mkConstructorFun failed - should be impossible"

litToPIR :: forall a. (a -> Var (BVar Ty) (FVar Ty)) -> Lit WithoutObjects (Exp WithoutObjects Ty a) -> State ConvertState  PIRTerm
litToPIR f = \case
    IntL i -> pure $ mkConstant () i
    NumL _ -> error "TODO: Doubles"
    StringL str -> -- REVIEW/TODO/FIXME: This is *TOTALLY* wrong
      pure . mkConstant () $ prettyPrintString str
    CharL c -> -- REVIEW/TODO/FIXME: Is this what we want?
      pure
      . mkConstant ()
      . toInteger
      . fromEnum
      $ c
    BoolL b -> pure $ mkConstant () b
    ArrayL arr -> error $ "Too complicated for 5am, do tomorrow"
                          <> " Make sure to check for nested array lits (also check that everywhere else)"

-- can't figure out if this is exported in w/e version of plutus-core we're on here
splitFunTyParts :: Type tyname uni a -> NE.NonEmpty (Type tyname uni a)
splitFunTyParts = \case
    TyFun _ t1 t2 -> t1 NE.<| splitFunTyParts t2
    t             -> pure t

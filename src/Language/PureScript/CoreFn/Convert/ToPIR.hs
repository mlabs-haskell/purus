{-# OPTIONS_GHC -Wno-orphans #-} -- has to be here (more or less)
{-# LANGUAGE ScopedTypeVariables #-}





{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Language.PureScript.CoreFn.Convert.ToPIR  where

import Prelude
import Language.PureScript.Names (Qualified (..), ProperName(..), runIdent, disqualify, ProperNameType (..), Ident)
import Language.PureScript.CoreFn.FromJSON ()
import Data.Map (Map)
import Data.Map qualified as M
import Data.Text qualified as T
import Language.PureScript.PSString (prettyPrintString)
import Bound ( Var(..) )
import Control.Monad ( (>=>), foldM )
import Language.PureScript.Constants.Prim qualified as C
 -- mainly for the module (we might need it for constructors? idk)
import Language.PureScript.CoreFn.Convert.IR
    ( Exp(CaseE, V, LitE, LamE, AppE, LetE),
      BindE(..),
      Lit(NumL, ArrayL, ConstArrayL, BoolL, CharL, IntL, StringL),
      FVar(..),
      BVar(..),
      Ty(..),
      expTy, Alt, getPat, Pat (..) )
import PlutusIR
    ( Type,
      Name(Name),
      VarDecl(VarDecl),
      Term(Builtin, Constant),
      Binding(TermBind),
      Strictness(Strict) )
import PlutusIR qualified as PIR
import PlutusCore.Default
    ( DefaultFun,
      pattern DefaultUniList,
      DefaultUni,
      Esc,
      ValueOf(ValueOf),
      Some(Some) )
import PlutusCore (Unique (..))
import Language.PureScript.Constants.PLC ( defaultFunMap )
import PlutusIR.MkPir ( mkConstant )
import Language.PureScript.CoreFn.Convert.IR qualified as IR
import Language.PureScript.CoreFn.Convert.DesugarObjects
    ( prettyStr )
import PlutusIR.Compiler.Provenance ( Provenance )
import PlutusCore qualified as PLC
import Language.PureScript.CoreFn.Convert.DesugarCore
    ( WithoutObjects )

import Data.Void ( Void )
import Data.Kind qualified as GHC
import Type.Reflection ( Typeable, TypeRep, typeRep )
import Bound.Scope (instantiateEither)
import Data.List.NonEmpty qualified as NE
import Language.PureScript.CoreFn.Desugar.Utils (showIdent')
import Language.PureScript.CoreFn.Convert.Datatypes
import Control.Monad.Except (MonadError(..))
import Language.PureScript.CoreFn.Module (Datatypes, getConstructorIndexAndDecl, getAllConstructorDecls, lookupDataDecl, cdCtorFields, dDataArgs)
import GHC.Word (Word64)
import Language.PureScript.CoreFn.TypeLike (TypeLike (..), getInstantiations, safeFunArgTypes)
import Data.Foldable (foldl')
import Data.Maybe (fromJust)
import Control.Lens.Operators ((^.), (&), (.~))
import Control.Lens (_2, mapped, view, ix)

showType :: forall a. Typeable a => String
showType = show (typeRep :: TypeRep a)


(#) :: PIRTerm -> PIRTerm -> PIRTerm
e1 # e2 = PIR.Apply () e1 e2

-- I think this is the right fixity? TODO: Check plutarch
infixl 9 #


sopUnit :: PIRType
sopUnit = PIR.TySOP () []

pirDelay :: PIRTerm -> DatatypeM PIRTerm
pirDelay term = do
  tyName <- PIR.TyName <$> freshName
  pure $ PIR.TyAbs () tyName (PLC.Type ()) term

pirForce :: PIRTerm -> PIRTerm
pirForce term = PIR.TyInst () term sopUnit

pirEqInt :: PIRTerm -> PIRTerm -> PIRTerm
pirEqInt i1 i2 =
  let pirEq = PIR.Builtin () PLC.EqualsInteger
  in pirEq # i1 # i2

pirEqString :: PIRTerm -> PIRTerm -> PIRTerm
pirEqString s1 s2 =
  let pirEq = PIR.Builtin () PLC.EqualsString
  in pirEq # s1 # s2

-- delayed (which is literally always what we want). Not sure if need to force here?
pirError :: PIRType -> DatatypeM PIRTerm
pirError t = pirForce <$> pirDelay (PIR.Error () t)

-- for builtin booleans jfc why don't they have thiiiisss
pirAnd :: PIRTerm -> PIRTerm -> DatatypeM PIRTerm
pirAnd t1 t2 = do
  tBranch <- pirIfThen t2 (mkConstant () True) (mkConstant () False)
  pirIfThen t1 tBranch (mkConstant () False)


argTy :: TypeLike t => t -> t
argTy = head . splitFunTyParts

-- lazy
pirIfThen :: PIRTerm -> PIRTerm -> PIRTerm -> DatatypeM PIRTerm
pirIfThen cond troo fawlse = do
  troo' <- pirDelay troo
  fawlse' <- pirDelay fawlse
  let strictIfThen = PIR.Builtin () PLC.IfThenElse
  pure $ pirForce $ foldl' (PIR.Apply ()) strictIfThen [cond,troo',fawlse']

-- utility for constructing LamAbs w/ a fresh variable name. We do this a lot in the case analysis stuff
freshLam :: Ty -- type of the fresh var being created
         -> (PIRType -> PIRTerm -> DatatypeM PIRTerm) -- fn from that fresh var to a term
         -> DatatypeM PIRTerm
freshLam t f = do
  name <- freshName
  t' <- toPIRType t
  PIR.LamAbs () name t' <$> f t' (PIR.Var () name)


type PLCProgram uni fun a = PLC.Program PLC.TyName PLC.Name uni fun (Provenance a)

type PIRTermBind  = Binding PLC.TyName  Name DefaultUni DefaultFun ()

firstPass :: forall a
           . Datatypes IR.Kind Ty
          -> (a -> Var (BVar Ty) (FVar Ty))
          -> Exp WithoutObjects Ty a
          -> DatatypeM PIRTerm
firstPass datatypes f = \case
  V x -> case f x of
    F (FVar _ ident) ->
      let nm = runIdent $ disqualify ident
      in case M.lookup (T.unpack nm) defaultFunMap of
        Just aBuiltinFun ->  pure $ Builtin () aBuiltinFun
        Nothing -> getConstructorName ident >>= \case
          Just aCtorNm -> pure $ PIR.Var () aCtorNm
          Nothing -> throwError
                     $ T.unpack nm
                     <> " isn't a builtin, and it shouldn't be possible to have a free variable that's anything but a builtin"
    B (BVar bvix _ (runIdent -> nm)) -> pure $ PIR.Var () (Name nm $ Unique bvix)
  LitE litTy lit -> firstPassLit litTy lit
  LamE _ (BVar bvIx bvTy bvNm) body -> do
    ty' <- toPIRType (argTy bvTy)
    let nm = Name (runIdent bvNm) $ Unique bvIx
        body' = instantiateEither (either (IR.V . B) (IR.V . F)) body
    body'' <- firstPass datatypes (>>= f) body'
    pure $ PIR.LamAbs () nm ty' body''
  AppE e1 e2 -> do
    e1' <- firstPass datatypes f e1
    e2' <- firstPass datatypes f e2
    pure $ PIR.Apply () e1' e2'
  LetE varMap binds body -> do
    boundTerms <- foldM (convertBind (mkDict varMap)) [] binds
    body' <- firstPass datatypes (>>= f) $ instantiateEither (either (IR.V . B) (IR.V . F)) body
    case NE.nonEmpty boundTerms of
      -- NOTE: For simplicity we assume here that all let bindings are mutually recursive.
      --       This might not be great for performance (depends on what the PIR compiler does)
      Just boundTerms' -> pure $ PIR.Let () PIR.Rec boundTerms' body'
      Nothing -> error "empty bindings"
  CaseE ty scrutinee alts  -> case getPat <$> alts of
    (p:ps) -> do
      -- Make a PIR function that maps the scrutinee to a temporary pattern ADT with one ctor for each case branch
      scrutineeToPatternsTmp <- toPatternsTmp 0 ty p ps
      -- Make a [PIRTerm] where each PIRTerm is a lambda with args matching its index in the pattern ADT
      branches <- mkCaseBranches alts
      scrutinee' <- firstPass datatypes f scrutinee
      ty' <- toPIRType ty
      pure $ PIR.Case () ty' (scrutineeToPatternsTmp # scrutinee') branches
    [] -> error ""

 where

   mkCaseBranches = undefined
   {- This returns a PIR function of type t -> Bool for the supplied Ty ~ t. We need this
      b/c we have to work "inside-out" when creating the Scrutinee -> TemporaryPattern
      expr, but we "bind variables" "outside-in".

      See `toPatternsImp` for an example of its use

   -}
   matches :: Ty
               -> Pat WithoutObjects (Exp WithoutObjects Ty) a
               -> DatatypeM PIRTerm
   matches t = \case
     AsP _ pat -> matches t pat
     VarP _    -> do
       t' <- toPIRType t
       nm <- freshName
       pure $ PIR.LamAbs () nm t' (mkConstant () True)
     WildP -> do
       t' <- toPIRType t
       nm <- freshName
       pure $ PIR.LamAbs () nm t' (mkConstant () True)
     _ -> error "TODO: Implement matches "


   toPatternsTmp :: Word64
                  -> Ty
                  -> [Pat WithoutObjects (Exp WithoutObjects Ty) a]
                  -> DatatypeM PIRTerm
   toPatternsTmp n t [] = pirError =<< toPIRType t
   toPatternsTmp n t (x:ys)  =  do
     -- TODO: Let bind the fallthrough (make sure to delay the error )
     fallThrough <- toPatternsTmp (n + 1) t ys
     case x of
        VarP _ -> do
          name <- freshName
          t' <- toPIRType t
          pure $ PIR.LamAbs () name t' (PIR.Constr () t' n [PIR.Var () name])
        WildP -> do
          name <- freshName
          t' <- toPIRType t
          pure $ PIR.LamAbs () name t' (PIR.Constr () t' n [PIR.Var () name])
      -- I guess we map the whole expr to the identifier as the first ctor arg
      -- then recurse w/ the body? dunno what else to do!
        AsP _ pat -> do
          name <- freshName
          t' <- toPIRType t
          -- :: t -> Bool (the builtin bool)
          matchesInner <- matches t pat
          inner <- toPatternsTmp 0 t [pat] -- this isn't right -_-
          let nameVar = PIR.Var () name
              innerApplied = inner # nameVar
          body <- pirIfThen
                    (matchesInner # nameVar)
                    (PIR.Constr () t' n [nameVar,innerApplied]) -- idk if this is right this is hard to visualize
                    fallThrough
          pure $ PIR.LamAbs () name t' body
        ConP tn cn ps -> do
          {- Ok this one is complicated so I want to write out what I'm trying to do.

             Step 1: Determine the index of the current constructor (indicated by cn). Can do this w/ the datatypes

             Step 2: Determine the total # of constructors for the type

             Step 3: Fill a list w/ the fallThrough case (which should be let bound so not much ast duplication)

             Step 4: Overwrite the index of the current constructor w/ the inner temp pattern function
          -}
          let (thisCtorIx,thisCtorDecl) = getConstructorIndexAndDecl cn datatypes
              ctorArgs = snd <$> thisCtorDecl ^. cdCtorFields
              allCtorDecls  = getAllConstructorDecls tn datatypes

              totalCtors = length allCtorDecls
              allFallThrough = replicate totalCtors fallThrough
              thisDataDecl = fromJust $ lookupDataDecl tn datatypes
              declArgVars = uncurry TyVar <$> thisDataDecl ^. dDataArgs
          {- if we're in this branch, the input Ty *has* to be a datatype, and it should have been maximally monomorphized
             by this point.

             lemme think through this w/ an example:
               data T a b c
                 = T1 a b
                 | T2
                 | T3 c c

             and suppose we are working w/ an input type
               T Int Boolean Char

             and a Ctor pattern for
               T3 c c
          -}
              dataTyCon    = TyCon tn
              polyTy    = foldl' applyType dataTyCon declArgVars

              polyCtorTy = foldr funTy polyTy ctorArgs

              instantiations = getInstantiations t polyTy

              -- This might not be exactly correct, it assumes that NO tyvars remain in the input type.
              -- If any remain, they might not line up with the var names in the declaration (probably won't
              -- since I think I generated the decl vars). Not sure how to fix this ATM?
              monoCtorTy = replaceAllTypeVars instantiations polyCtorTy

              -- if it's a unary constructor, we want to skip the error from funArgTypes being partial
              monoCtorArgs = safeFunArgTypes monoCtorTy

              -- Ctor argument patterns and their types, indexed for constructing the tempSOP
              argsAndInnerPats = zip [0..]
                                 $ zipIfSameLen "toPatternsTmp: Mismatched # of arguments in actual and deduced monomorphic ctor type"
                                   monoCtorArgs
                                   ps
          -- The SOP for the type that this constructor pattern is a constructor in
          dataSOP  <-   fmap (PIR.TySOP ())
                      . traverse (traverse (toPIRType . snd) . view cdCtorFields)
                      $ allCtorDecls



          {- We need:
              1. A PIR function that checks whether the inner pattern matches
              2. A PIR function that does mapping to temporary ADTs for the inner patterns


          -}


          let mkCtorInnerPatFun :: [(Ty, Pat WithoutObjects (Exp WithoutObjects Ty) a)] -> DatatypeM PIRTerm
              -- if this is empty then the constructor doesn't have any arguments and we should be able to just return True
              mkCtorInnerPatFun [] = error "mkCtorInnerPatFun: Can't make a temporary pattern SOP function"
              mkCtorInnerPatFun [(t,pat)] = do
                patFun <- toPatternsTmp 0 t [pat] 
                freshLam t $ \_ tX -> pure $ patFun # tX
              mkCtorInnerPatFun ((t,pat):rest) = do
                patFunRest <- mkCtorInnerPatFun rest
                patFunThis <- toPatternsTmp 0 t [pat]
                freshLam t $ \_ tX -> pure $  # tX


          -- :: ctorArg1 -> (...) -> ctorArgN -> BuiltinBool
          matchFun <- mkCtorInnerMatchFun argsAndInnerPats

          -- :: ctorArg1 -> (...) -> ctorArgN -> TempPat
          innerPatFun <- mkCtorInnerPatFun argsAndInnerPats



          -- This is the result type of the case expression that deconstructs
          -- the input if it matches this branch, NOT for the whole enclosing tmp pattern

          freshLam t $ \t' ctorX -> do
            let guard = matchFun # ctorX
                body = PIR.Case () dataSOP ctorX (allFallThrough & ix thisCtorIx .~ innerPatFun)
            undefined


        LitP litP -> case litP of
          IntL i -> freshLam t $ \t' xInt -> do
            let guard = pirEqInt (mkConstant () i)  xInt
            pirIfThen guard (PIR.Constr () t' n []) fallThrough
          NumL d -> error "number patterns not supported"
          StringL str -> freshLam t $ \t' xStr -> do
            let str' = mkConstant () $ prettyPrintString str
                guard = pirEqString str' xStr
            pirIfThen guard (PIR.Constr () t' n []) fallThrough
          CharL c -> freshLam t $ \t' xChar -> do
            let c' = mkConstant () . toInteger . fromEnum $ c
                guard = pirEqInt c' xChar
            pirIfThen guard (PIR.Constr () t' n []) fallThrough
          -- TODO: Remove boolean literals, they're subsumed by the constructor patterns, True/False are just normal ctors
          BoolL b -> freshLam t $ \t' xBool -> do
            let b' = mkConstant () b
            -- seriously? they don't have a fucking BuiltinAnd? jfc
            tBranch <- pirIfThen xBool (PIR.Constr () t' n []) fallThrough
            fBranch <- pirIfThen xBool fallThrough (PIR.Constr () t' n [])
            pirIfThen b' tBranch fBranch
          ArrayL{} -> error "TODO: Desugar Array patterns to ConP patterns for the SOP list and remove them"
          ConstArrayL{} -> error "TODO: Remove ConstArray and ConstArrayL"


   mkDict = M.fromList . fmap (\(a,FVar _ ident) -> (disqualify ident,a)) . M.toList

   convertBind :: Map Ident Int
               -> [PIRTermBind]
               -> BindE Ty (Exp WithoutObjects Ty) a
               -> DatatypeM [PIRTermBind]
   convertBind dict acc = \case
     NonRecursive ident expr -> do
       nonRec <- goBind ident expr
       pure $ nonRec:acc
     Recursive xs -> do
       xs' <- traverse (uncurry goBind) xs
       pure $ xs' <> acc
    where
      goBind ident expr = case M.lookup ident dict of
       Just i -> do
         let nm = Name (runIdent ident) $ Unique i
         ty <- toPIRType (expTy f expr)
         expr' <- firstPass datatypes f expr
         -- NOTE: Not sure if this should always be strict?
         pure $ TermBind () Strict (VarDecl () nm ty) expr'
       Nothing -> throwError $ "convertBind: Could not determine variable index for let-bound identifier " <> showIdent' ident

   firstPassLit :: Ty
                -> Lit WithoutObjects (Exp WithoutObjects Ty a)
                -> DatatypeM PIRTerm
   firstPassLit litTy = \case
    IntL i -> pure $ mkConstant () i
    NumL _ -> error "TODO: Doubles? (We could actually represent them as a builtin pair of integers so there is some sense to keeping them)"
    StringL str -> -- REVIEW/TODO/FIXME: This is probably wrong?
       pure $ mkConstant () $ prettyPrintString str
    CharL c -> -- REVIEW/TODO/FIXME: Is this what we want?
        pure
      . mkConstant ()
      . toInteger
      . fromEnum
      $ c
    BoolL b -> pure $ mkConstant () b
    ArrayL arr -> error "We need a SOP list data type defined somewhere to codegen array literals that have non-PLC-constant expressions in them"
    ConstArrayL constArr -> firstPassConstArray litTy constArr

   firstPassConstArray :: Ty -> [Lit WithoutObjects Void] -> DatatypeM PIRTerm
   firstPassConstArray (IR.TyApp (TyCon C.Array) litTy) ls = case ls of
     [] -> do
       SomeUni uni <- (toPIRType >=> extractUni) litTy
       pure $ Constant () (Some (ValueOf (DefaultUniList uni) []))
     lits ->  do
       SomeUni uni <- (toPIRType >=> extractUni) litTy
       lits' <- traverse (convertTrueLit uni) lits
       pure $ Constant () (Some $ ValueOf (DefaultUniList uni) lits')
   firstPassConstArray ty lits =
     throwError $ "firstPassConstArray: Expected an array type for the constant array:\n  "
            <> prettyStr lits
            <> "But received type: " <> prettyStr ty

convertTrueLit :: forall (x :: GHC.Type). DefaultUni (Esc x) -> Lit WithoutObjects Void -> DatatypeM x
convertTrueLit uni lit = case uni of
  PLC.DefaultUniData -> throwError  "convertTrueLit: Data literals not supported at this time"
  PLC.DefaultUniByteString -> throwError  "convertTrueLit: ByteString literals are not supported at this time (though I guess they could be?)"
  PLC.DefaultUniPair _ _ -> throwError  "convertTrueLit: Tuple literals are not supported at this time"
  PLC.DefaultUniBool -> case lit of
    BoolL b -> pure b
    other -> throwError (mkError other)
  PLC.DefaultUniInteger -> case lit of
    CharL c -> pure . toInteger . fromEnum $ c
    IntL i  -> pure i
    other   -> throwError (mkError other)
  PLC.DefaultUniString -> case lit of
    StringL str -> pure . prettyPrintString $ str
    other -> throwError (mkError other)
  PLC.DefaultUniList uni' -> case lit of
    ConstArrayL xs -> traverse (convertTrueLit uni') xs
    other -> throwError (mkError other)
  other -> throwError $ "Unsupported PLC Constant type " <> show other <> " for literal value: " <> prettyStr lit
 where
   mkError :: Lit WithoutObjects Void -> String
   mkError wrong = "convertTrueLit: Type mismatch. Expected a literal value of PLC DefaultUni type " <> show uni <> " but found: " <> prettyStr wrong

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

zipIfSameLen :: String -> [a] -> [b] -> [(a,b)]
zipIfSameLen _ [] [] = []
zipIfSameLen msg (x:xs) (y:ys) = (x,y) : zipIfSameLen msg xs ys
zipIfSameLen msg _ _ = error msg

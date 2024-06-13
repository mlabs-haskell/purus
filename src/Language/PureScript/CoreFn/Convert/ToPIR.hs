{-# OPTIONS_GHC -Wno-orphans #-} -- has to be here (more or less)
{-# LANGUAGE ScopedTypeVariables #-}





{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Fuse foldr/<$>" #-}
{-# HLINT ignore "Use fewer imports" #-}

module Language.PureScript.CoreFn.Convert.ToPIR  where

import Prelude
import Language.PureScript.Names (
  Qualified (..),
  ProperName(..), runIdent, disqualify, ProperNameType (..), Ident)
import Language.PureScript.CoreFn.FromJSON ()
import Data.Map (Map)
import Data.Map qualified as M
import Data.Text qualified as T
import Language.PureScript.PSString (prettyPrintString)
import Language.PureScript.Constants.Prim qualified as C
 -- mainly for the module (we might need it for constructors? idk)
import PlutusIR qualified as PIR
import PlutusCore (Unique (..), runQuoteT)
import Language.PureScript.CoreFn.Convert.IR qualified as IR
import PlutusCore qualified as PLC
import Language.PureScript.CoreFn.Convert.DesugarCore
    ( WithoutObjects )

import Data.Void ( Void )
import Data.Kind qualified as GHC
import Type.Reflection ( Typeable, TypeRep, typeRep )
import Bound.Scope (instantiateEither)
import Data.List.NonEmpty qualified as NE
import Language.PureScript.CoreFn.Desugar.Utils (showIdent', properToIdent)
import Language.PureScript.CoreFn.Convert.Datatypes
    ( extractUni,
      getConstructorName,
      toPIRType,
      freshName,
      SomeUni(SomeUni),
      DatatypeDictionary,
      DatatypeM,
      PIRType,
      PIRTerm,
      mkTypeBindDict,
      mkVar,
      pirDatatypes,
      withLocalVars )
import Control.Monad.Except (MonadError(..))
import Language.PureScript.CoreFn.Module (
  Datatypes,
  getConstructorIndexAndDecl,
  getAllConstructorDecls,
  lookupDataDecl,
  cdCtorFields,
  dDataArgs)
import GHC.Word (Word64)
import Language.PureScript.CoreFn.TypeLike (TypeLike (..), getInstantiations, safeFunArgTypes)
import Data.Foldable (foldl')
import Data.Maybe (fromJust)
import Control.Lens.Operators ((^.), (&), (.~))
import Control.Lens (view, ix)
import Data.Text (Text)
import PlutusIR.Compiler (CompilationCtx)
import PlutusCore.Evaluation.Machine.ExBudgetingDefaults qualified as PLC
import PlutusCore (getDefTypeCheckConfig)
import Control.Monad.Trans.Reader ( runReader, Reader )
import Control.Monad.Trans.Except ( ExceptT, runExceptT )
import Control.Monad.Trans.State ( evalState )
import PlutusIR.Compiler ( Compiling, toDefaultCompilationCtx )
import Data.Bifunctor (Bifunctor(first))
import PlutusIR.Core.Instance.Pretty.Readable (prettyPirReadable)
import Language.PureScript.CoreFn.FromJSON ()
import Bound ( Var(..) )
import Control.Monad ( join, (>=>), foldM, replicateM, void )
 -- mainly for the module (we might need it for constructors? idk)
import Language.PureScript.CoreFn.Convert.IR
    ( getPat,
      expTy,
      Alt(..),
      BindE(..),
      Pat(..),
      Lit(NumL, ArrayL, ConstArrayL, CharL, IntL, StringL),
      Exp(CaseE, V, LitE, LamE, AppE, LetE),
      FVar(..),
      BVar(..),
      Ty(TyCon) )
import PlutusIR
    ( Name(Name),
      VarDecl(VarDecl),
      Recursivity(Rec, NonRec),
      Strictness(Strict),
      Binding(TermBind),
      Term(Builtin, Constant) )
import PlutusCore.Default
    ( DefaultFun,
      pattern DefaultUniList,
      DefaultUni,
      Esc,
      ValueOf(ValueOf),
      Some(Some) )
import Language.PureScript.Constants.PLC ( defaultFunMap )
import PlutusIR.MkPir ( mkConstant )
import Language.PureScript.CoreFn.Convert.DesugarObjects
    ( prettyStr, prepPIR )
import PlutusIR.Compiler.Provenance ( Provenance(Original) )
import PlutusIR.Error ( Error )
import Control.Exception ( throwIO )
import PlutusCore.Evaluation.Machine.Ck
    ( EvaluationResult, unsafeEvaluateCk )
import Prettyprinter (Doc)
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

pirLetNonRec :: PIRType -- type of the expression we're let- binding
             -> PIRTerm
             -> (PIRTerm -> DatatypeM PIRTerm)
             -> DatatypeM PIRTerm
pirLetNonRec ty toLet f = do
  nm <- freshName
  let myvar = PIR.Var () nm
      varDecl = PIR.VarDecl () nm ty
      binding = TermBind () Strict varDecl toLet
  PIR.Let () NonRec (NE.singleton binding) <$> f myvar

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

runDatatypeM :: DatatypeDictionary -> DatatypeM a -> Either String a
runDatatypeM dict act = evalState (runExceptT act) dict

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
    [] -> error "empty case alternatives (should be impossible at this stage)"
    ps -> do
      let tmpSOPTyIR =  mkTmpSOP (expTy f scrutinee) <$> ps
      tmpSOPTy <- PIR.TySOP () <$> traverse (traverse toPIRType) tmpSOPTyIR
      -- Make a PIR function that maps the scrutinee to a temporary pattern ADT with one ctor for each case branch
      scrutineeToPatternsTmp <- toPatternsTmp tmpSOPTy 0 ty ps
      -- Make a [PIRTerm] where each PIRTerm is a lambda with args matching its index in the pattern ADT
      branches <- traverse (mkCaseBranch ty) alts
      scrutinee' <- firstPass datatypes f scrutinee
      ty' <- toPIRType ty
      pure $ PIR.Case () ty' (scrutineeToPatternsTmp # scrutinee') branches
 where
   toPIRList :: [Exp WithoutObjects Ty a] -> DatatypeM PIRTerm
   toPIRList es = firstPass datatypes id $ foldr goCons goNil (fmap f <$> es)
     where
       goNil =
         let nilTy = quantify $ applyType (IR.TyCon C.Array) (IR.TyVar "x" IR.KindType)
             nilIdent = properToIdent <$> C.C_Nil
         in V . F $ FVar nilTy nilIdent
       goCons x acc =
         let xTy = expTy id x
             xTyList = applyType (IR.TyCon C.Array) xTy
             consCtor = V  . F $ FVar (funTy xTy xTyList) (properToIdent <$> C.C_Cons)
         in AppE consCtor acc
   mkTmpSOP :: Ty -> Pat WithoutObjects (Exp WithoutObjects Ty) a -> [Ty]
   mkTmpSOP scrutineeTy = \case
     VarP _ -> pure  scrutineeTy
     WildP  ->  pure scrutineeTy
     AsP _ innerP ->
       scrutineeTy : mkTmpSOP scrutineeTy innerP
     -- T a b c ->
     ConP cn tn ps ->
       let (_,ctorFields) = monoCtorFields cn tn scrutineeTy datatypes
           ctorFieldsSOP =  uncurry mkTmpSOP <$> zip ctorFields ps
       in concat ctorFieldsSOP
     LitP lit -> case lit of
       ArrayL xs -> case snd $ stripQuantifiers scrutineeTy of
          IR.TyApp (TyCon C.Array) innerT -> concatMap (mkTmpSOP innerT) xs
          _ -> error "invalid array type"
       _ -> []

   collectPatBindings :: Ty -> Pat WithoutObjects (Exp WithoutObjects Ty) a -> [(Text,Ty)]
   collectPatBindings t p = case p of
     VarP i -> [(runIdent i,t)]
     WildP  -> []
     AsP asIdent innerP ->
       let this = (runIdent asIdent,t)
           rest =  collectPatBindings t innerP
       in this:rest
     ConP cn tn ps ->
       let (_,ctorFields) = monoCtorFields cn tn t datatypes
       in concatMap (uncurry collectPatBindings) $ zip ctorFields ps
     LitP lit -> case lit of
       ArrayL xs -> case snd $ stripQuantifiers t of
          IR.TyApp (TyCon C.Array) innerT -> concatMap (collectPatBindings innerT) xs
          _ -> error "invalid array type"
       _ -> []

   mkCaseBranch :: Ty
                -> Alt WithoutObjects Ty (Exp WithoutObjects Ty) a
                -> DatatypeM PIRTerm
   mkCaseBranch ty (UnguardedAlt _ pat scopedBody) = do
     let patBinders = collectPatBindings ty pat
     -- this just binds the names in the monad so the Uniques line up
     withLocalVars (fst <$> patBinders) $ do
       lambdaLHS <- foldM (\acc (nm,tx) -> do
                              nm' <- mkVar nm
                              tx' <- toPIRType tx
                              pure $ acc . PIR.LamAbs () nm' tx'
                          ) id patBinders
       lambdaRHS <- firstPass datatypes (>>= f) $ instantiateEither (either (IR.V . B) (IR.V . F)) scopedBody
       pure $ lambdaLHS lambdaRHS

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
     LitP lit -> case lit of
       IntL i -> freshLam t $ \_ xInt -> do
         let i' = mkConstant () i
         pure $ pirEqInt i' xInt
       NumL _ -> error "matches: number lit patterns aren't supported yet"
       StringL str -> freshLam t $ \_ xStr -> do
         let str' = mkConstant () (prettyPrintString str)
         pure $ pirEqString str' xStr
       CharL c -> freshLam t $ \_ xChar -> do
         let c' = mkConstant () . toInteger . fromEnum $ c
         pure $ pirEqInt c' xChar
       _ -> error "matches: Array pattern"
     ConP tn cn ps -> do
       let (ctorIx,fieldTys) = monoCtorFields tn cn t datatypes
       matchFun <- matchesCtorFields ctorIx fieldTys ps
       let allCtorDecls = getAllConstructorDecls tn datatypes
       allFalseFuns <- mkAllFalse $ map (map snd . view cdCtorFields) allCtorDecls
       let withTrueBranch = allFalseFuns & ix ctorIx .~ matchFun
           tyBuiltinBool  = PLC.TyBuiltin () (PLC.SomeTypeIn PLC.DefaultUniBool)
       freshLam t $ \_ scrutX -> pure $ PIR.Case () tyBuiltinBool scrutX withTrueBranch
      where
        mkAllFalse :: [[Ty]] -> DatatypeM [PIRTerm]
        mkAllFalse [] = pure []
        mkAllFalse (tys:rest) = do
          binderF <- foldM (\accF tx -> do
                               nm <- freshName
                               tx' <- toPIRType tx
                               pure $ accF . PIR.LamAbs () nm tx'
                           ) id tys
          let this = binderF (mkConstant () False)
          rest' <- mkAllFalse rest
          pure $ this:rest'

        
        matchesCtorFields :: Int
                          -> [Ty]
                          -> [Pat WithoutObjects (Exp WithoutObjects Ty) a]
                          -> DatatypeM PIRTerm
        matchesCtorFields _ tys pats = do
          names <- replicateM (length tys) freshName
          let everything = zip names (zip tys pats)
          boundF <- foldM mkCtorFieldBinders id everything
          boundF <$> goCtorMatchBody everything

        -- goCtorFields
        goCtorMatchBody :: [(Name, (Ty, Pat WithoutObjects (Exp WithoutObjects Ty) a))]
                        -> DatatypeM PIRTerm
        goCtorMatchBody [] = pure $ mkConstant () True
        goCtorMatchBody ((nm,(ty,pat)):rest) = do
          matchThis <- matches ty pat
          let matchThisApplied =  matchThis # PIR.Var () nm
          pirAnd matchThisApplied =<< goCtorMatchBody rest

        mkCtorFieldBinders :: (PIRTerm -> PIRTerm)
                           -> (Name,(Ty,Pat WithoutObjects (Exp WithoutObjects Ty) a))
                           -> DatatypeM (PIRTerm -> PIRTerm)
        mkCtorFieldBinders accF (nm,(ty,_)) = do
          ty' <- toPIRType ty
          pure $ accF . PIR.LamAbs () nm ty'

   toPatternsTmp :: PIRType
                  -> Word64
                  -> Ty
                  -> [Pat WithoutObjects (Exp WithoutObjects Ty) a]
                  -> DatatypeM PIRTerm
   toPatternsTmp tmpSOP _ _ [] = pirError tmpSOP
   toPatternsTmp tmpSOP n t (x:ys)  =  do
     -- TODO: Let bind the fallthrough (make sure to delay the error)
     fallThrough_ <- toPatternsTmp tmpSOP (n + 1) t ys
     pirLetNonRec tmpSOP fallThrough_ $ \fallThrough -> case x of
        VarP _ -> freshLam t $ \_ xVar ->
          pure $ PIR.Constr () tmpSOP n [xVar]
        WildP -> freshLam t $ \_ _ ->
          pure $ PIR.Constr () tmpSOP n []
        AsP _ pat -> do -- TODO/FIXME: THIS IS WRONG!!!!
          name <- freshName
          t' <- toPIRType t
          -- :: t -> Bool (the builtin bool)
          matchesInner <- matches t pat
          inner <- toPatternsTmp tmpSOP 0 t [pat] -- this isn't right -_-
          let nameVar = PIR.Var () name
              innerApplied = inner # nameVar
          body <- pirIfThen
                    (matchesInner # nameVar)
                    (PIR.Constr () t' n [nameVar,innerApplied]) -- idk if this is right this is hard to visualize
                    fallThrough
          pure $ PIR.LamAbs () name t' body
        LitP litP -> case litP of
          IntL i -> freshLam t $ \t' xInt -> do
            let guard = pirEqInt (mkConstant () i)  xInt
            pirIfThen guard (PIR.Constr () t' n []) fallThrough
          NumL _ -> error "number patterns not supported"
          StringL str -> freshLam t $ \t' xStr -> do
            let str' = mkConstant () $ prettyPrintString str
                guard = pirEqString str' xStr
            pirIfThen guard (PIR.Constr () t' n []) fallThrough
          CharL c -> freshLam t $ \t' xChar -> do
            let c' = mkConstant () . toInteger . fromEnum $ c
                guard = pirEqInt c' xChar
            pirIfThen guard (PIR.Constr () t' n []) fallThrough
          -- TODO: Remove boolean literals, they're subsumed by the constructor patterns, True/False are just normal ctors
          ArrayL{} -> error "TODO: Desugar Array patterns to ConP patterns for the SOP list and remove them"
          ConstArrayL{} -> error "TODO: Remove ConstArray and ConstArrayL"
        conp@(ConP tn cn ps) -> do
          matchFun <- matches t conp
          let (thisIx,argTys) = monoCtorFields tn cn t datatypes
          names <- replicateM (length argTys) freshName
          let nameTys = zip names argTys
          binderF <- foldM (\accF (nm,ty) -> do
                       ty' <- toPIRType ty
                       pure $ accF . PIR.LamAbs () nm ty'
                     ) id nameTys
          let nameTyPats = zip nameTys ps
              allCtorDecls = getAllConstructorDecls tn datatypes

          -- for a ctor `CT a0 ... aN` conToPatTmpFn should give us the *body* in \(n1 :: a1) ... (nN :: aN) -> body
          freshLam t $ \_ scrutX -> do
            let fallThroughApplied = fallThrough # scrutX
            allFallthrough <- mkAllFallthrough fallThroughApplied
                                $ map (map snd . view cdCtorFields) allCtorDecls
            conToPatTmpFun <- binderF . PIR.Constr () tmpSOP n . map snd <$> conToPatTmp  nameTyPats
            let withSuccessBranch = allFallthrough & ix thisIx .~ conToPatTmpFun
            pirIfThen (matchFun # scrutX) (PIR.Case () tmpSOP scrutX withSuccessBranch) fallThroughApplied
       where
         conToPatTmp :: [((Name, Ty), Pat WithoutObjects (Exp WithoutObjects Ty) a)]
                     -> DatatypeM [(PIRType,PIRTerm)]
         conToPatTmp [] = pure []
         conToPatTmp (((nm,ty),pat):rest) = do
           case pat of
             VarP _ -> do
               ty' <- toPIRType ty
               ((ty',PIR.Var () nm):) <$> conToPatTmp rest
             WildP  -> conToPatTmp rest
             LitP _ -> conToPatTmp rest -- technically this is broken ATM but we really need to remove not-really-literal literal patterns
             AsP _ innerP -> do
               ty' <- toPIRType ty
               let thisVar = (ty',PIR.Var () nm)
               innerRes <- conToPatTmp [((nm,ty),innerP)]
               pure $ thisVar:innerRes
             ConP tn cn ps -> do
               let (thisIx,argTys) = monoCtorFields tn cn ty datatypes
               names <- replicateM (length argTys) freshName
               binderF <- foldM (\accF (nmx,tyx) -> do
                       tyx' <- toPIRType tyx
                       pure $ accF . PIR.LamAbs () nmx tyx'
                     ) id $ zip names argTys
               let nameTyPats = zip (zip names argTys) ps
                   allCtorDecls = getAllConstructorDecls tn datatypes
               err <- pirError tmpSOP
               allFallthrough <- mkAllFallthrough err
                                $ map (map snd . view cdCtorFields) allCtorDecls
               here <- conToPatTmp nameTyPats
               let thisVar = PIR.Var () nm
                   these = map (\(tx,xx) -> (tx,) $ PIR.Case () tx thisVar $ allFallthrough & ix thisIx .~ binderF xx) here
               (these <>) <$> conToPatTmp rest


         mkAllFallthrough :: PIRTerm
                          -> [[Ty]]
                          -> DatatypeM [PIRTerm]
         mkAllFallthrough _ [] = pure []
         mkAllFallthrough fallthroughApplied (tys:rest) = do
           binderF <- foldM (\accF tx -> do
                                nm <- freshName
                                tx' <- toPIRType tx
                                pure $ accF . PIR.LamAbs () nm tx'
                            ) id tys
           (binderF fallthroughApplied :) <$> mkAllFallthrough fallthroughApplied rest


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
    ArrayL arr -> toPIRList arr
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


fuckThisMonadStack ::
      forall e m c  b.
      (e ~ Error DefaultUni DefaultFun (Provenance ())
      , c ~ CompilationCtx DefaultUni DefaultFun ()
      , m ~ ExceptT e (ExceptT e (PLC.QuoteT (Reader c)))
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

runPLCProgram :: PLCProgram DefaultUni DefaultFun () -> (EvaluationResult (PLC.Term PLC.TyName Name DefaultUni DefaultFun ()),[Text])
runPLCProgram (PLC.Program _ _ c) = unsafeEvaluateCk PLC.defaultBuiltinsRuntime $ void c

{-
runPLCProgramTest :: String
                  -> (EvaluationResult (PLC.Term PLC.TyName Name DefaultUni DefaultFun ()),[Text])
                  -> FilePath
                  -> Text
                  -> TestTree
runPLCProgramTest testName expected path decl  = testCase testName $ do
  prog <- declToUPLC path decl
  let out = runPLCProgram prog
  assertEqual "program output matches expected "  expected out
-}
declToPIR :: FilePath
           -> Text
           -> IO (Doc ann)
declToPIR path decl = prepPIR path decl >>= \case
  (mainExpr,datatypes) -> do
    case mkTypeBindDict datatypes mainExpr of
      Left err -> throwIO . userError $ err
      Right dict -> case runDatatypeM dict $ firstPass datatypes F mainExpr of
        Left err -> throwIO . userError $ err
        Right e  -> do
          let dtBinds = NE.fromList $  PIR.DatatypeBind () <$> M.elems (dict ^. pirDatatypes)
              result = PIR.Let () Rec dtBinds e
          putStrLn $ "-------\\/ PIR \\/ --------"
          pure $ prettyPirReadable e

{-
 where
   aghhhh = either (throwIO . userError) pure
   rethrowIO = \case
     Left te@(TypeConvertError _ _ _ ) -> error (prettyErrorT te)
     Right x -> pure x
-}



monoCtorFields
  :: Qualified (ProperName 'TypeName)
  -> Qualified (ProperName 'ConstructorName)
  -> Ty -- the type of the scrutinee
  -> Datatypes IR.Kind Ty
  -> (Int,[Ty]) -- Constructor index & list of field types
monoCtorFields tn cn t datatypes = (thisCtorIx,monoCtorArgs)
 where
   (thisCtorIx,thisCtorDecl) =  getConstructorIndexAndDecl cn datatypes
   ctorArgs = snd <$> thisCtorDecl ^. cdCtorFields
   thisDataDecl = fromJust $ lookupDataDecl tn datatypes
   declArgVars = uncurry IR.TyVar <$> thisDataDecl ^. dDataArgs
   dataTyCon    = TyCon tn
   polyTy    = foldl' applyType dataTyCon declArgVars
   polyCtorTy = foldr funTy polyTy ctorArgs
   instantiations = getInstantiations t polyTy
   -- This might not be exactly correct, it assumes that NO tyvars remain in the input type.
   -- If any remain, they might not line up with the var names in the declaration (probably won't
   -- since I think I generated the decl vars). Might require tweaking "instantiates" & co
   monoCtorTy = replaceAllTypeVars instantiations polyCtorTy
   -- if it's a unary constructor, we want to skip the error from funArgTypes being partial
   monoCtorArgs = safeFunArgTypes monoCtorTy

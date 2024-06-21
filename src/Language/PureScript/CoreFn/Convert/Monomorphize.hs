{-# OPTIONS_GHC -Wno-orphans #-} -- has to be here (more or less)
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications  #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use if" #-}
{-# LANGUAGE MultiWayIf #-}
module Language.PureScript.CoreFn.Convert.Monomorphize (runMonomorphize, testMono, isConstructorE) where

import Prelude

import Language.PureScript.CoreFn.Ann (Ann)
import Language.PureScript.CoreFn.Expr (PurusType)
import Language.PureScript.CoreFn.Module ( Module(..) )
import Language.PureScript.CoreFn.Convert.IR
    ( Exp(..),
      FVar(..),
      Lit(..),
      BindE(..),
      ppExp,
      unsafeAnalyzeApp,
      BVar(..),
      expTy,
      expTy',
      FuncType(..),
      Alt(..),
      Alt )
import Language.PureScript.Names (Ident(..), Qualified (..), QualifiedBy (..), ModuleName (..), showQualified, showIdent)
import Language.PureScript.Types
    ( RowListItem(..), SourceType, Type(..), replaceTypeVars, isMonoType )
import Language.PureScript.CoreFn.Desugar.Utils ( showIdent' )
import Language.PureScript.Environment (pattern (:->), pattern ArrayT, pattern RecordT, function, getFunArgTy)
import Language.PureScript.CoreFn.Pretty (prettyTypeStr)
import Language.PureScript.CoreFn.FromJSON ()
import Data.Text qualified as T
import Data.List (find)
import Data.Map (Map)
import Data.Map qualified as M
import Language.PureScript.PSString (PSString, prettyPrintString)
import Language.PureScript.AST.SourcePos (SourceAnn)
import Control.Lens
    ( (&), (^..), cosmos, transformM )
import Control.Monad (foldM, join)
import Control.Monad.RWS.Class (MonadReader(ask))
import Control.Monad.RWS (RWST(..))
import Control.Monad.Except (throwError)
import Debug.Trace (trace, traceM)
import Language.PureScript.CoreFn.Convert.DesugarCore
    ( desugarCoreModule, WithObjects )
import Bound.Var (Var(..))
import Bound.Scope (mapBound, fromScope, toScope)
import Language.PureScript.CoreFn.TypeLike
    ( TypeLike(..), quantify )
import Language.PureScript.CoreFn.Convert.Monomorphize.Utils
    ( mkFieldMap,
      findDeclBody,
      decodeModuleIO,
      MonoError(..),
      IR_Decl,
      extractAndFlattenAlts,
      findInlineDeclGroup,
      freshBVar,
      freshen,
      gLet,
      getModBinds,
      note,
      qualifyNull,
      unsafeApply,
      updateFreeVars,
      updateVarTyS,
      MonoState(MonoState),
      Monomorphizer, foldMScopeViaExp, distributeExp )

import Data.Text (Text)
import GHC.IO (throwIO)
import Control.Lens.Getter (to)
import Data.Set qualified as S
import Data.Foldable (traverse_)
import Data.Char (isUpper)
import Control.Lens.Plated
import Prettyprinter (Pretty)
import Language.PureScript.CoreFn.Pretty.Common (prettyAsStr)

{- Function for quickly testing/debugging monomorphization -}

testMono :: FilePath -> Text -> IO ()
testMono path decl = do
  myModCoreFn <- decodeModuleIO path
  (myMod,_) <- either (throwIO . userError) pure $ desugarCoreModule myModCoreFn
  Just myDecl <- pure $ findDeclBody decl myMod
  case runMonomorphize myMod [] myDecl of
    Left (MonoError msg ) -> throwIO $ userError $ "Couldn't monomorphize " <> T.unpack decl <> "\nReason:\n" <> msg
    Right body -> do
      let allSubExpressionTypes = S.toList . S.fromList $  body ^.. cosmos . to (expTy id) . cosmos
      putStrLn "All subexpression types:"
      traverse_ (\ty -> putStrLn (prettyTypeStr ty)) allSubExpressionTypes
      putStrLn $ "MONO RESULT: \n" <>  ppExp body
      -- pure unscopedBody

{- This is the top-level entry point for monomorphization. Typically,
   you will search the module for a 'main' decl and use its
   body as the Exp argument.
-}
runMonomorphize ::
  Module IR_Decl PurusType PurusType Ann ->
  [Module IR_Decl PurusType PurusType Ann] -> -- | Modules in scope, for declarations
  Exp WithObjects PurusType (FVar PurusType) ->
  Either MonoError (Exp WithObjects PurusType (Var (BVar PurusType) (FVar PurusType)))
runMonomorphize Module{..} modulesInScope expr =
  runRWST (monomorphize (F <$> expr) >>= inlineEverything) (moduleName,fmap F <$> moduleDecls) (MonoState 0) & \case
    Left err -> Left err
    Right (a,_,_) -> do
      traceM ("\nMONOMORPHIZE OUTPUT: \n" <> ppExp a <> "\n" <> replicate 20 '-')
      pure a

{- Entry point for inlining monomorphization.

   Broadly, we deduce the monomorphic type for a polymorphic function
   by looking at the arguments the function is applied to. Without
   arguments, we cannot deduce the monomorphic type at all, and so
   this function is `pure` if the provided expression is anything other than
   than an `AppE`


-}
monomorphize ::
  Exp WithObjects PurusType (Var (BVar PurusType) (FVar PurusType))  ->
  Monomorphizer (Exp WithObjects PurusType (Var (BVar PurusType) (FVar PurusType)))
monomorphize  xpr = trace ("\nmonomorphize " <>  "\n  " <> ppExp xpr)  $ case xpr of
  app@(AppE _f _arg) ->  do
    arg <- monomorphize _arg
    -- First, we split the
    let (f,args) = unsafeAnalyzeApp (AppE _f arg)
    traceM $ "FUN: " <> ppExp f
    traceM $ "ARGS: " <> show (ppExp <$> args)
    let types = concatMap (splitFunTyParts . expTy id)  args
    traceM $ "ARG TYPES:" <> show (prettyTypeStr <$> types)


    -- FIXME: Check for constructors as well as builtins
    if | isBuiltinE (f) ->  do
           traceM ("\nmonomorphize: Is builtin or ctor, skipping:\n" <> ppExp f)
           pure $ unsafeApply id  f args
       | isConstructorE f -> do
           monoArgs <- traverse monomorphize args
           let fTy' = instantiateWithArgs (expTy id f) (expTy id <$> monoArgs)
           f' <- monomorphizeWithType fTy' f
           pure $ unsafeApply id f' monoArgs
       | otherwise -> handleFunction id f args
  other -> trace ("\nmonomorphize: other: " <> ppExp other) $ pure (other)

inlineEverything :: Exp WithObjects PurusType (Var (BVar PurusType) (FVar PurusType))
                 -> Monomorphizer (Exp WithObjects PurusType (Var (BVar PurusType) (FVar PurusType)))
inlineEverything = transformM go
  where
    go :: Exp WithObjects PurusType (Var (BVar PurusType) (FVar PurusType))
       -> Monomorphizer (Exp WithObjects PurusType (Var (BVar PurusType) (FVar PurusType)))
    go = \case
      (V (F (FVar ty qi))) | not (isBuiltin qi || isConstructor qi) ->  inlineAs ty qi
      AppE e1 e2  -> AppE <$> go e1 <*> go e2
      CaseE t scrut alts -> do
        scrut' <- go scrut
        alts' <- traverse goAlt alts
        pure $ CaseE t scrut' alts'
      LamE t bv body -> do
        let scoped' = toScope
                  <$> join
                  <$> fmap distributeExp
                  <$> traverse (traverse (go . pure @(Exp WithObjects PurusType))) (fromScope body)
        LamE t bv <$> scoped'
      LetE binds decls body -> do
        let scoped' = toScope
                  <$> join
                  <$> fmap distributeExp
                  <$> traverse (traverse (go . pure @(Exp WithObjects PurusType))) (fromScope body)
            goDecl = \case
              NonRecursive ident expr -> NonRecursive ident <$> go expr
              Recursive xs -> Recursive <$> traverse (\(i,ex) -> (i,) <$> go ex) xs
        LetE binds <$> traverse goDecl decls <*> scoped'
      AccessorE x t pss e -> AccessorE x t pss <$> go e
      ObjectUpdateE x t e cf fs -> (\e' fs' -> ObjectUpdateE x t e' cf fs')
                                   <$> go  e
                                   <*> traverse (\(nm,expr) -> (nm,) <$> go expr) fs
      LitE t lit -> LitE t <$> traverse go lit
      other -> pure other

    goAlt (UnguardedAlt bs pat body ) = do
      let body' = toScope
                  <$> join
                  <$> fmap distributeExp
                  <$> traverse (traverse (go . pure @(Exp WithObjects PurusType))) (fromScope body)
      UnguardedAlt bs pat <$> body'


   -- Builtins shouldn't be inlined because they can't be.
   -- TODO/REVIEW: Figure out whether it's necessary to *monomorphize*
   --              polymorphic builtins. (Trivial to implement if needed)
isBuiltinE = \case
  V (F (FVar ty qi)) -> isBuiltin qi
  other -> False

isBuiltin (Qualified (ByModuleName (ModuleName "Builtin")) _ ) = True
isBuiltin _ = False

isConstructorE = \case
  V (F (FVar ty qi)) -> isConstructor qi
  _ -> False
   -- After the recent changes, constructors *can't* be inlined, i.e., they must remain
   -- free until the final PIR compilation stage
isConstructor (Qualified _ (Ident nm)) = isUpper (T.head nm)
isConstructor _  = False

{- "Control flow" for the inlining monomorphizer.

   The first argument is a polymorphic function or identifier that
   refers to a polymorphic function.

   The second argument is the ordered list of argument to which that
   polymorphic function is applied in the `AppE` which was initially
   passed to `monomorphize`.

   Will fail if it is passed anything other than a polymorphic function or a
   free variable. (Or an already-monomorphized expression)

   NOTE: The order of the cases matters here! Don't move them around haphazardly.
-}


handleFunction :: forall a
                . Pretty a
               => (a -> Var (BVar PurusType) (FVar PurusType))
               ->  Exp WithObjects PurusType a
               -> [Exp WithObjects PurusType a] -- TODO: List could be empty?
               -> Monomorphizer (Exp WithObjects PurusType (Var (BVar PurusType) (FVar PurusType)))
-- If we don't have any argument types, the only thing we can do is return the original expression.
handleFunction  toVar e [] = trace ("\nhandleFunction FIN: " <> ppExp e) $ pure (toVar <$> e)
{- If we have an explicit polymorphic lambda & argument types, we look at the argument types one-by-one
   and check whether those types allow us to deduce the type to which the TyVar bound by the Forall in
   the lambda's type ought to be instantiated. (See the `instantiates` function)
-}
handleFunction toVar expr@(LamE (quantify -> (ForAll _ _ var _ inner  _)) bv@(BVar bvIx _ bvIdent) body'') (arg:args) = do
  traceM  ("\nhandleFunction abs:\n  " <> ppExp expr)
  let t = expTy toVar arg -- get the type of the first expression to which the function is applied
  traceM $ prettyTypeStr t
  let polyArgT = getFunArgTy inner -- get the (possibly polymorphic) type of the first arg in the function's signature
      -- Get the (type) instantiation for the bound var, the (possibly) monomorphic type of the arg expr,
      -- and the (possibly) polymorphic type of the function's first arg
      --   REVIEW: Can we do all of the arguments at once?
      doInstantiate :: SourceType -> SourceType
      doInstantiate = case instantiates var t polyArgT of
                        Just tx -> replaceTypeVars var tx
                        Nothing -> id
      body' = updateVarTyS  bv t body'' -- update the type of the variable bound by the lambda in the body of the expression
      -- NOTE/REVIEW: Not 100% sure that this doesn't involve a problematic join
      bodyUnscoped  = join <$> fmap toVar <$> fromScope body'
  body <- toScope  <$>  (\b ->  handleFunction id b $ fmap toVar <$> args) (bodyUnscoped) -- recurse on the body, with the rest of the arguments
  traceM ("bodyUnscoped:\n" <> prettyAsStr bodyUnscoped)
  let bodyT = expTy' F body
      funT  = quantify $ doInstantiate $ function t bodyT
      firstArgT = headArg funT
      e' = LamE funT (BVar bvIx firstArgT bvIdent) body
  pure $  AppE  (F <$> e') (toVar <$> arg) -- Put the app back together. (Remember, the params to this function come from a deconstructed AppE)
{- If we have a free variable, we attempt to inline the variable (without altering the type)
   and then call `handleFunction` on the inlined expression with the same argument types

handleFunction  toVar v@(V (FVar ty  qn)) es = trace ("\nhandleFunction VarGo: " <> ppExp v) $ do
  e' <- inlineAs ty qn
  handleFunction toVar e' es
-}
handleFunction toVar v@(V (toVar -> F (FVar ty qn))) es
  | not (isBuiltin qn) = trace ("\nhandleFunction VarGo: " <> ppExp v) $ do
    e' <- inlineAs ty qn
    handleFunction id e' (fmap toVar <$> es)


{- If the function parameter is monomorphic then we rebuild the initial AppE and return it.
-}
handleFunction toVar e es | isMonoType (expTy toVar e)  = traceM ("\nhandleFunction isMono: " <> ppExp e) >>
                            pure $ unsafeApply id  (toVar <$> e) (fmap toVar <$> es)
-- Anything else is an error.
handleFunction toVar e es = throwError $ MonoError
                        $ "Error in handleFunction:\n  "
                        <> ppExp e
                        <> "\n  " <> show (ppExp <$> es)
                        <> "\n  is not an abstraction or variable"

{- | Monomorphizing inliner. Looks up the provided
     identifier in the module context (TODO: linker that lets us support multiple modules)
     and attempts to forcibly assign the provided type to that expression, then
     inlines the monomorphized expression.

     Inlines and monomorphizes recursively, so that all free variables in the expression being
     inlined are monomorphized to the appropriate type and themselves inlined. (I.e. inlines
     everything and monomorphizes everything to the maximum extend possible)
-}
inlineAs ::
  PurusType ->
  Qualified Ident ->
  Monomorphizer (Exp WithObjects PurusType (Var (BVar PurusType) (FVar PurusType)))
-- TODO/REVIEW: Check whether this has any purpose here \/
inlineAs ty = \case
  nm@(Qualified (ByModuleName (ModuleName "Builtin")) _ ) -> do
    traceM ("\ninlineAs BUILTIN: " <> T.unpack (showQualified showIdent nm))
    pure . V . F $ FVar ty nm
   -- TODO: Linker so we can make this work no matter what the source module is
  (Qualified (ByModuleName mn') ident) -> trace ("\ninlineAs: " <> showIdent' ident <> " :: " <>  prettyTypeStr ty) $ ask >>= \(mn,modDict) ->
    if mn ==  mn' then do
      let msg = "Couldn't find a declaration with identifier " <> showIdent' ident <> " to inline as " <> prettyTypeStr ty
      note msg  (findInlineDeclGroup ident modDict) >>= \case
        NonRecursive _ e ->  monomorphizeWithType ty e
        Recursive xs -> do
          let msg' = "Target expression with identifier " <> showIdent' ident <> " not found in mutually recursive group"
          (targIdent,targExpr) <- note msg' $ find (\x -> fst x == ident)  xs -- has to be there
          fresh <- freshen targIdent
          let initialRecDict = M.singleton targIdent (fresh,ty,targExpr)
          dict <- collectRecBinds  ty initialRecDict targExpr
          let renameMap = (\(i,t,_) -> (i,t)) <$> dict
              bindingMap = M.elems dict
          binds <- traverse (\(newId,newTy,oldE) -> makeBind renameMap newId newTy oldE) bindingMap
          case M.lookup targIdent renameMap of
            Just (newId,newTy) -> do
              let body = F <$> pure (FVar newTy $ qualifyNull newId)
              pure $ gLet binds body
            Nothing -> throwError
                       $ MonoError
                       $ "Couldn't inline "
                         <> showIdent' ident
                         <> " - identifier didn't appear in collected bindings:\n  "
                         <> show renameMap
     -- TODO: This is a temporary hack to get builtins working w/o a real linker.
     else  throwError $ MonoError "Imports aren't supported yet!"
  wrong@(Qualified (BySourcePos _) _) -> pure . V . F $ FVar ty wrong {-
    throwError
    $ MonoError
    $ "Cannot inline variable qualified by SourcePos. Such a variable should be bound (and ergo not exist at this stage of compilation)"
      <> "\n Variable: "
      <> show wrong -}
 where
   {- Arguments:
        1. A renaming dictionary,
        2. An Identifier which represents the new name of a binding (and should be present in the renaming dictionary),
        3. The new, possibly monomorphic (hopefully more-monomorphic at least) type which will be forcibly assigned to the body expression),
        4. A (possibly polymorphic) expression that will become the decl body

      Constructs a BindE where the Ident is the 2nd arg and the body is
      the 4th arg forcibly assigned the type of the 3rd arg.
   -}
   makeBind :: Map Ident (Ident,SourceType) -- Map OldName (NewName,TypeToAssign)
            -> Ident                        -- NewName
            -> SourceType                   -- TypeToAssign
            -> Exp WithObjects PurusType (Var (BVar PurusType) (FVar PurusType)) -- Declaration body
            -> Monomorphizer (BindE PurusType (Exp WithObjects PurusType) (Var (BVar PurusType) (FVar PurusType)))
   makeBind renameDict newIdent t e = trace ("\nmakeBind: " <> showIdent' newIdent) $ do
     e' <- updateFreeVars renameDict <$> monomorphizeWithType t  e
     pure $ NonRecursive newIdent e'

   -- Find a declaration body in the *module* scope
   findDeclarationBody :: Ident -> Monomorphizer (Maybe (Exp WithObjects PurusType (Var (BVar PurusType) (FVar PurusType))))
   findDeclarationBody nm = go <$> getModBinds
    where
      go :: [BindE PurusType (Exp WithObjects PurusType) (Var (BVar PurusType) (FVar PurusType))]
         -> Maybe (Exp WithObjects PurusType (Var (BVar PurusType) (FVar PurusType)))
      go [] = Nothing
      go (b:bs) = case b of
        NonRecursive nm' e -> if nm' == nm then Just e else go bs
        Recursive xs -> case find (\x -> fst x == nm) xs of
          Nothing -> go bs
          Just (_,e) -> Just e

   {- RECURSIVE BINDINGS

      First, we need to walk the target expression and collect a list of all of the used
      bindings and the type that they must be when monomorphized, and the new identifier for their
      monomorphized/instantiated version. (We *don't* change anything here)

      Each of these `collectX` functions is specialized tool for collecting used bindings and
      deducing the type to which they should be assigned (based, initially, on the SourceType
      argument to `inlineAs`)
   -}

   -- Top-level collection function
   collectRecBinds ::
     PurusType ->
     Map Ident (Ident, SourceType, Exp WithObjects PurusType (Var (BVar PurusType) (FVar PurusType))) ->
     Exp WithObjects PurusType (Var (BVar PurusType) (FVar PurusType)) ->
     Monomorphizer (Map Ident (Ident, SourceType, Exp WithObjects PurusType (Var (BVar PurusType) (FVar PurusType))))
   collectRecBinds t visited  e = trace ("\ncollectRecBinds:\n  " <> ppExp  e <> "\n  " <> prettyTypeStr t) $ case  e of
     LitE _ (ArrayL arr) -> trace "crbARRAYLIT" $ case t of
       ArrayT inner -> do
         innerBinds <- foldM (collectRecBinds inner) visited arr
         pure $ visited <> innerBinds
       _ -> throwError $ MonoError ("Failed to collect recursive binds: " <> prettyTypeStr t <> " is not an Array type")
     LitE _ (ObjectL _ fs) -> trace "crbOBJLIT" $ case t of
         RecordT fields -> do
           let fieldMap = mkFieldMap fields
           innerBinds <- collectRecFieldBinds visited fieldMap fs
           pure $ visited <> innerBinds
         _ -> throwError $ MonoError ("Failed to collect recursive binds: " <> prettyTypeStr t <> " is not a Record type")
     LitE _ _  -> trace "crbLIT" $ pure visited
     ObjectUpdateE _ _ _ _ updateFields -> trace "crbOBJUPDATE" $ case t of
        RecordT fields -> do
          let fieldMap = mkFieldMap fields
          innerBinds <- collectRecFieldBinds visited fieldMap  updateFields
          pure $ visited <> innerBinds
        _ -> throwError $ MonoError  ("Failed to collect recursive binds: " <> prettyTypeStr t <> " is not a Record type")
     AccessorE _ _ _ _ -> trace "crbACCSR" $ pure visited -- idk. given (x.a :: t) we can't say what x is.
     absE@(LamE _ _ _) -> trace ("crbABS TOARGS: " <> prettyTypeStr t) $ collectFun visited  absE (splitFunTyParts t)
     app@(AppE _ e2) -> trace "crbAPP" $ do
       let (f,args) = unsafeAnalyzeApp app
           types = (expTy id <$> args) <> [t]
       funBinds' <- collectFun visited f types
       let funBinds = visited <> funBinds'
       argBinds <- collectRecBinds (head types) funBinds  e2
       pure $ funBinds <> argBinds
     V (F (FVar _ (Qualified (ByModuleName _) nm))) -> trace ("crbVAR: " <> showIdent' nm)  $ case M.lookup nm visited of
       Nothing -> findDeclarationBody nm >>= \case
         Nothing -> throwError $ MonoError   $ "No declaration correponding to name " <> showIdent' nm <> " found in the module"
         Just ex -> do
           freshNm <- freshen nm
           let this = (freshNm,t,ex)
           pure $ M.insert nm this visited
       Just _ -> pure visited  -- might not be right, might need to check that the types are equal? ugh keeping track of scope is a nightmare
     V (F (FVar _ (Qualified _ nm))) -> trace ("crbVAR_: " <> showIdent' nm) $ pure visited
     CaseE _ _  alts -> trace "crbCASE" $ do
       let flatAlts = concatMap extractAndFlattenAlts alts
       foldMScopeViaExp visited (collectRecBinds t) flatAlts
     LetE _ _ ex -> collectRecBinds t visited  (join <$> fromScope ex)

   -- Collect from the fields of an object
   collectRecFieldBinds :: Map Ident (Ident, SourceType, Exp WithObjects PurusType (Var (BVar PurusType) (FVar PurusType)))
                        -> M.Map PSString (RowListItem SourceAnn)
                        -> [(PSString, Exp WithObjects PurusType (Var (BVar PurusType) (FVar PurusType)))]
                        -> Monomorphizer (Map Ident (Ident, SourceType, Exp WithObjects PurusType (Var (BVar PurusType) (FVar PurusType))))
   collectRecFieldBinds visited _ [] =  pure visited
   collectRecFieldBinds visited cxt ((lbl,e):rest) = trace "collectRecFieldBinds" $ do
     RowListItem{..} <- note ("No type for field with label " <> T.unpack (prettyPrintString lbl) <> " when collecting record binds")
                          $ M.lookup lbl cxt
     this <- collectRecBinds rowListType visited e
     collectRecFieldBinds (visited <> this) cxt rest

   -- Collect from a function expression
   collectFun :: Map Ident (Ident, SourceType, Exp WithObjects PurusType (Var (BVar PurusType) (FVar PurusType)))
              -> Exp WithObjects PurusType (Var (BVar PurusType) (FVar PurusType))
              -> [SourceType]
              -> Monomorphizer (Map Ident (Ident, SourceType, Exp WithObjects PurusType (Var (BVar PurusType) (FVar PurusType))))
   collectFun visited e [t] = trace ("collectFun FIN:\n  " <> {- -ppExp e <> " :: " -}  prettyTypeStr t)  $ do
     rest <- collectRecBinds t visited e
     pure $ visited <> rest
   collectFun visited  e (t:ts) =
     trace ("collectFun:\n  " <> ppExp e <> "\n  " <> prettyTypeStr t <> "\n" <> show ts)  $
      case  e of
        LamE (ForAll{}) bv  body'' -> do
          let body' =  fmap join . fromScope $ updateVarTyS bv t  body''
          collectFun visited body' ts

        (V (F (FVar _ (Qualified (ByModuleName _) nm)))) -> trace ("collectFun VAR: " <> showIdent' nm) $ do
         case M.lookup nm visited of
           Nothing -> do
             let t' = foldr1 function (t:ts)
                 msg =  "Couldn't find a declaration with identifier " <> showIdent' nm <> " to inline as " <> prettyTypeStr t
             declBody <-  note  msg =<< findDeclarationBody nm
             freshNm <- freshen nm
             let visited' = M.insert nm (freshNm,t',declBody) visited
             collectRecBinds t' visited' declBody
           Just _ -> pure visited

        other -> throwError $ MonoError $ "Unexpected expression in collectFun:\n  " <> ppExp other
   collectFun _ _ [] = throwError $ MonoError "Ran out of types in collectFun"



{- | "Forcibly" assigns the provided type to the provided expression.
     Works recursively, so that all of the types of all sub-expressions are
     also forcibly assigned so as to conform with the provided type.

     E.g. `monomorphizeWithType (Int -> Int) (\(x :: a) -> (f :: a -> Int) x)`
     should yield `(\(x :: Int) -> (f :: Int -> Int) x)`

     This is used during inlining. At the point where we attempt to inline a
     polymorphic function, we should know which types the type variables
     ought to be instantiated to (if it can be known).
-}
monomorphizeWithType ::
  PurusType ->
  Exp WithObjects PurusType (Var (BVar PurusType) (FVar PurusType)) ->
  Monomorphizer (Exp WithObjects PurusType (Var (BVar PurusType) (FVar PurusType)))
monomorphizeWithType ty expr
  | expTy id expr == ty = pure expr
  | otherwise = trace ("monomorphizeWithType:\n  " <> ppExp expr <> "\n  " <> prettyTypeStr ty) $ case expr of
      LitE ty' (ArrayL arr) -> case ty' of
        ArrayT inner -> LitE ty . ArrayL <$> traverse (monomorphizeWithType inner)  arr
        _ -> throwError $ MonoError  ("Failed to collect recursive binds: " <> prettyTypeStr ty <> " is not a Record type")

      LitE _ (ObjectL ext fs) -> case ty of
        RecordT fields -> do
          let fieldMap = mkFieldMap fields
          LitE ty . ObjectL ext <$> monomorphizeFieldsWithTypes fieldMap  fs
        _ -> throwError $ MonoError ("Failed to collect recursive binds: " <> prettyTypeStr ty <> " is not a Record type")

      LitE _ lit -> pure $ LitE ty lit

      ObjectUpdateE ext _ orig copyFields updateFields -> case ty of
        RecordT fields -> do
          let fieldMap = mkFieldMap fields
          -- idk. do we need to do something to the original expression or is this always sufficient?
          updateFields' <- monomorphizeFieldsWithTypes fieldMap updateFields
          pure $ ObjectUpdateE ext ty orig copyFields updateFields'
        _ -> throwError $ MonoError ("Failed to collect recursive binds: " <> prettyTypeStr ty <> " is not a Record type")

      AccessorE ext _ str e -> pure $ AccessorE ext ty str e -- idk?

      fun@(LamE _ bv body) -> trace ("MTABs:\n  " <> ppExp fun <> " :: " <> prettyTypeStr ty) $ do
        case ty of
          (a :-> b) ->  do
              -- REVIEW: If something is weirdly broken w/ bound vars look here first
              freshBV <- freshBVar a
              let replaceBVar = mapBound $ \x -> if x == bv then freshBV else x
                  body' = fmap join . fromScope $ replaceBVar $ updateVarTyS bv a body
              body'' <-  fmap F . toScope <$> monomorphizeWithType b body'
              pure $ LamE ty freshBV body''
          _ -> throwError $ MonoError  "Abs isn't a function"

      app@(AppE _ e2) -> trace ("MTAPP:\n  " <> ppExp app) $  do
        let (f,args) = unsafeAnalyzeApp app
        e1' <-  handleFunction id f args
        pure $ AppE e1' e2

      V a -> case a of
        F (FVar _ b) -> pure . V . F $ FVar ty b -- idk
        other -> pure $ V other

      CaseE _ scrut alts -> do
        let f = monomorphizeWithType ty
            goAlt :: Alt WithObjects PurusType (Exp WithObjects PurusType) (Var (BVar PurusType) (FVar PurusType))
                   -> Monomorphizer (Alt WithObjects PurusType (Exp WithObjects PurusType) (Var (BVar PurusType) (FVar PurusType)))
            goAlt (UnguardedAlt bindings binders result) = do
               let unscoped = join <$> fromScope result
               UnguardedAlt bindings binders . fmap F . toScope <$>  f unscoped
        CaseE ty scrut <$> traverse goAlt alts

      LetE a binds e -> do
        let unscoped = join <$> fromScope e
        LetE a binds . fmap F . toScope <$> monomorphizeWithType ty unscoped
  where
    monomorphizeFieldsWithTypes :: M.Map PSString (RowListItem SourceAnn)
                                -> [(PSString, Exp WithObjects PurusType (Var (BVar PurusType) (FVar PurusType)))]
                                -> Monomorphizer [(PSString, Exp WithObjects PurusType (Var (BVar PurusType) (FVar PurusType)))]
    monomorphizeFieldsWithTypes _ [] = pure []
    monomorphizeFieldsWithTypes cxt ((lbl,e):rest) = do
      RowListItem{..} <- note  ("No type for field with label " <> T.unpack (prettyPrintString lbl) <> " when monomorphizing record")
                         $ M.lookup lbl cxt
      rest' <- monomorphizeFieldsWithTypes cxt rest
      e' <- monomorphizeWithType rowListType e
      pure $ (lbl,e') : rest'


-- unsafe/partial

instantiateWithArgs :: PurusType -> [PurusType] -> PurusType
instantiateWithArgs f args = replaceAllTypeVars instantiations f
  where
    instantiations = getAllInstantiations f args

getAllInstantiations :: PurusType
                    -> [PurusType]
                    -> [(Text,PurusType)]
getAllInstantiations (quantify -> ForAll _ _ var _ inner _) (arg:args) = case instantiates var arg polyArg of
   Nothing -> getAllInstantiations inner args
   Just t  -> (var,t) : getAllInstantiations inner args
  where
    polyArg = getFunArgTy inner
getAllInstantiations _ _ = []

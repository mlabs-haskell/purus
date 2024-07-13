{-# OPTIONS_GHC -Wno-orphans #-} -- has to be here (more or less)
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications  #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use if" #-}
{-# HLINT ignore "Use <&>" #-}
module Language.PureScript.CoreFn.Convert.Monomorphize (runMonomorphize, testMono, isConstructorE, isConstructor, instantiateAllConstructors) where

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
import Language.PureScript.Names (Ident(..), Qualified (..), QualifiedBy (..), ModuleName (..), showQualified, showIdent, pattern ByNullSourcePos)
import Language.PureScript.Types
    ( RowListItem(..), SourceType, Type(..), replaceTypeVars, isMonoType )
import Language.PureScript.CoreFn.Desugar.Utils ( showIdent' )
import Language.PureScript.Environment (pattern (:->), pattern RecordT, function, getFunArgTy)
import Language.PureScript.CoreFn.Pretty (prettyTypeStr)
import Language.PureScript.CoreFn.FromJSON ()
import Data.Text qualified as T
import Data.List (find)
import Data.Map (Map)
import Data.Map qualified as M
import Language.PureScript.PSString (PSString, prettyPrintString)
import Language.PureScript.AST.SourcePos (SourceAnn)
import Control.Lens
    ( (&) )
import Control.Monad (join, foldM)
import Control.Monad.RWS.Class (MonadReader(ask))
import Control.Monad.RWS (RWST(..))
import Control.Monad.Except (throwError)
import Language.PureScript.CoreFn.Convert.DesugarCore
    ( desugarCoreModule, WithObjects, IR_Decl )
import Bound.Var (Var(..))
import Bound.Scope (mapBound, fromScope, toScope, Scope(..), abstract)
import Language.PureScript.CoreFn.TypeLike
    ( TypeLike(..), quantify, getAllInstantiations, instantiateWithArgs )
import Language.PureScript.CoreFn.Convert.Monomorphize.Utils
    ( mkFieldMap,
      findDeclBody,
      decodeModuleIO,
      MonoError(..),
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
      Monomorphizer, foldMScopeViaExp, distributeExp, freshUnique, containsBVar, isSelfRecursiveNR )

import Data.Text (Text)
import GHC.IO (throwIO)
import Data.Char (isUpper)
import Control.Lens.Plated
import Prettyprinter (Pretty)
import Language.PureScript.CoreFn.Pretty.Common (prettyAsStr)
import Data.Bifunctor (first)
import Language.PureScript.CoreFn.Convert.Debug


monomorphizeWithTypeRec ::
                  PurusType
               -> Ident
               -> Int
               -> Scope (BVar PurusType) (Exp WithObjects PurusType) (Var (BVar PurusType) (FVar PurusType))
               -> Monomorphizer (Exp WithObjects PurusType (Var (BVar PurusType) (FVar PurusType)))
monomorphizeWithTypeRec ty idnt indx expr | containsBVar idnt indx expr = do
  body <- monomorphizeWithType ty (join <$> fromScope expr)
  let abstr = abstract (\case {B bv -> Just bv; _ -> Nothing})
      rescoped = abstr body
      result = LetE M.empty [NonRecursive idnt indx rescoped] (toScope (V . B $ BVar indx ty idnt))
  pure  result
monomorphizeWithTypeRec ty _ _ expr = monomorphizeWithType ty (join <$> fromScope expr)


{- Function for quickly testing/debugging monomorphization -}

testMono :: FilePath -> Text -> IO ()
testMono path decl = do
  myModCoreFn <- decodeModuleIO path
  (myMod,_) <- either (throwIO . userError) pure $ desugarCoreModule myModCoreFn
  Just myDecl <- pure $ findDeclBody decl myMod
  case runMonomorphize myMod [] (join <$> fromScope myDecl) of
    Left (MonoError msg ) -> throwIO $ userError $ "Couldn't monomorphize " <> T.unpack decl <> "\nReason:\n" <> msg
    Right body -> do
      putStrLn $ "MONO RESULT: \n" <>  ppExp body

{- This is the top-level entry point for monomorphization. Typically,
   you will search the module for a 'main' decl and use its
   body as the Exp argument.
-}
runMonomorphize ::
  Module IR_Decl PurusType PurusType Ann ->
  [Module IR_Decl PurusType PurusType Ann] -> -- | Modules in scope, for declarations
  Exp WithObjects PurusType (Var (BVar PurusType) (FVar PurusType)) ->
  Either MonoError (Exp WithObjects PurusType (Var (BVar PurusType) (FVar PurusType)))
runMonomorphize Module{..} _modulesInScope expr =
  runRWST (transformM monomorphize ( expr) >>= inlineEverything >>= (pure . instantiateAllConstructors)) (moduleName, moduleDecls) (MonoState 100000) & \case -- FIXME: That 0 needs to be a MaxBv or we need to pass the real value from the core desugarer state
    Left err -> Left err
    Right (a,_,_) -> do
      doTraceM "runMonomorphize" ("OUTPUT: \n" <> ppExp a <> "\n" <> replicate 20 '-')
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
monomorphize  xpr =  case xpr of
  AppE _f _arg ->  do
    arg <- monomorphize _arg
    let (f,args) = unsafeAnalyzeApp (AppE _f arg)
    if isBuiltinE f || isConstructorE f then do
           let result =  unsafeApply id f args
           doTraceM "monomorphize" ("INPUT:\n" <> prettyAsStr xpr <> "\n\nOUTPUT:\n" <> prettyAsStr result)
           pure result
    else do
      result <- handleFunction id f args
      doTraceM "monomorphize" ("INPUT:\n" <> prettyAsStr xpr <> "\n\nOUTPUT:\n" <> prettyAsStr result)
      pure result
  other -> doTrace "monomorphize" ("UNCHANGED:\n" <> ppExp other) $ pure other


-- FIXME: Rewrite this 
inlineEverything :: Exp WithObjects PurusType (Var (BVar PurusType) (FVar PurusType))
                 -> Monomorphizer (Exp WithObjects PurusType (Var (BVar PurusType) (FVar PurusType)))
inlineEverything xp = do
    res <-  go xp
    let msg = "INPUT:\n" <> prettyAsStr xp <> "\n\nOUTPUT:\n" <> prettyAsStr res
    doTraceM "inlineEverything" msg
    pure res 
  where
    go :: Exp WithObjects PurusType (Var (BVar PurusType) (FVar PurusType))
       -> Monomorphizer (Exp WithObjects PurusType (Var (BVar PurusType) (FVar PurusType)))
    go = \case
      (V (F (FVar ty qi))) | not (isBuiltin qi || isConstructor qi) ->  inlineAs ty qi
      AppE e1 e2  -> do
        e1' <- go e1
        e2' <- go e2
        pure $ AppE e1' e2' -- AppE <$> go e1 <*> go e2
      CaseE t scrut alts -> do
        scrut' <- go scrut
        alts' <- traverse goAlt alts
        pure $ CaseE t scrut' alts'
      LamE t bv body -> do
        LamE t bv <$> scopeHelper body
      LetE binds decls body -> do
        let  goDecl = \case
              NonRecursive ident bvix expr ->
                 NonRecursive ident bvix <$> scopeHelper expr
              Recursive xs ->
                Recursive <$> traverse (\(i,ex) -> (i,) <$> scopeHelper ex) xs
        LetE binds <$> traverse goDecl decls <*> scopeHelper body
      AccessorE x t pss e -> AccessorE x t pss <$> go e
      ObjectUpdateE x t e cf fs -> (\e' fs' -> ObjectUpdateE x t e' cf fs')
                                   <$> go  e
                                   <*> traverse (\(nm,expr) -> (nm,) <$> go expr) fs
      LitE t lit -> LitE t <$> traverse go lit
      TyInstE t e -> TyInstE t <$> go e
      other -> pure other 

    scopeHelper :: Scope (BVar PurusType) (Exp WithObjects PurusType) (Var (BVar PurusType) (FVar PurusType))
                -> Monomorphizer (Scope (BVar PurusType) (Exp WithObjects PurusType) (Var (BVar PurusType) (FVar PurusType)))
    scopeHelper scoped = do
      let unscoped = join <$> fromScope scoped
      transformed <- go unscoped
      let rescoped = abstract (\case {B bv@(BVar{}) -> Just bv; _ -> Nothing}) transformed
      pure rescoped

    goAlt (UnguardedAlt bs pat body ) = do
      UnguardedAlt bs pat <$> scopeHelper body


   -- Builtins shouldn't be inlined because they can't be.
   -- TODO/REVIEW: Figure out whether it's necessary to *monomorphize*
   --              polymorphic builtins. (Trivial to implement if needed)
isBuiltinE :: Exp x ty1 (Var b (FVar ty2)) -> Bool
isBuiltinE = \case
  V (F (FVar _ qi)) -> isBuiltin qi
  TyInstE _ e -> isBuiltinE e
  _ -> False

isBuiltin :: Qualified a -> Bool
isBuiltin (Qualified (ByModuleName (ModuleName "Builtin")) _ ) = True
isBuiltin _ = False

isConstructorE :: Exp x ty1 (Var b (FVar ty2)) -> Bool
isConstructorE = \case
  V (F (FVar _ qi)) -> isConstructor qi
  TyInstE _ e -> isConstructorE e
  _ -> False
   -- After the recent changes, constructors *can't* be inlined, i.e., they must remain
   -- free until the final PIR compilation stage
isConstructor :: Qualified Ident -> Bool
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
handleFunction  toVar e [] = doTrace "handleFunction" ("FIN: " <> ppExp e) $ pure (toVar <$> e)
{- If we have an explicit polymorphic lambda & argument types, we look at the argument types one-by-one
   and check whether those types allow us to deduce the type to which the TyVar bound by the Forall in
   the lambda's type ought to be instantiated. (See the `instantiates` function)
-}
handleFunction toVar expr@(LamE (ForAll _ _ var _ inner  _) bv@(BVar bvIx _ bvIdent) body'') (arg:args) = do
  traceM  ("abs:\n  " <> ppExp expr)
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
      body' = updateVarTyS  bv t . fmap toVar $ body'' -- update the type of the variable bound by the lambda in the body of the expression
      -- NOTE/REVIEW: Not 100% sure that this doesn't involve a problematic join
      bodyUnscoped  = join <$> fromScope body'
  body <- toScope  <$>  (\b ->  handleFunction id b $ fmap toVar <$> args) bodyUnscoped -- recurse on the body, with the rest of the arguments
  traceM ("bodyUnscoped:\n" <> prettyAsStr bodyUnscoped)
  let bodyT = expTy' F body
      funT  = doInstantiate $ function t bodyT
      firstArgT = headArg funT
      e' = LamE funT (BVar bvIx firstArgT bvIdent) body
  pure $  AppE  (F <$> e') (toVar <$> arg) -- Put the app back together. (Remember, the params to this function come from a deconstructed AppE)
 where
   traceM = doTraceM "handleFunction"
   trace  = doTrace "handleFunction"
{- If we have a free variable, we attempt to inline the variable (without altering the type)
   and then call `handleFunction` on the inlined expression with the same argument types

handleFunction  toVar v@(V (FVar ty  qn)) es = trace ("\nhandleFunction VarGo: " <> ppExp v) $ do
  e' <- inlineAs ty qn
  handleFunction toVar e' es
-}
handleFunction toVar v@(V (toVar -> F (FVar ty qn))) es
  | not (isBuiltin qn || isConstructor qn) =  do
    e' <- inlineAs ty qn
    doTraceM "handleFunction" ("VarGo:\n\nVAR:\n" <> ppExp v <> "\n\nINLINED:\n" <> prettyAsStr e')
    handleFunction id e' (fmap toVar <$> es)

handleFunction toVar v@(V (toVar -> B (BVar bvix bvty bvident))) es = do
  case bvty of
    -- N.B. This is really just special handling for polymorphic let-bound functions.
    ForAll{} -> do
      let toInstantiate = reverse . fmap snd $ getAllInstantiations bvty (expTy toVar <$> es)
          v' = toVar <$> foldr TyInstE v toInstantiate
      pure $ unsafeApply id v' (fmap toVar <$> es)
    _ -> pure $ unsafeApply id (toVar <$> v) (fmap toVar <$> es)

{- If the function parameter is monomorphic then we rebuild the initial AppE and return it.
-}
handleFunction toVar e es | isMonoType (expTy toVar e)  = doTraceM "handleFunction" ("isMono:\n" <> ppExp e) >>
                            pure $ unsafeApply id  (toVar <$> e) (fmap toVar <$> es)
-- Anything else is an error.
handleFunction toVar e es = throwError $ MonoError
                        $ "Error in handleFunction:\n\n"
                        <> "FUN EXPR:\n  " <> prettyAsStr e
                        <> "\n\nFUN TY:\n  " <> prettyAsStr (expTy toVar e)
                        <> "\n\n ARGS:\n  " <> show (ppExp <$> es)
                        <> "\n\nREASON: Not an abstraction or variable"

{- | Monomorphizing inliner. Looks up the provided
     identifier in the module context (TODO: linker that lets us support multiple modules)
     and attempts to forcibly assign the provided type to that expression, then
     inlines the monomorphized expression.

     Inlines and monomorphizes recursively, so that all free variables in the expression being
     inlined are monomorphized to the appropriate type and themselves inlined. (I.e. inlines
     everything and monomorphizes everything to the maximum extent possible)
-}

inlineable :: Qualified Ident -> Bool
inlineable nm = not (isConstructor nm || isBuiltin nm )

inlineAs ::
  PurusType ->
  Qualified Ident ->
  Monomorphizer (Exp WithObjects PurusType (Var (BVar PurusType) (FVar PurusType)))
inlineAs t ident = do
  res <- inlineAs' t ident
  let msg = "IDENT:" <> show ident  <>  "\n\nTY:\n" <> prettyAsStr t <>  "\n\nINPUT:\n" <> prettyAsStr res <> "\n\nOUTPUT:\n" <> prettyAsStr res
  doTraceM "inlineAs" msg
  pure res
  
inlineAs' ::
  PurusType ->
  Qualified Ident ->
  Monomorphizer (Exp WithObjects PurusType (Var (BVar PurusType) (FVar PurusType)))
-- TODO/REVIEW: Check whether this has any purpose here \/
inlineAs' ty inNm
  | not (inlineable inNm) = pure (V . F $ FVar ty inNm)
  | otherwise = case inNm of
   -- TODO: Linker so we can make this work no matter what the source module is
  (Qualified (ByModuleName mn') ident) -> ask >>= \(mn,modDict) ->
    if mn ==  mn' then do
      let msg = "Couldn't find a declaration with identifier " <> showIdent' ident <> " to inline as " <> prettyTypeStr ty
      note msg  (findInlineDeclGroup ident modDict) >>= \case
        NonRecursive nrid nrix e ->  monomorphizeWithTypeRec ty nrid nrix e
        Recursive xs -> do
          let msg' = "Target expression with identifier " <> showIdent' ident <> " not found in mutually recursive group"
          ((targIdent,targIx),targExpr) <- note msg' $ find (\x -> fst (fst x) == ident)  xs -- has to be there
          newIx <- freshUnique
          let newTargIdent = Ident $ showIdent targIdent <> T.pack (show newIx)
          let initialRecDict = M.singleton (targIdent,targIx) ((newTargIdent,newIx),ty,targExpr)
          dict <- collectRecBinds  ty initialRecDict targExpr
          let renameMap = (\(i,t,_) -> (i,t)) <$> dict
              bindingMap = (\(old,(a,b,c)) -> (old,a,b,join <$> fromScope c)) <$> M.toList dict
          binds <- traverse (\(oldIx,newId,newTy,oldE) -> makeBind renameMap newId newTy oldIx oldE) bindingMap
          case M.lookup (targIdent,targIx) renameMap of
            Just ((newId,newIx'),newTy) -> do
              let body = pure $ B (BVar newIx' newTy newId)
              pure $ gLet binds body
            Nothing -> throwError
                       $ MonoError
                       $ "Couldn't inline "
                         <> showIdent' ident
                         <> " - identifier didn't appear in collected bindings:\n  "
                         <> show renameMap
     -- TODO: This is a temporary hack to get builtins working w/o a real linker.
     else  throwError $ MonoError "Imports aren't supported yet!"
  wrong@(Qualified (BySourcePos _) _) -> throwError
    $ MonoError
    $ "Cannot inline variable qualified by SourcePos. Such a variable should be bound (and ergo not exist at this stage of compilation)"
      <> "\n Variable: "
      <> show wrong 
 where
   {- Arguments:
        1. A renaming dictionary,
        2. An Identifier which represents the new name of a binding (and should be present in the renaming dictionary),
        3. The new, possibly monomorphic (hopefully more-monomorphic at least) type which will be forcibly assigned to the body expression),
        4. A (possibly polymorphic) expression that will become the decl body

      Constructs a BindE where the Ident is the 2nd arg and the body is
      the 4th arg forcibly assigned the type of the 3rd arg.
   -}
   makeBind :: Map (Ident,Int) ((Ident,Int),SourceType) -- Map OldName (NewName,TypeToAssign)
            -> (Ident,Int)                        -- NewName
            -> SourceType                   -- TypeToAssign
            -> (Ident,Int)                  -- Old Name & Index
            -> Exp WithObjects PurusType (Var (BVar PurusType) (FVar PurusType)) -- Declaration body
            -> Monomorphizer (BindE PurusType (Exp WithObjects PurusType) (Var (BVar PurusType) (FVar PurusType)))
   makeBind renameDict (newIdent,newIx) t (oldIdent,oldIx) e = doTrace "makeBind" (showIdent'  newIdent) $ do
     let renameDictForUpdate = M.mapKeys fst . M.map (first fst) $ renameDict
         renamed = abstract (\case {B bv -> Just bv; _ -> Nothing}) $ updateFreeVars renameDictForUpdate e
     e' <- monomorphizeWithTypeRec t newIdent newIx renamed 
     let abstr = abstract $ \case
                   fv@(F (FVar a (Qualified ByNullSourcePos fvId))) | fvId == oldIdent -> case M.lookup (fvId,oldIx) renameDict of
                     Nothing ->  Nothing
                     Just ((newIdent',newIx'),newTy) -> Just (BVar newIx' newTy newIdent')
                   B (bv@BVar{}) -> Just bv
                   _ -> Nothing 

     pure $ NonRecursive newIdent newIx (abstr e')

   -- Find a declaration body in the *module* scope
   findDeclarationBody :: Ident -> Monomorphizer (Maybe (Int,Scope (BVar PurusType) (Exp WithObjects PurusType) (Var (BVar PurusType) (FVar PurusType))))
   findDeclarationBody nm = go <$> getModBinds
    where
      go :: [BindE PurusType (Exp WithObjects PurusType) (Var (BVar PurusType) (FVar PurusType))]
         -> Maybe (Int,Scope (BVar PurusType) (Exp WithObjects PurusType) (Var (BVar PurusType) (FVar PurusType)))
      go [] = Nothing
      go (b:bs) = case b of
        NonRecursive nm' bvix e -> if nm' == nm then Just (bvix,e) else go bs
        Recursive xs -> case find (\x -> fst (fst x) == nm) xs of
          Nothing -> go bs
          Just ((_,bvix),e) -> Just (bvix,e)

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
     Map (Ident,Int) ((Ident,Int), SourceType, Scope (BVar PurusType) (Exp WithObjects PurusType) (Var (BVar PurusType) (FVar PurusType))) ->
     Scope (BVar PurusType) (Exp WithObjects PurusType) (Var (BVar PurusType) (FVar PurusType)) ->
     Monomorphizer (Map (Ident,Int) ((Ident,Int), SourceType, Scope (BVar PurusType) (Exp WithObjects PurusType) (Var (BVar PurusType) (FVar PurusType))))
   collectRecBinds t visited  e = doTrace "collectRecBinds" (prettyAsStr  (fromScope e) <> "\n  " <> prettyTypeStr t) $ case  join <$> fromScope e of
     LitE _ (ObjectL _ fs) -> doTrace "collectRecBinds" "crbOBJLIT" $ case t of
         RecordT fields -> do
           let fieldMap = mkFieldMap fields
           innerBinds <- collectRecFieldBinds visited fieldMap fs
           pure $ visited <> innerBinds
         _ -> throwError $ MonoError ("Failed to collect recursive binds: " <> prettyTypeStr t <> " is not a Record type")
     LitE _ _  -> doTrace "collectRecBinds" "crbLIT" $ pure visited
     ObjectUpdateE _ _ _ _ updateFields -> doTrace "collectRecBinds"  "crbOBJUPDATE" $ case t of
        RecordT fields -> do
          let fieldMap = mkFieldMap fields
          innerBinds <- collectRecFieldBinds visited fieldMap  updateFields
          pure $ visited <> innerBinds
        _ -> throwError $ MonoError  ("Failed to collect recursive binds: " <> prettyTypeStr t <> " is not a Record type")
     AccessorE _ _ _ _ -> doTrace "collectRecBinds" "crbACCSR" $ pure visited -- idk. given (x.a :: t) we can't say what x is.
     absE@(LamE _ _ _) ->  doTrace "collectRecBinds" ("crbABS TOARGS: " <> prettyTypeStr t) $  do
       let scoped = F <$> toScope absE
       collectFun visited scoped  (splitFunTyParts t)
     app@(AppE _ e2) -> doTrace "collectRecBinds" "crbAPP" $ do
       let (f,args) = unsafeAnalyzeApp app
           types = (expTy id <$> args) <> [t]
           f' = F <$> toScope f
           e2' = F <$> toScope e2
       funBinds' <- collectFun visited f' types
       let funBinds = visited <> funBinds'
       argBinds <- collectRecBinds (head types) funBinds  e2'
       pure $ funBinds <> argBinds
     V (F (FVar _ (Qualified (ByModuleName _) nm))) -> doTrace "collectRecBinds" ("crbVAR: " <> showIdent' nm)  $ case M.lookup nm (M.mapKeys fst visited) of
       Nothing -> findDeclarationBody nm >>= \case
         Nothing -> throwError $ MonoError   $ "No declaration correponding to name " <> showIdent' nm <> " found in the module"
         Just (oldIx,ex) -> do
           u <- freshUnique
           let freshNm = Ident $ showIdent nm <> T.pack (show u)
           let this = ((freshNm,u),t,ex)
           pure $ M.insert (nm,oldIx) this visited
       Just _ -> pure visited  -- might not be right, might need to check that the types are equal? ugh keeping track of scope is a nightmare
     V (F (FVar _ (Qualified _ nm))) -> doTrace "collectRecBinds" ("crbVAR_: " <> showIdent' nm) $ pure visited
     CaseE _ _  alts -> doTrace "collectRecBinds" "crbCASE" $ do
       let flatAlts = concatMap extractAndFlattenAlts alts
       foldM (collectRecBinds t) visited flatAlts
     LetE _ _ ex -> collectRecBinds t visited ex
     TyInstE _ inner -> collectRecBinds t visited (toScope . fmap F $ inner)
     _ -> error "collectRecBinds"


   -- Collect from the fields of an object
   collectRecFieldBinds :: Map (Ident,Int) ((Ident,Int), SourceType, Scope (BVar PurusType) (Exp WithObjects PurusType) (Var (BVar PurusType) (FVar PurusType)))
                        -> M.Map PSString (RowListItem SourceAnn)
                        -> [(PSString, Exp WithObjects PurusType (Var (BVar PurusType) (FVar PurusType)))]
                        -> Monomorphizer (Map (Ident,Int) ((Ident,Int), SourceType, Scope (BVar PurusType) (Exp WithObjects PurusType) (Var (BVar PurusType) (FVar PurusType))) )
   collectRecFieldBinds visited _ [] =  pure visited
   collectRecFieldBinds visited cxt ((lbl,e):rest) = doTrace "collectRecFieldBinds" (prettyAsStr lbl <> "\n" <> prettyAsStr e) $ do
     RowListItem{..} <- note ("No type for field with label " <> T.unpack (prettyPrintString lbl) <> " when collecting record binds")
                          $ M.lookup lbl cxt
     this <- collectRecBinds rowListType visited (toScope . fmap F $ e)
     collectRecFieldBinds (visited <> this) cxt rest

   -- Collect from a function expression
   collectFun :: Map (Ident,Int) ((Ident,Int), SourceType, Scope (BVar PurusType) (Exp WithObjects PurusType) (Var (BVar PurusType) (FVar PurusType)))
              -> Scope (BVar PurusType) (Exp WithObjects PurusType) (Var (BVar PurusType) (FVar PurusType))
              -> [SourceType]
              -> Monomorphizer (Map (Ident,Int) ((Ident,Int), SourceType, Scope (BVar PurusType) (Exp WithObjects PurusType) (Var (BVar PurusType) (FVar PurusType))))
   collectFun visited e [t] = doTrace "collectFun" ("FIN:\n  " <> {- -ppExp e <> " :: " -}  prettyTypeStr t)  $ do
     rest <- collectRecBinds t visited e
     pure $ visited <> rest
   collectFun visited  e (t:ts) =
     doTrace "collectFun" (prettyAsStr (fromScope e) <> "\n  " <> prettyTypeStr t <> "\n" <> show ts)  $
      case join <$> fromScope e of
        LamE (ForAll{}) bv  body'' -> do
          let body' =  updateVarTyS bv t  body''
          collectFun visited body' ts

        LamE _ _ body'' ->  collectFun visited body'' ts

        (V (F (FVar _ (Qualified (ByModuleName _) nm)))) -> doTrace "collectFun" ("VAR: " <> showIdent' nm) $ do
         let msg = "Couldn't find a declaration with identifier " <> showIdent' nm <> " to inline as " <> prettyTypeStr t
         (bvix,declBody) <- note msg =<< findDeclarationBody nm
         case M.lookup (nm,bvix) visited of
           Nothing -> do
             let t' = foldr1 function (t:ts)
             freshNm <- freshen nm
             u <- freshUnique
             let visited' = M.insert (nm,bvix) ((freshNm,u),t',declBody) visited
             collectRecBinds t' visited' declBody
           Just _ -> pure visited

        other -> throwError $ MonoError $ "\n\nUnexpected expression in collectFun:\n\n" <> ppExp other <> "\n\n" <> show other
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
  | otherwise = doTrace "monomorphizeWithType" ("INPUT:\n" <> ppExp expr <> "\n\nINPUT TY:\n" <> prettyTypeStr ty) $ case expr of
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

      fun@(LamE _ bv body) -> doTrace "monomorphizeWithType" ("ABS:\n  " <> ppExp fun <> "\n\nTARGET TY:\n" <> prettyTypeStr ty <> "\n\nABS TY:\n" <> prettyAsStr (expTy id fun)) $ do
        case ty of
          (a :-> b) ->  do
              -- REVIEW: If something is weirdly broken w/ bound vars look here first
              freshBV <- freshBVar a
              let replaceBVar = mapBound $ \x -> if x == bv then freshBV else x
                  body' = fmap join . fromScope $ replaceBVar $ updateVarTyS bv a body
              body'' <-  fmap F . toScope <$> monomorphizeWithType b body'
              pure $ LamE ty freshBV body''
          other -> throwError $ MonoError  $ "Expected Function Type for Expression:\n\n" <> prettyAsStr fun <> "\n\nbut got type:\n\n" <> prettyAsStr other 

      app@(AppE _ e2) -> doTrace "monomorphizeWithType" ("APP:\n  " <> ppExp app) $  do
        let (f,args) = unsafeAnalyzeApp app
        handleFunction id f args
        

      V a -> case a of
        F (FVar _ b) -> pure . V . F $ FVar ty b -- idk
        other -> pure $ V other

      CaseE _ scrut alts -> do
        let f = monomorphizeWithType ty
            goAlt :: Alt WithObjects PurusType (Exp WithObjects PurusType) (Var (BVar PurusType) (FVar PurusType))
                   -> Monomorphizer (Alt WithObjects PurusType (Exp WithObjects PurusType) (Var (BVar PurusType) (FVar PurusType)))
            goAlt (UnguardedAlt bindings binders result) = do
               let unscoped = join <$> fromScope result
                   abstr = abstract (\case {B bv -> Just bv; _ -> Nothing})
               unscoped' <- f unscoped
               pure $  UnguardedAlt bindings binders (abstr unscoped')

        CaseE ty scrut <$> traverse goAlt alts

      LetE a binds e -> do
        let unscoped = join <$> fromScope e
        LetE a binds . fmap F . toScope <$> monomorphizeWithType ty unscoped

      -- type instantiations shouldn't be touched here
      other -> pure other
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


instantiateAllConstructors :: forall x t
                            . (TypeLike t, Pretty t, Pretty (KindOf t))
                           => Exp x t (Var (BVar t) (FVar t))
                           -> Exp x t (Var (BVar t) (FVar t))
instantiateAllConstructors = transform $ \case
  AppE e1 e2 -> case unsafeAnalyzeApp (AppE e1 e2) of
    (f,args) | isConstructorE f || isBuiltinE f ->
               let f' = instantiateConstructorWithArgs id f args
               in unsafeApply id f' args
    _ -> AppE e1 e2
  other -> other



-- should also work with builtins 
instantiateConstructorWithArgs :: forall x t a
                                . (TypeLike t, Pretty t, Pretty (KindOf t))
                               => (a -> Var (BVar t) (FVar t))
                               -> Exp x t a -- a non-monomorphized constructor
                               -> [Exp x t a] -- the arguments to which it is applied
                               -> Exp x t a
instantiateConstructorWithArgs _ fun [] = fun
instantiateConstructorWithArgs f fun args = doTrace "instantiateConstructorWithArgs" msg result 
  where
    quantifiedTyVars = fmap (\(a,b,c) -> (b,c))
                       . fst
                       . stripQuantifiers
                       $ expTy f fun

    msg = "INPUT FUN:\n" <> prettyAsStr (f <$> fun)
          <> "\n\nINPUT FUN TY:\n" <> prettyAsStr (expTy f fun)
          <> "\n\nINPUT ARGS:\n" <> prettyAsStr (fmap f <$> args)
          <> "\n\nINPUT ARG TYPES:\n" <> prettyAsStr (expTy f <$> args)
          <> "\n\nRESULT FUN:\n" <> prettyAsStr (f <$> result)
          <> "\n\nRESULT FUN TY:\n" <> prettyAsStr (expTy f result)
          <> "\n\nQUANTIFIED TYVARS:\n" <> prettyAsStr quantifiedTyVars
    result = runInst fun (reverse quantifiedTyVars)
    instantiationDict = M.fromList $ getAllInstantiations (expTy f fun) (expTy f <$> args)
    runInst e [] = e
    runInst e ((t,_):usedRest) = case M.lookup t instantiationDict of
      Just new -> runInst (TyInstE new e) usedRest
      Nothing  -> runInst e usedRest -- maybe this should be an error?

{-# OPTIONS_GHC -Wno-orphans #-} -- has to be here (more or less)
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiWayIf #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use if" #-}
module Language.PureScript.CoreFn.Convert.Monomorphize (
  MonoError (..),
  decodeModuleIO,
  findDeclBody,
  mkFieldMap,
  monomorphizeExpr,
  ) where

import Prelude
import Data.Bifunctor
import Data.Maybe

import Language.PureScript.CoreFn.Ann (Ann)
import Language.PureScript.CoreFn.Expr (PurusType)
import Language.PureScript.CoreFn.Module ( Module(..) )
import Language.PureScript.CoreFn.Convert.IR (Exp(..), FVar(..), Alt(..), Lit(..), BindE(..), ppExp, unsafeAnalyzeApp)
import Language.PureScript.Names (Ident(..), Qualified (..), QualifiedBy (..), pattern ByNullSourcePos, ModuleName (..))
import Language.PureScript.Types
    ( rowToList, RowListItem(..), SourceType, Type(..), replaceTypeVars, isMonoType )
import Language.PureScript.CoreFn.Pretty.Common ( analyzeApp )
import Language.PureScript.CoreFn.Desugar.Utils ( showIdent' )
import Language.PureScript.Environment (pattern (:->), pattern ArrayT, pattern RecordT, function, getFunArgTy)
import Language.PureScript.CoreFn.Pretty (prettyTypeStr)
import Language.PureScript.CoreFn.FromJSON ()
import Data.Aeson qualified as Aeson
import Data.Text qualified as T
import Data.List (find, foldl')
import Data.Map (Map)
import Data.Map qualified as M
import Language.PureScript.Label (Label(runLabel))
import Language.PureScript.PSString (PSString, prettyPrintString)
import Language.PureScript.AST.SourcePos (SourceAnn, pattern NullSourceSpan)
import Data.Bitraversable (Bitraversable(bitraverse))
import Control.Lens.IndexedPlated ( itransform, itransformM )
import Control.Lens
    ( Identity(runIdentity),
      (<&>),
      (&),
      (^?),
      preview,
      (^.),
      (.~),
      Ixed(ix), view )
import Control.Monad.RWS.Class (MonadReader(ask), gets, modify')
import Control.Monad.RWS (RWST(..))
import Control.Monad.Except (throwError)
import Language.PureScript.CoreFn.Utils (Context, exprType, instantiates)
import Control.Exception
import Data.Text (Text)
import Debug.Trace (trace, traceM)
import Language.PureScript.CoreFn.Convert.DesugarCore (WithObjects, desugarCore)
import Bound (fromScope)
import Bound.Var (Var(..))

{- Instead of mutual recursion, return ADTs that describe the "next step" of the computation

-}

{-
trace :: String  -> p2 -> p2
trace _ x = x

traceM :: forall m. Monad m => String -> m ()
traceM _ = pure ()
-}
-- hopefully a better API than the existing traversal machinery (which is kinda weak!)
-- Adapted from https://twanvl.nl/blog/haskell/traversing-syntax-trees

-- TODO: better error messages
data MonoError
 = MonoError Context String deriving (Show)

note :: Context  -> String -> Maybe b -> Monomorphizer b
note d err = \case
  Nothing -> throwError $ MonoError d err
  Just x -> pure x

-- ok we need monads
data MonoState = MonoState {
  {- Original Identifier -> Type -> (Fresh Ident, Expr)
  -}
  visited :: Map Ident (Map SourceType (Ident, Exp WithObjects PurusType (FVar PurusType))),
  unique :: Int
}
-- TODO: Logging, make a more useful state than S.Set Ident
type Monomorphizer a = RWST (ModuleName, [BindE PurusType (Exp WithObjects PurusType) (FVar PurusType)]) () MonoState (Either MonoError)  a
type Monomorphizer' a = RWST (ModuleName, [BindE PurusType (Exp WithObjects PurusType) (FVar PurusType)]) () MonoState Identity (Maybe a)

type IR_Decl = BindE PurusType (Exp WithObjects PurusType) (FVar PurusType)

defInstantiate scoped = instantiateEither  (either (V . B) (V . F))  scoped

hoist1 ::
  MonoState ->
  Monomorphizer a ->
  RWST ( ModuleName
       , [BindE PurusType (Exp WithObjects PurusType) (FVar PurusType)])
       ()
       MonoState
       Identity
       (Maybe a)
hoist1 st act = RWST $ \r s -> f (runRWST act r s)
  where
    f :: Either MonoError (a, MonoState, ()) -> Identity (Maybe a, MonoState, ())
    f = \case
      Left (MonoError d msg) -> do
        traceM $ "MonoError:  " <> msg <> ":\n" <> "Context: " <> show d
        pure (Nothing,st,())
      Right (x,st',_) -> pure (Just x, st', ())

monomorphizeExpr ::
  Module IR_Decl Ann ->
  Exp WithObjects PurusType (FVar PurusType) ->
  Either MonoError (Exp WithObjects PurusType (FVar PurusType))
monomorphizeExpr m@Module{..} expr =
  runRWST (monomorphizeA M.empty expr) (moduleName,moduleDecls) (MonoState M.empty 0) & \case
    Left err -> Left err
    Right (a,_,_) -> Right a

monomorphizeMain ::
  Module IR_Decl Ann ->
  Maybe (Exp WithObjects PurusType (FVar PurusType))
monomorphizeMain Module{..} =  runMono g
  where
    emptySt = MonoState M.empty 0

    g =  monomorphizeB M.empty mainE

    monomorphizeB ::
      Context ->
      Exp WithObjects PurusType (FVar PurusType) ->
      Monomorphizer' (Exp WithObjects PurusType (FVar PurusType))
    monomorphizeB d e = hoist1 emptySt (monomorphizeA d e)

    (mainE,otherDecls) = partitionDecls moduleDecls

    runMono :: RWST (ModuleName,[BindE PurusType (Exp WithObjects PurusType) (FVar PurusType)]) () MonoState Identity a  ->  a
    runMono act  = case runIdentity (runRWST act (moduleName,otherDecls) (MonoState M.empty 0)) of
                     (a,_,_) -> a

monomorphizeMain' ::
  Module IR_Decl Ann ->
  Either MonoError (Exp WithObjects PurusType (FVar PurusType))
monomorphizeMain' Module{..} =  g
  where
    g = runMono $ itransformM monomorphizeA M.empty mainE

    (mainE,otherDecls) = partitionDecls moduleDecls

    runMono :: RWST (ModuleName,[BindE PurusType (Exp WithObjects PurusType) (FVar PurusType)]) () MonoState (Either MonoError) a  ->  Either MonoError a
    runMono act  = case runRWST act (moduleName,otherDecls) (MonoState M.empty 0) of
                     Left err -> Left err
                     Right (a,_,_) -> Right a

decodeModuleIO :: FilePath -> IO (Module IR_Decl Ann)
decodeModuleIO path = Aeson.eitherDecodeFileStrict' path >>= \case
  Left err -> throwIO $ userError err
  Right modx -> pure modx

getModName :: Monomorphizer ModuleName
getModName = ask <&> fst

getModBinds :: Monomorphizer [BindE PurusType (Exp WithObjects PurusType) (FVar PurusType)]
getModBinds = ask <&> snd

freshen :: Ident -> Monomorphizer Ident
freshen ident = do
  u <- gets unique
  modify' $ \(MonoState v _) -> MonoState v (u + 1)
  let uTxt = T.pack (show u)
  case ident of
    Ident t -> pure $ Ident $ t <> "_$$" <> uTxt
    GenIdent (Just t) i -> pure $ GenIdent (Just $ t <> "_$$" <> uTxt) i -- we only care about a unique ord property for the maps
    GenIdent Nothing i  -> pure $ GenIdent (Just $ "var_$$" <> uTxt) i
    -- other two shouldn't exist at this stage
    other -> pure other

checkVisited :: Ident -> SourceType -> Monomorphizer (Maybe (Ident,Exp WithObjects PurusType (FVar PurusType)))
checkVisited ident st = gets (preview (ix ident . ix st) . visited)

markVisited :: Ident  -> SourceType -> Exp WithObjects PurusType (FVar PurusType) -> Monomorphizer Ident
markVisited ident st e = do
  v <- gets visited
  newIdent <- freshen ident
  let v' = v & ix ident . ix st .~ (newIdent,e)
  modify' $ \(MonoState _ u) -> MonoState v' u
  pure newIdent

-- returns (main,rest)
-- NOTE: Assumes main isn't part of a recursive binding group (it really shouldn't be?)
partitionDecls ::
  [BindE PurusType (Exp WithObjects PurusType) (FVar PurusType)] ->
  ( Exp WithObjects PurusType (FVar PurusType)
  , [BindE PurusType (Exp WithObjects PurusType) (FVar PurusType)])
partitionDecls bs = first fromJust $ foldr go (Nothing,[]) bs
  where
    go :: BindE PurusType (Exp WithObjects PurusType) (FVar PurusType) -> (Maybe (Exp WithObjects PurusType (FVar PurusType)), [BindE PurusType (Exp WithObjects PurusType) (FVar PurusType)]) -> (Maybe (Exp WithObjects PurusType (FVar PurusType)), [BindE PurusType (Exp WithObjects PurusType) (FVar PurusType)])
    go b acc = case b of
      nonrec@(NonRecursive ident expr) -> case ident of
        Ident "main" -> first (const $ Just expr) acc
        _ -> second (nonrec:) acc
      other -> second (other:) acc

stripQuantifiers :: SourceType -> SourceType
stripQuantifiers = \case
  ForAll _ _ _  _ inner _ -> stripQuantifiers inner
  other -> other

getResult :: SourceType -> SourceType
getResult (_ :-> b) = getResult b
getResult other = other

findDeclBody :: Text -> Module IR_Decl  Ann -> Maybe (Exp WithObjects PurusType (FVar PurusType))
findDeclBody nm Module{..} = case findInlineDeclGroup (Ident nm) moduleDecls of
  Nothing -> Nothing
  Just decl -> case decl of
    NonRecursive _ e -> Just e
    Recursive xs -> snd <$> find (\x -> snd (fst x) == Ident nm) xs

findInlineDeclGroup ::
  Ident ->
  [BindE PurusType (Exp x ty) a] ->
  Maybe (BindE PurusType (Exp x ty) a)
findInlineDeclGroup _ [] = Nothing
findInlineDeclGroup ident (NonRecursive ident' expr:rest)
  | ident == ident' = Just $ NonRecursive ident' expr
  | otherwise = findInlineDeclGroup ident rest
findInlineDeclGroup ident (Recursive xs:rest) = case  find (\x -> snd (fst x) == ident) xs of
  Nothing -> findInlineDeclGroup ident rest
         -- idk if we need to specialize the whole group?
  Just _ -> Just (Recursive xs)

monomorphizeA ::
  Context ->
  Exp WithObjects PurusType (FVar PurusType) ->
  Monomorphizer (Exp WithObjects PurusType (FVar PurusType))
monomorphizeA d xpr = trace ("monomorphizeA " <>  "\n  " <> ppExp xpr)  $ case xpr of
  app@(AppE _ arg) ->  do
    let (f,args) = unsafeAnalyzeApp app
    traceM $ "FUN: " <> ppExp f
    traceM $ "ARGS: " <> show (ppExp <$> args)
    let types = concatMap (toArgs . exprType)  args
    traceM $ "ARG TYPES:" <> show (prettyTypeStr <$> types)
     -- maybe trace or check that the types match?
     -- need to re-quantify? not sure. CHECK!

    if isBuiltin f
      then pure app
      else either (uncurry gLet) id <$> handleFunction d f args
  other -> pure other
 where
   isMonomorphizedVar :: Exp WithObjects PurusType (FVar PurusType)  -> Bool
   isMonomorphizedVar (V (FVar sty  _)) = stripQuantifiers sty == sty

isBuiltin :: forall x. Exp x PurusType (FVar PurusType) -> Bool
isBuiltin (V (FVar vTy (Qualified (ByModuleName (ModuleName "Builtin")) _))) = True
isBuiltin _ = False

gLet ::
  [BindE PurusType (Exp WithObjects PurusType) (FVar PurusType)] ->
  Exp WithObjects PurusType (FVar PurusType) ->
  Exp WithObjects PurusType (FVar PurusType)
gLet binds e = LetE binds e

nameShadows :: Context -> Ident -> Bool
nameShadows cxt iden = isJust $ M.lookup iden cxt

unsafeApply ::
  Exp WithObjects PurusType (FVar PurusType) ->
  [Exp WithObjects PurusType (FVar PurusType)] ->
  Exp WithObjects PurusType (FVar PurusType)
unsafeApply e (arg:args)= case exprType e of
  (a :-> b) -> unsafeApply (AppE e arg) args
  other -> Prelude.error $ "boom: " <> prettyTypeStr other
unsafeApply e [] = e


handleFunction :: Context
               -> Exp WithObjects PurusType (FVar PurusType)
               -> [Exp WithObjects PurusType (FVar PurusType)] -- TODO: List could be empty?
               -> Monomorphizer (Either ([BindE PurusType (Exp WithObjects PurusType) (FVar PurusType)], Exp WithObjects PurusType (FVar PurusType)) (Exp WithObjects PurusType (FVar PurusType)))
-- handleFunction d exp args | isBuiltin exp = trace ("handleFunction: Builtin") $ pure . Right $ unsafeApply exp args
handleFunction  _ e [] = trace ("handleFunction FIN: " <> ppExp e) $ pure (pure e)
handleFunction  d expr@(LamE (ForAll _ _ var _ inner  _) ident body'') (arg:args) = do
  traceM  ("handleFunction abs:\n  " <> ppExp expr <> "\n  " <> show (ppExp <$> (arg:args)))
  let t = exprType arg
  traceM $ prettyTypeStr t
  let polyArgT = getFunArgTy inner
      -- WRONG! Probably need to check all of the args at once
      doInstantiate = case instantiates var t polyArgT of
                        Just tx -> replaceTypeVars var tx
                        Nothing -> id
      body' = updateVarTy d ident t body''
      cxt   = M.insert ident t d
  handleFunction cxt body' args  >>= \case
    Left (binds,body) -> do
      let bodyT = exprType body
          funT  = doInstantiate $ function t bodyT
          e' = Abs ann funT  ident body
      pure $ Left (binds, App nullAnn e' arg)
    Right body -> do
      let bodyT = exprType body
          funT  = doInstantiate $ function t bodyT
          e' = Abs ann funT ident body
      pure $ Right $ App nullAnn e' arg -- Abs ann (function t bodyT) ident body)
handleFunction  d v@(V (FVar ty  qn)) es = trace ("handleFunction VarGo: " <> ppExp v) $ do
  traceM (ppExp v)
  traceM (show $ ppExp <$> es)
  e' <- either (uncurry gLet) id <$> inlineAs d ty qn
  handleFunction d e' es
handleFunction  d e es | isMonoType (exprType e)  = pure . Right $ unsafeApply e es
handleFunction d e es = throwError $ MonoError d
                        $ "Error in handleFunction:\n  "
                        <> ppExp e
                        <> "\n  " <> show (ppExp <$> es)
                        <> "\n  is not an abstraction or variable"

-- I *think* all CTors should be translated to functions at this point?
-- TODO: We can make sure the variables are well-scoped too
updateVarTy :: Context -> Ident -> PurusType -> Exp WithObjects PurusType (FVar PurusType) -> Exp WithObjects PurusType (FVar PurusType)
updateVarTy d ident ty = itransform goVar d
  where
    goVar :: Context -> Exp WithObjects PurusType (FVar PurusType) -> Exp WithObjects PurusType (FVar PurusType)
    goVar _d expr = undefined {- -case expr ^? _Var of
      Just (ann,_,Qualified q@(BySourcePos _) varId) | varId == ident -> Var ann ty (Qualified q ident)
      _ -> expr
-}

updateFreeVar :: M.Map Ident (Ident,SourceType) -> Context -> Exp WithObjects PurusType (FVar PurusType) -> Exp WithObjects PurusType (FVar PurusType)
updateFreeVar dict _ expr = undefined  {-

  case expr ^? _Var of
     Just (_,_,Qualified (ByModuleName _) varId) -> case M.lookup varId dict of
       Nothing -> expr
       Just (newId,newType) -> Var nullAnn newType (Qualified ByNullSourcePos newId)
     _ -> expr
-}
updateFreeVars :: Map Ident (Ident, SourceType) -> Context -> Exp WithObjects PurusType (FVar PurusType) -> Exp WithObjects PurusType (FVar PurusType)
updateFreeVars dict = itransform (updateFreeVar dict)

-- doesn't change types!
renameBoundVar :: Ident -> Ident -> Context -> Exp WithObjects PurusType (FVar PurusType) -> Exp WithObjects PurusType (FVar PurusType)
renameBoundVar old new _ e = undefined  {-
 case e ^? _Var of
  Just (ann,ty,Qualified (BySourcePos sp) varId) | varId == old -> Var ann ty (Qualified (BySourcePos sp) new)
  _ -> e
-}
renameBoundVars :: Ident -> Ident -> Context -> Exp WithObjects PurusType (FVar PurusType) -> Exp WithObjects PurusType (FVar PurusType)
renameBoundVars old new  = itransform (renameBoundVar old new)
                                                                    -- \/ Replace with ([BindE PurusType (Exp WithObjects PurusType) (FVar PurusType)], Exp WithObjects PurusType (FVar PurusType))
inlineAs ::
  Context ->
  PurusType ->
  Qualified Ident ->
  Monomorphizer (Either
                  ( [BindE PurusType (Exp WithObjects PurusType) (FVar PurusType)]
                  , Exp WithObjects PurusType (FVar PurusType))
                  (Exp WithObjects PurusType (FVar PurusType)))
-- TODO: Review whether this has any purpose here \/
inlineAs _ ty nm@(Qualified (ByModuleName (ModuleName "Builtin")) idnt) = trace ("inlineAs BUILTIN:\n  " <> "IDENT: " <> showIdent' idnt <> "\n  TYPE: " <> prettyTypeStr ty)
  $ pure . Right $ undefined -- Var nullAnn ty nm
-- TODO: Probably can inline locally bound variables? FIX: Keep track of local name bindings
inlineAs d _ (Qualified (BySourcePos _) ident) = throwError $ MonoError d  $ "can't inline bound variable " <> showIdent' ident
inlineAs  d ty qmn@(Qualified (ByModuleName mn') ident) = trace ("inlineAs: " <> showIdent' ident <> " :: " <>  prettyTypeStr ty) $ ask >>= \(mn,modDict) ->
  if | mn == mn' -> do
      let msg = "Couldn't find a declaration with identifier " <> showIdent' ident <> " to inline as " <> prettyTypeStr ty
      note d msg  (findInlineDeclGroup ident modDict) >>= \case
        NonRecursive _ e -> do
          e' <- monomorphizeWithType ty d e
          pure . Right $ e'
        Recursive xs -> do
          traceM $ "RECURSIVE GROUP:\n" <> concatMap (\((_,xId),t) -> showIdent' xId <> " :: " <> ppExp t <> "\n") xs
          let msg' = "Target expression with identifier " <> showIdent' ident <> " not found in mutually recursive group"
          (targIdent,targExpr) <- note d msg' $ find (\x -> fst x == ident) (first snd <$> xs) -- has to be there
          fresh <- freshen targIdent
          let initialRecDict = M.singleton targIdent (fresh,ty,targExpr)
          dict <- collectRecBinds initialRecDict ty d targExpr
          let renameMap = (\(i,t,_) -> (i,t)) <$> dict
              bindingMap = M.elems dict
              cxt = foldl' (\acc (idx,tyx)-> M.insert idx tyx acc) d $ (\(a,b,_) -> (a,b)) <$> M.elems dict
          binds <- traverse (\(newId,newTy,oldE) -> makeBind renameMap cxt newId newTy oldE) bindingMap
          case M.lookup targIdent renameMap of
            Just (newId,newTy) -> pure $ Left (binds, undefined ) {- -Var nullAnn newTy (Qualified ByNullSourcePos newId)) -}
            Nothing -> throwError
                       $ MonoError d
                       $ "Couldn't inline " <> showIdent' ident <> " - identifier didn't appear in collected bindings:\n  "  <> show renameMap
     -- TODO: This is a temporary hack to get builtins working w/o a real linker.
     | otherwise -> throwError $ MonoError d "Imports aren't supported!"
 where
   makeBind :: Map Ident (Ident,SourceType) -> Context -> Ident -> SourceType -> Exp WithObjects PurusType (FVar PurusType) -> Monomorphizer (BindE PurusType (Exp WithObjects PurusType) (FVar PurusType))
   makeBind renameDict depth newIdent t e = trace ("makeBind: " <> showIdent' newIdent) $ do
     e' <- updateFreeVars renameDict depth  <$> monomorphizeWithType t depth e
     pure $ NonRecursive newIdent e'

   -- Find a declaration body in the *module* scope
   findDeclarationBody :: Ident -> Monomorphizer (Maybe (Exp WithObjects PurusType (FVar PurusType)))
   findDeclarationBody nm = go <$> getModBinds
    where
      go :: [BindE PurusType (Exp WithObjects PurusType) (FVar PurusType)] -> Maybe (Exp WithObjects PurusType (FVar PurusType))
      go [] = Nothing
      go (b:bs) = case b of
        NonRecursive nm' e -> if nm' == nm then Just e else go bs
        Recursive xs -> case find (\x -> snd (fst x) == nm) xs of
          Nothing -> go bs
          Just ((_,_),e) -> Just e

   {- RECURSIVE BINDINGS

      First, we need to walk the target expression and collect a list of all of the used
      bindings and the type that they must be when monomorphized, and the new identifier for their
      monomorphized/instantiated version. (We *don't* change anything here)
   -}
   collectMany :: Map Ident (Ident, SourceType, Exp WithObjects PurusType (FVar PurusType)) -> PurusType -> Context -> [Exp WithObjects PurusType (FVar PurusType)] -> Monomorphizer (Map Ident (Ident, SourceType, Exp WithObjects PurusType (FVar PurusType)))
   collectMany acc _ _ [] = trace "collectMany" $ pure acc
   collectMany acc t dx (x:xs) = do
     xBinds <- collectRecBinds acc t dx x
     let acc' = acc <> xBinds
     collectMany acc' t d xs

   collectRecFieldBinds :: Map Ident (Ident, SourceType, Exp WithObjects PurusType (FVar PurusType))
                        -> M.Map PSString (RowListItem SourceAnn)
                        -> [(PSString, Exp WithObjects PurusType (FVar PurusType))]
                        -> Monomorphizer (Map Ident (Ident, SourceType, Exp WithObjects PurusType (FVar PurusType)))
   collectRecFieldBinds visited _ [] =  pure visited
   collectRecFieldBinds visited cxt ((lbl,e):rest) = trace "collectRecFieldBinds" $ do
     RowListItem{..} <- note d ("No type for field with label " <> T.unpack (prettyPrintString lbl) <> " when collecting record binds")
                          $ M.lookup lbl cxt
     this <- collectRecBinds visited rowListType d e
     collectRecFieldBinds (visited <> this) cxt rest

   collectFun :: Map Ident (Ident, SourceType, Exp WithObjects PurusType (FVar PurusType))
              -> Context
              -> Exp WithObjects PurusType (FVar PurusType)
              -> [SourceType]
              -> Monomorphizer (Map Ident (Ident, SourceType, Exp WithObjects PurusType (FVar PurusType)))
   collectFun visited _ e [t] = trace ("collectFun FIN:\n  " <> ppExp e <> " :: " <> prettyTypeStr t)  $ do
     rest <- collectRecBinds visited t d e
     pure $ visited <> rest
   collectFun visited dx e@(LamE (ForAll{}) idx body'') (t:ts) = trace ("collectFun:\n  " <> ppExp e <> "\n  " <> prettyTypeStr t <> "\n" <> show ts)  $ do
      let body' = updateVarTy d idx t body''
          cxt   = M.insert idx t dx
      collectFun visited cxt body' ts

   collectFun visited dx (V (FVar _ (Qualified (ByModuleName _) nm))) (t:ts)= trace ("collectFun VAR: " <> showIdent' nm) $ do
     case M.lookup nm visited of
       Nothing -> do
         let t' = foldr1 function (t:ts)
             msg =  "Couldn't find a declaration with identifier " <> showIdent' nm <> " to inline as " <> prettyTypeStr t
         declBody <- note dx msg =<< findDeclarationBody nm
         freshNm <- freshen nm
         let visited' = M.insert nm (freshNm,t',declBody) visited
         collectRecBinds visited' t' d declBody
       Just _ -> pure visited

   collectFun _ dx e _ = throwError $ MonoError dx $ "Unexpected expression in collectFun:\n  " <> ppExp e

   collectRecBinds ::
     Map Ident (Ident, SourceType, Exp WithObjects PurusType (FVar PurusType)) ->
     PurusType ->
     Context ->
     Exp WithObjects PurusType (FVar PurusType) ->
     Monomorphizer (Map Ident (Ident, SourceType, Exp WithObjects PurusType (FVar PurusType)))
   collectRecBinds visited t dx e = trace ("collectRecBinds:\n  " <> ppExp e <> "\n  " <> prettyTypeStr t) $ case e of
     LitE _ (ArrayL arr) -> trace "crbARRAYLIT" $ case t of
       ArrayT inner -> do
         innerBinds <- collectMany visited inner dx  arr
         pure $ visited <> innerBinds
       _ -> throwError $ MonoError dx ("Failed to collect recursive binds: " <> prettyTypeStr t <> " is not an Array type")
     LitE _ (ObjectL _ fs) -> trace "crbOBJLIT" $ case t of
         RecordT fields -> do
           let fieldMap = mkFieldMap fields
           innerBinds <- collectRecFieldBinds visited fieldMap fs
           pure $ visited <> innerBinds
         _ -> throwError $ MonoError dx ("Failed to collect recursive binds: " <> prettyTypeStr t <> " is not a Record type")
     LitE _ _  -> trace "crbLIT" $ pure visited
     CtorE _ _ _ _ -> trace "crbCTOR" $ pure visited
     ObjectUpdateE _ _ _ _ updateFields -> trace "crbOBJUPDATE" $ case t of
        RecordT fields -> do
          let fieldMap = mkFieldMap fields
          -- idk. do we need to do something to the original expression or is this always sufficient?
          innerBinds <- collectRecFieldBinds visited fieldMap updateFields
          pure $ visited <> innerBinds
        _ -> throwError $ MonoError dx ("Failed to collect recursive binds: " <> prettyTypeStr t <> " is not a Record type")
     AccessorE _ _ _ _ -> trace "crbACCSR" $ pure visited -- idk. given (x.a :: t) we can't say what x is
     absE@(LamE _ _ _) -> trace ("crbABS TOARGS: " <> prettyTypeStr t) $ collectFun visited dx absE (toArgs t)
     app@(AppE _ e2) -> trace "crbAPP" $ do
       (f,args) <-  note dx ("Not an App: " <> ppExp app) $ analyzeApp app
       let types = (exprType <$> args) <> [t]
       funBinds' <- collectFun visited dx f types  -- collectRecBinds visited funTy d e1
       let funBinds = visited <> funBinds'
       argBinds <- collectRecBinds funBinds (head types) dx  e2
       pure $ funBinds <> argBinds
     V (FVar _ (Qualified (ByModuleName _) nm)) -> trace ("crbVAR: " <> showIdent' nm)  $ case M.lookup nm visited of
       Nothing -> findDeclarationBody nm >>= \case
         Nothing -> throwError $ MonoError dx  $ "No declaration correponding to name " <> showIdent' nm <> " found in the module"
         Just ex -> do
           freshNm <- freshen nm
           let this = (freshNm,t,ex)
           pure $ M.insert nm this visited
       Just _ -> pure visited  -- might not be right, might need to check that the types are equal? ugh keeping track of scope is a nightmare
     V (FVar _ (Qualified _ nm)) -> trace ("crbVAR_: " <> showIdent' nm) $ pure visited
     CaseE _ _  alts -> trace "crbCASE" $ do
       let flatAlts = concatMap extractAndFlattenAlts alts
       aInner <- collectMany visited t dx flatAlts
       pure $ visited <> aInner
     LetE _ _ ex ->
       -- not sure abt this
       collectRecBinds visited t dx ex


-- TODO: Remove?
extractAndFlattenAlts :: Alt x ty (Exp x ty) a -> [Exp WithObjects PurusType (FVar PurusType)]
extractAndFlattenAlts (UnguardedAlt _ _ res) = [res]


-- I think this one actually requires case analysis? dunno how to do it w/ the lenses in less space (w/o having prisms for types which seems dumb?)
-- This *forces* the expression to have the provided type (and returns nothing if it cannot safely do that)
monomorphizeWithType ::
  PurusType ->
  Context ->
  Exp WithObjects PurusType (FVar PurusType) ->
  Monomorphizer (Exp WithObjects PurusType (FVar PurusType))
monomorphizeWithType ty d expr
  | exprType expr == ty = pure expr
  | otherwise = trace ("monomorphizeWithType:\n  " <> ppExp expr <> "\n  " <> prettyTypeStr ty) $ case expr of
      LitE ty (ArrayL arr) -> case ty of
        ArrayT inner -> LitE ty . ArrayL <$> traverse (monomorphizeWithType inner d)  arr
        _ -> throwError $ MonoError d ("Failed to collect recursive binds: " <> prettyTypeStr ty <> " is not a Record type")

      LitE _ (ObjectL ext fs) -> case ty of
        RecordT fields -> do
          let fieldMap = mkFieldMap fields
          LitE ty . ObjectL ext <$> monomorphizeFieldsWithTypes fieldMap  fs
        _ -> throwError $ MonoError d ("Failed to collect recursive binds: " <> prettyTypeStr ty <> " is not a Record type")

      LitE ty lit -> pure $ LitE ty lit

      CtorE _ tName cName fs -> pure $ CtorE ty tName cName fs

      ObjectUpdateE ext _ orig copyFields updateFields -> case ty of
        RecordT fields -> do
          let fieldMap = mkFieldMap fields
          -- idk. do we need to do something to the original expression or is this always sufficient?
          updateFields' <- monomorphizeFieldsWithTypes fieldMap updateFields
          pure $ ObjectUpdateE ext ty orig copyFields updateFields'
        _ -> throwError $ MonoError d ("Failed to collect recursive binds: " <> prettyTypeStr ty <> " is not a Record type")

      AccessorE ext _ str e -> pure $ AccessorE ext ty str e -- idk?
      fun@(LamE _ ident body) -> trace ("MTABs:\n  " <> ppExp fun <> " :: " <> prettyTypeStr ty) $ do
        case ty of
          (a :-> b) -> case nameShadows d ident of
            False -> do
              let cxt = M.insert ident a d
              body' <- monomorphizeWithType b cxt $ updateVarTy cxt ident a body
              pure $ LamE ty ident body'
            True -> do
              freshIdent <- freshen ident
              let body' = renameBoundVar ident freshIdent d $ updateVarTy d ident a body
                  cxt   = M.insert freshIdent a d
              body'' <- monomorphizeWithType b cxt body'
              error "TODO"
              -- pure $ Abs nullAnn ty freshIdent body''
          _ -> throwError $ MonoError d "Abs isn't a function"

      app@(AppE _ e2) -> trace ("MTAPP:\n  " <> ppExp app) $  do
        (f,args) <- note d ("Not an app: " <> ppExp app) $ analyzeApp app
        let types = (exprType <$> args) <> [ty]
        e1' <- either (uncurry gLet) id <$> handleFunction d f args
        pure $ AppE e1' e2

      V a -> pure $ V a -- idk

      CaseE _ scrut alts -> error "TODO: wtf?"
        -- let f = monomorphizeWithType  ty d
        --     -- goAlt :: Alt WithObjects PurusType -> Monomorphizer (CaseAlternative Ann)
        --     goAlt (CaseAlternative binders results) =
        --       CaseAlternative binders <$> bitraverse (traverse (bitraverse f f)) f results
        -- in Case a ty scrut <$> traverse goAlt alts

      LetE a binds e -> LetE a binds <$> monomorphizeWithType ty d e
  where
    monomorphizeFieldsWithTypes :: M.Map PSString (RowListItem SourceAnn) -> [(PSString, Exp WithObjects PurusType (FVar PurusType))] -> Monomorphizer [(PSString, Exp WithObjects PurusType (FVar PurusType))]
    monomorphizeFieldsWithTypes _ [] = pure []
    monomorphizeFieldsWithTypes cxt ((lbl,e):rest) = do
      RowListItem{..} <- note d ("No type for field with label " <> T.unpack (prettyPrintString lbl) <> " when monomorphizing record")
                         $ M.lookup lbl cxt
      rest' <- monomorphizeFieldsWithTypes cxt rest
      e' <- monomorphizeWithType  rowListType d e
      pure $ (lbl,e') : rest'

mkFieldMap :: SourceType -> M.Map PSString (RowListItem SourceAnn)
mkFieldMap fs = M.fromList $ (\x -> (runLabel (rowListLabel x),x)) <$> (fst . rowToList $ fs)

toArgs :: SourceType -> [SourceType]
toArgs = \case
  (a :-> b) -> a : toArgs b
  other -> [other]

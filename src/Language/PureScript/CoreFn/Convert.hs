{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Language.PureScript.CoreFn.Convert where

import qualified Prelude as P
import Prelude hiding (error)
import Data.Bifunctor
import Data.Maybe

import Language.PureScript.CoreFn.Expr
import Language.PureScript.CoreFn.Module
import PlutusIR.Core qualified as PIR
import Language.PureScript.Names (Ident(..), Qualified (..), QualifiedBy (..), runModuleName, pattern ByNullSourcePos, ModuleName)
import Language.PureScript.Types
import Language.PureScript.CoreFn.Pretty.Common
import Language.PureScript.CoreFn.Desugar.Utils
import Language.PureScript.Environment (pattern (:->), pattern ArrayT, pattern RecordT, function)
import Data.Functor.Identity (Identity(..))
import Language.PureScript.CoreFn.Pretty
import Language.PureScript.Make.Monad (readJSONFileIO)
import Language.PureScript.CoreFn.FromJSON
import Language.PureScript.CoreFn.Ann (Ann)
import Data.ByteString qualified as BS
import Data.Aeson qualified as Aeson
import Text.Pretty.Simple (pShow)
import Data.Text.Lazy qualified as LT
import Data.Text qualified as T
import Data.List (find)
import Debug.Trace
import Language.PureScript.AST.Literals (Literal(..))
import Data.Map (Map)
import Data.Map qualified as M
import Language.PureScript.Label (Label(runLabel))
import Language.PureScript.PSString (PSString, decodeStringWithReplacement, prettyPrintString)
import Language.PureScript.AST (SourceAnn)
import Control.Concurrent
import Language.PureScript.AST.SourcePos
    ( pattern NullSourceAnn, pattern NullSourceSpan )
import Data.Set qualified as S
import GHC.Natural
import Data.Bitraversable (Bitraversable(bitraverse))
import Control.Lens.IndexedPlated
import Control.Lens
import Control.Lens.Operators
import Control.Monad.State
import Control.Monad.RWS.Class
import Control.Monad.RWS
import Control.Monad.Except (throwError)

-- hopefully a better API than the existing traversal machinery (which is kinda weak!)
-- Adapted from https://twanvl.nl/blog/haskell/traversing-syntax-trees

type Depth = Natural

instance IndexedPlated Natural (Expr a) where
  iplate d f = \case
    Literal ann ty lit -> Literal ann ty <$> traverseLit (indexed f d) lit
    Accessor ann ty field e -> Accessor ann ty field <$>  indexed f d e
    ObjectUpdate ann ty orig copyFields updateFields ->
      (\orig' updateFields' -> ObjectUpdate ann ty orig' copyFields updateFields')
      <$> indexed f d orig
      <*> traverse (sequenceA . second (indexed f d)) updateFields
    Abs ann ty ident body -> Abs ann ty ident <$> indexed f (d + 1) body
    App ann ty fE argE -> App ann ty <$> indexed f d fE <*> indexed f d argE
    Case a ty scrutinees alternatives ->
      Case a ty <$> traverse (indexed f d) scrutinees <*> traverseAltE (indexed f d) alternatives
    Let a ty binds e -> Let a ty <$> traverseBinds (indexed f d) binds <*> indexed f d e
    other -> pure other -- ctors and vars don't contain any sub-expressions
   where
     traverseBinds :: forall f. Applicative f => (Expr a -> f (Expr a)) -> [Bind a] -> f [Bind a]
     traverseBinds g binds = traverse go binds
       where
         go :: Bind a -> f (Bind a)
         go = \case
           NonRec ann ident e -> NonRec ann ident <$> g e
           Rec es -> Rec <$> traverse (traverse g) es

     traverseAltE :: forall f. Applicative f => (Expr a -> f (Expr a)) -> [CaseAlternative a] -> f [CaseAlternative a]
     traverseAltE g alts = traverse go alts
       where
         go :: CaseAlternative a -> f (CaseAlternative a)
         go (CaseAlternative binders result) =
           CaseAlternative binders
           <$> helper result -- hellishly complex
         helper :: Either [(Guard a, Expr a)] (Expr a) -> f (Either [(Guard a, Expr a)] (Expr a))
         helper = bitraverse (traverse $  bitraverse g g) g

-- TODO: better error messages
data MonoError
 = MonoError Depth String

note :: Depth -> String -> Maybe b -> Monomorphizer b
note d err = \case
  Nothing -> throwError $ MonoError d err
  Just x -> pure x

-- ok we need monads
data MonoState = MonoState {
  {- Original Identifier -> Type -> (Fresh Ident, Expr)
  -}
  visited :: Map Ident (Map SourceType (Ident,Expr Ann)),
  unique :: Int
}
-- TODO: Logging, make a more useful state than S.Set Ident
type Monomorphizer a = RWST (ModuleName,[Bind Ann]) () MonoState (Either MonoError)  a
type Monomorphizer' a = RWST (ModuleName,[Bind Ann]) () MonoState Identity (Maybe a)

hoist1 ::  MonoState -> Monomorphizer a -> RWST (ModuleName,[Bind Ann]) () MonoState Identity (Maybe a)
hoist1 st act = RWST $ \r s -> f (runRWST act r s)
  where
    f :: Either MonoError (a, MonoState, ()) -> Identity (Maybe a, MonoState, ())
    f = \case
      Left (MonoError d msg) -> do
        traceM $ "MonoError at depth " <> show d <> ":\n  " <> msg
        pure (Nothing,st,())
      Right (x,st',_) -> pure (Just x, st', ())

monomorphizeMain :: Module Ann -> Maybe (Expr Ann)
monomorphizeMain Module{..} =  runMono g
  where
    emptySt = MonoState M.empty 0

    g =  monomorphizeB 0 mainE

    monomorphizeB :: Depth -> Expr Ann -> Monomorphizer' (Expr Ann)
    monomorphizeB d e = hoist1 emptySt (monomorphizeA d e)

    (mainE,otherDecls) = partitionDecls moduleDecls

    runMono :: RWST (ModuleName,[Bind Ann]) () MonoState Identity a  ->  a
    runMono act  = case runIdentity (runRWST act (moduleName,otherDecls) (MonoState M.empty 0)) of
                     (a,_,_) -> a


monomorphizeMain' :: Module Ann -> Either MonoError (Expr Ann)
monomorphizeMain' Module{..} =  g
  where
    emptySt = MonoState M.empty 0

    g = runMono $ itransformM monomorphizeA 0 mainE

    (mainE,otherDecls) = partitionDecls moduleDecls

    runMono :: RWST (ModuleName,[Bind Ann]) () MonoState (Either MonoError) a  ->  Either MonoError a
    runMono act  = case runRWST act (moduleName,otherDecls) (MonoState M.empty 0) of
                     Left err -> Left err
                     Right (a,_,_) -> Right a

runMonoTest :: FilePath -> IO ()
runMonoTest path = do
  emod <- Aeson.eitherDecodeFileStrict' path
  case emod of
    Left err -> putStrLn $ "Couldn't deserialize module:\n  " <>  err
    Right mod -> case  monomorphizeMain mod of
      Nothing -> putStrLn "fail :-("
      Just res -> putStrLn $ renderExprStr res <> "\n"

runMonoTest' :: FilePath -> IO ()
runMonoTest' path = do
  emod <- Aeson.eitherDecodeFileStrict' path
  case emod of
    Left err -> putStrLn $ "Couldn't deserialize module:\n  " <>  err
    Right mod -> do
       case monomorphizeMain' mod of
         Left (MonoError d err) -> putStrLn $ "Failure at depth " <> show d <>  ":\n  " <> err
         Right e -> do
           putStrLn "Success! Result:\n  "
           putStr (renderExprStr e <> "\n")

getModName :: Monomorphizer ModuleName
getModName = ask <&> fst

getModBinds :: Monomorphizer [Bind Ann]
getModBinds = ask <&> snd

freshen :: Ident -> Monomorphizer Ident
freshen ident = do
  u <- gets unique
  modify' $ \(MonoState v _) -> MonoState v (u+1)
  let uTxt = T.pack (show u)
  case ident of
    Ident t -> pure $ Ident $ t <> "_$$" <> uTxt
    GenIdent (Just t) i -> pure $ GenIdent (Just $ t <> "_$$" <> uTxt) i -- we only care about a unique ord property for the maps
    GenIdent Nothing i  -> pure $ GenIdent (Just $ "var_$$" <> uTxt) i
    -- other two shouldn't exist at this state
    other -> pure other


checkVisited :: Ident -> SourceType -> Monomorphizer (Maybe (Ident,Expr Ann))
checkVisited ident st = gets (preview (ix ident . ix st) . visited)

markVisited :: Ident  -> SourceType -> Expr Ann -> Monomorphizer Ident
markVisited ident st e = do
  v <- gets visited
  newIdent <- freshen ident
  let v' = v & ix ident . ix st .~ (newIdent,e)
  modify' $ \(MonoState _ u) -> MonoState v' u
  pure newIdent

-- returns (main,rest)
-- NOTE: Assumes main isn't part of a recursive binding group (it really shouldn't be?)
partitionDecls :: [Bind Ann] -> (Expr Ann, [Bind Ann])
partitionDecls bs = first fromJust $ foldr go (Nothing,[]) bs
  where
    go :: Bind Ann -> (Maybe (Expr Ann), [Bind Ann]) -> (Maybe (Expr Ann), [Bind Ann])
    go b acc = case b of
      nonrec@(NonRec _ ident expr) -> case ident of
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

nullAnn :: Ann
nullAnn = (NullSourceSpan,[],Nothing)

findInlineDeclGroup :: Ident -> [Bind a] -> Maybe (Bind a)
findInlineDeclGroup _ [] = Nothing
findInlineDeclGroup ident (NonRec ann ident' expr:rest)
  | ident == ident' = Just $ NonRec ann ident' expr
  | otherwise = findInlineDeclGroup ident rest
findInlineDeclGroup ident (Rec xs:rest) = case  find (\x -> snd (fst x) == ident) xs of
  Nothing -> findInlineDeclGroup ident rest
         -- idk if we need to specialize the whole group?
  Just _ -> Just (Rec xs)

monomorphizeA :: Depth -> Expr Ann -> Monomorphizer (Expr Ann)
monomorphizeA d = \case
  app@(App ann ty _ arg) -> trace ("monomorphizeA " <> prettyTypeStr ty) $ do
    (f,args) <-  note d ("Not an App: " <> renderExprStr app) $ analyzeApp app
    let  types = (^. eType) <$> args
     -- maybe trace or check that the types match?
     -- need to re-quantify? not sure. CHECK!
    handleFunction d f (types <> [ty]) >>= \case
      Left (binds,fun) -> do
        pure $ gLet  binds (App ann (getResult $ exprType fun) fun arg)
      Right fun -> pure $ App ann (getResult $ exprType fun) fun arg
  other -> pure other

gLet :: [Bind Ann] -> Expr Ann -> Expr Ann
gLet binds e = Let nullAnn (e ^. eType) binds e


handleFunction :: Depth
               -> Expr Ann
               -> [PurusType]
               -> Monomorphizer (Either ([Bind Ann], Expr Ann) (Expr Ann))
handleFunction  _ e [] = pure (pure e)
handleFunction  d abs@(Abs ann (ForAll{}) ident body'') (t:ts) = trace ("handleFunction abs:\n  " <> renderExprStr abs <> "\n  " <> prettyTypeStr t) $  do
  let body' = updateVarTy d ident t body''
  handleFunction (d + 1) body' ts >>= \case
    Left (binds,body) -> do
      let bodyT = body ^. eType
          e' = Abs ann (function t bodyT) ident body
      pure $ Left (binds,e')
    Right body -> do
      let bodyT = body ^. eType
      pure $ Right (Abs ann (function t bodyT) ident body)

handleFunction  d (Var a ty qn) [t] = inlineAs d t qn
handleFunction d (Var a ty qn) ts = inlineAs d (foldr1 function ts) qn
handleFunction  d e _ = throwError $ MonoError d
                        $ "Error in handleFunction:\n  "
                        <> renderExprStr e
                        <> "\n  is not an abstraction or variable"

-- I *think* all CTors should be translated to functions at this point?
-- TODO: We can make sure the variables are well-scoped too
updateVarTy :: Depth -> Ident -> PurusType -> Expr Ann -> Expr Ann
updateVarTy d ident ty = itransform goVar d
  where
    goVar :: Depth -> Expr Ann -> Expr Ann
    goVar _d expr = case expr ^? _Var of
      Just (ann,_,Qualified q@(BySourcePos _) varId) | varId == ident -> Var ann ty (Qualified q ident)
      _ -> expr

inlineAs :: Depth -> PurusType -> Qualified Ident -> Monomorphizer (Either ([Bind Ann], Expr Ann) (Expr Ann))
inlineAs d _ (Qualified (BySourcePos _) ident) = throwError $ MonoError d  $ "can't inline locally scoped identifier " <> showIdent' ident
inlineAs  d ty (Qualified (ByModuleName mn') ident) = trace ("inlineAs: " <> showIdent' ident <> " :: " <>  prettyTypeStr ty) $ ask >>= \(mn,modDict) ->
  if mn == mn'
    then do
      let msg = "Couldn't find a declaration with identifier " <> showIdent' ident <> " to inline as " <> prettyTypeStr ty
      note d msg  (findInlineDeclGroup ident modDict) >>= \case
        NonRec _ _ e -> do
          e' <- monomorphizeWithType ty d e
          pure . Right $ e'
        Rec xs -> do
          traceM $ "RECURSIVE GROUP:\n" <> (concatMap (\((_,xId),t) -> showIdent' xId <> " :: " <> renderExprStr t <> "\n") xs)
          let msg' = "Target expression with identifier " <> showIdent' ident <> " not found in mutually recursive group"
          (targIdent,targExpr) <- note d msg'  $ find (\x -> fst x == ident) (first snd <$> xs) -- has to be there
          fresh <- freshen targIdent
          let initialRecDict = M.singleton targIdent (fresh,ty,targExpr)
          dict <- collectRecBinds initialRecDict ty d targExpr
          let renameMap = (\(i,t,_) -> (i,t)) <$> dict
              bindingMap = M.elems dict
          binds <- traverse (\(newId,newTy,oldE) -> makeBind renameMap d newId newTy oldE) bindingMap
          case M.lookup targIdent renameMap of
            Just (newId,newTy) -> pure $ Left (binds,Var nullAnn newTy (Qualified ByNullSourcePos newId))
            Nothing -> throwError
                       $ MonoError d
                       $ "Couldn't inline " <> showIdent' ident <> " - identifier didn't appear in collected bindings:\n  "  <> show renameMap

          -- pure $ Left (monoBinds,exp)
    else throwError $ MonoError d "Imports aren't supported!"
 where
   makeBind :: Map Ident (Ident,SourceType) -> Depth -> Ident -> SourceType -> Expr Ann -> Monomorphizer (Bind Ann)
   makeBind renameDict depth newIdent t e = trace ("makeBind: " <> showIdent' newIdent) $ do
     e' <- updateFreeVars renameDict depth  <$> monomorphizeWithType t depth e
     pure $ NonRec nullAnn  newIdent e'

   updateFreeVar :: M.Map Ident (Ident,SourceType) -> Depth -> Expr Ann -> Expr Ann
   updateFreeVar dict depth expr = case expr ^? _Var of
     Just (ann,_,Qualified (ByModuleName _) varId) -> case M.lookup varId dict of
       Nothing -> expr
       Just (newId,newType) -> Var nullAnn newType (Qualified ByNullSourcePos newId)
     _ -> expr

   -- Find a declaration body in the *module* scope
   findDeclarationBody :: Ident -> Monomorphizer (Maybe (Expr Ann))
   findDeclarationBody nm = go <$> getModBinds
    where
      go :: [Bind Ann] -> Maybe (Expr Ann)
      go [] = Nothing
      go (b:bs) = case b of
        NonRec _  nm' e -> if nm' == nm then Just e else go bs
        Rec xs -> case find (\x -> snd (fst x) == nm) xs of
          Nothing -> go bs
          Just ((_,_),e) -> Just e

   {- RECURSIVE BINDINGS

      First, we need to walk the target expression and collect a list of all of the used
      bindings and the type that they must be when monomorphized, and the new identifier for their
      monomorphized/instantiated version. (We *don't* change anything here)
   -}
   collectMany :: Map Ident (Ident, SourceType, Expr Ann) -> PurusType -> Depth -> [Expr Ann] -> Monomorphizer (Map Ident (Ident, SourceType, Expr Ann))
   collectMany acc t d [] = trace "collectMany" $ pure acc
   collectMany acc t d (x:xs) = do
     xBinds <- collectRecBinds acc t d x
     let acc' = acc <> xBinds
     collectMany acc' t d xs

   collectRecFieldBinds :: Map Ident (Ident, SourceType, Expr Ann)
                        -> M.Map PSString (RowListItem SourceAnn)
                        -> [(PSString, Expr Ann)]
                        -> Monomorphizer (Map Ident (Ident, SourceType, Expr Ann))
   collectRecFieldBinds visited _ [] =  pure visited
   collectRecFieldBinds visited cxt ((lbl,e):rest) = trace "collectRecFieldBinds" $ do
     RowListItem{..} <- note d ("No type for field with label " <> T.unpack (prettyPrintString lbl) <> " when collecting record binds")
                          $ M.lookup lbl cxt
     this <- collectRecBinds visited rowListType d e
     collectRecFieldBinds (visited <> this) cxt rest

   collectFun :: Map Ident (Ident, SourceType, Expr Ann)
              -> Depth
              -> Expr Ann
              -> [SourceType]
              -> Monomorphizer (Map Ident (Ident, SourceType, Expr Ann))
   collectFun visited d e [t] = trace ("collectFun FIN:\n  " <> renderExprStr e <> " :: " <> prettyTypeStr t)  $ do
     rest <- collectRecBinds visited t d e
     pure $ visited <> rest
   collectFun visited d e@(Abs ann (ForAll{}) ident body'') (t:ts) = trace ("collectFun:\n  " <> renderExprStr e <> "\n  " <> prettyTypeStr t <>"\n" <> show ts)  $ do
     let body' = updateVarTy d ident t body''
     collectFun visited (d+1) body' ts
   collectFun visited d (Var _ _ (Qualified (ByModuleName _) nm)) (t:ts)= trace ("collectFun VAR: " <> showIdent' nm) $ do
     case M.lookup nm visited of
       Nothing -> do
         let t' = foldr1 function (t:ts)
             msg =  "Couldn't find a declaration with identifier " <> showIdent' nm <> " to inline as " <> prettyTypeStr t
         declBody <- note d msg =<< findDeclarationBody nm
         freshNm <- freshen nm
         let visited' = M.insert nm (freshNm,t',declBody) visited
         collectRecBinds visited' t' d declBody
       Just _ -> pure visited

   collectFun _ d e _ = throwError $ MonoError d $ "Unexpected expression in collectFun:\n  " <> renderExprStr e


   collectRecBinds :: Map Ident (Ident,SourceType,Expr Ann) -> PurusType -> Depth -> Expr Ann -> Monomorphizer (Map Ident (Ident,SourceType,Expr Ann))
   collectRecBinds visited t d e = trace ("collectRecBinds:\n  " <> renderExprStr e <> "\n  " <> prettyTypeStr t) $ case e of
     Literal ann _ (ArrayLiteral arr) -> trace "crbARRAYLIT" $ case t of
       ArrayT inner -> do
         innerBinds <- collectMany visited inner d  arr
         pure $ visited <> innerBinds
       other -> throwError $ MonoError d ("Failed to collect recursive binds: " <> prettyTypeStr t <> " is not an Array type")
     Literal ann _ (ObjectLiteral fs) -> trace "crbOBJLIT" $ case t of
         RecordT fields -> do
           let fieldMap = mkFieldMap fields
           innerBinds <- collectRecFieldBinds visited fieldMap fs
           pure $ visited <> innerBinds
         other -> throwError $ MonoError d ("Failed to collect recursive binds: " <> prettyTypeStr t <> " is not a Record type")
     Literal ann _ lit -> trace "crbLIT" $ pure visited
     Constructor ann _ tName cName fs -> trace "crbCTOR" $ pure visited
     ObjectUpdate a _ orig copyFields updateFields -> trace "crbOBJUPDATE" $ case t of
        RecordT fields -> do
          let fieldMap = mkFieldMap fields
          -- idk. do we need to do something to the original expression or is this always sufficient?
          innerBinds <- collectRecFieldBinds visited fieldMap updateFields
          pure $ visited <> innerBinds
        _ -> throwError $ MonoError d ("Failed to collect recursive binds: " <> prettyTypeStr t <> " is not a Record type")
     Accessor{} -> trace "crbACCSR" $ pure visited -- idk
     abs@(Abs{}) -> trace ("crbABS TOARGS: " <> prettyTypeStr t) $ collectFun visited d abs (toArgs t)
     app@(App _ _ _ e2) -> trace "crbAPP" $ do
       (f,args) <-  note d ("Not an App: " <> renderExprStr app) $ analyzeApp app
       let types = (exprType <$> args) <> [t]
       funBinds' <- collectFun visited d f types  -- collectRecBinds visited funTy d e1
       let funBinds = visited <> funBinds'
       argBinds <- collectRecBinds funBinds (head types) d e2
       pure $ funBinds <> argBinds
     Var a _ (Qualified (ByModuleName _) nm) -> trace ("crbVAR: " <> showIdent' nm)  $ case M.lookup nm visited of
       Nothing -> findDeclarationBody nm >>= \case
         Nothing -> throwError $ MonoError d  $ "No declaration correponding to name " <> showIdent' nm <> " found in the module"
         Just e -> do
           freshNm <- freshen nm
           let this = (freshNm,t,e)
           pure $ M.insert nm this visited
       Just _ -> pure visited  -- might not be right, might need to check that the types are equal? ugh keeping track of scope is a nightmare
     Var a _ (Qualified _ nm) -> trace ("crbVAR_: " <> showIdent' nm)$ pure visited
     Case a _ scruts alts -> trace "crbCASE" $ do
       let flatAlts = concatMap extractAndFlattenAlts alts
       aInner <- collectMany visited t d flatAlts
       pure $ visited <> aInner
     Let _ _ _ e ->
       -- not sure abt this
       collectRecBinds visited t d e

   updateFreeVars dict = itransform (updateFreeVar dict)

extractAndFlattenAlts :: CaseAlternative Ann -> [Expr Ann]
extractAndFlattenAlts (CaseAlternative _ res) = case res of
  Left xs -> concatMap (\(x,y) -> [x,y]) xs
  Right x -> [x]


-- I think this one actually requires case analysis? dunno how to do it w/ the lenses in less space (w/o having prisms for types which seems dumb?)
-- This *forces* the expression to have the provided type (and returns nothing if it cannot safely do that)
monomorphizeWithType :: PurusType -> Depth -> Expr Ann -> Monomorphizer (Expr Ann)
monomorphizeWithType  t d expr
  | expr ^. eType == t = pure expr
  | otherwise = trace ("monomorphizeWithType:\n  " <> renderExprStr expr <> "\n  " <> prettyTypeStr t) $ case expr of
      Literal ann _ (ArrayLiteral arr) -> case t of
        ArrayT inner -> Literal ann t . ArrayLiteral <$> traverse (monomorphizeWithType inner d)  arr
        _ -> throwError $ MonoError d ("Failed to collect recursive binds: " <> prettyTypeStr t <> " is not a Record type")
      Literal ann _ (ObjectLiteral fs) -> case t of
        RecordT fields -> do
          let fieldMap = mkFieldMap fields
          Literal ann t . ObjectLiteral <$> monomorphizeFieldsWithTypes fieldMap  fs
        _ -> throwError $ MonoError d ("Failed to collect recursive binds: " <> prettyTypeStr t <> " is not a Record type")
      Literal ann _ lit -> pure $ Literal ann t lit
      Constructor ann _ tName cName fs -> pure $ Constructor ann t tName cName fs
      ObjectUpdate a _ orig copyFields updateFields -> case t of
        RecordT fields -> do
          let fieldMap = mkFieldMap fields
          -- idk. do we need to do something to the original expression or is this always sufficient?
          updateFields' <- monomorphizeFieldsWithTypes fieldMap updateFields
          pure $ ObjectUpdate a t orig copyFields updateFields'
        _ -> throwError $ MonoError d ("Failed to collect recursive binds: " <> prettyTypeStr t <> " is not a Record type")
      Accessor a _ str e -> pure $ Accessor a t str e -- idk?
      fun@(Abs _ _ ident body) -> trace ("MTABs:\n  " <> renderExprStr fun <> " :: " <> prettyTypeStr t) $ do
        pure $ Abs nullAnn t ident body
        -- othher -> P.error $ "mtabs fail: " <> renderExprStr fun
      app@(App a _ _ e2) -> trace ("MTAPP:\n  " <> renderExprStr app) $  do
        (f,args) <- note d ("Not an app: " <> renderExprStr app) $ analyzeApp app
        let types = (exprType <$> args) <> [t]
        traceM $ renderExprStr f
        e1' <- either (uncurry gLet) id <$> handleFunction d f types
        pure $ App a t e1' e2
      Var a _ nm -> pure $ Var a t nm -- idk
      Case a _ scrut alts ->
        let f = monomorphizeWithType  t d
            goAlt :: CaseAlternative Ann -> Monomorphizer (CaseAlternative Ann)
            goAlt (CaseAlternative binders results) =
              CaseAlternative binders <$> bitraverse (traverse (bitraverse f f)) f results
        in Case a t scrut <$> traverse goAlt alts
      Let a _ binds e -> Let a t binds <$> monomorphizeWithType t d e
  where
    monomorphizeFieldsWithTypes :: M.Map PSString (RowListItem SourceAnn) -> [(PSString, Expr Ann)] -> Monomorphizer [(PSString, Expr Ann)]
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


{-

-}

{-
goFun :: (Expr Ann -> Expr Ann) -> [SourceType] -> Expr Ann -> Expr Ann
goFun _ [] e = trace (sep <> "GOFUN (terminal):\n  " <> renderExprStr e) e
goFun cb (t:ts) xp@(Abs ann _ ident body) = trace ("GOFUN (abs):\n  " <> prettyTypeStr t <> "\n  " <> renderExprStr xp)
  cb $ Abs ann (function t (exprType inner)) ident inner
 where
   inner = goFun id ts $ updateVarTy ident t body
goFun cb (t:ts) xp@(Var a ty (Qualified (ByModuleName mn') ident))
  | mn' == mn = trace (sep <> "GOFUN (var):\n   " <> prettyTypeStr t <> "\n  " <> renderExprStr xp)
                $ inlineAs cb t ident
  | otherwise = error $
      "Free variable "
      <> showIdent' ident
      <> "is imported from Module "
      <>  T.unpack (runModuleName mn')
      <> " but imports don't work yet!"
goFun _ _ e = error $ "goFun - Not a function: " <> renderExprStr e <> "\n" <> LT.unpack (pShow e)


monomorphize :: Expr Ann -> Expr Ann
monomorphize e = trace (sep <> "MONOMORPHIZE:\n" <> renderExprStr e)  $ case e of
  app@(App ann _ _ _arg) -> case analyzeApp app of
    Just (f',args') ->
          let arg = monomorphize <$> args'
              types = exprType <$> arg
              callback = \x -> App ann appTy x (monomorphize _arg)
              f = goFun callback types $ monomorphize f'
              funTy = exprType f'
              appTy = getResult $ stripQuantifiers funTy
              traceStr = sep <> "APP:\n  "
                           <> "FUN:\n  " <> renderExprStr f
                           <> "\nARG:\n  " <> concatMap (\x -> prettyTypeStr x <> "\n") types
                           <> "\nRESULT:\n  " <> prettyTypeStr appTy
          in trace traceStr $ App ann appTy f (monomorphize _arg)
  other -> other
-}
{-
runPIRTest :: FilePath -> IO ()
runPIRTest path = do
  emod <- Aeson.eitherDecodeFileStrict' path
  case emod of
    Left err -> error err
    Right mod -> do
       moduleToPIR mod


sep :: String
sep = "\n--------------------------------\n"

map2 :: Bifunctor b => (a -> c) -> b a a -> b c c
map2 f = bimap f f

moduleToPIR :: forall a uni fun. Module Ann -> IO () -- PIR.Program PIR.TyName PIR.Name uni fun a
moduleToPIR Module{..} = do
  let main' = monomorphize mainDecl'
      mainTy' = exprType main'
      !msg  = sep <> "RESULT:\n" <> prettyTypeStr mainTy' <> "\n" <> renderExprStr main'
  if trace msg $ length msg == 6669101 then putStr msg else putStr ""
  where
    (mainDecl',otherDecls) = partitionDecls moduleDecls

    mn = moduleName

    monomorphize :: Expr Ann -> Expr Ann
    monomorphize e = trace (sep <> "MONOMORPHIZE:\n" <> renderExprStr e)  $ case e of
      app@(App ann _ _ _arg) -> case analyzeApp app of
        Just (f',args') ->
              let arg = monomorphize <$> args'
                  types = exprType <$> arg
                  callback = \x -> App ann appTy x (monomorphize _arg)
                  f = goFun callback types $ monomorphize f'
                  funTy = exprType f'
                  appTy = getResult $ stripQuantifiers funTy
                  traceStr = sep <> "APP:\n  "
                               <> "FUN:\n  " <> renderExprStr f
                               <> "\nARG:\n  " <> concatMap (\x -> prettyTypeStr x <> "\n") types
                               <> "\nRESULT:\n  " <> prettyTypeStr appTy
              in trace traceStr $ App ann appTy f (monomorphize _arg)
      other -> other



    monomorphizeWithType :: SourceType -> Expr Ann -> Expr Ann
    monomorphizeWithType t e = trace (sep <> "MONOMORPHIZE WITH TYPE:\n  " {-<> prettyTypeStr t -} <> "\n  " <> renderExprStr e) $ case e of
      lit@(Literal ann _ (ArrayLiteral arr)) -> case t of
        ArrayT inner -> Literal ann t $ ArrayLiteral (monomorphizeWithType inner <$> arr)
        other -> error
                 $ "Can't monomorphize an array to type to something that isn't an ArrayT:\n"
                   <> show other
                   <> "\n"
                   <> renderExprStr lit
      lit@(Literal ann _ (ObjectLiteral fs)) -> case t of
        RecordT fields ->
          let fieldMap = M.fromList $ (\x -> (runLabel (rowListLabel x),x)) <$> (fst . rowToList $ fields)
          in Literal ann t . ObjectLiteral $ monomorphizeFieldsWithTypes fieldMap  fs
        other -> error
                 $ "Can't monomorphize an object to type to something that isn't a RecordT:\n"
                   <> show other
                   <> "\n"
                   <> renderExprStr lit
      Literal ann _ lit -> Literal ann t lit
      Constructor ann _ tName cName fs -> Constructor ann t tName cName fs
      Accessor a _ str expr -> Accessor a t str expr -- idk?
      fun@(Abs a _ nm body) -> goFun id (toArgs t) fun
      App a ty e1 e2 -> undefined
      Var a ty nm -> Var a t nm -- idk
      Case a ty scrut alts ->
        let f = monomorphizeWithType t
            goAlt :: CaseAlternative Ann -> CaseAlternative Ann
            goAlt (CaseAlternative binders results) =
              CaseAlternative binders $ bimap (map (map2 f)) f results
        in Case a t scrut $ goAlt <$> alts
      Let a _ binds expr -> Let a t binds $ monomorphizeWithType t expr

    monomorphizeFieldsWithTypes :: M.Map PSString (RowListItem SourceAnn) -> [(PSString, Expr Ann)] -> [(PSString, Expr Ann)]
    monomorphizeFieldsWithTypes _ [] = []
    monomorphizeFieldsWithTypes cxt ((lbl,e):rest) = case M.lookup lbl cxt of
      Just RowListItem{..} -> (lbl,monomorphizeWithType rowListType e): monomorphizeFieldsWithTypes cxt rest
      Nothing -> error $ "Missing field " <> decodeStringWithReplacement lbl <> " when monomorphizing object"



    goFun :: (Expr Ann -> Expr Ann) -> [SourceType] -> Expr Ann -> Expr Ann
    goFun _ [] e = trace (sep <> "GOFUN (terminal):\n  " <> renderExprStr e) e
    goFun cb (t:ts) xp@(Abs ann _ ident body) = trace ("GOFUN (abs):\n  " <> prettyTypeStr t <> "\n  " <> renderExprStr xp)
      cb $ Abs ann (function t (exprType inner)) ident inner
     where
       inner = goFun id ts $ updateVarTy ident t body
    goFun cb (t:ts) xp@(Var a ty (Qualified (ByModuleName mn') ident))
      | mn' == mn = trace (sep <> "GOFUN (var):\n   " <> prettyTypeStr t <> "\n  " <> renderExprStr xp)
                    $ inlineAs cb t ident
      | otherwise = error $
          "Free variable "
          <> showIdent' ident
          <> "is imported from Module "
          <>  T.unpack (runModuleName mn')
          <> " but imports don't work yet!"
    goFun _ _ e = error $ "goFun - Not a function: " <> renderExprStr e <> "\n" <> LT.unpack (pShow e)

    inlineAs :: (Expr Ann -> Expr Ann) -> SourceType -> Ident -> Expr Ann
    inlineAs  cb newTy ident = trace (sep <> "INLINEAS:\n  " <>prettyTypeStr newTy <> "\n  " <> showIdent' ident)
      $ case findInlineDeclGroup otherDecls of
          Nothing -> error $ "Local declaration for " <> showIdent' ident  <> " not found in module (impossible?)"
          Just (NonRec _ _ e) -> cb $ monomorphizeWithType newTy e
          Just (Rec xs ) ->
            let (targIdent,targExpr) = fromJust $ find (\x -> fst x == ident) (first snd <$> xs)
                dict = M.fromList $ (\(i,_) -> (i,unsafeMonoIdent i)) . first snd <$> xs
                targIdent'  = unsafeMonoIdent targIdent
                binds = fmap (\(i,e) -> (((NullSourceSpan,[],Nothing),i),e))  $ M.toList $ monomorphizeBinds dict M.empty targIdent' newTy targExpr
            in Let (NullSourceSpan,[],Nothing) (getResult newTy) [Rec binds] $ cb (Var  (NullSourceSpan,[],Nothing) newTy (Qualified ByNullSourcePos $ unsafeMonoIdent targIdent))
     where
       -- TODO: Need to keep track of what the type of each "unsolved" binding in the rec group *SHOULD* be (this probably has to be monadic)
       monomorphizeBind :: M.Map Ident (Ident,Expr Ann) ->  SourceType -> Expr Ann -> Expr Ann
       monomorphizeBind  dict t e = undefined
        where
          collect = \case
            lit@(Literal ann _ (ArrayLiteral arr)) -> case t of
              ArrayT inner -> Literal ann t $ ArrayLiteral (monomorphizeBind dict inner <$> arr)
              other -> error
                       $ "Can't monomorphize an array to type to something that isn't an ArrayT:\n"
                         <> show other
                         <> "\n"
                         <> renderExprStr lit
            lit@(Literal ann _ (ObjectLiteral fs)) -> case t of
              RecordT fields ->
                let fieldMap = M.fromList $ (\x -> (runLabel (rowListLabel x),x)) <$> (fst . rowToList $ fields)
                in Literal ann t . ObjectLiteral $ monomorphizeFieldsWithTypes fieldMap  fs -- TODO recursive
              other -> error
                       $ "Can't monomorphize an object to type to something that isn't a RecordT:\n"
                         <> show other
                         <> "\n"
                         <> renderExprStr lit
            Literal ann _ lit -> Literal ann t lit
            Constructor ann _ tName cName fs -> Constructor ann t tName cName fs
            Accessor a _ str expr -> Accessor a t str expr -- idk?
            fun@(Abs a _ nm body) -> goRecFun (toArgs t) fun
            App a ty e1 e2 -> undefined
            vx@(Var a ty (Qualified (ByModuleName mn') ident'))
              | mn' == mn -> case M.lookup ident' dict of
                  Nothing -> monomorphizeWithType t vx
                  Just (newIdent,expr) -> Var (NullSourceSpan,[],Nothing) t (Qualified ByNullSourcePos newIdent)
            Case a ty scrut alts ->
              let f = monomorphizeBind dict  t
                  goAlt :: CaseAlternative Ann -> CaseAlternative Ann
                  goAlt (CaseAlternative binders results) =
                    CaseAlternative binders $ bimap (map (map2 f)) f results
              in Case a t scrut $ goAlt <$> alts
            Let a _ binds expr -> Let a t binds $ monomorphizeBind dict t expr

          goRecFun :: [SourceType] -> Expr Ann -> Expr Ann
          goRecFun  [] e = trace (sep <> "GOFUN (terminal):\n  " <> renderExprStr e) e
          goRecFun (tx:ts) xp@(Abs ann _ ident' body) = trace ("GORECFUN (abs):\n  " <> prettyTypeStr t <> "\n  " <> renderExprStr xp)
              $ Abs ann (function tx (exprType inner)) ident' inner
           where
             inner = goRecFun ts $ updateVarTy ident' t body
          goRecFun (tx:ts) xp@(Var a _ (Qualified (ByModuleName mn') ident'))
            | mn' == mn = trace (sep <> "GOFUN (var):\n   " <> prettyTypeStr t <> "\n  " <> renderExprStr xp) $  case M.lookup ident' dict of
                                Nothing -> monomorphizeWithType tx xp
                                Just (identX,_) -> Var (NullSourceSpan,[],Nothing) tx (Qualified ByNullSourcePos identX)
            | otherwise = error $ "Free variable "
                <> showIdent' ident
                <> "is imported from Module "
                <>  T.unpack (runModuleName mn')
                <> " but imports don't work yet!"
          goRecFun  _ e = error $ "goFun - Not a function: " <> renderExprStr e <> "\n" <> LT.unpack (pShow e)

       findInlineDeclGroup [] = Nothing
       findInlineDeclGroup (NonRec ann ident' expr:rest)
        | ident == ident' = Just $ NonRec ann ident' expr
        | otherwise = findInlineDeclGroup rest
       findInlineDeclGroup (Rec xs:rest) = case  find (\x -> snd (fst x) == ident) xs of
         Nothing -> findInlineDeclGroup rest
         -- idk if we need to specialize the whole group?
         Just _ -> Just (Rec xs)

       unsafeMonoIdent :: Ident -> Ident
       unsafeMonoIdent (Ident txt) = Ident (txt <> "_dsafjklsdajfdsafjiodsafjiosdajf903240f3280f32893289") -- TODO: better



    updateVarTy :: Ident -> SourceType -> Expr Ann -> Expr Ann
    updateVarTy ident t e = trace (sep <> "UPDATEVAR TY:\n  IDENT: " <> showIdent' ident  <> "\n  TYPE:" <> prettyTypeStr t <> "\n  EXPR:" <> renderExprStr e) $ case e of
      Literal ann ty lit -> Literal ann ty $ runIdentity $ traverseLit (pure . updateVarTy ident t) lit  -- idk
      ctor@Constructor{} -> ctor
      Accessor a ty str expr -> Accessor a ty str $ updateVarTy ident t expr
      ObjectUpdate a ty orig copyFields updateFields ->
        let go = updateVarTy ident t
        in ObjectUpdate a ty (go orig) copyFields (second go <$> updateFields)
      Abs a ty nm body -> Abs a ty nm (updateVarTy ident t body)
      App a ty e1 e2 -> App a ty (updateVarTy ident t e1) (updateVarTy ident t e2)
      Var a ty nm@(Qualified q@(BySourcePos _) varId) -> trace ("updateVar1 " <> showIdent' ident <> prettyTypeStr t)  $
        if varId == ident
        then Var a t (Qualified q varId)
        else Var a ty nm
      v@Var{} -> trace "updateVar2" v
      Case a ty scrut alts ->
        Case a ty (updateVarTy ident t <$> scrut) $ (goAlt <$> alts)
      Let a ty binds expr -> Let a ty (goBind <$> binds) (updateVarTy ident t expr)
     where
       goAlt :: CaseAlternative Ann -> CaseAlternative Ann
       goAlt (CaseAlternative binders results) =
         let go = updateVarTy ident t
         in CaseAlternative binders (bimap (map (bimap go go)) go results)

       goBind :: Bind Ann -> Bind Ann
       goBind = \case
         NonRec a nm expr -> NonRec a nm (updateVarTy ident t expr)
         Rec xs -> Rec $  map (second (updateVarTy ident t)) xs
-}

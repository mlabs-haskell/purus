{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications, ScopedTypeVariables #-}
module Language.PureScript.CoreFn.Convert where

import Prelude
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
import Data.Map qualified as M
import Language.PureScript.Label (Label(runLabel))
import Language.PureScript.PSString (PSString, decodeStringWithReplacement)
import Language.PureScript.AST (SourceAnn)
import Control.Concurrent
import Language.PureScript.AST.SourcePos (pattern NullSourceAnn)
import Language.PureScript.AST.SourcePos (pattern NullSourceSpan)
import Data.Set qualified as S
import GHC.Natural
import Data.Bitraversable (Bitraversable(bitraverse))
import Control.Lens.IndexedPlated
import Control.Lens
import Control.Lens.Operators
import Control.Monad.State
import Control.Monad.RWS.Class
import Control.Monad.RWS

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
         helper x = bitraverse (traverse $  bitraverse g g) g x

-- ok we need monads
data MonoState = MonoState {
  visited :: M.Map Ident (M.Map QualifiedBy (M.Map SourceType (Expr Ann))),
  unique :: Int
}
-- TODO: Logging, make a more useful state than S.Set Ident
type Monomorphizer  = RWST (ModuleName,[Bind Ann]) () MonoState Maybe

getModName :: Monomorphizer ModuleName
getModName = ask >>= pure . fst

getModBinds :: Monomorphizer [Bind Ann]
getModBinds = ask >>= pure . snd

freshen :: Ident -> Monomorphizer Ident
freshen ident = do
  u <- gets unique
  let uTxt = T.pack (show u)
  case ident of
    Ident t -> pure $ Ident $ t <> "_$$" <> uTxt
    GenIdent (Just t) i -> pure $ GenIdent (Just $ t <> "_$$" <> uTxt) i -- we only care about a unique ord property for the maps
    GenIdent Nothing i  -> pure $ GenIdent (Just $ "var_$$" <> uTxt) i
    -- other two shouldn't exist at this state
    other -> pure other


checkVisited :: Qualified Ident -> SourceType -> Monomorphizer (Maybe (Expr Ann))
checkVisited (Qualified qb ident) st = gets visited >>= pure . preview (ix ident . ix qb . ix st)

markVisited :: Qualified Ident  -> SourceType -> Expr Ann -> Monomorphizer ()
markVisited (Qualified qb ident) st e = do
  v <- gets visited
  let v' = v & (ix ident . ix qb . ix st) .~ e
  modify' $ \(MonoState _ u) -> MonoState v' u

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
monomorphizeA d e =  do -- ez way to avoid clashing var names!
  (mn,modDict) <- ask
  (ann,ty,_,arg) <- lift $ e ^? _App
  (f,args) <- lift $ analyzeApp e
  let  types = (^. eType) <$> args
   -- maybe trace or check that the types match?
   -- need to re-quantify? not sure. CHECK!
  handleFunction d f types >>= \case
    Left (binds,fun) -> do
      pure $ gLet  binds (App ann ty fun arg)
    Right fun -> pure $ App ann ty fun arg

gLet :: [Bind Ann] -> Expr Ann -> Expr Ann
gLet binds e = Let nullAnn (e ^. eType) binds e

-- for handling individual, non-recursive binds
-- TODO Use a depth map? Maybe index by the enclosing declaration? Something better than this?
handleBind :: Depth -> Maybe SourceType -> Ident -> Expr Ann ->  Monomorphizer (Bind Ann)
handleBind d Nothing ident e = do
  e' <- monomorphizeA d e
  ident' <- Qualified ByNullSourcePos <$> freshen ident -- idk
  markVisited ident' (e' ^. eType) e'
  pure $ NonRec nullAnn ident e'
handleBind d (Just t) ident e = do
  let qIdent = Qualified ByNullSourcePos ident
  checkVisited qIdent t >>= \case
    Nothing -> do
      e' <- monomorphizeWithType t d e
      ident' <- Qualified ByNullSourcePos <$> freshen ident
      markVisited ident' t e'
      pure $ NonRec nullAnn ident e'
    Just e' -> pure $ NonRec nullAnn ident e'

handleFunction :: Depth
               -> Expr Ann
               -> [PurusType]
               -> Monomorphizer (Either ([Bind Ann], Expr Ann) (Expr Ann))
handleFunction  _ e [] = pure (pure e)
handleFunction  d (Abs ann (ForAll{}) ident body'') (t:ts) = do
  (mn,modDict) <- ask
  let body' = updateVarTy d ident t body''
  handleFunction (d + 1) body' ts >>= \case
    Left (binds,body) -> do
      let bodyT = body ^. eType
          e' = Abs ann (function t bodyT) ident body
      pure $ Left (binds,e')
    Right body -> do
      let bodyT = body ^. eType
      pure $ Right (Abs ann (function t bodyT) ident body)
handleFunction  d (Var a ty qn) (t:ts) = inlineAs d t qn
handleFunction  _ _ _ = lift Nothing
-- I *think* all CTors should be translated to functions at this point?

-- TODO: We can make sure the variables are well-scoped too
updateVarTy :: Depth -> Ident -> PurusType -> Expr Ann -> Expr Ann
updateVarTy d ident ty e = itransform goVar d e
  where
    goVar :: Depth -> Expr Ann -> Expr Ann
    goVar _d expr = case expr ^? _Var of
      Just (ann,_,Qualified q@(BySourcePos _) varId) | varId == ident -> Var ann ty (Qualified q ident)
      _ -> expr

inlineAs :: Depth -> PurusType -> Qualified Ident -> Monomorphizer (Either ([Bind Ann], Expr Ann) (Expr Ann))
inlineAs  d t (Qualified (ByModuleName mn') ident) = ask >>= \(mn,modDict) ->
  if mn == mn'
    then do
      lift (findInlineDeclGroup ident modDict) >>= \case
        NonRec _ _ e -> do
          e' <- monomorphizeWithType t d e
          pure . Right $ e'
        Rec xs -> do
          (targIdent,targExpr) <- lift $ find (\x -> fst x == ident) (first snd <$> xs) -- has to be there
          let groupDict =  first snd <$> xs
          monoBinds <- traverse (uncurry (handleBind d)) groupDict   -- flip evalState S.empty $ monomorphizeRec mn modDict d groupDict targIdent targExpr
          Just
          pure $ Left (monoBinds,exp)
    else error "Imports aren't supported!"


-- I think this one actually requires case analysis? dunno how to do it w/ the lenses in less space (w/o having prisms for types which seems dumb?)
-- This *forces* the expression to have the provided type (and returns nothing if it cannot safely do that)
monomorphizeWithType :: PurusType -> Depth -> Expr Ann -> Monomorphizer (Expr Ann)
monomorphizeWithType  t d expr
  | expr ^. eType == t = pure expr
  | otherwise = case expr of
      Literal ann _ (ArrayLiteral arr) -> case t of
        ArrayT inner -> Literal ann t . ArrayLiteral <$> traverse (monomorphizeWithType inner d)  arr
        _ -> lift Nothing
      Literal ann _ (ObjectLiteral fs) -> case t of
        RecordT fields -> do
          let fieldMap = mkFieldMap fields
          Literal ann t . ObjectLiteral <$> monomorphizeFieldsWithTypes fieldMap  fs
        _ -> lift Nothing
      Literal ann _ lit -> pure $ Literal ann t lit
      Constructor ann _ tName cName fs -> pure $ Constructor ann t tName cName fs
      ObjectUpdate a _ orig copyFields updateFields -> case t of
        RecordT fields -> do
          let fieldMap = mkFieldMap fields
          -- idk. do we need to do something to the original expression or is this always sufficient?
          updateFields' <- monomorphizeFieldsWithTypes fieldMap updateFields
          pure $ ObjectUpdate a t orig copyFields updateFields'
        _ -> lift Nothing
      Accessor a _ str e -> pure $ Accessor a t str e -- idk?
      fun@(Abs{}) -> either (uncurry gLet) id <$> handleFunction d fun (toArgs t)
      App a _ e1 e2 -> do
        let args =  toArgs . exprType $ e1
            args' = args & _last .~ t
        e1' <- either (uncurry gLet) id <$> handleFunction d e1 args'
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
    mkFieldMap :: SourceType -> M.Map PSString (RowListItem SourceAnn)
    mkFieldMap fs = M.fromList $ (\x -> (runLabel (rowListLabel x),x)) <$> (fst . rowToList $ fs)

    monomorphizeFieldsWithTypes :: M.Map PSString (RowListItem SourceAnn) -> [(PSString, Expr Ann)] -> Monomorphizer [(PSString, Expr Ann)]
    monomorphizeFieldsWithTypes _ [] = pure []
    monomorphizeFieldsWithTypes cxt ((lbl,e):rest) = do
      RowListItem{..} <- lift $ M.lookup lbl cxt
      rest' <- monomorphizeFieldsWithTypes cxt rest
      e' <- monomorphizeWithType  rowListType d e
      pure $ (lbl,e') : rest'


toArgs :: SourceType -> [SourceType]
toArgs = \case
  (a :-> b) -> a : toArgs b
  other -> [other]

-- Eventually everything will need to run in a monad stack that has a StdGen, but
-- that makes something that is already immensely complicated even more complicated.
-- Gonna get things working w/ fake generated names then build a real stack
unsafeMonoIdent :: Ident -> Ident
unsafeMonoIdent (Ident txt) = Ident (txt <> "_$101") -- TODO: better

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
    inlineAs  cb newTy ident = trace (sep <> "INLINEAS:\n  " <> prettyTypeStr newTy <> "\n  " <> showIdent' ident)
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

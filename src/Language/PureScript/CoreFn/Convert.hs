{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
module Language.PureScript.CoreFn.Convert where

import Prelude
import Data.Bifunctor
import Data.Maybe

import Language.PureScript.CoreFn.Expr
import Language.PureScript.CoreFn.Module
import PlutusIR.Core qualified as PIR
import Language.PureScript.Names (Ident(Ident), Qualified (..), QualifiedBy (..), runModuleName, pattern ByNullSourcePos)
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

    stripQuantifiers :: SourceType -> SourceType
    stripQuantifiers = \case
      ForAll _ _ _  _ inner _ -> stripQuantifiers inner
      other -> other

    getResult :: SourceType -> SourceType
    getResult (_ :-> b) = getResult b
    getResult other = other

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

    partitionDecls :: [Bind Ann] -> (Expr Ann, [Bind Ann])
    partitionDecls bs = first fromJust $ foldr go (Nothing,[]) bs
      where
        go :: Bind Ann -> (Maybe (Expr Ann), [Bind Ann]) -> (Maybe (Expr Ann), [Bind Ann])
        go b acc = case b of
          nonrec@(NonRec _ ident expr) -> case ident of
            Ident "main" -> first (const $ Just expr) acc
            _ -> second (nonrec:) acc
          other -> second (other:) acc

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

    toArgs :: SourceType -> [SourceType]
    toArgs = \case
      (a :-> b) -> a : toArgs b
      other -> [other]

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

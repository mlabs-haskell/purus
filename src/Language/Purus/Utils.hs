module Language.Purus.Utils where

import Prelude

import Language.PureScript.AST.SourcePos (SourceAnn)
import Language.PureScript.Label (Label (runLabel))
import Language.PureScript.PSString (PSString)
import Language.PureScript.Types (RowListItem (rowListLabel), rowToList)

import Language.PureScript.CoreFn.Ann (Ann)
import Language.PureScript.CoreFn.Expr (Bind, PurusType)
import Language.PureScript.CoreFn.FromJSON ()
import Language.PureScript.CoreFn.Module (Module (..))
import Language.PureScript.Names (
  Ident (Ident),
  Qualified (..),
  pattern ByNullSourcePos,
 )

import Language.Purus.Debug (doTrace)
import Language.Purus.IR (BVar, BindE (..), Exp)
import Language.Purus.IR.Utils (IR_Decl, Vars, WithObjects, foldBinds, toExp)

import PlutusCore qualified as PLC
import UntypedPlutusCore qualified as UPLC
import PlutusCore.Compiler.Erase (eraseTerm)

import Control.Exception (throwIO)

import Data.Default.Class (def)
import Data.Functor (void)
import Data.List (find)

import Data.ByteString (ByteString)

import Data.Map (Map)
import Data.Map qualified as M

import Data.Text (Text)
import Data.Text qualified as T

import Data.Aeson qualified as Aeson

import Bound (Scope)

plcToUplc :: PLC.Term PLC.TyName PLC.Name PLC.DefaultUni PLC.DefaultFun () -> 
  Either (PLC.Error PLC.DefaultUni PLC.DefaultFun ()) (UPLC.Term UPLC.NamedDeBruijn PLC.DefaultUni PLC.DefaultFun ())
plcToUplc t = do
  PLC.runQuoteT $ do
    tcConfig <- PLC.TypeCheckConfig PLC.defKindCheckConfig <$> PLC.builtinMeaningsToTypes def ()
    void . PLC.inferType tcConfig $ t
  UPLC.deBruijnTerm . eraseTerm $ t

{- IO utility. Reads a CoreFn module from a source file.

-}
decodeModuleIO :: FilePath -> IO (Module (Bind Ann) PurusType PurusType Ann)
decodeModuleIO path =
  Aeson.eitherDecodeFileStrict' path >>= \case
    Left err -> throwIO $ userError err
    Right modx -> pure modx

decodeModuleBS :: ByteString -> Module (Bind Ann) PurusType PurusType Ann
decodeModuleBS bs = case Aeson.eitherDecodeStrict' bs of
  Left err -> error err
  Right mdl -> mdl 

{- Turns a Row Type into a Map of field names to Row item data.

   NOTE: Be sure to unwrap the enclosing record if you're working w/ a
         record type.
-}
mkFieldMap :: PurusType -> Map PSString (RowListItem SourceAnn)
mkFieldMap fs = M.fromList $ (\x -> (runLabel (rowListLabel x), x)) <$> (fst . rowToList $ fs)

{- Find the body of a declaration with the given name in the given module.

-}
findMain ::
  forall k.
  Text ->
  Module IR_Decl k PurusType Ann ->
  Maybe ((Ident, Int), Scope (BVar PurusType) (Exp WithObjects PurusType) (Vars PurusType))
findMain nm Module {..} = doTrace "findDeclBody" ("NAME: " <> T.unpack nm) $ findMain' (Ident nm) moduleDecls

findMain' ::
  forall x ty.
  Ident ->
  [BindE ty (Exp x ty) (Vars ty)] ->
  Maybe ((Ident, Int), Scope (BVar ty) (Exp x ty) (Vars ty))
findMain' ident binds = case findInlineDeclGroup ident binds of
  Nothing -> Nothing
  Just decl -> case decl of
    NonRecursive nrid nrix e -> Just ((nrid, nrix), e)
    Recursive xs -> case find (\x -> fst (fst x) == ident) xs of
      Nothing -> Nothing
      Just ((idnt, indx), e) -> Just ((idnt, indx), e)

{- Find the declaration *group* to which a given identifier belongs.
-}
findInlineDeclGroup ::
  Ident ->
  [BindE ty (Exp x ty) a] ->
  Maybe (BindE ty (Exp x ty) a)
findInlineDeclGroup _ [] = Nothing
findInlineDeclGroup ident (NonRecursive ident' bvix expr : rest)
  | ident == ident' = Just $ NonRecursive ident' bvix expr
  | otherwise = findInlineDeclGroup ident rest
findInlineDeclGroup ident (Recursive xs : rest) = case find (\x -> fst (fst x) == ident) xs of
  Nothing -> findInlineDeclGroup ident rest
  Just _ -> Just (Recursive xs)

qualifyNull :: a -> Qualified a
qualifyNull = Qualified ByNullSourcePos

--
findDeclBodyWithIndex ::
  Ident ->
  Int ->
  [BindE ty (Exp x ty) (Vars ty)] ->
  Maybe (Exp x ty (Vars ty))
findDeclBodyWithIndex nm i binds =
  foldBinds go Nothing binds
  where
    go ::
      Maybe (Exp x ty (Vars ty)) ->
      (Ident, Int) ->
      Scope (BVar ty) (Exp x ty) (Vars ty) ->
      Maybe (Exp x ty (Vars ty))
    go (Just res) _ _ = Just res
    go Nothing (nm', i') body
      | nm' == nm && i' == i = Just (toExp body)
      | otherwise = Nothing

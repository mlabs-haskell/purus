{-# OPTIONS_GHC -Wno-orphans #-}
module Language.Purus.Pipeline.GenerateDatatypes.Utils where

import Prelude

import Data.Map (Map)
import Data.Map qualified as M
import Data.Text (Text)
import Data.Text qualified as T

import Control.Monad.State (gets, modify)
import Data.Foldable (foldl')
import Debug.Trace ( traceM )

import Language.PureScript.Constants.Prim qualified as C
import Language.PureScript.Names (
  Ident (..),
  ProperName (..),
  ProperNameType (..),
  Qualified (..),
  QualifiedBy (ByModuleName),
  disqualify,
  runIdent,
  showIdent,
  showQualified,
 )
import Language.PureScript.CoreFn.TypeLike
    ( TypeLike(splitFunTyParts) )

import Language.Purus.IR (
  BVar (BVar),
  Ty (..),
 )
import Language.Purus.IR qualified as IR
import Language.Purus.Debug ( doTraceM )
import Language.Purus.Pipeline.Monad
    ( MonadCounter(next), PlutusContext )
import Language.Purus.Types
    ( constrNames,
      destructors,
      tyNames,
      tyVars,
      vars,
      DatatypeDictionary(_tyVars) )

import PlutusCore qualified as PLC
import PlutusIR (
  Name (Name),
  TyName,
 )
import PlutusIR qualified as PIR

import Control.Lens (
  ix,
  over,
  view,
  preview,
  (^?),
  _1,
 )
import Control.Monad.Except (
  MonadError (throwError),
 )
import Prettyprinter (Pretty (..))
import System.Random (mkStdGen, randomR)


foldr1Err :: (Foldable t) => String -> (a -> a -> a) -> t a -> a
foldr1Err msg f ta
  | null ta = error msg
  | otherwise = foldr1 f ta

pseudoRandomChar :: Int -> Char
pseudoRandomChar i = fst $ randomR ('a', 'z') (mkStdGen i)

mkTyName :: Qualified (ProperName 'TypeName) -> PlutusContext PIR.TyName
mkTyName qn =
  doTraceM "mkTyName" (prettyQPN qn) >> gets (view tyNames) >>= \tnames -> case M.lookup qn tnames of
    Just tyname -> pure tyname
    Nothing -> do
      uniq <- next
      let tyname = PIR.TyName $ Name (runProperName . disqualify $ qn) $ PLC.Unique uniq
      modify $ over tyNames (M.insert qn tyname)
      pure tyname

mkConstrName :: Qualified Ident -> Int -> PlutusContext PIR.Name
mkConstrName qi cix =
  doTraceM "mkConstrName" (prettyQI qi) >> gets (view constrNames) >>= \cnames -> case M.lookup qi cnames of
    Just cname -> pure $ fst cname
    Nothing -> do
      uniq <- next
      let nm = Name (showIdent . disqualify $ qi) $ PLC.Unique uniq
      modify $ over constrNames (M.insert qi (nm, cix))
      pure nm

-- | Only gives you a TyName, doesn't insert anything into the context
mkNewTyVar :: Text -> PlutusContext TyName
mkNewTyVar nm =
  doTraceM "mkNewTyVar" (T.unpack nm) >> do
    uniq <- next
    pure . PIR.TyName $ PIR.Name nm $ PLC.Unique uniq

-- | Only gives you a Name, doesn't insert anything into the context
mkNewVar :: Text -> PlutusContext PIR.Name
mkNewVar nm = doTraceM "mkNewVar" (T.unpack nm) >> PIR.Name nm . PLC.Unique <$> next

freshName :: PlutusContext PIR.Name
freshName = do
  uniq <- next
  let c = pseudoRandomChar uniq
  let nm = T.pack (c : '#' : show uniq)
  pure $ PIR.Name nm (PLC.Unique uniq)

mkVar :: Text -> PlutusContext PIR.Name
mkVar nm =
  doTraceM "mkVar" (T.unpack nm) >> gets (view vars) >>= \names -> case M.lookup nm names of
    Nothing -> do
      var <- mkNewVar nm
      modify $ over vars (M.insert nm var)
      pure var
    Just var -> pure var

getBoundTyVarName :: Text -> PlutusContext PIR.TyName
getBoundTyVarName nm =
  doTraceM "mkBoundTyVarName" (T.unpack nm) >> do
    boundTyVars <- gets _tyVars
    case M.lookup nm boundTyVars of
      Just tyName -> pure tyName
      Nothing -> error $ "Free type variable in IR: " <> T.unpack nm

-- Sometimes (e.g. when typing lambdas) we have to branch on whether the tv is already bound
lookupTyVar :: Text -> PlutusContext (Maybe PIR.TyName)
lookupTyVar nm = gets (preview (tyVars . ix nm))

bindTV :: Text -> PIR.TyName -> PlutusContext ()
bindTV txt nm = modify $ over tyVars (M.insert txt nm)

insertMany :: forall k v. (Ord k) => [(k, v)] -> Map k v -> Map k v
insertMany new acc = foldl' (flip $ uncurry M.insert) acc new

deleteMany :: forall k v. (Ord k) => [k] -> Map k v -> Map k v
deleteMany xs acc = foldl' (flip M.delete) acc xs

note :: String -> Maybe a -> PlutusContext a
note msg = maybe (throwError msg) pure

isTupleCtor :: Qualified (ProperName a) -> Bool
isTupleCtor (Qualified (ByModuleName C.M_Prim) (ProperName xs)) = T.isPrefixOf "Tuple" xs
isTupleCtor _ = False

getDestructorTy :: Qualified (ProperName 'TypeName) -> PlutusContext PLC.Name
getDestructorTy qn = do
  dctors <- gets (view destructors)
  case M.lookup qn dctors of
    Nothing ->
      throwError $
        "No destructor defined for datatype "
          <> T.unpack (showQualified runProperName qn)
          <> ". This indicates a compiler bug (datatype declaration not generated)"
    Just dctor -> pure dctor

getConstructorName :: Qualified Ident -> PlutusContext (Maybe PLC.Name)
getConstructorName qi =
  doTraceM "getConstructorName" (show qi) >> do
    ctors <- gets (view constrNames)
    traceM $ show ctors
    pure $ ctors ^? ix qi . _1

prettyQPN :: Qualified (ProperName 'TypeName) -> String
prettyQPN = T.unpack . showQualified runProperName

instance Pretty (Qualified (ProperName 'TypeName)) where
  pretty = pretty . prettyQPN

prettyQI :: Qualified Ident -> String
prettyQI = T.unpack . showQualified runIdent

instance Pretty (Qualified Ident) where
  pretty = pretty . prettyQI

bvTy :: BVar ty -> ty
bvTy (BVar _ t _) = t

funResultTy :: (TypeLike t) => t -> t
funResultTy = last . splitFunTyParts

analyzeTyApp :: Ty -> Maybe (Ty, [Ty])
analyzeTyApp t = (,tyAppArgs t) <$> tyAppFun t
  where
    tyAppArgs (IR.TyApp t1 t2) = tyAppArgs t1 <> [t2]
    tyAppArgs _ = []

    tyAppFun (IR.TyApp tF _) = go tF
      where
        go (IR.TyApp tX _) = case tyAppFun tX of
          Nothing -> Just tX
          Just tX' -> Just tX'
        go other = Just other
    tyAppFun _ = Nothing

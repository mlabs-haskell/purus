module Language.PureScript.CoreFn.Pretty.Types (prettyType, prettyTypeStr, prettyTypeTxt) where

import Prelude hiding ((<>))

import Control.Monad.Reader (MonadReader (ask), Reader)
import Data.Bifunctor (Bifunctor (..), first)
import Data.Text (Text)

import Language.PureScript.Environment (
  tyFunction,
  tyRecord,
  pattern ArrayT,
 )
import Language.PureScript.Names (OpName (..), ProperName (..), showQualified)
import Language.PureScript.PSString (prettyPrintString)
import Language.PureScript.Types (Constraint (..), Type (..), TypeVarVisibility (..), WildcardData (..), eqType)

import Language.PureScript.CoreFn.Pretty.Common (
  LineFormat,
  Printer,
  arrow,
  asOneLine,
  fmtSep,
  openRecord,
  openRow,
  recordLike,
  runPrinter,
  (<::>),
 )
import Prettyprinter (
  Doc,
  Pretty (pretty),
  group,
  hcat,
  hsep,
  parens,
  tupled,
  (<+>),
  (<>),
 )

-- need for debugging

import Data.Text qualified as T
import Prettyprinter (
  defaultLayoutOptions,
  layoutPretty,
 )
import Prettyprinter.Render.Text (renderStrict)

prettyType :: forall a ann. (Show a) => Type a -> Printer ann
prettyType t =
  group <$> case t of
    ArrayT tx -> do
      -- this is a stupid hack, figure out the proper fix later
      inner <- parens <$> prettyType tx
      pure $ "Array" <+> inner
    TUnknown _ n -> pure $ "t" <> pretty n
    TypeVar _ txt ki -> do
      ki' <- prettyType ki
      pure $ parens (pretty txt <::> ki')
    TypeLevelString _ pss -> pure . pretty . prettyPrintString $ pss
    TypeLevelInt _ i -> pure $ pretty i
    TypeWildcard _ wcd -> case wcd of
      HoleWildcard txt -> pure $ "?" <> pretty txt
      _ -> pure "_"
    TypeConstructor _ qPropName -> pure . pretty . showQualified runProperName $ qPropName
    TypeOp _ opName -> pure . pretty $ showQualified runOpName opName
    TypeApp _ t1 t2 -> goTypeApp t1 t2
    KindApp _ k1 k2 -> do
      k1' <- prettyType k1
      k2' <- prettyType k2
      pure $ k1' <> ("@" <> k2')
    ForAll _ vis var mKind inner' _ -> case stripQuantifiers inner' of
      (quantified, inner) -> goForall ([(vis, var, mKind)] <> quantified) inner
    ConstrainedType _ cstrnt innertype -> do
      cstrnt' <- prettyConstraint cstrnt
      inner' <- prettyType innertype
      pure . group $ cstrnt' <+> "=>" <+> inner'
    Skolem _ var _ i _ -> pure $ pretty var <> "#" <> pretty i
    REmpty _ -> pure "{}"
    rcons@RCons {} -> either openRow (pure . tupled) =<< rowFields rcons
    -- this might be backwards
    KindedType _ ty kind -> do
      ty' <- prettyType ty
      kind' <- prettyType kind
      pure . parens $ ty' <::> kind' -- prettyType ty fmt <::> prettyType kind fmt

    -- not sure what this is?
    BinaryNoParensType _ op l r -> do
      l' <- prettyType l
      op' <- prettyType op
      r' <- prettyType r
      pure $ l' <+> op' <+> r' -- prettyType l fmt <+> prettyType op fmt  <+> prettyType r fmt
    ParensInType _ ty -> parens <$> prettyType ty
  where
    goForall :: [(TypeVarVisibility, Text, (Type a))] -> Type a -> Printer ann
    goForall xs inner = do
      boundVars <- fmtSep =<< traverse renderBoundVar xs
      inner' <- prettyType inner
      pure $
        "forall" <+> boundVars <> "." <+> inner'

    prefixVis :: TypeVarVisibility -> Doc ann -> Doc ann
    prefixVis vis tv = case vis of
      TypeVarVisible -> hcat ["@", tv]
      TypeVarInvisible -> tv

    renderBoundVar :: (TypeVarVisibility, Text, (Type a)) -> Printer ann
    renderBoundVar (vis, var, k) = do
      ty' <- prettyType k
      pure . parens $ prefixVis vis (pretty var) <::> ty'

    stripQuantifiers :: Type a -> ([(TypeVarVisibility, Text, (Type a))], Type a)
    stripQuantifiers = \case
      ForAll _ vis var mk inner _ -> first ((vis, var, mk) :) $ stripQuantifiers inner
      other -> ([], other)

    goTypeApp :: Type a -> Type a -> Printer ann
    goTypeApp (TypeApp _ f a) b
      | eqType f tyFunction = do
          a' <- prettyType a
          b' <- parens <$> prettyType b
          parens <$> fmtSep [a' <+> arrow, b']
      | otherwise = do
          f' <- goTypeApp f a
          b' <- parens <$> prettyType b
          pure $ parens $ f' <+> b'
    goTypeApp o ty@RCons {}
      | eqType o tyRecord =
          either openRecord recordLike =<< rowFields ty
    goTypeApp a b = fmap parens $ fmtSep =<< sequence [prettyType a, parens <$> prettyType b]

    rowFields :: Type a -> Reader LineFormat (Either ([Doc ann], Doc ann) [Doc ann])
    rowFields = \case
      RCons _ lbl ty rest -> do
        fmt <- ask
        let f = ((pretty lbl <::> runPrinter fmt (prettyType ty)) :)
        rest' <- rowFields rest
        pure $ bimap (first f) f rest'
      REmpty _ -> pure $ Right []
      KindApp _ REmpty {} _ -> pure $ Right [] -- REmpty is sometimes wrapped in a kind app
      TypeVar _ txt k -> do
        k' <- prettyType k
        pure $ Left ([], parens (pretty txt <::> k'))
      other -> Right . pure <$> prettyType other --  error $ "Malformed row fields: \n" <> prettyTypeStr other

prettyConstraint :: forall a ann. (Show a) => Constraint a -> Printer ann
prettyConstraint Constraint {..} = do
  let classNm = pretty $ showQualified runProperName constraintClass
  argTypes <- hsep <$> traverse prettyType constraintArgs
  pure . group $ classNm <+> argTypes

-- TODO For debugging, remove later
smartRender :: Doc ann -> Text
smartRender = renderStrict . layoutPretty defaultLayoutOptions

prettyTypeStr :: forall a. (Show a) => Type a -> String
prettyTypeStr = T.unpack . smartRender . asOneLine prettyType

prettyTypeTxt :: forall a. (Show a) => Type a -> Text
prettyTypeTxt = smartRender . asOneLine prettyType

module Language.PureScript.CoreFn.Pretty.Types (prettyType) where

import Prelude hiding ((<>))

import Data.Text (Text)
import Data.Bifunctor (first, Bifunctor (..))
import Control.Monad.Reader ( MonadReader(ask), Reader )

import Language.PureScript.Environment
    ( tyRecord, tyFunction )
import Language.PureScript.Names (OpName(..), ProperName(..), disqualify, showQualified)
import Language.PureScript.Types (Type (..), WildcardData (..), TypeVarVisibility (..), eqType)
import Language.PureScript.PSString (prettyPrintString)

import Prettyprinter
    ( (<>),
      tupled,
      parens,
      (<+>),
      hcat,
      group,
      Doc,
      Pretty(pretty) )
import Language.PureScript.CoreFn.Pretty.Common
    ( Printer,
      LineFormat,
      runPrinter,
      fmtSep,
      openRow,
      openRecord,
      recordLike,
      (<::>),
      arrow, asOneLine )
-- need for debugging
import Prettyprinter
    ( layoutSmart,
      defaultLayoutOptions,
      layoutPretty,
      Doc )
import Prettyprinter.Render.Text ( renderIO, renderStrict )
import Data.Text (Text)
import Data.Text qualified as T


prettyType :: forall a ann. Show a => Type a -> Printer ann
prettyType t  =  group <$> case t of
  TUnknown _ n -> pure $ "t" <> pretty n

  TypeVar _ txt -> pure $ pretty txt

  TypeLevelString _ pss -> pure . pretty . prettyPrintString $ pss

  TypeLevelInt _ i -> pure $ pretty i

  TypeWildcard _ wcd -> case wcd of
    HoleWildcard txt -> pure $ "?" <> pretty txt
    _ -> pure "_"

  TypeConstructor _ qPropName -> pure . pretty . runProperName . disqualify $ qPropName

  TypeOp _ opName -> pure . pretty $ showQualified runOpName opName

  TypeApp _ t1 t2 -> goTypeApp t1 t2

  KindApp _ k1 k2 -> do
    k1' <- prettyType k1
    k2' <- prettyType k2
    pure $ k1' <> ("@" <> k2' )

  ForAll _ vis var mKind inner' _ -> case stripQuantifiers inner' of
    (quantified,inner) ->  goForall ([(vis,var,mKind)] <> quantified) inner

  ConstrainedType _ _ _ -> error "TODO: ConstrainedType (shouldn't ever appear in Purus CoreFn)"

  Skolem _ _ _ _ _ -> error "TODO: Skolem (shouldn't ever appear in Purus CoreFn)"

  REmpty _ -> pure "{}"

  rcons@RCons{} -> either openRow (pure . tupled) =<< rowFields rcons

  -- this might be backwards
  KindedType _ ty kind -> do
    ty' <- prettyType ty
    kind' <- prettyType kind
    pure .  parens $  ty' <::> kind' -- prettyType ty fmt <::> prettyType kind fmt

  -- not sure what this is?
  BinaryNoParensType _ op l r -> do
    l' <- prettyType l
    op' <- prettyType op
    r' <- prettyType r
    pure $ l' <+> op' <+> r' -- prettyType l fmt <+> prettyType op fmt  <+> prettyType r fmt

  ParensInType _ ty -> parens <$> prettyType ty
 where
   goForall :: [(TypeVarVisibility,Text,Maybe (Type a))] -> Type a -> Printer ann
   goForall xs inner = do
     boundVars <- fmtSep =<< traverse renderBoundVar xs
     inner'    <- prettyType inner
     pure $
       "forall" <+> boundVars <> "." <+> inner'

   prefixVis :: TypeVarVisibility -> Doc ann -> Doc ann
   prefixVis vis tv = case vis of
     TypeVarVisible -> hcat ["@",tv]
     TypeVarInvisible -> tv

   renderBoundVar :: (TypeVarVisibility, Text, Maybe (Type a)) -> Printer ann
   renderBoundVar (vis,var,mk) =  case mk of
     Just k -> do
       ty' <- prettyType k
       pure . parens $ prefixVis vis (pretty var) <::> ty'
     Nothing -> pure $ prefixVis vis (pretty var)

   stripQuantifiers :: Type a -> ([(TypeVarVisibility,Text,Maybe (Type a))],Type a)
   stripQuantifiers = \case
     ForAll _ vis var mk inner _ -> first ((vis,var,mk):) $ stripQuantifiers inner
     other -> ([],other)

   goTypeApp :: Type a -> Type a -> Printer ann
   goTypeApp (TypeApp _ f a) b
     | eqType f tyFunction = do
         a' <- prettyType a
         b' <- prettyType b
         fmtSep [a' <+> arrow,b']
     | otherwise = do
         f' <- goTypeApp f a
         b' <- prettyType b
         pure $ parens $ f' <+> b'
   goTypeApp o ty@RCons{}
     | eqType o tyRecord =
         either openRecord recordLike =<< rowFields ty
   goTypeApp a b =  fmtSep =<< traverse prettyType [a,b]

   rowFields :: Type a -> Reader LineFormat (Either ([Doc ann], Doc ann) [Doc ann])
   rowFields = \case
         RCons _ lbl ty rest -> do
           fmt <- ask
           let f = ((pretty lbl <::> runPrinter fmt (prettyType ty)):)
           rest' <- rowFields rest
           pure $ bimap (first f) f rest'
         REmpty _ -> pure $ Right []
         KindApp _ REmpty{} _  -> pure $ Right [] -- REmpty is sometimes wrapped in a kind app
         TypeVar _ txt -> pure $ Left ([],pretty txt)
         other -> error $ "Malformed row fields: \n" <> prettyTypeStr other


-- TODO For debugging, remove later
smartRender ::  Doc ann -> Text
smartRender = renderStrict . layoutPretty defaultLayoutOptions

prettyTypeStr :: forall a. Show a => Type a -> String
prettyTypeStr = T.unpack . smartRender . asOneLine prettyType

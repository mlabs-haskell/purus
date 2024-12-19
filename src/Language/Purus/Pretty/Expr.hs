{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Language.Purus.Pretty.Expr where

import Prelude hiding ((<>))

import Control.Monad.Reader (MonadReader (ask), runReader)
import Data.Bifunctor (Bifunctor (..))
import Data.Map (Map)
import Data.Map qualified as M
import Data.Text (Text)
import Data.Text qualified as T

import Language.PureScript.AST.Literals (Literal (..))
import Language.PureScript.CoreFn.Binders (Binder (..))
import Language.PureScript.CoreFn.Expr (
  Bind (..),
  CaseAlternative (CaseAlternative),
  Expr (..),
  Guard,
 )
import Language.PureScript.CoreFn.Module
import Language.PureScript.Environment (
  DataDeclType (..),
  getFunArgTy,
 )
import Language.PureScript.Names (Ident, ModuleName, ProperName (..), disqualify, runIdent, showIdent, showQualified)
import Language.PureScript.PSString (PSString, decodeStringWithReplacement, prettyPrintString)

import Language.PureScript.CoreFn.TypeLike
import Language.PureScript.Environment (function, pattern (:->))
import Language.PureScript.Types (SourceType)
import Language.Purus.Pretty.Common (
  LineFormat (MultiLine, OneLine),
  Printer,
  analyzeApp,
  arrow,
  asDynamic,
  asOneLine,
  commaSep,
  fmtCat,
  fmtIndent,
  fmtSep,
  ignoreFmt,
  lam,
  oneLineList,
  printer,
  recordLike,
  (<//>),
  (<::>),
  (<:>),
  (<=>),
 )
import Language.Purus.Pretty.Types (prettyType)
import Prettyprinter (
  Doc,
  Pretty (pretty),
  align,
  colon,
  dot,
  flatAlt,
  group,
  hardline,
  hcat,
  hsep,
  indent,
  line,
  list,
  parens,
  punctuate,
  space,
  vcat,
  viaShow,
  vsep,
  (<+>),
  (<>),
 )

foldl1x :: (Foldable t) => String -> (a -> a -> a) -> t a -> a
foldl1x msg f xs
  | null xs = Prelude.error msg
  | otherwise = foldl1 f xs

exprType :: (Show a) => Expr a -> SourceType
exprType = \case
  Literal _ ty _ -> ty
  Accessor _ ty _ _ -> ty
  ObjectUpdate _ ty _ _ _ -> ty
  Abs _ ty _ _ -> ty
  App _ t1 t2 -> appType t1 t2
  Var _ ty __ -> ty
  Case _ ty _ _ -> ty
  Let _ _ e -> exprType e

appType :: (Show a) => Expr a -> Expr a -> SourceType
appType fe ae = case stripQuantifiers' fTy of
  ([], ft) ->
    let numArgs = length argTypes
     in foldl1x "appType first branch (CoreFn.Utils)" function . drop numArgs . splitFunTyParts $ ft
  (xs, ft) ->
    let funArgs = splitFunTyParts ft -- funArgTypes ft
        dict = mkInstanceMap M.empty xs argTypes funArgs
        numArgs = length argTypes
     in quantify
          . foldl1x "" function
          . drop numArgs
          . splitFunTyParts
          . replaceAllTypeVars (M.toList dict)
          $ ft
  where
    stripQuantifiers' :: SourceType -> ([Text], SourceType)
    stripQuantifiers' st = first (map (\(_, b, _) -> b)) $ stripQuantifiers st

    (f, args) = appFunArgs fe ae
    fTy = exprType f
    argTypes = exprType <$> args

    mkInstanceMap :: Map Text SourceType -> [Text] -> [SourceType] -> [SourceType] -> Map Text SourceType
    mkInstanceMap acc [] _ _ = acc
    mkInstanceMap acc _ [] _ = acc
    mkInstanceMap acc _ _ [] = acc
    mkInstanceMap acc (var : vars) (mt : mts) (pt : pts) = case instantiates var mt pt of
      Nothing ->
        mkInstanceMap acc [var] mts pts
          <> mkInstanceMap M.empty vars (mt : mts) (pt : pts)
      Just t -> mkInstanceMap (M.insert var t acc) vars (mt : mts) (pt : pts)

appFunArgs :: Expr a -> Expr a -> (Expr a, [Expr a])
appFunArgs f args = (appFun f, appArgs f args)
  where
    appArgs :: Expr a -> Expr a -> [Expr a]
    appArgs (App _ t1 t2) t3 = appArgs t1 t2 <> [t3]
    appArgs _ t3 = [t3]

    appFun :: Expr a -> Expr a
    appFun (App _ t1 _) = appFun t1
    appFun res = res

-- TODO: Pretty print the datatypes too
prettyModule :: (Pretty k, Pretty t, Pretty b) => Module b k t a -> Doc ann
prettyModule (Module _ _ modName modPath modImports modExports modReExports modForeign modDecls modDatatypes) =
  vsep
    [ pretty modName <+> parens (pretty modPath)
    , line <> "Imported Modules: " <> spacer
    , indent 2 . commaSep $ pretty . snd <$> modImports
    , line <> "Exports: " <> spacer
    , indent 2 . commaSep $ pretty <$> modExports -- hang 2?
    , line <> "Re-Exports: " <> spacer
    , indent 2 . commaSep $ goReExport <$> M.toList modReExports
    , line <> "Foreign: " <> spacer
    , indent 2 . commaSep . map pretty $ modForeign
    , line <> "Datatypes: " <> spacer
    , prettyDatatypes modDatatypes <> line
    , line <> "Declarations: " <> spacer
    , vcat . punctuate line $ pretty <$> modDecls
    ]
  where
    spacer = line <> pretty (T.pack $ replicate 30 '-')
    goReExport :: (ModuleName, [Ident]) -> Doc ann
    goReExport (mn', idents) = vcat $ flip map idents $ \i -> pretty mn' <> "." <> pretty i

instance (Pretty k, Pretty t) => Pretty (Datatypes k t) where
  pretty = prettyDatatypes

prettyDatatypes :: forall k t ann. (Pretty k, Pretty t) => Datatypes k t -> Doc ann
prettyDatatypes (Datatypes tDict _) = vcat . punctuate line $ map prettyDataDecl (M.elems tDict)

prettyDeclType :: DataDeclType -> Doc ann
prettyDeclType = \case
  Data -> "data"
  Newtype -> "newtype"

prefix :: Doc ann -> [Doc ann] -> [Doc ann]
prefix sep [] = []
prefix sep [x] = [x]
prefix sep (x : xs) = x : goPrefix xs
  where
    goPrefix [] = []
    goPrefix (y : ys) = (sep <> y) : goPrefix ys

instance (Pretty k, Pretty t) => Pretty (DataDecl k t) where
  pretty = prettyDataDecl

prettyDataDecl :: forall k t ann. (Pretty k, Pretty t) => DataDecl k t -> Doc ann
prettyDataDecl (DataDecl newtypeOrData qName args ctors) =
  let dType = prettyDeclType newtypeOrData
      tName = pretty $ runProperName (disqualify qName)
      mkArg :: (Text, k) -> Doc ann
      mkArg (txt, k) = parens (pretty txt <::> pretty k)
      dArgs = hsep $ mkArg <$> args
      dCtors = indent 2 . vcat $ prefix "| " $ prettyCtorDecl <$> ctors
   in dType <+> tName <+> dArgs <=> line <> dCtors

prettyCtorDecl :: (Pretty t) => CtorDecl t -> Doc ann
prettyCtorDecl (CtorDecl nm fs) =
  pretty (runIdent $ disqualify nm) <+> hsep (parens . pretty . snd <$> fs)

-- Is a printer for consistency mainly
prettyObjectKey :: PSString -> Printer ann
prettyObjectKey = pure . pretty . decodeStringWithReplacement

prettyObject :: forall a ann. (Show a) => [(PSString, Maybe (Expr a))] -> Printer ann
prettyObject fields = do
  fields' <- traverse prettyProperty fields
  recordLike fields'
  where
    prettyProperty :: (PSString, Maybe (Expr a)) -> Printer ann
    prettyProperty (key, value) = do
      key' <- prettyObjectKey key
      props' <- maybe (pure $ pretty @Text "_") prettyValue value
      pure (key' <:> props')

prettyUpdateEntry :: (Show a) => PSString -> Expr a -> Printer ann
prettyUpdateEntry key val = do
  key' <- prettyObjectKey key
  val' <- prettyValue val
  pure $ key' <=> val'

-- | Pretty-print an expression
prettyValue :: (Show a) => Expr a -> Printer ann
prettyValue (Accessor _ _ prop val) = do
  prop' <- prettyObjectKey prop
  val' <- prettyValueAtom val
  fmtCat [val', hcat [dot, prop']]
prettyValue (ObjectUpdate _ _ty o _copyFields ps) = do
  obj <- prettyValueAtom o
  updateEntries <- traverse goUpdateEntry ps >>= recordLike
  pure $ obj <+> updateEntries
  where
    goUpdateEntry = uncurry prettyUpdateEntry
prettyValue app@(App _ t1 t2) = case analyzeApp app of
  Just (fun, args) -> do
    atom <- fmtSep =<< traverse prettyValueAtom (fun : args)
    pure . group . align $ atom
  -- ty   <- prettyType $ appType t1 t2
  -- pure . group . align $ parens (atom <:> ty)
  {- TODO: change back
  ask >>= \case
  OneLine -> pure . group . align . hsep .  map (asOneLine prettyValueAtom) $ (fun:args)
  MultiLine -> pure . group . align . vsep . map (asDynamic prettyValueAtom) $ (fun:args) -}
  Nothing -> error "App isn't an App (impossible)"
prettyValue (Abs _ ty arg val) = do
  ty' <- prettyType (getFunArgTy ty)
  body' <- fmtIndent =<< prettyValue val
  pure $
    lam
      <> parens (align $ pretty (showIdent arg) <:> ty')
      <+> arrow
      <+> body'
-- TODO: Actually implement the one line bracketed format for case exps (I think PS is the same as Haskell?)
prettyValue (Case _ _ values binders) =
  pure $
    "case"
      <+> group (hsep scrutinees)
      <+> "of"
        <//> indent 2 (vcat $ map group branches)
  where
    scrutinees = asOneLine prettyValueAtom <$> values
    branches = group . asDynamic prettyCaseAlternative <$> binders
-- technically we could have a one line version of this but that's ugly af imo
prettyValue (Let _ ds val) =
  pure . align $
    vcat
      [ "let"
      , indent 2 . vcat $ asDynamic prettyDeclaration <$> ds
      , "in" <+> align (asDynamic prettyValue val)
      ]
prettyValue (Literal _ ty l) = ask >>= \case OneLine -> oneLine; MultiLine -> multiLine
  where
    -- No type anns for object literals (already annotated in the fields, makes too ugly)
    oneLine = pure . parens $ asOneLine prettyLiteralValue l <:> asOneLine prettyType ty
    multiLine = pure . parens $ asDynamic prettyLiteralValue l <:> asDynamic prettyType ty
prettyValue expr@Var {} = prettyValueAtom expr

-- | Pretty-print an atomic expression, adding parentheses if necessary.
prettyValueAtom :: forall a ann. (Show a) => Expr a -> Printer ann
prettyValueAtom lit@(Literal _ _ l) = prettyValue lit -- prettyLiteralValue l
prettyValueAtom (Var _ ty ident) =
  prettyType ty >>= \ty' ->
    pure . parens $ pretty (showIdent (disqualify ident)) <:> ty'
prettyValueAtom expr = do
  -- TODO change this back (need more anns for testing)
  v <- prettyValue expr
  -- t <- prettyType (exprType expr)
  pure $ parens v -- <:> t)

prettyLiteralValue :: forall a ann. (Show a) => Literal (Expr a) -> Printer ann
prettyLiteralValue (NumericLiteral n) = ignoreFmt $ pretty $ either show show n
prettyLiteralValue (StringLiteral s) = ignoreFmt $ pretty . T.unpack $ prettyPrintString s
prettyLiteralValue (CharLiteral c) = ignoreFmt $ viaShow . show $ c
prettyLiteralValue (BooleanLiteral True) = ignoreFmt "true"
prettyLiteralValue (BooleanLiteral False) = ignoreFmt "false"
prettyLiteralValue (ListLiteral xs) = printer oneLine multiLine
  where
    oneLine = oneLineList $ asOneLine prettyValue <$> xs
    -- N.B. I think it makes more sense to ensure that list *elements* are always oneLine
    multiLine = list $ asOneLine prettyValue <$> xs
prettyLiteralValue (ObjectLiteral ps) = prettyObject $ second Just `map` ps

instance (Show a) => Pretty (Bind a) where
  pretty = asDynamic id . prettyDeclaration

prettyDeclaration :: forall a ann. (Show a) => Bind a -> Printer ann
prettyDeclaration b = case b of
  NonRec _ ident expr -> goBind ident expr
  Rec bindings -> vcat <$> traverse (\((_, ident), expr) -> goBind ident expr) bindings
  where
    goBind :: Ident -> Expr a -> Printer ann
    goBind ident expr = do
      inner' <- goInner ident expr
      let ty' = asOneLine prettyType (exprType expr)
      pure $
        pretty ident <::> ty'
          <> hardline
          <> inner'
    goInner :: Ident -> Expr a -> Printer ann
    goInner ident expr = do
      fmt <- ask
      let ind docs = runReader (fmtIndent docs) fmt
          f g = pretty ident <=> g (asDynamic prettyValue expr)
      pure $ group $ flatAlt (f ind) (f id)

prettyCaseAlternative :: forall a ann. (Show a) => CaseAlternative a -> Printer ann
prettyCaseAlternative (CaseAlternative binders result) = do
  let binders' = asOneLine prettyBinderAtom <$> binders
  result' <- prettyResult result
  pure $ hsep binders' <> result'
  where
    prettyResult :: Either [(Guard a, Expr a)] (Expr a) -> Printer ann
    prettyResult = \case
      Left ges -> vcat <$> traverse prettyGuardedValueSep' ges
      Right exp' -> do
        body' <- prettyValue exp' >>= fmtIndent
        pure $ space <> arrow <+> body'

    prettyGuardedValueSep' :: (Guard a, Expr a) -> Printer ann
    prettyGuardedValueSep' (guardE, resultE) = do
      guardE' <- prettyValue guardE
      resultE' <- prettyValue resultE
      pure $ " | " <> guardE' <+> arrow <+> resultE'

prettyBinderAtom :: Binder a -> Printer ann
prettyBinderAtom (NullBinder _) = pure "_"
prettyBinderAtom (LiteralBinder _ l) = prettyLiteralBinder l
prettyBinderAtom (VarBinder _ ident ty) = do
  ty' <- prettyType ty
  pure $ parens (pretty ident <::> ty')
prettyBinderAtom (ConstructorBinder _ _ ctor []) = pure . pretty $ runProperName (disqualify ctor)
prettyBinderAtom b@ConstructorBinder {} = prettyBinder b
prettyBinderAtom (NamedBinder _ ident binder) = do
  binder' <- prettyBinder binder
  pure $ pretty ident <> "@" <> binder'

prettyLiteralBinder :: Literal (Binder a) -> Printer ann
prettyLiteralBinder (StringLiteral str) = pure . pretty $ prettyPrintString str
prettyLiteralBinder (CharLiteral c) = pure $ viaShow c
prettyLiteralBinder (NumericLiteral num) = pure $ either pretty pretty num
prettyLiteralBinder (BooleanLiteral True) = pure "true"
prettyLiteralBinder (BooleanLiteral False) = pure "false"
prettyLiteralBinder (ObjectLiteral bs) = recordLike =<< traverse prettyObjectPropertyBinder bs
  where
    prettyObjectPropertyBinder :: (PSString, Binder a) -> Printer ann
    prettyObjectPropertyBinder (key, binder) = do
      key' <- prettyObjectKey key
      binder' <- prettyBinder binder
      pure $ key' <:> binder'
prettyLiteralBinder (ListLiteral bs) = list <$> traverse prettyBinder bs

prettyBinder :: Binder a -> Printer ann
prettyBinder (ConstructorBinder _ _ ctor []) = pure . pretty $ runProperName (disqualify ctor)
prettyBinder (ConstructorBinder _ _ ctor args) = do
  args' <- fmtSep =<< traverse prettyBinderAtom args
  pure $ pretty (runProperName (disqualify ctor)) <+> args' -- fmtSep fmt (asFmt fmt prettyBinderAtom <$> args)
prettyBinder b = prettyBinderAtom b

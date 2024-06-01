{-# LANGUAGE TypeApplications, ScopedTypeVariables #-}
module Language.PureScript.CoreFn.Pretty.Expr where


import Prelude hiding ((<>))

import Data.Text (Text)
import Data.Text qualified as T
import Data.Map qualified as M
import Data.Bifunctor (Bifunctor (..))
import Control.Monad.Reader ( MonadReader(ask), runReader )

import Language.PureScript.Environment
    ( getFunArgTy )
import Language.PureScript.CoreFn.Expr
    ( Guard,
      Bind(..),
      CaseAlternative(CaseAlternative),
      Expr(..) )
import Language.PureScript.CoreFn.Module ( Module(Module) )
import Language.PureScript.CoreFn.Utils
import Language.PureScript.AST.Literals ( Literal(..) )
import Language.PureScript.CoreFn.Binders ( Binder(..) )
import Language.PureScript.Names (ProperName(..), disqualify, showIdent, Ident, ModuleName)
import Language.PureScript.PSString (PSString, prettyPrintString, decodeStringWithReplacement)

import Prettyprinter
    ( (<>),
      list,
      viaShow,
      colon,
      parens,
      dot,
      hardline,
      (<+>),
      punctuate,
      indent,
      line,
      space,
      vcat,
      hcat,
      vsep,
      hsep,
      flatAlt,
      align,
      group,
      Doc,
      Pretty(pretty) )
import Language.PureScript.CoreFn.Pretty.Common
    ( Printer,
      LineFormat(MultiLine, OneLine),
      asOneLine,
      asDynamic,
      ignoreFmt,
      fmtSep,
      fmtCat,
      fmtIndent,
      printer,
      recordLike,
      commaSep,
      (<:>),
      (<::>),
      (<=>),
      (<//>),
      arrow,
      lam,
      oneLineList,
      analyzeApp )
import Language.PureScript.CoreFn.Pretty.Types ( prettyType )

-- TODO: DataDecls
prettyModule :: Module (Bind a) a -> Doc ann
prettyModule (Module _ _ modName modPath modImports modExports modReExports modForeign modDecls _) =
  vsep
    [ pretty modName <+>  parens (pretty modPath)
    , "Imported Modules: "
    , indent 2 . commaSep $ pretty . snd  <$>  modImports
    ,"Exports: "
    , indent 2 . commaSep $ pretty <$> modExports -- hang 2?
    , "Re-Exports: "
    , indent 2 . commaSep $ goReExport <$> M.toList modReExports
    , "Foreign: "
    , indent 2 . commaSep . map pretty $  modForeign
    , "Declarations: "
    , vcat . punctuate line $  asDynamic prettyDeclaration  <$> modDecls
    ]
 where
   goReExport :: (ModuleName,[Ident]) -> Doc ann
   goReExport (mn',idents) = vcat $ flip map idents $ \i -> pretty mn' <> "." <> pretty i

-- Is a printer for consistency mainly
prettyObjectKey :: PSString -> Printer ann
prettyObjectKey = pure . pretty . decodeStringWithReplacement

prettyObject ::  [(PSString, Maybe (Expr a))] -> Printer ann
prettyObject fields  = do
  fields' <- traverse prettyProperty fields
  recordLike fields'
  where
    prettyProperty :: (PSString, Maybe (Expr a)) -> Printer ann
    prettyProperty  (key, value) = do
      key' <- prettyObjectKey key
      props' <- maybe (pure $ pretty @Text "_") prettyValue value
      pure (key' <:> props')

prettyUpdateEntry :: PSString -> Expr a -> Printer ann
prettyUpdateEntry key val  = do
  key' <- prettyObjectKey key
  val' <- prettyValue val
  pure $ key' <=> val'

-- | Pretty-print an expression
prettyValue :: Expr a -> Printer ann
prettyValue (Accessor _ _ prop val)  =  do
  prop' <- prettyObjectKey prop
  val' <- prettyValueAtom val
  fmtCat [val',hcat[dot,prop']]
prettyValue (ObjectUpdate _ _ty o _copyFields ps)  = do
  obj <- prettyValueAtom o
  updateEntries <- traverse goUpdateEntry ps >>= recordLike
  pure $ obj <+> updateEntries
  where
    goUpdateEntry = uncurry prettyUpdateEntry
prettyValue app@(App _  t1 t2)  = case analyzeApp app of
  Just (fun,args) -> do
    atom <- fmtSep =<< traverse prettyValueAtom (fun:args)
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
  pure $ lam
         <> parens (align $ pretty (showIdent arg) <:> ty')
         <+> arrow
         <+> body'
-- TODO: Actually implement the one line bracketed format for case exps (I think PS is the same as Haskell?)
prettyValue (Case _ _ values binders) = pure $
  "case"
  <+> group (hsep scrutinees)
  <+> "of"
  <//> indent 2 (vcat $ map group branches)
 where
   scrutinees = asOneLine prettyValueAtom <$> values
   branches = group . asDynamic  prettyCaseAlternative <$> binders
-- technically we could have a one line version of this but that's ugly af imo
prettyValue (Let _  ds val)  = pure . align $ vcat [
  "let",
  indent 2 . vcat $ asDynamic prettyDeclaration <$> ds,
  "in" <+> align (asDynamic  prettyValue val)
  ]
prettyValue (Literal _ ty l)  = ask >>= \case {OneLine ->  oneLine; MultiLine -> multiLine}
  where
    -- No type anns for object literals (already annotated in the fields, makes too ugly)
    oneLine =  pure . parens $ asOneLine prettyLiteralValue l <:> asOneLine prettyType ty
    multiLine =  pure . parens $ asDynamic prettyLiteralValue l <:> asDynamic prettyType ty

prettyValue expr@Var{}  = prettyValueAtom expr

-- | Pretty-print an atomic expression, adding parentheses if necessary.
prettyValueAtom :: Expr a -> Printer ann
prettyValueAtom lit@(Literal _  _ l)  = prettyValue lit  -- prettyLiteralValue l
prettyValueAtom (Var _ ty ident)  =  prettyType ty >>= \ty' ->
  pure . parens $ pretty  (showIdent (disqualify ident)) <:> ty'
prettyValueAtom expr = do -- TODO change this back (need more anns for testing)
  v <- prettyValue expr
  -- t <- prettyType (exprType expr)
  pure $ parens v -- <:> t)

prettyLiteralValue :: Literal (Expr a) -> Printer ann
prettyLiteralValue (NumericLiteral n) = ignoreFmt $ pretty $ either show show n
prettyLiteralValue (StringLiteral s) = ignoreFmt $ pretty . T.unpack $ prettyPrintString s
prettyLiteralValue (CharLiteral c) = ignoreFmt $ viaShow . show $ c
prettyLiteralValue (BooleanLiteral True) = ignoreFmt "true"
prettyLiteralValue (BooleanLiteral False) = ignoreFmt "false"
prettyLiteralValue (ArrayLiteral xs) = printer oneLine multiLine
  where
    oneLine = oneLineList $  asOneLine prettyValue <$>  xs
    -- N.B. I think it makes more sense to ensure that list *elements* are always oneLine
    multiLine = list $ asOneLine prettyValue <$> xs
prettyLiteralValue (ObjectLiteral ps) = prettyObject  $ second Just `map` ps

prettyDeclaration :: forall a ann. Bind a -> Printer ann
prettyDeclaration b  = case b of
  NonRec _ ident expr -> goBind ident expr
  Rec bindings -> vcat <$> traverse  (\((_,ident),expr) -> goBind ident expr) bindings
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
     let ind docs =  runReader (fmtIndent docs) fmt
         f g = pretty ident <=> g (asDynamic prettyValue  expr)
     pure $ group $ flatAlt (f ind)  (f id)

prettyCaseAlternative ::  forall a ann. CaseAlternative a -> Printer ann
prettyCaseAlternative (CaseAlternative binders result)  = do
  let binders' =  asOneLine prettyBinderAtom <$> binders
  result'  <- prettyResult result
  pure $ hsep binders' <> result'
  where
  prettyResult :: Either [(Guard a, Expr a)] (Expr a) -> Printer ann
  prettyResult = \case
    Left ges -> vcat <$>  traverse prettyGuardedValueSep'   ges
    Right exp' -> do
      body' <- prettyValue exp' >>= fmtIndent
      pure $ space <> arrow <+> body'

  prettyGuardedValueSep' :: (Guard a, Expr a) -> Printer ann
  prettyGuardedValueSep' (guardE, resultE) = do
    guardE' <- prettyValue guardE
    resultE' <- prettyValue resultE
    pure $ " | " <> guardE'  <+> arrow  <+> resultE'




prettyBinderAtom :: Binder a -> Printer ann
prettyBinderAtom (NullBinder _) = pure "_"
prettyBinderAtom (LiteralBinder _ l) = prettyLiteralBinder l
prettyBinderAtom (VarBinder _ ident) = pure $ pretty ident
prettyBinderAtom (ConstructorBinder _ _ ctor []) = pure . pretty $ runProperName (disqualify ctor)
prettyBinderAtom b@ConstructorBinder{} = prettyBinder b
prettyBinderAtom (NamedBinder _ ident binder)= do
  binder' <- prettyBinder binder
  pure $ pretty ident <> "@" <> binder'

prettyLiteralBinder ::  Literal (Binder a) -> Printer ann
prettyLiteralBinder (StringLiteral str)  = pure . pretty $ prettyPrintString str
prettyLiteralBinder (CharLiteral c) = pure $ viaShow c
prettyLiteralBinder (NumericLiteral num) = pure $ either pretty pretty num
prettyLiteralBinder (BooleanLiteral True) = pure "true"
prettyLiteralBinder (BooleanLiteral False) = pure "false"
prettyLiteralBinder (ObjectLiteral bs) =  recordLike =<< traverse prettyObjectPropertyBinder bs
  where
  prettyObjectPropertyBinder :: (PSString, Binder a) -> Printer ann
  prettyObjectPropertyBinder (key, binder) = do
    key' <- prettyObjectKey key
    binder' <- prettyBinder binder
    pure $ key' <:> binder'
prettyLiteralBinder (ArrayLiteral bs) = list <$> traverse prettyBinder bs

prettyBinder :: Binder a -> Printer ann
prettyBinder (ConstructorBinder _ _ ctor [])  = pure . pretty $ runProperName (disqualify ctor)
prettyBinder (ConstructorBinder _ _ ctor args) = do
  args' <- fmtSep =<< traverse prettyBinderAtom args
  pure $ pretty (runProperName (disqualify ctor)) <+> args' -- fmtSep fmt (asFmt fmt prettyBinderAtom <$> args)
prettyBinder b = prettyBinderAtom b

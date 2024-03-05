{-# LANGUAGE TypeApplications, ScopedTypeVariables, RecordWildCards #-}
module Language.PureScript.CoreFn.Pretty (
  writeModule,
  ppType,
  prettyTypeStr,
  renderExprStr,
  prettyModuleTxt
) where

import Prelude hiding ((<>))

import Data.Text (Text)
import Data.Text qualified as T
import Data.Map qualified as M
import Data.Bifunctor (first, Bifunctor (..))
import Control.Monad.Reader

import Language.PureScript.Environment
    ( tyRecord, tyFunction, getFunArgTy )
import Language.PureScript.CoreFn.Expr
    ( exprType,
      Guard,
      Bind(..),
      CaseAlternative(CaseAlternative),
      Expr(..) )
import Language.PureScript.CoreFn.Module ( Module(Module) )
import Language.PureScript.AST.Literals ( Literal(..) )
import Language.PureScript.CoreFn.Binders ( Binder(..) )
import Language.PureScript.Label (Label (..))
import Language.PureScript.Names (OpName(..), ProperName(..), disqualify, runModuleName, showIdent, Ident, ModuleName, showQualified)
import Language.PureScript.Types (Type (..), WildcardData (..), TypeVarVisibility (..), eqType)
import Language.PureScript.PSString (PSString, prettyPrintString, decodeStringWithReplacement)
import System.IO (Handle)

import Prettyprinter
    ( (<>),
      tupled,
      layoutSmart,
      defaultLayoutOptions,
      layoutPretty,
      list,
      viaShow,
      colon,
      parens,
      dot,
      brackets,
      hardline,
      (<+>),
      rbrace,
      lbrace,
      rparen,
      lparen,
      pipe,
      comma,
      punctuate,
      enclose,
      indent,
      line,
      softline,
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
import Prettyprinter.Render.Text ( renderIO, renderStrict )

{- Rewritten prettyprinter that uses a modern printer library & is less convoluted.

   We primarily need this for writing the "prettified" CoreFn files for development purposes.
   The existing printer is extremely difficult to modify for our needs (e.g. there isn't a clear way to force
   an expression or type to print on one line). Because reading the CoreFn output is necessary
   to ensure correctness, it's important that we get get something legible.

-}


data LineFormat
  = OneLine --  *DEFINITELY* Print on one line, even if doing so exceeds the page width
  | MultiLine --  *Possibly* Print multiple lines.
  deriving (Show, Eq)

-- TODO: Refactor to reader monad?
type Printer ann =  Reader LineFormat (Doc ann)

type Formatter = forall a ann. (a -> Printer ann) -> a -> Doc ann

runPrinter :: LineFormat -> Printer ann -> Doc ann
runPrinter fmt p = runReader p fmt

asFmt :: LineFormat -> (a -> Printer ann) -> a -> Doc ann
asFmt fmt f x = case fmt of
  OneLine -> asOneLine f x
  MultiLine -> asDynamic f x

asOneLine :: Formatter
asOneLine p x = runPrinter OneLine (p x)

asDynamic :: Formatter
asDynamic p x = group $ align $ flatAlt (runPrinter MultiLine (p x)) (runPrinter OneLine (p x))

ignoreFmt :: Doc ann -> Printer ann
ignoreFmt doc = printer doc doc

fmtSep :: [Doc ann] -> Printer ann
fmtSep docs = ask >>= \case
  OneLine -> pure $ hsep docs
  MultiLine -> pure $ vsep docs

fmtCat :: [Doc ann] -> Printer ann
fmtCat docs = ask >>= \case
  OneLine -> pure $ hcat docs
  MultiLine -> pure $ vcat docs

fmtSpacer :: Printer  ann
fmtSpacer = ask >>= \case
  OneLine -> pure space
  MultiLine -> pure softline

fmtIndent :: Doc ann -> Printer ann
fmtIndent doc = ask >>= \case
  OneLine -> pure doc
  MultiLine -> pure $ line <> indent 2 doc

printer :: Doc ann -> Doc ann -> Printer ann
printer one multi = ask >>= \case
  OneLine ->  pure one
  MultiLine ->  pure multi

withOpenRow :: forall ann. Doc ann -> Doc ann -> ([Doc ann],Doc ann) -> Printer ann
withOpenRow l r (fields,open) = do
  spacer <- fmtSpacer
  fmtFields <- fmtSep $ punctuate comma fields'
  pure . group . align $ enclose (l <> spacer) (spacer <> r) fmtFields
  where
    fields' =  foldr (\x acc -> case acc of
                      [] -> [hsep [x,pipe <++> open]]
                      xs -> x : xs
                    ) [] fields

openRow :: ([Doc ann], Doc ann) -> Printer ann
openRow = withOpenRow lparen rparen

openRecord :: ([Doc ann], Doc ann) -> Printer ann
openRecord = withOpenRow lbrace rbrace

recordLike ::  [Doc ann] -> Printer ann
recordLike  fields  = do
  spacer <- fmtSpacer
  fields' <- fmtSep $ punctuate comma fields
  pure $ enclose (lbrace <> spacer) (space <> rbrace) fields'

record :: [Doc ann] -> Printer ann
record = recordLike

object :: [Doc ann] -> Printer ann
object = recordLike

commaSep :: [Doc ann] -> Doc ann
commaSep = vsep . punctuate comma

indent' :: Int -> Doc ann -> Doc ann
indent' i doc = group . align $ flatAlt (indent i doc) doc

parens' :: Doc ann -> Doc ann
parens' d = group $ align $ enclose (lparen <> softline) (rparen <> softline) d


-- TODO: Remove
ppType :: Show a => Int -> Type a -> String
ppType i t = prettyTypeStr t

instance Pretty Ident where
  pretty = pretty . showIdent

instance Pretty PSString where
  pretty = pretty . decodeStringWithReplacement

instance Pretty ModuleName where
  pretty = pretty . runModuleName

instance Pretty Label where
  pretty = pretty . runLabel

(<:>) :: Doc ann -> Doc ann -> Doc ann
a <:> b = hcat [a,":"] <++> b

(<::>) :: Doc ann -> Doc ann -> Doc ann
a <::> b = a <++> "::" <++> b

(<=>) :: Doc ann -> Doc ann -> Doc ann
a <=> b = a <+> "=" <+> b

(<//>) :: Doc ann -> Doc ann -> Doc ann
a <//> b = a <+> hardline <+> b

-- ensures the things being concatenated are always on the same line
(<++>) :: Doc ann -> Doc ann -> Doc ann
a <++> b = hsep [a,b]

arrow :: Doc ann
arrow = "->"

lam :: Doc ann
lam = "\\"

oneLineList :: [Doc ann] -> Doc ann
oneLineList = brackets . hcat . punctuate (comma <> space)

-- helpers to ensure even formatting of applications
analyzeApp :: Expr a -> Maybe (Expr a,[Expr a])
analyzeApp t = (,appArgs t) <$> appFun t
  where
    appArgs :: Expr a -> [Expr a]
    appArgs (App _ _ t1 t2) = appArgs t1 <> [t2]
    appArgs _  = []

    appFun :: Expr a -> Maybe (Expr a)
    appFun (App _ _ t1 _) = go t1
      where
        go (App _ _ tx _) = case appFun tx of
          Nothing -> Just tx
          Just tx' -> Just tx'
        go other = Just other
    appFun _ = Nothing


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
      pure (key' <:> props') --  prettyObjectKey key <:> maybe (pretty @Text "_") (flip prettyValue fmt) value

prettyUpdateEntry :: PSString -> Expr a -> Printer ann
prettyUpdateEntry key val  = do
  key' <- prettyObjectKey key
  val' <- prettyValue val
  pure $ key' <=> val'

-- | Pretty-print an expression
prettyValue :: Expr a -> Printer ann
-- prettyValue _ | d < 0 = text "..."
prettyValue (Accessor _ ty prop val)  =  do
  prop' <- prettyObjectKey prop
  val' <- prettyValueAtom val
  fmtCat [val',hcat[dot,prop']]
prettyValue (ObjectUpdate ann _ty o _copyFields ps)  = do
  obj <- prettyValueAtom o
  updateEntries <- traverse goUpdateEntry ps >>= recordLike
  pure $ obj <+> updateEntries -- prettyValueAtom o   <+> recordLike ( goUpdateEntry  <$> ps) fmt
  where
    goUpdateEntry = uncurry prettyUpdateEntry
prettyValue app@(App ann ty val arg)  = case analyzeApp app of
  Just (fun,args) -> ask >>= \case
    OneLine -> pure . group . align . hsep .  map (asOneLine prettyValueAtom) $ (fun:args)
    MultiLine -> pure . group . align . vcat . map (asDynamic prettyValueAtom) $ (fun:args)
  Nothing -> error "App isn't an App (impossible)"

prettyValue (Abs ann ty arg val) = do
  ty' <- prettyType (getFunArgTy ty)
  body' <- fmtIndent =<< prettyValue val
  pure $ lam
         <> parens (align $ pretty (showIdent arg) <:> ty')
         <+> arrow
         <+> body'
      {-   lam
      <> parens (align $ pretty (showIdent arg) <:> prettyType (getFunArgTy ty) fmt)
      <+> arrow
      <+> fmtIndent fmt (asFmt fmt prettyValue val)
      -}
-- TODO: Actually implement the one line bracketed format for case exps (I think PS is the same as Haskell?)
prettyValue (Case ann ty values binders) = pure $
  "case"
  <+> group (hsep scrutinees)
  <+> "of"
  <//> indent 2 (vcat $ map group branches)
 where
   scrutinees = asOneLine prettyValueAtom <$> values
   branches = group . asDynamic  prettyCaseAlternative <$> binders
-- technically we could have a one line version of this but that's ugly af
prettyValue (Let _ _  ds val)  = pure . align $ vcat [
  "let",
  indent 2 . vcat $ asDynamic prettyDeclaration <$> ds,
  "in" <+> align (asDynamic  prettyValue val)
  ]
prettyValue (Literal _ ty l)  = ask >>= \case {OneLine -> pure oneLine; MultiLine -> pure multiLine}
  where
    oneLine = parens $ hcat [
      asOneLine prettyLiteralValue l,
      colon,
      space,
      asOneLine prettyType ty
      ]
    multiLine = parens $ asDynamic prettyLiteralValue l <:> asDynamic prettyType ty
prettyValue expr@Constructor{}  = prettyValueAtom  expr
prettyValue expr@Var{}  = prettyValueAtom  expr

-- | Pretty-print an atomic expression, adding parentheses if necessary.
prettyValueAtom :: Expr a -> Printer ann
prettyValueAtom (Literal _  _ l)  = prettyLiteralValue l
prettyValueAtom (Constructor _ _ _ name _) = pure . pretty $ T.unpack $ runProperName name
prettyValueAtom (Var ann ty ident)  =  prettyType ty >>= \ty' ->
  pure . parens $ pretty  (showIdent (disqualify ident)) <:> ty'
prettyValueAtom expr =  parens <$> prettyValue expr

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
-- REVIEW: Maybe we don't want to ignore the format?
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
-- prettyCaseAlternative d _ | d < 0 = ellipsis
prettyCaseAlternative (CaseAlternative binders result)  = do
  binders' <- traverse prettyBinderAtom binders
  result'  <- prettyResult result
  pure $ hsep binders' <> result' -- hsep (asFmt fmt prettyBinderAtom  <$> binders) <> prettyResult result
  where
  prettyResult :: Either [(Guard a, Expr a)] (Expr a) -> Printer ann
  prettyResult = \case
    Left ges -> vcat <$>  traverse prettyGuardedValueSep'   ges
    Right exp' -> do
      body' <- prettyValue exp' >>= fmtIndent
      pure $ space <> arrow <+> body'
      -- space <> arrow <+> fmtIndent fmt (prettyValue  exp' fmt)

  prettyGuardedValueSep' :: (Guard a, Expr a) -> Printer ann
  prettyGuardedValueSep' (guardE, resultE) = do
    guardE' <- prettyValue guardE
    resultE' <- prettyValue resultE
    pure $ " | " <> guardE'  <+> arrow  <+> resultE'


prettyModule :: Module a -> Doc ann
prettyModule (Module modSS modComments modName modPath modImports modExports modReExports modForeign modDecls) =
  vsep  $
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

smartRender ::  Doc ann -> Text
smartRender = renderStrict . layoutPretty defaultLayoutOptions

writeModule :: Handle -> Module a -> IO ()
writeModule h m = renderIO h
                . layoutSmart defaultLayoutOptions
                $ prettyModule m

prettyModuleTxt :: Module a -> Text
prettyModuleTxt = renderStrict  . layoutPretty defaultLayoutOptions .  prettyModule

renderExpr :: Expr a -> Text
renderExpr = smartRender . asDynamic prettyValue

renderExprStr :: Expr a -> String
renderExprStr = T.unpack . renderExpr

prettyTypeStr :: forall a. Show a => Type a -> String
prettyTypeStr = T.unpack . smartRender . asOneLine prettyType

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
prettyLiteralBinder (ObjectLiteral bs) =  object =<< traverse prettyObjectPropertyBinder bs
  where
  prettyObjectPropertyBinder :: (PSString, Binder a) -> Printer ann
  prettyObjectPropertyBinder (key, binder) = do
    key' <- prettyObjectKey key
    binder' <- prettyBinder binder
    pure $ key' <:> binder'
prettyLiteralBinder (ArrayLiteral bs) = list <$> traverse prettyBinder bs

-- |
-- Generate a pretty-printed string representing a Binder
--
prettyBinder :: Binder a -> Printer ann
prettyBinder (ConstructorBinder _ _ ctor [])  = pure . pretty $ runProperName (disqualify ctor)
prettyBinder (ConstructorBinder _ _ ctor args) = do
  args' <- fmtSep =<< traverse prettyBinderAtom args
  pure $ pretty (runProperName (disqualify ctor)) <+> args' -- fmtSep fmt (asFmt fmt prettyBinderAtom <$> args)
prettyBinder b = prettyBinderAtom b


{- TYPES (move later) -}

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

  TypeOp a opName -> pure . pretty $ showQualified runOpName opName

  TypeApp _ t1 t2 -> goTypeApp t1 t2

  KindApp a k1 k2 -> do
    k1' <- prettyType k1
    k2' <- prettyType k2
    pure $ k1' <> ("@" <> k2' )

  ForAll _ vis var mKind inner' _ -> case stripQuantifiers inner' of
    (quantified,inner) ->  goForall ([(vis,var,mKind)] <> quantified) inner

  ConstrainedType _ constraint inner -> error "TODO: ConstrainedType (shouldn't ever appear in Purus CoreFn)"

  Skolem _ txt mKind inner mSkolScope -> error "TODO: Skolem (shouldn't ever appear in Purus CoreFn)"

  REmpty _ -> pure "{}"

  rcons@RCons{} -> either openRow (pure . tupled) =<< rowFields rcons

  -- this might be backwards
  KindedType a ty kind -> do
    ty' <- prettyType ty
    kind' <- prettyType kind
    pure .  parens $  ty' <::> kind' -- prettyType ty fmt <::> prettyType kind fmt

  -- not sure what this is?
  BinaryNoParensType a op l r -> do
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
         -- fmtSep fmt [prettyType a fmt <+> arrow, prettyType b fmt]
     | otherwise = do
         f' <- goTypeApp f a
         b' <- prettyType b
         pure $ parens $ f' <+> b'
   goTypeApp o ty@RCons{}
     | eqType o tyRecord =
         -- TODO: Rows aren't records -_-
         either openRecord record =<< rowFields ty

   goTypeApp a b =  fmtSep =<< traverse prettyType [a,b] -- [prettyType a fmt,prettyType b fmt]

   rowFields :: Type a -> Reader LineFormat (Either ([Doc ann], Doc ann) [Doc ann])
   rowFields = \case
         RCons _ lbl ty rest -> do
           fmt <- ask
           let f = ((pretty lbl <::> runPrinter fmt (prettyType ty)):)
           rest' <- rowFields rest
           pure $ bimap (first f) f rest'
         REmpty _ -> pure $ Right []
         KindApp _ REmpty{} _  -> pure $ Right [] -- REmpty is sometimes wrapped in a kind app?
         TypeVar _ txt -> pure $ Left ([],pretty txt)
         other -> error $ "Malformed row fields: \n" <> show other

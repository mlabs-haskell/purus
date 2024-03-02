{-# LANGUAGE TypeApplications, ScopedTypeVariables, RecordWildCards #-}
module Language.PureScript.CoreFn.Pretty where

import Prelude hiding ((<>))



import Data.Text (Text)
import Data.List.NonEmpty qualified as NEL
import Data.Monoid qualified as Monoid ((<>))
import Data.Text qualified as T

import Language.PureScript.Environment
import Language.PureScript.CoreFn.Expr
import Language.PureScript.CoreFn.Module
import Language.PureScript.AST.Literals
import Language.PureScript.CoreFn.Binders
import Language.PureScript.Crash (internalError)
import Language.PureScript.Names (OpName(..), ProperName(..), Qualified(..), disqualify, runModuleName, showIdent, Ident, ModuleName, showQualified)
import Language.PureScript.Pretty.Common (before, beforeWithSpace, parensT)
import Language.PureScript.Types (Constraint(..), Type (..), WildcardData (..), TypeVarVisibility (..), eqType)
import Language.PureScript.PSString (PSString, prettyPrintString, decodeStringWithReplacement)
import System.IO (Handle)

import Data.Map qualified as M

import Prettyprinter
import Prettyprinter.Render.Text
import qualified Prettyprinter.Render.String as STR
import Data.Bifunctor (first, Bifunctor (..))
import Language.PureScript.Label (Label (..))
import Control.Monad (void)

data LineFormat
  = OneLine --  *DEFINITELY* Print on one line, even if doing so exceeds the page width
  | MultiLine --  *Possibly* Print multiple lines.
  deriving (Show, Eq)

-- TODO: Refactor to reader monad?
type Printer ann =  LineFormat  -> Doc ann

type Formatter = forall a ann. (a -> Printer ann) -> a -> Doc ann

runPrinter :: LineFormat -> Printer ann -> Doc ann
runPrinter fmt p = p fmt

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

fmtSep :: LineFormat -> [Doc ann] -> Doc ann
fmtSep = \case
  OneLine -> hsep
  MultiLine -> vsep

fmtCat :: LineFormat -> [Doc ann] -> Doc ann
fmtCat = \case
  OneLine -> hcat
  MultiLine -> vcat

fmtSpacer :: LineFormat -> Doc ann
fmtSpacer = \case
  OneLine -> space
  MultiLine -> softline

fmtIndent :: LineFormat -> Doc ann -> Doc ann
fmtIndent = \case
  OneLine -> id
  MultiLine -> \doc -> line <> indent 2 doc

printer :: Doc ann -> Doc ann -> Printer ann
printer one multi = \case
  OneLine -> one
  MultiLine -> multi

withOpenRow :: forall ann. Doc ann -> Doc ann -> ([Doc ann],Doc ann) -> Printer ann
withOpenRow l r (fields,open) fmt =  group $ align $ enclose (l <> spacer) (spacer <> r) $ fmtSep fmt  $ punctuate comma fields'
  where
    spacer = fmtSpacer fmt
    fields' =  foldr (\x acc -> case acc of
                      [] -> [hsep [x,pipe <++> open]]
                      xs -> x : xs
                    ) [] fields

openRow :: ([Doc ann], Doc ann) -> Printer ann
openRow = withOpenRow lparen rparen

openRecord :: ([Doc ann], Doc ann) -> Printer ann
openRecord = withOpenRow lbrace rbrace

recordLike ::  [Doc ann] -> Printer ann
recordLike  fields fmt   =
  enclose (lbrace <> spacer) (space <> rbrace)
  . fmtSep fmt
  . punctuate comma
  $ fields
 where
   spacer = fmtSpacer fmt
-- let fmtObj = encloseSep (lbrace <> softline) (softline <> rbrace) (comma <> softline)
-- in  group $ align (fmtObj fields)

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


-- I can't figure out why their type pretty printer mangles record types, this is an incredibly stupid temporary hack
ppType :: Int -> Type a -> String
ppType i t = "<TYPE>" {- go [] $ prettyType i t
  where
    go :: String -> String -> String
    go acc [] = acc
    go acc (' ':xs) = case dropWhile (== ' ') xs of
      [] -> acc
      more -> go (acc `mappend` [' ']) more
    go acc (x:xs) = go (acc `mappend` [x]) xs
-}

instance Pretty Ident where
  pretty = pretty . showIdent

instance Pretty PSString where
  pretty = pretty . decodeStringWithReplacement

instance Pretty ModuleName where
  pretty = pretty . runModuleName

instance Pretty Label where
  pretty = pretty . runLabel

ellipsis :: Doc ann
ellipsis =  "..."

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

doubleColon :: Doc ann
doubleColon = hcat [colon,colon]

prettyObjectKey :: PSString -> Doc ann
prettyObjectKey = pretty . decodeStringWithReplacement

prettyObject ::  [(PSString, Maybe (Expr a))] -> Printer ann
prettyObject fields fmt = recordLike (prettyProperty  <$> fields) fmt
  where
    prettyProperty :: (PSString, Maybe (Expr a)) -> Doc ann
    prettyProperty  (key, value) = prettyObjectKey key <:> maybe (pretty @Text "_") (flip prettyValue fmt) value

prettyUpdateEntry :: PSString -> Expr a -> Printer ann
prettyUpdateEntry key val fmt =  prettyObjectKey key <=> prettyValue val fmt

-- | Pretty-print an expression
prettyValue :: Expr a -> Printer ann
-- prettyValue _ | d < 0 = text "..."
prettyValue (Accessor _ ty prop val) fmt =  fmtCat fmt [prettyValueAtom val fmt,hcat[dot,prettyObjectKey prop]]
prettyValue (ObjectUpdate ann _ty o _copyFields ps) fmt = asFmt fmt prettyValueAtom o   <+> recordLike ( goUpdateEntry  <$> ps) fmt
  where
    goUpdateEntry (str,e) =  prettyUpdateEntry str e fmt
prettyValue (App ann ty val arg) fmt =  group . align $ fmtSep fmt  [prettyValueAtom val fmt, prettyValueAtom arg fmt]

prettyValue (Abs ann ty arg val) fmt  =
         lam
      <> parens (align $ pretty (showIdent arg) <:> prettyType (getFunArgTy ty) fmt)
      <+> arrow
      <+> fmtIndent fmt (asFmt fmt prettyValue val)
     -- <> fmtSpacer fmt
     -- <> hang 2 (asFmt fmt prettyValue val)

-- TODO: Actually implement the one line bracketed format for case exps (I think PS is the same as Haskell?)
prettyValue (Case ann ty values binders) _ =
  "case"
  <+> group (hsep scrutinees)
  <+> "of"
  <//> indent 2 (vcat $ map group branches)
 where
   scrutinees = asOneLine prettyValueAtom <$> values
   branches = group . asDynamic  prettyCaseAlternative <$> binders
-- technically we could have a one line version of this but that's ugly af
prettyValue (Let _ _  ds val) fmt = align $ vcat [
  "let",
  indent 2 . vcat $ asDynamic prettyDeclaration <$> ds,
  "in" <+> align (asDynamic  prettyValue val)
  ]
 where
   prefix = case fmt of
     OneLine -> align
     MultiLine -> (line <>) . indent 2
prettyValue (Literal _ ty l) fmt =  case fmt of {OneLine -> oneLine; MultiLine -> multiLine}
  where
    oneLine = parens $ hcat [
      asOneLine prettyLiteralValue l,
      colon,
      space,
      asOneLine prettyType ty
      ]
    multiLine = parens $ asDynamic prettyLiteralValue l <:> asDynamic prettyType ty
prettyValue expr@Constructor{} fmt = prettyValueAtom  expr fmt
prettyValue expr@Var{} fmt = prettyValueAtom  expr fmt

-- | Pretty-print an atomic expression, adding parentheses if necessary.
prettyValueAtom :: Expr a -> Printer ann
prettyValueAtom (Literal _  _ l) fmt = prettyLiteralValue l fmt
prettyValueAtom (Constructor _ _ _ name _) _ = pretty $ T.unpack $ runProperName name
prettyValueAtom (Var ann ty ident) fmt  =   parens $  pretty  (showIdent (disqualify ident)) <:> prettyType ty fmt
prettyValueAtom expr fmt =  prettyValue expr fmt

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
prettyDeclaration b fmt = case b of
  NonRec _ ident expr -> goBind ident expr
  Rec bindings -> vcat $ map  (\((_,ident),expr) -> goBind ident expr) bindings
 where
   goBind :: Ident -> Expr a -> Doc ann
   goBind ident expr =
     pretty ident <::> asOneLine prettyType (exprType expr)
     <> hardline
     <> goInner ident expr
   goInner :: Ident -> Expr a -> Doc ann
   goInner ident expr =
     let f g = pretty ident <=> g (asDynamic prettyValue  expr)
     in group $ flatAlt (f  (fmtIndent fmt))  (f id)

prettyCaseAlternative ::  forall a ann. CaseAlternative a -> Printer ann
-- prettyCaseAlternative d _ | d < 0 = ellipsis
prettyCaseAlternative (CaseAlternative binders result) fmt =
  hsep (asFmt fmt prettyBinderAtom  <$> binders) <> prettyResult result
  where
  prettyResult :: Either [(Guard a, Expr a)] (Expr a) -> Doc ann
  prettyResult = \case
    Left ges -> vcat $  map prettyGuardedValueSep'   ges
    Right exp' -> space <> arrow <+> fmtIndent fmt (prettyValue  exp' fmt)

  prettyGuardedValueSep' :: (Guard a, Expr a) -> Doc ann
  prettyGuardedValueSep' (guardE, resultE) = " | " <> prettyValue  guardE fmt <+> arrow  <+> prettyValue  resultE fmt


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

prettyModuleStr :: Module a -> String
prettyModuleStr = STR.renderString . layoutPretty defaultLayoutOptions . prettyModule

renderExpr :: Expr a -> Text
renderExpr = smartRender . asDynamic prettyValue

renderExprStr :: Expr a -> String
renderExprStr = T.unpack . renderExpr

prettyTypeStr :: forall a. Show a => Type a -> String
prettyTypeStr = T.unpack . smartRender . asOneLine prettyType

prettyBinderAtom :: Binder a -> Printer ann
prettyBinderAtom (NullBinder _) _ = "_"
prettyBinderAtom (LiteralBinder _ l) fmt = prettyLiteralBinder l fmt
prettyBinderAtom (VarBinder _ ident) _ = pretty ident
prettyBinderAtom (ConstructorBinder _ _ ctor []) _ = pretty $ runProperName (disqualify ctor)
prettyBinderAtom b@ConstructorBinder{} fmt = prettyBinder b fmt
prettyBinderAtom (NamedBinder _ ident binder) fmt = pretty ident <> "@" <> prettyBinder binder fmt

prettyLiteralBinder ::  Literal (Binder a) -> Printer ann
prettyLiteralBinder (StringLiteral str) _  = pretty $ prettyPrintString str
prettyLiteralBinder (CharLiteral c) _ = viaShow c
prettyLiteralBinder (NumericLiteral num) _ = either pretty pretty num
prettyLiteralBinder (BooleanLiteral True) _ = "true"
prettyLiteralBinder (BooleanLiteral False) _ = "false"
prettyLiteralBinder (ObjectLiteral bs) fmt = asFmt fmt object $ prettyObjectPropertyBinder <$> bs
  where
  prettyObjectPropertyBinder :: (PSString, Binder a) -> Doc ann
  prettyObjectPropertyBinder (key, binder) = prettyObjectKey key <:> prettyBinder binder fmt
prettyLiteralBinder (ArrayLiteral bs) fmt = list (asFmt fmt prettyBinder  <$> bs)

-- |
-- Generate a pretty-printed string representing a Binder
--
prettyBinder :: Binder a -> Printer ann
prettyBinder (ConstructorBinder _ _ ctor []) fmt = pretty $ runProperName (disqualify ctor)
prettyBinder (ConstructorBinder _ _ ctor args) fmt =
  pretty (runProperName (disqualify ctor)) <+> fmtSep fmt (asFmt fmt prettyBinderAtom <$> args)
prettyBinder b fmt= prettyBinderAtom b fmt


{- TYPES (move later) -}

prettyType :: forall a ann. Show a => Type a -> Printer ann
prettyType t fmt =  group $ case t of
  TUnknown _ n -> "t" <> pretty n

  TypeVar _ txt -> pretty txt

  TypeLevelString _ pss -> pretty . prettyPrintString $ pss

  TypeLevelInt _ i -> pretty i

  TypeWildcard _ wcd -> case wcd of
    HoleWildcard txt -> "?" <> pretty txt
    _ -> "_"

  TypeConstructor _ qPropName -> pretty . runProperName . disqualify $ qPropName

  TypeOp a opName -> pretty $ showQualified runOpName opName

  TypeApp _ t1 t2 -> goTypeApp t1 t2

  KindApp a k1 k2 -> prettyType k1 fmt <> ("@" <> prettyType k2 fmt)

  ForAll _ vis var mKind inner' _ -> case stripQuantifiers inner' of
    (quantified,inner) ->  goForall ([(vis,var,mKind)] <> quantified) inner

  ConstrainedType _ constraint inner -> error "TODO: ConstrainedType"

  Skolem _ txt mKind inner mSkolScope -> error "TODO: Skolem"

  REmpty _ -> "{}"

  rcons@RCons{} -> either (asFmt fmt openRow) tupled $ rowFields rcons

  -- this might be backwards
  KindedType a ty kind -> parens $ prettyType ty fmt <::> prettyType kind fmt

  -- not sure what this is?
  BinaryNoParensType a op l r -> prettyType l fmt <+> prettyType op fmt  <+> prettyType r fmt

  ParensInType _ ty -> parens (prettyType ty fmt)
 where

   goForall :: [(TypeVarVisibility,Text,Maybe (Type a))] -> Type a -> Doc ann
   goForall xs inner = "forall" <+> fmtCat fmt (renderBoundVar <$> xs) <> "." <+> prettyType inner fmt

   prefixVis :: TypeVarVisibility -> Doc ann -> Doc ann
   prefixVis vis tv = case vis of
     TypeVarVisible -> hcat ["@",tv]
     TypeVarInvisible -> tv

   renderBoundVar :: (TypeVarVisibility, Text, Maybe (Type a)) -> Doc ann
   renderBoundVar (vis,var,mk) =  case mk of
     Just k -> parens $ prefixVis vis (pretty var) <::> prettyType k fmt
     Nothing -> prefixVis vis (pretty var)

   stripQuantifiers :: Type a -> ([(TypeVarVisibility,Text,Maybe (Type a))],Type a)
   stripQuantifiers = \case
     ForAll _ vis var mk inner _ -> first ((vis,var,mk):) $ stripQuantifiers inner
     other -> ([],other)

   goTypeApp :: Type a -> Type a -> Doc ann
   goTypeApp (TypeApp _ f a) b
     | eqType f tyFunction = fmtSep fmt [prettyType a fmt <+> arrow, prettyType b fmt]
     | otherwise = parens $ goTypeApp f a  <+> prettyType b fmt
   goTypeApp o ty@RCons{}
     | eqType o tyRecord = either (asFmt fmt openRecord) (asFmt fmt record) $ rowFields ty
   goTypeApp a b =  fmtSep fmt [prettyType a fmt,prettyType b fmt]

   rowFields :: Type a -> Either ([Doc ann], Doc ann) [Doc ann]
   rowFields = \case
         RCons _ lbl ty rest ->
           let f = ((pretty lbl <::> prettyType ty fmt):)
           in bimap (first f) f $ rowFields rest
         REmpty _ -> Right []
         KindApp _ REmpty{} _  -> Right [] -- REmpty is sometimes wrapped in a kind app?
         TypeVar _ txt -> Left ([],pretty txt)
         other -> error $ "Malformed row fields: \n" <> show other

{-# LANGUAGE TypeApplications, ScopedTypeVariables #-}
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



withOpenRow :: forall ann. Doc ann -> Doc ann -> ([Doc ann],Doc ann) -> Doc ann
withOpenRow l r (fields,open) =  group $ align $ enclose (l <> softline) (softline <> r) $ hsep $ punctuate comma fields'
  where
    fields' =  foldr (\x acc -> case acc of
                      [] -> [hsep [x,pipe <++> open]]
                      xs -> x : xs
                    ) [] fields

openRow :: ([Doc ann], Doc ann) -> Doc ann
openRow = withOpenRow lparen rparen

openRecord :: ([Doc ann], Doc ann) -> Doc ann
openRecord = withOpenRow lbrace rbrace

recordLike ::  [Doc ann] -> Doc ann
recordLike  fields  =
  let fmtObj = encloseSep (lbrace <> softline) (softline <> rbrace) (comma <> softline)
  in  group $ align (fmtObj fields)

record :: [Doc ann] -> Doc ann
record = recordLike

object :: [Doc ann] -> Doc ann
object = recordLike

commaSep :: [Doc ann] -> Doc ann
commaSep = vsep . punctuate comma

indent' :: Int -> Doc ann -> Doc ann
indent' i doc = group . align $ flatAlt (indent i doc) doc

parens' :: Doc ann -> Doc ann
parens' d = group $ align $ enclose (lparen <> softline) (rparen <> softline) d


-- I can't figure out why their type pretty printer mangles record types, this is an incredibly stupid temporary hack
ppType :: Int -> Type a -> String
ppType i t = "<TYPE>" {- go [] $ prettyPrintType i t
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
a <//> b = a <+> line <+> b

-- ensures the things being concatenated are always on the same line
(<++>) :: Doc ann -> Doc ann -> Doc ann
a <++> b = hsep [a,b]

arrow :: Doc ann
arrow = "->"

lam :: Doc ann
lam = "\\"

doubleColon :: Doc ann
doubleColon = hcat [colon,colon]

caseOf :: [Doc ann] -> [Doc ann] -> Doc ann
caseOf scrutinees branches = "case" <+> group (hsep scrutinees) <+> "of" <//> indent 2 (vcat . map group $ branches) -- if wrong try hang instead of hang

prettyPrintObjectKey :: PSString -> Doc ann
prettyPrintObjectKey = pretty . decodeStringWithReplacement

prettyPrintObject ::  [(PSString, Maybe (Expr a))] -> Doc ann
prettyPrintObject  = encloseSep "{"  "}" "," . map prettyPrintObjectProperty
  where
  prettyPrintObjectProperty :: (PSString, Maybe (Expr a)) -> Doc ann
  prettyPrintObjectProperty (key, value) = (prettyPrintObjectKey key Monoid.<> ": ") <> maybe (pretty @Text "_") prettyPrintValue value

prettyPrintUpdateEntry :: PSString -> Expr a -> Doc ann
prettyPrintUpdateEntry key val =  prettyPrintObjectKey key <+> "=" <+> prettyPrintValue val

-- | Pretty-print an expression
prettyPrintValue ::  Expr a -> Doc ann
-- prettyPrintValue _ | d < 0 = text "..."
prettyPrintValue (Accessor _ ty prop val) = group . align $ vcat [prettyPrintValueAtom  val,hcat[dot,prettyPrintObjectKey prop]]
prettyPrintValue (ObjectUpdate ann _ty o _copyFields ps) = prettyPrintValueAtom  o <+> encloseSep "{" "}" "," (uncurry prettyPrintUpdateEntry <$> ps)
prettyPrintValue (App ann ty val arg) = group . align $ vsep [prettyPrintValueAtom  val,prettyPrintValueAtom  arg]
prettyPrintValue (Abs ann ty arg val) = group . align $ flatAlt multiLine oneLine
  where
    multiLine = lam
      <> parens (align $ pretty (showIdent arg) <:> prettyType (getFunArgTy ty))
      <+> arrow
      <> hardline
      <> hang 2 (prettyPrintValue  val)

    oneLine = lam
      <> parens (align $ pretty (showIdent arg) <:> prettyType (getFunArgTy ty))
      <+> arrow
      <+> prettyPrintValue val

prettyPrintValue (Case ann ty values binders) =
  caseOf (prettyPrintValueAtom <$> values) (prettyPrintCaseAlternative <$> binders)
prettyPrintValue (Let _ _  ds val) =  mappend line $ indent 2 $ vcat [
  "let",
  indent 2 $ vcat $  prettyPrintDeclaration <$> ds,
  "in" <+> align (prettyPrintValue val)
  ]

prettyPrintValue (Literal _ ty l) = parens $  prettyPrintLiteralValue l <:> prettyType ty
prettyPrintValue expr@Constructor{} = prettyPrintValueAtom  expr
prettyPrintValue expr@Var{} = prettyPrintValueAtom  expr

-- | Pretty-print an atomic expression, adding parentheses if necessary.
prettyPrintValueAtom :: Expr a -> Doc ann
prettyPrintValueAtom (Literal _  _ l) = prettyPrintLiteralValue l
prettyPrintValueAtom (Constructor _ _ _ name _) = pretty $ T.unpack $ runProperName name
prettyPrintValueAtom (Var ann ty ident) =  parens $   pretty  (showIdent (disqualify ident)) <:> prettyType ty
prettyPrintValueAtom expr =  (prettyPrintValue expr)

prettyPrintLiteralValue :: Literal (Expr a) -> Doc ann
prettyPrintLiteralValue (NumericLiteral n) = pretty $ either show show n
prettyPrintLiteralValue (StringLiteral s) = pretty . T.unpack $ prettyPrintString s
prettyPrintLiteralValue (CharLiteral c) = viaShow . show $ c
prettyPrintLiteralValue (BooleanLiteral True) = "true"
prettyPrintLiteralValue (BooleanLiteral False) = "false"
prettyPrintLiteralValue (ArrayLiteral xs) = list $ prettyPrintValue <$>  xs
prettyPrintLiteralValue (ObjectLiteral ps) = prettyPrintObject  $ second Just `map` ps

prettyPrintDeclaration :: Bind a -> Doc ann
-- prettyPrintDeclaration d _ | d < 0 = ellipsis
prettyPrintDeclaration b = case b of
  NonRec _ ident expr -> vcat  [
       pretty ident <::> prettyType (exprType expr),
       pretty ident <=>  prettyPrintValue expr -- not sure about the d here
    ]
  Rec bindings -> vcat $ concatMap (\((_,ident),expr) -> [
          pretty ident <::> prettyType (exprType expr),
          pretty ident <=> prettyPrintValue  expr
      ]) bindings

prettyPrintCaseAlternative ::  forall a ann. CaseAlternative a -> Doc ann
-- prettyPrintCaseAlternative d _ | d < 0 = ellipsis
prettyPrintCaseAlternative (CaseAlternative binders result) =
  hsep (map prettyPrintBinderAtom binders) <> prettyPrintResult result
  where
  prettyPrintResult :: Either [(Guard a, Expr a)] (Expr a) -> Doc ann
  prettyPrintResult = \case
    Left ges -> vcat $  map prettyPrintGuardedValueSep'  ges
    Right exp' -> space <> arrow <+> prettyPrintValue  exp'

  prettyPrintGuardedValueSep' ::  (Guard a, Expr a) -> Doc ann
  prettyPrintGuardedValueSep'  (guardE, resultE) =
    " | " <> prettyPrintValue  guardE <+> arrow  <+> prettyPrintValue  resultE


prettyPrintModule :: Module a -> Doc ann
prettyPrintModule (Module modSS modComments modName modPath modImports modExports modReExports modForeign modDecls) =
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
    , vcat . punctuate line $  prettyPrintDeclaration  <$> modDecls
    ]
 where
   goReExport :: (ModuleName,[Ident]) -> Doc ann
   goReExport (mn',idents) = vcat $ flip map idents $ \i -> pretty mn' <> "." <> pretty i

smartRender ::  Doc ann -> Text
smartRender = renderStrict . layoutPretty defaultLayoutOptions

writeModule :: Handle -> Module a -> IO ()
writeModule h m = renderIO h
                . layoutSmart defaultLayoutOptions
                $ prettyPrintModule m

prettyPrintModuleTxt :: Module a -> Text
prettyPrintModuleTxt = renderStrict  . layoutPretty defaultLayoutOptions .  prettyPrintModule

prettyPrintModuleStr :: Module a -> String
prettyPrintModuleStr = STR.renderString . layoutPretty defaultLayoutOptions . prettyPrintModule

renderExpr :: Expr a -> Text
renderExpr = smartRender . prettyPrintValue

renderExprStr :: Expr a -> String
renderExprStr = T.unpack . renderExpr

prettyTypeStr :: forall a. Show a => Type a -> String
prettyTypeStr = T.unpack . smartRender . prettyType

prettyPrintBinderAtom :: Binder a -> Doc ann
prettyPrintBinderAtom (NullBinder _) = "_"
prettyPrintBinderAtom (LiteralBinder _ l) = prettyPrintLiteralBinder l
prettyPrintBinderAtom (VarBinder _ ident) = pretty ident
prettyPrintBinderAtom (ConstructorBinder _ _ ctor []) = pretty $ runProperName (disqualify ctor)
prettyPrintBinderAtom b@ConstructorBinder{} = prettyPrintBinder b
prettyPrintBinderAtom (NamedBinder _ ident binder) = pretty ident <> "@" <> prettyPrintBinder binder

prettyPrintLiteralBinder :: Literal (Binder a) -> Doc ann
prettyPrintLiteralBinder (StringLiteral str) = pretty $ prettyPrintString str
prettyPrintLiteralBinder (CharLiteral c) = viaShow c
prettyPrintLiteralBinder (NumericLiteral num) = either pretty pretty num
prettyPrintLiteralBinder (BooleanLiteral True) = "true"
prettyPrintLiteralBinder (BooleanLiteral False) = "false"
prettyPrintLiteralBinder (ObjectLiteral bs) = object $ prettyPrintObjectPropertyBinder <$> bs
  where
  prettyPrintObjectPropertyBinder :: (PSString, Binder a) -> Doc ann
  prettyPrintObjectPropertyBinder (key, binder) = prettyPrintObjectKey key <:> prettyPrintBinder binder
prettyPrintLiteralBinder (ArrayLiteral bs) = list (prettyPrintBinder <$> bs)

-- |
-- Generate a pretty-printed string representing a Binder
--
prettyPrintBinder :: Binder a -> Doc ann
prettyPrintBinder (ConstructorBinder _ _ ctor []) = pretty $ runProperName (disqualify ctor)
prettyPrintBinder (ConstructorBinder _ _ ctor args) =
  pretty (runProperName (disqualify ctor)) <+> hcat (prettyPrintBinderAtom <$> args)
prettyPrintBinder b = prettyPrintBinderAtom b


{- TYPES (move later) -}

prettyType :: forall a ann. Show a => Type a -> Doc ann
prettyType t=  group $ case t of
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

  KindApp a k1 k2 -> prettyType k1 <> ("@" <> prettyType k2)

  ForAll _ vis var mKind inner' _ -> case stripQuantifiers inner' of
    (quantified,inner) ->  goForall ([(vis,var,mKind)] <> quantified) inner

  ConstrainedType _ constraint inner -> error "TODO: ConstrainedType"

  Skolem _ txt mKind inner mSkolScope -> error "TODO: Skolem"

  REmpty _ -> "{}"

  rcons@RCons{} -> either openRow tupled $ rowFields rcons

  -- this might be backwards
  KindedType a ty kind -> parens $ prettyType ty <::> prettyType kind

  -- not sure what this is?
  BinaryNoParensType a op l r -> prettyType l <++> prettyType op <++> prettyType r

  ParensInType _ ty -> parens (prettyType ty)
 where
   goForall :: [(TypeVarVisibility,Text,Maybe (Type a))] -> Type a -> Doc ann
   goForall xs inner = "forall" <++> hcat (renderBoundVar <$> xs) <> "." <++> prettyType inner

   prefixVis :: TypeVarVisibility -> Doc ann -> Doc ann
   prefixVis vis tv = case vis of
     TypeVarVisible -> hcat ["@",tv]
     TypeVarInvisible -> tv

   renderBoundVar :: (TypeVarVisibility, Text, Maybe (Type a)) -> Doc ann
   renderBoundVar (vis,var,mk) =  case mk of
     Just k -> parens $ prefixVis vis (pretty var) <::> prettyType k
     Nothing -> prefixVis vis (pretty var)

   stripQuantifiers :: Type a -> ([(TypeVarVisibility,Text,Maybe (Type a))],Type a)
   stripQuantifiers = \case
     ForAll _ vis var mk inner _ -> first ((vis,var,mk):) $ stripQuantifiers inner
     other -> ([],other)

   goTypeApp :: Type a -> Type a -> Doc ann
   goTypeApp (TypeApp _ f a) b
     | eqType f tyFunction = prettyType a <++> arrow <++> prettyType b
     | otherwise = parens $ goTypeApp f a  <++> prettyType b
   goTypeApp o ty@RCons{}
     | eqType o tyRecord = either openRecord record $ rowFields ty
   goTypeApp a b =  prettyType a <++> prettyType b

   rowFields :: Type a -> Either ([Doc ann], Doc ann) [Doc ann]
   rowFields = \case
         RCons _ lbl ty rest ->
           let f = ((pretty lbl <::> prettyType ty):)
           in bimap (first f) f $ rowFields rest
         REmpty _ -> Right []
         KindApp _ REmpty{} _  -> Right [] -- REmpty is sometimes wrapped in a kind app?
         TypeVar _ txt -> Left ([],pretty txt)
         other -> error $ "Malformed row fields: \n" <> show other

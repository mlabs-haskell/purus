module Language.PureScript.CoreFn.Pretty where

import Prelude hiding ((<>))

import Control.Arrow (second)

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
import Language.PureScript.Names (OpName(..), ProperName(..), Qualified(..), disqualify, runModuleName, showIdent, Ident, ModuleName)
import Language.PureScript.Pretty.Common (before, beforeWithSpace, parensT)
import Language.PureScript.Pretty.Types ( typeAsBox, typeAtomAsBox, prettyPrintObjectKey)
import Language.PureScript.Types (Constraint(..), Type)
import Language.PureScript.PSString (PSString, prettyPrintString)

import Text.PrettyPrint.Boxes (Box, left, moveRight, text, vcat, hcat, vsep, (//), (<>), render)
import Language.PureScript.Pretty.Types
import Data.Map qualified as M

-- I can't figure out why their type pretty printer mangles record types, this is an incredibly stupid temporary hack
ppType :: Int -> Type a -> String
ppType i t = go [] $ prettyPrintType i t
  where
    go :: String -> String -> String
    go acc [] = acc
    go acc (' ':xs) = case dropWhile (== ' ') xs of
      [] -> acc
      more -> go (acc `mappend` [' ']) more
    go acc (x:xs) = go (acc `mappend` [x]) xs

textT :: Text -> Box
textT = text . T.unpack

oneLine :: String -> String
oneLine = filter (/= '\n')

-- | Render an aligned list of items separated with commas
list :: Char -> Char -> (a -> Box) -> [a] -> Box
list open close _ [] = text [open, close]
list open close f xs = vcat left (zipWith toLine [0 :: Int ..] xs ++ [ text [ close ] ])
  where
  toLine i a = text [ if i == 0 then open else ',', ' ' ] <> f a


hlist :: Char -> Char -> (a -> Box) -> [a] -> Box
hlist open close _ [] = text [open, close]
hlist open close f xs = hcat left (zipWith toLine [0 :: Int ..] xs ++ [ text [ close ] ])
  where
  toLine i a = text [ if i == 0 then open else ',', ' ' ] <> f a


ellipsis :: Box
ellipsis = text "..."

prettyPrintObject :: Int -> [(PSString, Maybe (Expr a))] -> Box
prettyPrintObject d = hlist '{' '}' prettyPrintObjectProperty
  where
  prettyPrintObjectProperty :: (PSString, Maybe (Expr a)) -> Box
  prettyPrintObjectProperty (key, value) = textT (prettyPrintObjectKey key Monoid.<> ": ") <> maybe (text "_") (prettyPrintValue (d - 1)) value

prettyPrintUpdateEntry :: Int -> PSString -> Expr a -> Box
prettyPrintUpdateEntry d key val = textT (prettyPrintObjectKey key) <> text " = " <> prettyPrintValue (d - 1) val

-- | Pretty-print an expression
prettyPrintValue :: Int -> Expr a -> Box
-- prettyPrintValue d _ | d < 0 = text "..."
prettyPrintValue d (Accessor _ ty prop val) = prettyPrintValueAtom (d - 1) val `before` textT ("." Monoid.<> prettyPrintObjectKey prop)
prettyPrintValue d (ObjectUpdate ann _ty o _copyFields ps) = prettyPrintValueAtom (d - 1) o `beforeWithSpace` list '{' '}' (uncurry (prettyPrintUpdateEntry d)) ps
prettyPrintValue d (App ann ty val arg) = prettyPrintValueAtom (d - 1) val `beforeWithSpace` prettyPrintValueAtom (d - 1) arg
prettyPrintValue d (Abs ann ty arg val) = text (oneLine $ '\\' : "(" ++ T.unpack (showIdent arg) ++ ": " ++ ppType (d) (getFunArgTy ty) ++ ") -> ") //  (prettyPrintValue (d-1)  val)
prettyPrintValue d (Case ann ty values binders) =
  (text "case " <> foldr beforeWithSpace (text "of") (map (prettyPrintValueAtom (d - 1)) values)) //
    moveRight 2 (vcat left (map (prettyPrintCaseAlternative (d - 1)) binders))
prettyPrintValue d (Let _ _  ds val) =
  text "let" //
    moveRight 2 (vcat left (map (prettyPrintDeclaration (d - 1)) ds)) //
    (text "in " <> prettyPrintValue (d - 1) val)
-- TODO: constraint kind args
prettyPrintValue d (Literal _ ty l) = text "(" <> prettyPrintLiteralValue d l <> ": " <> text (oneLine (ppType 100 ty)) <> text ")"
prettyPrintValue d expr@Constructor{} = prettyPrintValueAtom d expr
prettyPrintValue d expr@Var{} = prettyPrintValueAtom d expr

-- | Pretty-print an atomic expression, adding parentheses if necessary.
prettyPrintValueAtom :: Int -> Expr a -> Box
prettyPrintValueAtom d (Literal _  _ l) = prettyPrintLiteralValue d l
prettyPrintValueAtom _ (Constructor _ _ _ name _) = text $ T.unpack $ runProperName name
prettyPrintValueAtom d (Var ann ty ident) = text . oneLine $ "(" ++  T.unpack (showIdent (disqualify ident)) ++ ": " ++ ppType d ty ++  ")"
prettyPrintValueAtom d expr = (text "(" <> prettyPrintValue d expr) `before` text ")"

prettyPrintLiteralValue :: Int -> Literal (Expr a) -> Box
prettyPrintLiteralValue _ (NumericLiteral n) = text $ either show show n
prettyPrintLiteralValue _ (StringLiteral s) = text $ T.unpack $ prettyPrintString s
prettyPrintLiteralValue _ (CharLiteral c) = text $ show c
prettyPrintLiteralValue _ (BooleanLiteral True) = text "true"
prettyPrintLiteralValue _ (BooleanLiteral False) = text "false"
prettyPrintLiteralValue d (ArrayLiteral xs) = list '[' ']' (prettyPrintValue (d - 1)) xs
prettyPrintLiteralValue d (ObjectLiteral ps) = prettyPrintObject (d - 1) $ second Just `map` ps

prettyPrintDeclaration :: Int -> Bind a -> Box
-- prettyPrintDeclaration d _ | d < 0 = ellipsis
prettyPrintDeclaration d b = case b of
  NonRec _ ident expr ->
    vcat left [
      text (oneLine $ T.unpack (showIdent ident) ++ " :: " ++ ppType 0 (exprType expr)  ),
      text (T.unpack (showIdent ident) ++ " = ") <> prettyPrintValue d expr -- not sure about the d here

    ]
  Rec bindings -> vsep 1 left $ map (\((_,ident),expr) ->
        vcat left [
          text (oneLine $ T.unpack (showIdent ident) ++ " :: " ++ ppType 0 (exprType expr)  ),
          text (T.unpack (showIdent ident) ++ " = ") <> prettyPrintValue (d-1) expr

      ]) bindings

prettyPrintCaseAlternative :: Int -> CaseAlternative a -> Box
-- prettyPrintCaseAlternative d _ | d < 0 = ellipsis
prettyPrintCaseAlternative d (CaseAlternative binders result) =
  text (T.unpack (T.unwords (map prettyPrintBinderAtom binders))) <> prettyPrintResult result
  where
  prettyPrintResult :: Either [(Guard a, Expr a)] (Expr a) -> Box
  prettyPrintResult = \case
    Left ges -> vcat left $  map (prettyPrintGuardedValueSep' (text " | ")) ges
    Right exp -> text " -> " <> prettyPrintValue (d-1) exp

  prettyPrintGuardedValueSep' :: Box -> (Guard a, Expr a) -> Box
  prettyPrintGuardedValueSep' sep (guardE, resultE) =
    prettyPrintValue (d-1) guardE <> text " -> " <> prettyPrintValue (d-1) resultE


prettyPrintModule :: Module a -> Box
prettyPrintModule (Module modSS modComments modName modPath modImports modExports modReExports modForeign modDecls) =
  vcat left $
    [text (show modName ++ " (" ++ modPath ++ ")")]
    ++ ["Imported Modules: "]
    ++ map (moveRight 2 . text . show . snd) modImports
    ++ ["Exports: "]
    ++ map (moveRight 2 . text . T.unpack . showIdent) modExports
    ++ ["Re-Exports: "]
    ++ map (moveRight 2 . goReExport) (M.toList modReExports)
    ++ ["Foreign: "]
    ++ map (moveRight 2. text . T.unpack . showIdent) modForeign
    ++ ["Declarations: "]
    ++ map (prettyPrintDeclaration 0) modDecls
 where
   goReExport :: (ModuleName,[Ident]) -> Box
   goReExport (mn,idents) = vcat left $ flip map idents $ \i -> text (show mn ++ "." ++ T.unpack (showIdent i))

prettyPrintModule' :: Module a -> String
prettyPrintModule' = render . prettyPrintModule

renderExpr :: Int -> Expr a -> String
renderExpr i e = render $ prettyPrintValue i e
{-
  prettyPrintResult [GuardedExpr [] v] = text " -> " <> prettyPrintValue (d - 1) v
  prettyPrintResult gs =
    vcat left (map (prettyPrintGuardedValueSep (text " | ")) gs)

  prettyPrintGuardedValueSep :: Box -> GuardedExpr -> Box
  prettyPrintGuardedValueSep _ (GuardedExpr [] val) =
    text " -> " <> prettyPrintValue (d - 1) val

  prettyPrintGuardedValueSep sep (GuardedExpr [guard] val) =
    foldl1 before [ sep
                  , prettyPrintGuard guard
                  , prettyPrintGuardedValueSep sep (GuardedExpr [] val)
                  ]

  prettyPrintGuardedValueSep sep (GuardedExpr (guard : guards) val) =
    vcat left [ foldl1 before
                [ sep
                , prettyPrintGuard guard
                ]
              , prettyPrintGuardedValueSep (text " , ") (GuardedExpr guards val)
              ]

  prettyPrintGuard (ConditionGuard cond) =
    prettyPrintValue (d - 1) cond
  prettyPrintGuard (PatternGuard binder val) =
    foldl1 before
    [ text (T.unpack (prettyPrintBinder binder))
    , text " <- "
    , prettyPrintValue (d - 1) val
    ]
-}

prettyPrintBinderAtom :: Binder a -> Text
prettyPrintBinderAtom (NullBinder _) = "_"
prettyPrintBinderAtom (LiteralBinder _ l) = prettyPrintLiteralBinder l
prettyPrintBinderAtom (VarBinder _ ident) = showIdent ident
prettyPrintBinderAtom (ConstructorBinder _ _ ctor []) = runProperName (disqualify ctor)
prettyPrintBinderAtom b@ConstructorBinder{} = parensT (prettyPrintBinder b)
prettyPrintBinderAtom (NamedBinder _ ident binder) = showIdent ident Monoid.<> "@" Monoid.<> prettyPrintBinder binder

prettyPrintLiteralBinder :: Literal (Binder a) -> Text
prettyPrintLiteralBinder (StringLiteral str) = prettyPrintString str
prettyPrintLiteralBinder (CharLiteral c) = T.pack (show c)
prettyPrintLiteralBinder (NumericLiteral num) = either (T.pack . show) (T.pack . show) num
prettyPrintLiteralBinder (BooleanLiteral True) = "true"
prettyPrintLiteralBinder (BooleanLiteral False) = "false"
prettyPrintLiteralBinder (ObjectLiteral bs) =
  "{ "
  Monoid.<> T.intercalate ", " (map prettyPrintObjectPropertyBinder bs)
  Monoid.<> " }"
  where
  prettyPrintObjectPropertyBinder :: (PSString, Binder a) -> Text
  prettyPrintObjectPropertyBinder (key, binder) = prettyPrintObjectKey key Monoid.<> ": " Monoid.<> prettyPrintBinder binder
prettyPrintLiteralBinder (ArrayLiteral bs) =
  "[ "
  Monoid.<> T.intercalate ", " (map prettyPrintBinder bs)
  Monoid.<> " ]"

-- |
-- Generate a pretty-printed string representing a Binder
--
prettyPrintBinder :: Binder a -> Text
prettyPrintBinder (ConstructorBinder _ _ ctor []) = runProperName (disqualify ctor)
prettyPrintBinder (ConstructorBinder _ _ ctor args) = runProperName (disqualify ctor) Monoid.<> " " Monoid.<> T.unwords (map prettyPrintBinderAtom args)
prettyPrintBinder b = prettyPrintBinderAtom b

module Language.PureScript.CoreFn.Typed.Pretty where

import Prelude hiding ((<>))

import Control.Arrow (second)

import Data.Text (Text)
import Data.List.NonEmpty qualified as NEL
import Data.Monoid qualified as Monoid ((<>))
import Data.Text qualified as T

import Language.PureScript.Environment
import Language.PureScript.CoreFn
import Language.PureScript.Crash (internalError)
import Language.PureScript.Names (OpName(..), ProperName(..), Qualified(..), disqualify, runModuleName, showIdent, Ident, ModuleName)
import Language.PureScript.Pretty.Common (before, beforeWithSpace, parensT)
import Language.PureScript.Pretty.Types (typeAsBox, typeAtomAsBox, prettyPrintObjectKey)
import Language.PureScript.Types (Constraint(..), Type)
import Language.PureScript.PSString (PSString, prettyPrintString)

import Text.PrettyPrint.Boxes (Box, left, moveRight, text, vcat, hcat, vsep, (//), (<>), render)
import Language.PureScript.Pretty.Types
import Data.Map qualified as M

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

ellipsis :: Box
ellipsis = text "..."

prettyPrintObject :: Int -> [(PSString, Maybe (Expr (Type ())))] -> Box
prettyPrintObject d = list '{' '}' prettyPrintObjectProperty
  where
  prettyPrintObjectProperty :: (PSString, Maybe (Expr (Type ()))) -> Box
  prettyPrintObjectProperty (key, value) = textT (prettyPrintObjectKey key Monoid.<> ": ") <> maybe (text "_") (prettyPrintValue (d - 1)) value

prettyPrintUpdateEntry :: Int -> PSString -> Expr (Type ()) -> Box
prettyPrintUpdateEntry d key val = textT (prettyPrintObjectKey key) <> text " = " <> prettyPrintValue (d - 1) val

-- | Pretty-print an expression
prettyPrintValue :: Int -> Expr (Type ())-> Box
-- prettyPrintValue d _ | d < 0 = text "..."
prettyPrintValue d (Accessor _ prop val) = prettyPrintValueAtom (d - 1) val `before` textT ("." Monoid.<> prettyPrintObjectKey prop)
prettyPrintValue d (ObjectUpdate _ty o _copyFields ps) = prettyPrintValueAtom (d - 1) o `beforeWithSpace` list '{' '}' (uncurry (prettyPrintUpdateEntry d)) ps
prettyPrintValue d (App _ val arg) = prettyPrintValueAtom (d - 1) val `beforeWithSpace` prettyPrintValueAtom (d - 1) arg
prettyPrintValue d (Abs ty arg val) = text (oneLine $ '\\' : T.unpack (showIdent arg) ++ ": " ++ prettyPrintType (d) (getFunArgTy ty) ++ " -> ") //  (prettyPrintValue (d-1)  val)
prettyPrintValue d (Case _ values binders) =
  (text "case " <> foldr beforeWithSpace (text "of") (map (prettyPrintValueAtom (d - 1)) values)) //
    moveRight 2 (vcat left (map (prettyPrintCaseAlternative (d - 1)) binders))
prettyPrintValue d (Let _  ds val) =
  text "let" //
    moveRight 2 (vcat left (map (prettyPrintDeclaration (d - 1)) ds)) //
    (text "in " <> prettyPrintValue (d - 1) val)
-- TODO: constraint kind args
prettyPrintValue d (Literal _ l) = prettyPrintLiteralValue d l
prettyPrintValue d expr@Constructor{} = prettyPrintValueAtom d expr
prettyPrintValue d expr@Var{} = prettyPrintValueAtom d expr

-- | Pretty-print an atomic expression, adding parentheses if necessary.
prettyPrintValueAtom :: Int -> Expr (Type ()) -> Box
prettyPrintValueAtom d (Literal _ l) = prettyPrintLiteralValue d l
prettyPrintValueAtom _ (Constructor _ _ name _) = text $ T.unpack $ runProperName name
prettyPrintValueAtom d (Var ty ident) = text . oneLine $ "(" ++  T.unpack (showIdent (disqualify ident)) ++ ": " ++ prettyPrintType d ty ++  ")"
prettyPrintValueAtom d expr = (text "(" <> prettyPrintValue d expr) `before` text ")"

prettyPrintLiteralValue :: Int -> Literal (Expr (Type ())) -> Box
prettyPrintLiteralValue _ (NumericLiteral n) = text $ either show show n
prettyPrintLiteralValue _ (StringLiteral s) = text $ T.unpack $ prettyPrintString s
prettyPrintLiteralValue _ (CharLiteral c) = text $ show c
prettyPrintLiteralValue _ (BooleanLiteral True) = text "true"
prettyPrintLiteralValue _ (BooleanLiteral False) = text "false"
prettyPrintLiteralValue d (ArrayLiteral xs) = list '[' ']' (prettyPrintValue (d - 1)) xs
prettyPrintLiteralValue d (ObjectLiteral ps) = prettyPrintObject (d - 1) $ second Just `map` ps

prettyPrintDeclaration :: Int -> Bind (Type ()) -> Box
-- prettyPrintDeclaration d _ | d < 0 = ellipsis
prettyPrintDeclaration d b = case b of
  NonRec ty ident expr ->
    vcat left [
      text (oneLine $ T.unpack (showIdent ident) ++ " :: " ++ prettyPrintType 0 ty ),
      text (T.unpack (showIdent ident) ++ " = ") <> prettyPrintValue d expr -- not sure about the d here
    ]
  Rec bindings -> vsep 1 left $ map (\((ty,ident),expr) ->
        vcat left [
          text (oneLine $ T.unpack (showIdent ident) ++ " :: " ++ prettyPrintType 0 ty ),
          text (T.unpack (showIdent ident) ++ " = ") <> prettyPrintValue (d-1) expr
      ]) bindings

prettyPrintCaseAlternative :: Int -> CaseAlternative (Type ()) -> Box
-- prettyPrintCaseAlternative d _ | d < 0 = ellipsis
prettyPrintCaseAlternative d (CaseAlternative binders result) =
  text (T.unpack (T.unwords (map prettyPrintBinderAtom binders))) <> prettyPrintResult result
  where
  prettyPrintResult :: Either [(Guard (Type ()), Expr (Type ()))] (Expr (Type ())) -> Box
  prettyPrintResult = \case
    Left ges -> vcat left $  map (prettyPrintGuardedValueSep' (text " | ")) ges
    Right exp -> text " -> " <> prettyPrintValue (d-1) exp

  prettyPrintGuardedValueSep' :: Box -> (Guard (Type ()), Expr (Type ())) -> Box
  prettyPrintGuardedValueSep' sep (guardE, resultE) =
    prettyPrintValue (d-1) guardE <> text " -> " <> prettyPrintValue (d-1) resultE


prettyPrintModule :: Module (Type ()) -> Box
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

prettyPrintModule' :: Module (Type ()) -> String
prettyPrintModule' = render . prettyPrintModule
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

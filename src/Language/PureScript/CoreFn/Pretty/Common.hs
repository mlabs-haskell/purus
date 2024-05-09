module Language.PureScript.CoreFn.Pretty.Common where

import Prelude hiding ((<>))

import Control.Monad.Reader ( MonadReader(ask), runReader, Reader )

import Language.PureScript.CoreFn.Expr
    ( Expr(..) )
import Language.PureScript.Label (Label (..))
import Language.PureScript.Names (runModuleName, showIdent, Ident, ModuleName)
import Language.PureScript.PSString (PSString, decodeStringWithReplacement)

import Prettyprinter
    ( (<>),
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

{- One thing that we often wish to do, but cannot easily do either with
   the Prettyprinter library or the ancient lib PureScript uses, is to
   *force* particular sub-expressions to print on a single line.

   (`Prettyprinter.group` does give us the ability to express: "Try to
   print this on one line, but if you can't, use the multi-line format", and we
   use that when choosing between one- and multi-line formats.)

   This gives us a nice little abstraction for convenient auto-formatting
   (single line/multi line) where we want it, while also giving us the ability to
   override particular locations in the AST that we want to force to one-line (e.g. case
   expression binders, applied types, etc).
-}
data LineFormat
  = OneLine --  *DEFINITELY* Print on one line, even if doing so exceeds the page width
  | MultiLine --  *Possibly* Print multiple lines.
  deriving (Show, Eq)

-- A document with a structure that depends on a formatting context
type Printer ann =  Reader LineFormat (Doc ann)

-- Convenience type
type Formatter = forall a ann. (a -> Printer ann) -> a -> Doc ann

-- runReader with flipped arguments (how it should be!)
runPrinter :: LineFormat -> Printer ann -> Doc ann
runPrinter fmt p = runReader p fmt

asOneLine :: Formatter
asOneLine p x = runPrinter OneLine (p x)

-- Helper for dynamic formatting. `asMultiLine` doesn't make sense (we always want to choose
-- between single and multiline formats in a context where we aren't forcing a one-line format)
asDynamic :: Formatter
asDynamic p x = group $ align $ flatAlt (runPrinter MultiLine (p x)) (runPrinter OneLine (p x))

-- Applies the supplied function to the Doc if we're in a Multiline context.
-- Primarily used for correct formatting of Records/Rows/Objects
onMultiline :: (Doc ann -> Doc ann) -> Doc ann -> Printer ann
onMultiline f doc = ask >>= \case
  OneLine -> pure doc
  MultiLine -> pure . f $ doc

-- For docs w/ a structure that does not vary based on the line format options
-- Used primarily for `let` expressions (where we want uniformity)
ignoreFmt :: Doc ann -> Printer ann
ignoreFmt doc = printer doc doc

-- Choose between hsep and vsep based on the context
fmtSep :: [Doc ann] -> Printer ann
fmtSep docs = ask >>= \case
  OneLine -> pure $ hsep docs
  MultiLine -> pure $ vsep docs

-- Choose between hcat and vcat based on the context
fmtCat :: [Doc ann] -> Printer ann
fmtCat docs = ask >>= \case
  OneLine -> pure $ hcat docs
  MultiLine -> pure $ vcat docs

-- Choose between newline + indent or no change, depending on the context.
-- NOTE: This is kind of the whole reason we need LineFormat + the Reader monad.
--       `group` isn't sufficient here
fmtIndent :: Doc ann -> Printer ann
fmtIndent doc = ask >>= \case
  OneLine -> pure doc
  MultiLine -> pure $ line <> indent 2 doc

-- Helper function for constructing a printer expr
printer :: Doc ann -> Doc ann -> Printer ann
printer one multi = ask >>= \case
  OneLine ->  pure one
  MultiLine ->  pure multi

{- Higher-order Printers for Row Types, Record Types, and Object lits -}

-- Helper for open rows. The `| r` part requires special handling.
withOpenRow :: forall ann. Doc ann -> Doc ann -> ([Doc ann],Doc ann) -> Printer ann
withOpenRow l r (fields,open) = do
  fmtFields <- onMultiline (indent 2) =<< fmtSep (punctuate comma fields')
  group . align <$> fmtSep [l,fmtFields, r] -- fmtFields
  where
    fields' =  foldr (\x acc -> case acc of
                      [] -> [hsep [x,pipe <+> open]]
                      xs -> x : xs
                    ) [] fields

openRow :: ([Doc ann], Doc ann) -> Printer ann
openRow = withOpenRow lparen rparen

openRecord :: ([Doc ann], Doc ann) -> Printer ann
openRecord = withOpenRow lbrace rbrace

-- Printer for record like things (Object literals, record types)
recordLike ::  [Doc ann] -> Printer ann
recordLike  fields  = do
  fields' <- onMultiline (indent 2) =<< fmtSep (punctuate comma fields)
  group . align <$> fmtSep  [lbrace,fields',rbrace]

{- Misc Utils and custom combinators.
   Most of these are just for readability. (a <:> type),
   to me anyway, is a lot easier on the eyes than
   (a <> ":" <> space <> type)
-}
commaSep :: [Doc ann] -> Doc ann
commaSep = vsep . punctuate comma

-- Our "special" type annotations are indicated w/ a single colon.
(<:>) :: Doc ann -> Doc ann -> Doc ann
a <:> b = hcat [a,":"] <+> b

-- Actual type annotations & signatures (that are in the source explicitly or
-- inferred by the compiler before we get the AST) are indicated in the normal way,
-- that is, with '::'
(<::>) :: Doc ann -> Doc ann -> Doc ann
a <::> b = a <+> "::" <+> b

(<=>) :: Doc ann -> Doc ann -> Doc ann
a <=> b = a <+> "=" <+> b

-- Forces a line break. Shouldn't be used except in cases where we want to ignore
-- the dynamic formatting (e.g. case expressions)
(<//>) :: Doc ann -> Doc ann -> Doc ann
a <//> b = a <+> hardline <+> b

arrow :: Doc ann
arrow = "->"

lam :: Doc ann
lam = "\\"

-- Like `list` but forces one line format.
oneLineList :: [Doc ann] -> Doc ann
oneLineList = brackets . hcat . punctuate (comma <> space)

-- Splits an `App` expr into a function/ctor and a list of arguments.
analyzeApp :: Expr a -> Maybe (Expr a,[Expr a])
analyzeApp t = (,appArgs t) <$> appFun t
  where
    appArgs :: Expr a -> [Expr a]
    appArgs (App _  t1 t2) = appArgs t1 <> [t2]
    appArgs _  = []

    appFun :: Expr a -> Maybe (Expr a)
    appFun (App _  t1 _) = go t1
      where
        go (App _  tx _) = case appFun tx of
          Nothing -> Just tx
          Just tx' -> Just tx'
        go other = Just other
    appFun _ = Nothing




-- TODO: Move to modules where types are defined
instance Pretty Ident where
  pretty = pretty . showIdent

instance Pretty PSString where
  pretty = pretty . decodeStringWithReplacement

instance Pretty ModuleName where
  pretty = pretty . runModuleName

instance Pretty Label where
  pretty = pretty . runLabel

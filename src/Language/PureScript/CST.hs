module Language.PureScript.CST (
  parseFromFile,
  parseFromFiles,
  parseModuleFromFile,
  parseModulesFromFiles,
  unwrapParserError,
  toMultipleErrors,
  toMultipleWarnings,
  toPositionedError,
  toPositionedWarning,
  pureResult,
  module Language.PureScript.CST.Convert,
  module Language.PureScript.CST.Errors,
  module Language.PureScript.CST.Lexer,
  module Language.PureScript.CST.Monad,
  module Language.PureScript.CST.Parser,
  module Language.PureScript.CST.Print,
  module Language.PureScript.CST.Types,
) where

import Prelude hiding (lex)

import Control.Monad.Error.Class (MonadError (..))
import Control.Parallel.Strategies (evalTuple2, parList, r0, rseq, withStrategy)
import Data.List.NonEmpty qualified as NE
import Data.Text (Text)
import Language.PureScript.AST qualified as AST
import Language.PureScript.CST.Convert
import Language.PureScript.CST.Errors
import Language.PureScript.CST.Lexer
import Language.PureScript.CST.Monad (LexResult, Parser, ParserM (..), ParserState (..), runParser, runTokenParser)
import Language.PureScript.CST.Parser
import Language.PureScript.CST.Print
import Language.PureScript.CST.Types
import Language.PureScript.Errors qualified as E

pureResult :: a -> PartialResult a
pureResult a = PartialResult a ([], pure a)

parseModulesFromFiles ::
  forall m k.
  (MonadError E.MultipleErrors m) =>
  (k -> FilePath) ->
  [(k, Text)] ->
  m [(k, PartialResult AST.Module)]
parseModulesFromFiles toFilePath input =
  flip E.parU (handleParserError toFilePath)
    . inParallel
    . flip fmap input
    $ \(k, a) -> (k, parseModuleFromFile (toFilePath k) a)

parseFromFiles ::
  forall m k.
  (MonadError E.MultipleErrors m) =>
  (k -> FilePath) ->
  [(k, Text)] ->
  m [(k, ([ParserWarning], AST.Module))]
parseFromFiles toFilePath input =
  flip E.parU (handleParserError toFilePath)
    . inParallel
    . flip fmap input
    $ \(k, a) -> (k, sequence $ parseFromFile (toFilePath k) a)

parseModuleFromFile :: FilePath -> Text -> Either (NE.NonEmpty ParserError) (PartialResult AST.Module)
parseModuleFromFile fp content = fmap (convertModule fp) <$> parseModule (lexModule content)

parseFromFile :: FilePath -> Text -> ([ParserWarning], Either (NE.NonEmpty ParserError) AST.Module)
parseFromFile fp content = fmap (convertModule fp) <$> parse content

handleParserError ::
  forall m k a.
  (MonadError E.MultipleErrors m) =>
  (k -> FilePath) ->
  (k, Either (NE.NonEmpty ParserError) a) ->
  m (k, a)
handleParserError toFilePath (k, res) =
  (k,) <$> unwrapParserError (toFilePath k) res

unwrapParserError ::
  forall m a.
  (MonadError E.MultipleErrors m) =>
  FilePath ->
  Either (NE.NonEmpty ParserError) a ->
  m a
unwrapParserError fp =
  either (throwError . toMultipleErrors fp) pure

toMultipleErrors :: FilePath -> NE.NonEmpty ParserError -> E.MultipleErrors
toMultipleErrors fp =
  E.MultipleErrors . NE.toList . fmap (toPositionedError fp)

toMultipleWarnings :: FilePath -> [ParserWarning] -> E.MultipleErrors
toMultipleWarnings fp =
  E.MultipleErrors . fmap (toPositionedWarning fp)

toPositionedError :: FilePath -> ParserError -> E.ErrorMessage
toPositionedError name perr =
  E.ErrorMessage [E.positionedError $ sourceSpan name $ errRange perr] (E.ErrorParsingCSTModule perr)

toPositionedWarning :: FilePath -> ParserWarning -> E.ErrorMessage
toPositionedWarning name perr =
  E.ErrorMessage [E.positionedError $ sourceSpan name $ errRange perr] (E.WarningParsingCSTModule perr)

inParallel :: [(k, Either (NE.NonEmpty ParserError) a)] -> [(k, Either (NE.NonEmpty ParserError) a)]
inParallel = withStrategy (parList (evalTuple2 r0 rseq))

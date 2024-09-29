{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE DeriveAnyClass #-}

module Language.PureScript.PSString (
  PSString,
  toText,
  decodeString,
  decodeStringEither,
  decodeStringWithReplacement,
  prettyPrintString,
  prettyPrintStringJS,
  mkString,
) where

import Codec.Serialise (Serialise)
import Control.DeepSeq (NFData)
import Data.Aeson qualified as A
import Data.Char qualified as Char
import Data.String (IsString (..))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Word (Word16)
import GHC.Generics (Generic)
import Numeric (showHex)
import Prelude
import Language.Haskell.TH.Syntax (Lift)

{- |
Strings in Purus match the ones in Plutus: Unicode strings encoded using UTF-8.
-}
newtype PSString = PSString {toText :: Text}
  deriving (Eq, Ord, Semigroup, Monoid, Show, IsString, A.ToJSON, A.FromJSON) via Text
  deriving stock (Generic, Lift)
  deriving anyclass (NFData, Serialise)

{- |
Convert a 'PSString' into a 'String'.
-}
codePoints :: PSString -> String
codePoints = T.unpack . toText

{- |
Same as 'codePoints'.
-}
decodeStringWithReplacement :: PSString -> String
decodeStringWithReplacement = codePoints

{- |
Decode a PSString as UTF-16. 
-}
decodeStringEither :: PSString -> [Either Word16 Char]
decodeStringEither = fmap Right . T.unpack . toText

{- |
Convert a 'PSString' into a 'Text'
-}
decodeString :: PSString -> Maybe Text
decodeString = Just . toText

{- |
Pretty print a PSString, using PureScript escape sequences.
-}
prettyPrintString :: PSString -> Text
prettyPrintString s = "\"" <> foldMap encodeChar (decodeStringEither s) <> "\""
  where
    encodeChar :: Either Word16 Char -> Text
    encodeChar (Left c) = "\\x" <> showHex' 6 c
    encodeChar (Right c)
      | c == '\t' = "\\t"
      | c == '\r' = "\\r"
      | c == '\n' = "\\n"
      | c == '"' = "\\\""
      | c == '\'' = "\\\'"
      | c == '\\' = "\\\\"
      | shouldPrint c = T.singleton c
      | otherwise = "\\x" <> showHex' 6 (Char.ord c)

    -- Note we do not use Data.Char.isPrint here because that includes things
    -- like zero-width spaces and combining punctuation marks, which could be
    -- confusing to print unescaped.
    shouldPrint :: Char -> Bool
    -- The standard space character, U+20 SPACE, is the only space char we should
    -- print without escaping
    shouldPrint ' ' = True
    shouldPrint c =
      Char.generalCategory c
        `elem` [ Char.UppercaseLetter
               , Char.LowercaseLetter
               , Char.TitlecaseLetter
               , Char.OtherLetter
               , Char.DecimalNumber
               , Char.LetterNumber
               , Char.OtherNumber
               , Char.ConnectorPunctuation
               , Char.DashPunctuation
               , Char.OpenPunctuation
               , Char.ClosePunctuation
               , Char.InitialQuote
               , Char.FinalQuote
               , Char.OtherPunctuation
               , Char.MathSymbol
               , Char.CurrencySymbol
               , Char.ModifierSymbol
               , Char.OtherSymbol
               ]

{- |
Unused stub, same as 'prettyPrintString'. 
-}
prettyPrintStringJS :: PSString -> Text
prettyPrintStringJS = prettyPrintString

showHex' :: (Enum a) => Int -> a -> Text
showHex' width c =
  let hs = showHex (fromEnum c) ""
   in T.pack (replicate (width - length hs) '0' <> hs)

mkString :: Text -> PSString
mkString = fromString . T.unpack

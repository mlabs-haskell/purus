module Language.PureScript.TypeClassDictionaries where

import Prelude

import Control.DeepSeq (NFData)
import Data.Text (Text, pack)
import GHC.Generics (Generic)

import Language.PureScript.AST.Declarations.ChainId (ChainId)
import Language.PureScript.Names (Ident, ProperName (..), ProperNameType (..), Qualified, disqualify)
import Language.PureScript.Types (SourceConstraint, SourceType)

--
-- Data representing a type class dictionary which is in scope
--
data TypeClassDictionaryInScope v = TypeClassDictionaryInScope
  { tcdChain :: Maybe ChainId
  -- ^ The instance chain
  , tcdIndex :: Integer
  -- ^ Index of the instance chain
  , tcdValue :: v
  -- ^ The value with which the dictionary can be accessed at runtime
  , tcdPath :: [(Qualified (ProperName 'ClassName), Integer)]
  -- ^ How to obtain this instance via superclass relationships
  , tcdClassName :: Qualified (ProperName 'ClassName)
  -- ^ The name of the type class to which this type class instance applies
  , tcdForAll :: [(Text, SourceType)]
  -- ^ Quantification of type variables in the instance head and dependencies
  , tcdInstanceKinds :: [SourceType]
  -- ^ The kinds to which this type class instance applies
  , tcdInstanceTypes :: [SourceType]
  -- ^ The types to which this type class instance applies
  , tcdDependencies :: Maybe [SourceConstraint]
  -- ^ Type class dependencies which must be satisfied to construct this dictionary
  , tcdDescription :: Maybe SourceType
  -- ^ If this instance was unnamed, the type to use when describing it in
  -- error messages
  }
  deriving (Show, Functor, Foldable, Traversable, Generic)

instance (NFData v) => NFData (TypeClassDictionaryInScope v)

type NamedDict = TypeClassDictionaryInScope (Qualified Ident)

{- | Generate a name for a superclass reference which can be used in
generated code.
-}
superclassName :: Qualified (ProperName 'ClassName) -> Integer -> Text
superclassName pn index = runProperName (disqualify pn) -- <> pack (show index)

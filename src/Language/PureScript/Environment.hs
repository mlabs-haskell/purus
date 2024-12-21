{-# HLINT ignore "Redundant bracket" #-}
{-# HLINT ignore "Use tuple-section" #-}
-- anyone who thinks this is *always* clearer is drunk
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Language.PureScript.Environment where

import Prelude

import Codec.Serialise (Serialise)
import Control.DeepSeq (NFData)
import Control.Monad (unless, void)
import Data.Aeson ((.:), (.=))
import Data.Aeson qualified as A
import Data.Foldable (Foldable (foldl'), find, fold)
import Data.Functor ((<&>))
import Data.IntMap qualified as IM
import Data.IntSet qualified as IS
import Data.List.NonEmpty qualified as NEL
import Data.Map qualified as M
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Semigroup (First (..))
import Data.Set qualified as S
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics (Generic)

import Language.PureScript.AST.SourcePos (nullSourceAnn, pattern NullSourceAnn)
import Language.PureScript.Constants.Prim qualified as C
import Language.PureScript.Constants.Purus qualified as PLC
import Language.PureScript.Crash (internalError)
import Language.PureScript.Names (Ident (..), ProperName (..), ProperNameType (..), Qualified (..), QualifiedBy (..), coerceProperName, disqualify)
import Language.PureScript.Roles (Role (..))
import Language.PureScript.TypeClassDictionaries (NamedDict)
import Language.PureScript.Types (SourceConstraint, SourceType, Type (..), TypeVarVisibility (..), eqType, freeTypeVariables, quantify, srcTypeApp, srcTypeConstructor)

import Language.Purus.Config (maxTupleSize)

-- | The @Environment@ defines all values and types which are currently in scope:
data Environment = Environment
  { names :: M.Map (Qualified Ident) (SourceType, NameKind, NameVisibility)
  -- ^ Values currently in scope
  , types :: M.Map (Qualified (ProperName 'TypeName)) (SourceType, TypeKind)
  -- ^ Type names currently in scope
  , dataConstructors :: M.Map (Qualified (ProperName 'ConstructorName)) (DataDeclType, ProperName 'TypeName, SourceType, [Ident])
  -- ^ Data constructors currently in scope, along with their associated type
  -- constructor name, argument types and return type.
  , typeSynonyms :: M.Map (Qualified (ProperName 'TypeName)) ([(Text, SourceType)], SourceType)
  -- ^ Type synonyms currently in scope
  , typeClassDictionaries :: M.Map QualifiedBy (M.Map (Qualified (ProperName 'ClassName)) (M.Map (Qualified Ident) (NEL.NonEmpty NamedDict)))
  -- ^ Available type class dictionaries. When looking up 'Nothing' in the
  -- outer map, this returns the map of type class dictionaries in local
  -- scope (ie dictionaries brought in by a constrained type).
  , typeClasses :: M.Map (Qualified (ProperName 'ClassName)) TypeClassData
  -- ^ Type classes
  }
  deriving (Show, Generic)

instance NFData Environment

-- | Information about a type class
data TypeClassData = TypeClassData
  { typeClassArguments :: [(Text, SourceType)]
  -- ^ A list of type argument names, and their kinds, where kind annotations
  -- were provided.
  , typeClassMembers :: [(Ident, SourceType, Maybe (S.Set (NEL.NonEmpty Int)))]
  -- ^ A list of type class members and their types and whether or not
  -- they have type variables that must be defined using Visible Type Applications.
  -- Type arguments listed above are considered bound in these types.
  , typeClassSuperclasses :: [SourceConstraint]
  -- ^ A list of superclasses of this type class. Type arguments listed above
  -- are considered bound in the types appearing in these constraints.
  , typeClassDependencies :: [FunctionalDependency]
  -- ^ A list of functional dependencies for the type arguments of this class.
  , typeClassDeterminedArguments :: S.Set Int
  -- ^ A set of indexes of type argument that are fully determined by other
  -- arguments via functional dependencies. This can be computed from both
  -- typeClassArguments and typeClassDependencies.
  , typeClassCoveringSets :: S.Set (S.Set Int)
  -- ^ A sets of arguments that can be used to infer all other arguments.
  , typeClassIsEmpty :: Bool
  -- ^ Whether or not dictionaries for this type class are necessarily empty.
  }
  deriving (Show, Generic)

instance NFData TypeClassData

{- | A functional dependency indicates a relationship between two sets of
type arguments in a class declaration.
-}
data FunctionalDependency = FunctionalDependency
  { fdDeterminers :: [Int]
  -- ^ the type arguments which determine the determined type arguments
  , fdDetermined :: [Int]
  -- ^ the determined type arguments
  }
  deriving (Show, Generic)

instance NFData FunctionalDependency
instance Serialise FunctionalDependency

instance A.FromJSON FunctionalDependency where
  parseJSON = A.withObject "FunctionalDependency" $ \o ->
    FunctionalDependency
      <$> o .: "determiners"
      <*> o .: "determined"

instance A.ToJSON FunctionalDependency where
  toJSON FunctionalDependency {..} =
    A.object
      [ "determiners" .= fdDeterminers
      , "determined" .= fdDetermined
      ]

{- | The initial environment with only builtin PLC functions and Prim PureScript types defined
TODO: Move all of the purus-specific stuff out of this module,
      reset the initEnvironment to the default, and
      modify it at the call site (Language.PureScript.Make)

      This will improve the dependency structure of the project, but also,
      allows someone else to adapt Purus for another purpose. The pipeline up to
      up to `GenerateDataTypes` is more-or-less backend agnostic, so
      someone could easily use this to compile PureScript to another typed
      functional language using the IR.
-}
initEnvironment :: Environment
initEnvironment = Environment (builtinFunctions <> primFunctions) allPrimTypes primCtors M.empty M.empty allPrimClasses

{- | A constructor for TypeClassData that computes which type class arguments are fully determined
and argument covering sets.
Fully determined means that this argument cannot be used when selecting a type class instance.
A covering set is a minimal collection of arguments that can be used to find an instance and
therefore determine all other type arguments.

An example of the difference between determined and fully determined would be with the class:
```class C a b c | a -> b, b -> a, b -> c```
In this case, `a` must differ when `b` differs, and vice versa - each is determined by the other.
Both `a` and `b` can be used in selecting a type class instance. However, `c` cannot - it is
fully determined by `a` and `b`.

Define a graph of type class arguments with edges being fundep determiners to determined. Each
argument also has a self looping edge.
An argument is fully determined if doesn't appear at the start of a path of strongly connected components.
An argument is not fully determined otherwise.

The way we compute this is by saying: an argument X is fully determined if there are arguments that
determine X that X does not determine. This is the same thing: everything X determines includes everything
in its SCC, and everything determining X is either before it in an SCC path, or in the same SCC.
-}
makeTypeClassData ::
  [(Text, SourceType)] ->
  [(Ident, SourceType)] ->
  [SourceConstraint] ->
  [FunctionalDependency] ->
  Bool ->
  TypeClassData
makeTypeClassData args m s deps = TypeClassData args m' s deps determinedArgs coveringSets
  where
    (determinedArgs, coveringSets) = computeCoveringSets (length args) deps

    coveringSets' = S.toList coveringSets

    m' = map (\(a, b) -> (a, b, addVtaInfo b)) m

    addVtaInfo :: SourceType -> Maybe (S.Set (NEL.NonEmpty Int))
    addVtaInfo memberTy = do
      let mentionedArgIndexes = S.fromList (mapMaybe (argToIndex . fst) $ freeTypeVariables memberTy)
      let leftovers = map (`S.difference` mentionedArgIndexes) coveringSets'
      S.fromList <$> traverse (NEL.nonEmpty . S.toList) leftovers

    argToIndex :: Text -> Maybe Int
    argToIndex = flip M.lookup $ M.fromList (zipWith ((,) . fst) args [0 ..])

-- A moving frontier of sets to consider, along with the fundeps that can be
-- applied in each case. At each stage, all sets in the frontier will be the
-- same size, decreasing by 1 each time.
type Frontier = M.Map IS.IntSet (First (IM.IntMap (NEL.NonEmpty IS.IntSet)))

--                         ^                 ^          ^          ^
--         when *these* parameters           |          |          |
--         are still needed,                 |          |          |
--                              *these* parameters      |          |
--                              can be determined       |          |
--                                         from a non-zero         |
--                                         number of fundeps,      |
--                                                      which accept *these*
--                                                      parameters as inputs.

computeCoveringSets :: Int -> [FunctionalDependency] -> (S.Set Int, S.Set (S.Set Int))
computeCoveringSets nargs deps = (determinedArgs, coveringSets)
  where
    argumentIndices = S.fromList [0 .. nargs - 1]

    -- Compute all sets of arguments that determine the remaining arguments via
    -- functional dependencies. This is done in stages, where each stage
    -- considers sets of the same size to share work.
    allCoveringSets :: S.Set (S.Set Int)
    allCoveringSets = S.map (S.fromDistinctAscList . IS.toAscList)
      $ fst
      $ search
      $
      -- The initial frontier consists of just the set of all parameters and all
      -- fundeps organized into the map structure.
      M.singleton
        (IS.fromList [0 .. nargs - 1])
      $ First
      $ IM.fromListWith (<>)
      $ do
        fd <- deps
        let srcs = pure (IS.fromList (fdDeterminers fd))
        tgt <- fdDetermined fd
        pure (tgt, srcs)
      where
        -- Recursively advance the frontier until all frontiers are exhausted
        -- and coverings sets found. The covering sets found during the process
        -- are locally-minimal, in that none can be reduced by a fundep, but
        -- there may be subsets found from other frontiers.
        search :: Frontier -> (S.Set IS.IntSet, ())
        search frontier = unless (null frontier) $ M.foldMapWithKey step frontier >>= search

        -- The input set from the frontier is known to cover all parameters, but
        -- it may be able to be reduced by more fundeps.
        step :: IS.IntSet -> First (IM.IntMap (NEL.NonEmpty IS.IntSet)) -> (S.Set IS.IntSet, Frontier)
        step needed (First inEdges)
          -- If there are no applicable fundeps, record it as a locally minimal
          -- covering set. This has already been reduced to only applicable fundeps
          | IM.null inEdges = (S.singleton needed, M.empty)
          | otherwise = (S.empty, foldMap removeParameter paramsToTry)
          where
            determined = IM.keys inEdges
            -- If there is an acyclically determined functional dependency, prefer
            -- it to reduce the number of cases to check. That is a dependency
            -- that does not help determine other parameters.
            acycDetermined = find (`IS.notMember` (IS.unions $ concatMap NEL.toList $ IM.elems inEdges)) determined
            paramsToTry = maybe determined pure acycDetermined

            -- For each parameter to be removed to build the next frontier,
            -- delete the fundeps that determine it and filter out the fundeps
            -- that make use of it. Of course, if it an acyclic fundep we already
            -- found that there are none that use it.
            removeParameter :: Int -> Frontier
            removeParameter y =
              M.singleton
                (IS.delete y needed)
                $ case acycDetermined of
                  Just _ -> First $ IM.delete y inEdges
                  Nothing ->
                    First $ IM.mapMaybe (NEL.nonEmpty . NEL.filter (y `IS.notMember`)) $ IM.delete y inEdges

    -- Reduce to the inclusion-minimal sets
    coveringSets = S.filter (\v -> not (any (\c -> c `S.isProperSubsetOf` v) allCoveringSets)) allCoveringSets

    -- An argument is determined if it is in no covering set
    determinedArgs = argumentIndices `S.difference` fold coveringSets

-- | The visibility of a name in scope
data NameVisibility
  = -- | The name is defined in the current binding group, but is not visible
    Undefined
  | -- | The name is defined in the another binding group, or has been made visible by a function binder
    Defined
  deriving (Show, Eq, Generic)

instance NFData NameVisibility
instance Serialise NameVisibility

{- | A flag for whether a name is for an private or public value - only public values will be
included in a generated externs file.
-}
data NameKind
  = -- | A private value introduced as an artifact of code generation (class instances, class member
    -- accessors, etc.)
    Private
  | -- | A public value for a module member or foreign import declaration
    Public
  | -- | A name for member introduced by foreign import
    External
  deriving (Show, Eq, Generic)

instance NFData NameKind
instance Serialise NameKind

-- | The kinds of a type
data TypeKind
  = -- | Data type
    DataType DataDeclType [(Text, SourceType, Role)] [(ProperName 'ConstructorName, [SourceType])]
  | -- | Type synonym
    TypeSynonym
  | -- | Foreign data
    ExternData [Role]
  | -- | A local type variable
    LocalTypeVariable
  | -- | A scoped type variable
    ScopedTypeVar
  deriving (Show, Eq, Generic)

instance NFData TypeKind
instance Serialise TypeKind

-- | The type ('data' or 'newtype') of a data type declaration
data DataDeclType
  = -- | A standard data constructor
    Data
  | -- | A newtype constructor
    Newtype
  deriving (Show, Eq, Ord, Generic)

instance NFData DataDeclType
instance Serialise DataDeclType

showDataDeclType :: DataDeclType -> Text
showDataDeclType Data = "data"
showDataDeclType Newtype = "newtype"

instance A.ToJSON DataDeclType where
  toJSON = A.toJSON . showDataDeclType

instance A.FromJSON DataDeclType where
  parseJSON = A.withText "DataDeclType" $ \case
    "data" -> return Data
    "newtype" -> return Newtype
    other -> fail $ "invalid type: '" ++ T.unpack other ++ "'"

-- | Kind of ground types
kindType :: SourceType
kindType = srcTypeConstructor C.Type

kindConstraint :: SourceType
kindConstraint = srcTypeConstructor C.Constraint

kindSymbol :: SourceType
kindSymbol = srcTypeConstructor C.Symbol

kindDoc :: SourceType
kindDoc = srcTypeConstructor C.Doc

kindOrdering :: SourceType
kindOrdering = srcTypeConstructor C.TypeOrdering

kindRowList :: SourceType -> SourceType
kindRowList = TypeApp nullSourceAnn (srcTypeConstructor C.RowList)

kindRow :: SourceType -> SourceType
kindRow = TypeApp nullSourceAnn (srcTypeConstructor C.Row)

kindOfREmpty :: SourceType
kindOfREmpty = tyForall "k" kindType (kindRow (tyVar "k" kindType))

-- | Type constructor for functions
tyFunction :: SourceType
tyFunction = srcTypeConstructor C.Function

-- | Type constructor for strings
tyString :: SourceType
tyString = srcTypeConstructor C.String

-- | Type constructor for strings
tyChar :: SourceType
tyChar = srcTypeConstructor C.Char

-- | Type constructor for numbers
tyNumber :: SourceType
tyNumber = srcTypeConstructor C.Number

-- | Type constructor for integers
tyInt :: SourceType
tyInt = srcTypeConstructor C.Int

-- | Type constructor for booleans
tyBoolean :: SourceType
tyBoolean = srcTypeConstructor C.Boolean

-- | Type constructor for arrays
tyList :: SourceType
tyList = srcTypeConstructor C.List

-- | Type constructor for Delayed functions
tyDelayed :: SourceType
tyDelayed = srcTypeConstructor C.Delayed

-- | Type constructor for records
tyRecord :: SourceType
tyRecord = srcTypeConstructor C.Record

tyVar :: Text -> SourceType -> SourceType
tyVar = TypeVar nullSourceAnn

tyForall :: Text -> SourceType -> SourceType -> SourceType
tyForall var k ty = ForAll nullSourceAnn TypeVarInvisible var k ty Nothing

tyForallVis :: Text -> SourceType -> SourceType -> SourceType
tyForallVis var k ty = ForAll nullSourceAnn TypeVarVisible var k ty Nothing 

-- | Smart constructor for function types
function :: SourceType -> SourceType -> SourceType
function = TypeApp nullSourceAnn . TypeApp nullSourceAnn tyFunction

purusFun :: Type a -> Type a -> Type ()
purusFun = f . g
  where
    f x = TypeApp () x . void
    g = TypeApp () tyFunctionNoAnn . void
    tyFunctionNoAnn = TypeConstructor () C.Function

-- This is borderline necessary
pattern (:->) :: Type a -> Type a -> Type a
pattern a :-> b <-
  TypeApp
    _
    (TypeApp _ (TypeConstructor _ C.Function) a)
    b

pattern ListT :: Type a -> Type a
pattern ListT a <-
  TypeApp _ (TypeConstructor _ C.List) a

arrayT :: SourceType -> SourceType
arrayT = TypeApp NullSourceAnn (TypeConstructor NullSourceAnn C.List)

delayedT :: SourceType -> SourceType
delayedT = TypeApp NullSourceAnn (TypeConstructor NullSourceAnn C.Delayed)

pattern RecordT :: Type a -> Type a
pattern RecordT a <-
  TypeApp _ (TypeConstructor _ C.Record) a

mkRecordT :: SourceType -> SourceType
mkRecordT = TypeApp nullSourceAnn (TypeConstructor nullSourceAnn C.Record)

getFunArgTy :: Type a -> Type a
getFunArgTy = \case
  a :-> _ -> a
  ForAll _ _ _ _ t _ -> getFunArgTy t
  other -> other

-- To make reading the kind signatures below easier
(-:>) :: SourceType -> SourceType -> SourceType
(-:>) = function
infixr 4 -:>

primClass :: Qualified (ProperName 'ClassName) -> (SourceType -> SourceType) -> [(Qualified (ProperName 'TypeName), (SourceType, TypeKind))]
primClass name mkKind =
  [ let k = mkKind kindConstraint
     in (coerceProperName <$> name, (k, ExternData (nominalRolesForKind k)))
  , let k = mkKind kindType
     in (dictTypeName . coerceProperName <$> name, (k, TypeSynonym))
  ]

primCtors :: M.Map (Qualified (ProperName 'ConstructorName)) (DataDeclType, ProperName 'TypeName, SourceType, [Ident])
primCtors =
  M.fromList tupleCtors
    <> M.fromList
      [ (mkCtor "True", (Data, disqualify C.Boolean, srcTypeConstructor C.Boolean, []))
      , (mkCtor "False", (Data, disqualify C.Boolean, srcTypeConstructor C.Boolean, []))
      , (mkCtor "Nil", (Data, disqualify C.List, forallT "x" $ \x -> arrayT x, []))
      , (mkCtor "Cons", (Data, disqualify C.List, forallT "x" $ \x -> x -:> arrayT x -:> arrayT x, []))
      ]

mkCtor :: Text -> Qualified (ProperName 'ConstructorName)
mkCtor nm = Qualified (ByModuleName C.M_Prim) (ProperName nm)

tupleCtors :: [(Qualified (ProperName 'ConstructorName), (DataDeclType, ProperName 'TypeName, SourceType, [a]))]
tupleCtors =
  [1 .. 100] <&> \n ->
    let ctorNm = mkCtor ("Tuple" <> T.pack (show n))
        ctorTyNm = coerceProperName @_ @'TypeName $ disqualify ctorNm
        ctorTy = mkCtorTy (coerceProperName <$> ctorNm) n
     in (ctorNm, (Data, ctorTyNm, ctorTy, []))

-- These need to be exported b/c we need them in CoreFn -> IR desugaring
vars :: Int -> [Text]
vars n = map (\x -> "t" <> T.pack (show x)) [1 .. n]

mkCtorTy :: Qualified (ProperName 'TypeName) -> Int -> SourceType
mkCtorTy tNm n =
  let nVars = (\x -> TypeVar NullSourceAnn x kindType) <$> vars n
      nTyCon = TypeConstructor NullSourceAnn tNm
      resTy = foldl' srcTypeApp nTyCon nVars
   in quantify $ foldr (-:>) resTy nVars

{- | The primitive types in the external environment with their
associated kinds. There are also pseudo `Fail`, `Warn`, and `Partial` types
that correspond to the classes with the same names.
-}
primTypes :: M.Map (Qualified (ProperName 'TypeName)) (SourceType, TypeKind)
primTypes =
  tupleTypes
    <> M.fromList
      [ (C.Type, (kindType, ExternData []))
      , (C.Constraint, (kindType, ExternData []))
      , (C.Symbol, (kindType, ExternData []))
      , (C.Row, (kindType -:> kindType, ExternData [Phantom]))
      , (C.Function, (kindType -:> kindType -:> kindType, ExternData [Representational, Representational]))
      , (C.List, (kindType -:> kindType, listData))
      , (C.Record, (kindRow kindType -:> kindType, ExternData [Representational]))
      , (C.String, (kindType, ExternData []))
      , (C.Char, (kindType, ExternData []))
      , (C.Number, (kindType, ExternData []))
      , (C.Int, (kindType, ExternData []))
      , (C.Boolean, (kindType, boolData))
      , (C.Partial <&> coerceProperName, (kindConstraint, ExternData []))
      , (C.Unit, (kindType, ExternData []))
      , (C.Delayed, (kindType -:> kindType, ExternData [Representational]))
      ]
  where
    boolData =
      DataType
        Data
        []
        [ (disqualify C.C_True, [])
        , (disqualify C.C_False, [])
        ]
    listData =
      DataType
        Data
        [("a", kindType, Representational)]
        [ (disqualify C.C_Nil, [])
        , (disqualify C.C_Cons, [TypeVar NullSourceAnn "a" kindType, arrayT (TypeVar NullSourceAnn "a" kindType)])
        ]

-- | This 'Map' contains all of the prim types from all Prim modules.
allPrimTypes :: M.Map (Qualified (ProperName 'TypeName)) (SourceType, TypeKind)
allPrimTypes =
  M.unions
    [ primTypes
    , primBooleanTypes
    , primCoerceTypes
    , primOrderingTypes
    , primRowTypes
    , primRowListTypes
    , primSymbolTypes
    , primIntTypes
    , primTypeErrorTypes
    , -- For the sake of simplicity I'm putting the builtins here as well
      builtinTypes
    ]

tupleTypes :: M.Map (Qualified (ProperName 'TypeName)) (SourceType, TypeKind)
tupleTypes = M.fromList $ go <$> [1 .. maxTupleSize]
  where
    mkTupleKind :: Int -> SourceType
    mkTupleKind n = foldr (-:>) kindType (replicate n kindType)

    mkTupleArgVars n = vars n <&> \v -> (v, kindType, Representational)

    mkCtorTVArgs n = vars n <&> \v -> TypeVar NullSourceAnn v kindType

    go :: Int -> (Qualified (ProperName 'TypeName), (SourceType, TypeKind))
    go n =
      let tName = mkTupleTyName n
          tKind = mkTupleKind n
          tArgs = mkTupleArgVars n

          ctorNm = coerceProperName . disqualify $ tName
          ctorArgs = mkCtorTVArgs n

          datType = DataType Data tArgs [(ctorNm, ctorArgs)]
       in (tName, (tKind, datType))

-- needs exported for DesugarCore
mkTupleTyName :: Int -> Qualified (ProperName 'TypeName)
mkTupleTyName x =
  Qualified (ByModuleName C.M_Prim) (ProperName $ "Tuple" <> T.pack (show x))

primBooleanTypes :: M.Map (Qualified (ProperName 'TypeName)) (SourceType, TypeKind)
primBooleanTypes =
  M.fromList
    [ (C.True, (tyBoolean, ExternData []))
    , (C.False, (tyBoolean, ExternData []))
    ]

primCoerceTypes :: M.Map (Qualified (ProperName 'TypeName)) (SourceType, TypeKind)
primCoerceTypes =
  M.fromList $
    mconcat
      [ primClass C.Coercible (\kind -> tyForall "k" kindType $ tyVar "k" kindType -:> tyVar "k" kindType -:> kind)
      ]

primOrderingTypes :: M.Map (Qualified (ProperName 'TypeName)) (SourceType, TypeKind)
primOrderingTypes =
  M.fromList
    [ (C.TypeOrdering, (kindType, ExternData []))
    , (C.LT, (kindOrdering, ExternData []))
    , (C.EQ, (kindOrdering, ExternData []))
    , (C.GT, (kindOrdering, ExternData []))
    ]

primRowTypes :: M.Map (Qualified (ProperName 'TypeName)) (SourceType, TypeKind)
primRowTypes =
  M.fromList $
    mconcat
      [ primClass C.RowUnion (\kind -> tyForall "k" kindType $ kindRow (tyVar "k" kindType) -:> kindRow (tyVar "k" kindType) -:> kindRow (tyVar "k" kindType) -:> kind)
      , primClass C.RowNub (\kind -> tyForall "k" kindType $ kindRow (tyVar "k" kindType) -:> kindRow (tyVar "k" kindType) -:> kind)
      , primClass C.RowLacks (\kind -> tyForall "k" kindType $ kindSymbol -:> kindRow (tyVar "k" kindType) -:> kind)
      , primClass C.RowCons (\kind -> tyForall "k" kindType $ kindSymbol -:> tyVar "k" kindType -:> kindRow (tyVar "k" kindType) -:> kindRow (tyVar "k" kindType) -:> kind)
      ]

primRowListTypes :: M.Map (Qualified (ProperName 'TypeName)) (SourceType, TypeKind)
primRowListTypes =
  M.fromList $
    [ (C.RowList, (kindType -:> kindType, ExternData [Phantom]))
    , (C.RowListCons, (tyForall "k" kindType $ kindSymbol -:> tyVar "k" kindType -:> kindRowList (tyVar "k" kindType) -:> kindRowList (tyVar "k" kindType), ExternData [Phantom, Phantom, Phantom]))
    , (C.RowListNil, (tyForall "k" kindType $ kindRowList (tyVar "k" kindType), ExternData []))
    ]
      <> mconcat
        [ primClass C.RowToList (\kind -> tyForall "k" kindType $ kindRow (tyVar "k" kindType) -:> kindRowList (tyVar "k" kindType) -:> kind)
        ]

primSymbolTypes :: M.Map (Qualified (ProperName 'TypeName)) (SourceType, TypeKind)
primSymbolTypes =
  M.fromList $
    mconcat
      [ primClass C.SymbolAppend (\kind -> kindSymbol -:> kindSymbol -:> kindSymbol -:> kind)
      , primClass C.SymbolCompare (\kind -> kindSymbol -:> kindSymbol -:> kindOrdering -:> kind)
      , primClass C.SymbolCons (\kind -> kindSymbol -:> kindSymbol -:> kindSymbol -:> kind)
      ]

primIntTypes :: M.Map (Qualified (ProperName 'TypeName)) (SourceType, TypeKind)
primIntTypes =
  M.fromList $
    mconcat
      [ primClass C.IntAdd (\kind -> tyInt -:> tyInt -:> tyInt -:> kind)
      , primClass C.IntCompare (\kind -> tyInt -:> tyInt -:> kindOrdering -:> kind)
      , primClass C.IntMul (\kind -> tyInt -:> tyInt -:> tyInt -:> kind)
      , primClass C.IntToString (\kind -> tyInt -:> kindSymbol -:> kind)
      ]

primTypeErrorTypes :: M.Map (Qualified (ProperName 'TypeName)) (SourceType, TypeKind)
primTypeErrorTypes =
  M.fromList $
    [ (C.Doc, (kindType, ExternData []))
    , (C.Fail <&> coerceProperName, (kindDoc -:> kindConstraint, ExternData [Nominal]))
    , (C.Warn <&> coerceProperName, (kindDoc -:> kindConstraint, ExternData [Nominal]))
    , (C.Text, (kindSymbol -:> kindDoc, ExternData [Phantom]))
    , (C.Quote, (tyForall "k" kindType $ tyVar "k" kindType -:> kindDoc, ExternData [Phantom]))
    , (C.QuoteLabel, (kindSymbol -:> kindDoc, ExternData [Phantom]))
    , (C.Beside, (kindDoc -:> kindDoc -:> kindDoc, ExternData [Phantom, Phantom]))
    , (C.Above, (kindDoc -:> kindDoc -:> kindDoc, ExternData [Phantom, Phantom]))
    ]
      <> mconcat
        [ primClass C.Fail (\kind -> kindDoc -:> kind)
        , primClass C.Warn (\kind -> kindDoc -:> kind)
        ]

{- | The primitive class map. This just contains the `Partial` class.
`Partial` is used as a kind of magic constraint for partial functions.
-}
primClasses :: M.Map (Qualified (ProperName 'ClassName)) TypeClassData
primClasses =
  M.fromList
    [ (C.Partial, makeTypeClassData [] [] [] [] True)
    ]

-- | This contains all of the type classes from all Prim modules.
allPrimClasses :: M.Map (Qualified (ProperName 'ClassName)) TypeClassData
allPrimClasses =
  M.unions
    [ primClasses
    , primCoerceClasses
    , primRowClasses
    , primRowListClasses
    , primSymbolClasses
    , primIntClasses
    , primTypeErrorClasses
    ]

primCoerceClasses :: M.Map (Qualified (ProperName 'ClassName)) TypeClassData
primCoerceClasses =
  M.fromList
    -- class Coercible (a :: k) (b :: k)
    [
      ( C.Coercible
      , makeTypeClassData
          [ ("a", (tyVar "k" kindType))
          , ("b", (tyVar "k" kindType))
          ]
          []
          []
          []
          True
      )
    ]

primRowClasses :: M.Map (Qualified (ProperName 'ClassName)) TypeClassData
primRowClasses =
  M.fromList
    -- class Union (left :: Row k) (right :: Row k) (union :: Row k) | left right -> union, right union -> left, union left -> right
    [
      ( C.RowUnion
      , makeTypeClassData
          [ ("left", (kindRow (tyVar "k" kindType)))
          , ("right", (kindRow (tyVar "k" kindType)))
          , ("union", (kindRow (tyVar "k" kindType)))
          ]
          []
          []
          [ FunctionalDependency [0, 1] [2]
          , FunctionalDependency [1, 2] [0]
          , FunctionalDependency [2, 0] [1]
          ]
          True
      )
    , -- class Nub (original :: Row k) (nubbed :: Row k) | original -> nubbed

      ( C.RowNub
      , makeTypeClassData
          [ ("original", (kindRow (tyVar "k" kindType)))
          , ("nubbed", (kindRow (tyVar "k" kindType)))
          ]
          []
          []
          [ FunctionalDependency [0] [1]
          ]
          True
      )
    , -- class Lacks (label :: Symbol) (row :: Row k)

      ( C.RowLacks
      , makeTypeClassData
          [ ("label", kindSymbol)
          , ("row", (kindRow (tyVar "k" kindType)))
          ]
          []
          []
          []
          True
      )
    , -- class RowCons (label :: Symbol) (a :: k) (tail :: Row k) (row :: Row k) | label tail a -> row, label row -> tail a

      ( C.RowCons
      , makeTypeClassData
          [ ("label", kindSymbol)
          , ("a", (tyVar "k" kindType))
          , ("tail", (kindRow (tyVar "k" kindType)))
          , ("row", (kindRow (tyVar "k" kindType)))
          ]
          []
          []
          [ FunctionalDependency [0, 1, 2] [3]
          , FunctionalDependency [0, 3] [1, 2]
          ]
          True
      )
    ]

primRowListClasses :: M.Map (Qualified (ProperName 'ClassName)) TypeClassData
primRowListClasses =
  M.fromList
    -- class RowToList (row :: Row k) (list :: RowList k) | row -> list
    [
      ( C.RowToList
      , makeTypeClassData
          [ ("row", (kindRow (tyVar "k" kindType)))
          , ("list", (kindRowList (tyVar "k" kindType)))
          ]
          []
          []
          [ FunctionalDependency [0] [1]
          ]
          True
      )
    ]

primSymbolClasses :: M.Map (Qualified (ProperName 'ClassName)) TypeClassData
primSymbolClasses =
  M.fromList
    -- class Append (left :: Symbol) (right :: Symbol) (appended :: Symbol) | left right -> appended, right appended -> left, appended left -> right
    [
      ( C.SymbolAppend
      , makeTypeClassData
          [ ("left", kindSymbol)
          , ("right", kindSymbol)
          , ("appended", kindSymbol)
          ]
          []
          []
          [ FunctionalDependency [0, 1] [2]
          , FunctionalDependency [1, 2] [0]
          , FunctionalDependency [2, 0] [1]
          ]
          True
      )
    , -- class Compare (left :: Symbol) (right :: Symbol) (ordering :: Ordering) | left right -> ordering

      ( C.SymbolCompare
      , makeTypeClassData
          [ ("left", kindSymbol)
          , ("right", kindSymbol)
          , ("ordering", kindOrdering)
          ]
          []
          []
          [ FunctionalDependency [0, 1] [2]
          ]
          True
      )
    , -- class Cons (head :: Symbol) (tail :: Symbol) (symbol :: Symbol) | head tail -> symbol, symbol -> head tail

      ( C.SymbolCons
      , makeTypeClassData
          [ ("head", kindSymbol)
          , ("tail", kindSymbol)
          , ("symbol", kindSymbol)
          ]
          []
          []
          [ FunctionalDependency [0, 1] [2]
          , FunctionalDependency [2] [0, 1]
          ]
          True
      )
    ]

primIntClasses :: M.Map (Qualified (ProperName 'ClassName)) TypeClassData
primIntClasses =
  M.fromList
    -- class Add (left :: Int) (right :: Int) (sum :: Int) | left right -> sum, left sum -> right, right sum -> left
    [
      ( C.IntAdd
      , makeTypeClassData
          [ ("left", tyInt)
          , ("right", tyInt)
          , ("sum", tyInt)
          ]
          []
          []
          [ FunctionalDependency [0, 1] [2]
          , FunctionalDependency [0, 2] [1]
          , FunctionalDependency [1, 2] [0]
          ]
          True
      )
    , -- class Compare (left :: Int) (right :: Int) (ordering :: Ordering) | left right -> ordering

      ( C.IntCompare
      , makeTypeClassData
          [ ("left", tyInt)
          , ("right", tyInt)
          , ("ordering", kindOrdering)
          ]
          []
          []
          [ FunctionalDependency [0, 1] [2]
          ]
          True
      )
    , -- class Mul (left :: Int) (right :: Int) (product :: Int) | left right -> product

      ( C.IntMul
      , makeTypeClassData
          [ ("left", tyInt)
          , ("right", tyInt)
          , ("product", tyInt)
          ]
          []
          []
          [ FunctionalDependency [0, 1] [2]
          ]
          True
      )
    , -- class ToString (int :: Int) (string :: Symbol) | int -> string

      ( C.IntToString
      , makeTypeClassData
          [ ("int", tyInt)
          , ("string", kindSymbol)
          ]
          []
          []
          [ FunctionalDependency [0] [1]
          ]
          True
      )
    ]

primTypeErrorClasses :: M.Map (Qualified (ProperName 'ClassName)) TypeClassData
primTypeErrorClasses =
  M.fromList
    -- class Fail (message :: Symbol)
    [
      ( C.Fail
      , makeTypeClassData
          [("message", kindDoc)]
          []
          []
          []
          True
      )
    , -- class Warn (message :: Symbol)

      ( C.Warn
      , makeTypeClassData
          [("message", kindDoc)]
          []
          []
          []
          True
      )
    ]

-- | Finds information about data constructors from the current environment.
lookupConstructor :: Environment -> Qualified (ProperName 'ConstructorName) -> (DataDeclType, ProperName 'TypeName, SourceType, [Ident])
lookupConstructor env ctor =
  fromMaybe (internalError "Data constructor not found") $ ctor `M.lookup` dataConstructors env

-- | Finds information about values from the current environment.
lookupValue :: Environment -> Qualified Ident -> Maybe (SourceType, NameKind, NameVisibility)
lookupValue env ident = ident `M.lookup` names env

dictTypeName' :: Text -> Text
dictTypeName' = (<> "$Dict")

dictTypeName :: ProperName a -> ProperName a
dictTypeName = ProperName . dictTypeName' . runProperName

isDictTypeName :: ProperName a -> Bool
isDictTypeName = T.isSuffixOf "$Dict" . runProperName

{- |
Given the kind of a type, generate a list @Nominal@ roles. This is used for
opaque foreign types as well as type classes.
-}
nominalRolesForKind :: Type a -> [Role]
nominalRolesForKind k = replicate (kindArity k) Nominal

kindArity :: Type a -> Int
kindArity = length . fst . unapplyKinds

unapplyKinds :: Type a -> ([Type a], Type a)
unapplyKinds = go []
  where
    go kinds (TypeApp _ (TypeApp _ fn k1) k2)
      | eqType fn tyFunction = go (k1 : kinds) k2
    go kinds (ForAll _ _ _ _ k _) = go kinds k
    go kinds k = (reverse kinds, k)

{- |
Plutus Data / Builtins:
We need to provide primitives for Data-encoded objects,
builtin functions, etc
-}
tyBuiltinData :: SourceType
tyBuiltinData = srcTypeConstructor PLC.BuiltinData

tyAsData :: SourceType -> SourceType
tyAsData = TypeApp nullSourceAnn (srcTypeConstructor PLC.AsData)

tyBuiltinPair :: SourceType -> SourceType -> SourceType
tyBuiltinPair a b =
  TypeApp
    nullSourceAnn
    ( TypeApp
        nullSourceAnn
        (srcTypeConstructor PLC.BuiltinPair)
        a
    )
    b

tyBuiltinList :: SourceType -> SourceType
tyBuiltinList = TypeApp nullSourceAnn (srcTypeConstructor PLC.BuiltinList)

tyByteString :: SourceType
tyByteString = srcTypeConstructor PLC.BuiltinByteString

tyElementG1 :: SourceType
tyElementG1 = srcTypeConstructor PLC.BuiltinElementG1

tyElementG2 :: SourceType
tyElementG2 = srcTypeConstructor PLC.BuiltinElementG2

tyMlResult :: SourceType
tyMlResult = srcTypeConstructor PLC.BuiltinMlResult

tyUnit :: SourceType
tyUnit = srcTypeConstructor C.Unit

-- just for readability
(#@) :: Qualified Ident -> SourceType -> (Qualified Ident, SourceType)
f #@ t = (f, t)
infixr 0 #@

-- the kind is Type here. This is just to avoid potentially making a typo (and to make the manual function sigs more readable)
forallT :: Text -> (SourceType -> SourceType) -> SourceType
forallT txt f = tyForall txt kindType (f $ tyVar txt kindType)

forallTVis :: Text -> (SourceType -> SourceType) -> SourceType
forallTVis txt f = tyForallVis txt kindType (f $ tyVar txt kindType)

builtinTypes :: M.Map (Qualified (ProperName 'TypeName)) (SourceType, TypeKind)
builtinTypes =
  M.fromList
    [ (PLC.BuiltinData, (kindType, ExternData []))
    , (PLC.BuiltinPair, (kindType -:> kindType -:> kindType, ExternData [Representational, Representational]))
    , (PLC.BuiltinList, (kindType -:> kindType, ExternData [Representational]))
    , (PLC.BuiltinByteString, (kindType, ExternData []))
    , (PLC.BuiltinElementG1, (kindType, ExternData []))
    , (PLC.BuiltinElementG2, (kindType, ExternData []))
    , (PLC.BuiltinMlResult, (kindType, ExternData []))
    ]

primFunctions :: M.Map (Qualified Ident) (SourceType, NameKind, NameVisibility)
primFunctions = M.fromList primFuns
  where
    primFuns = [ (Qualified (ByModuleName C.M_Prim) (Ident "unit"), (tyUnit, Public, Defined))
               , (Qualified (ByModuleName C.M_Prim) (Ident "error"), (errTy, Public, Defined))
               , (Qualified (ByModuleName C.M_Prim) (Ident "delay"), (delayTy, Public, Defined))
               , (Qualified (ByModuleName C.M_Prim) (Ident "force"), (forceTy, Public, Defined))
               ]
    errTy = forallTVis "x" id
    delayTy = forallT "x" $ \x -> x -:> delayedT x
    forceTy = forallT "x" $ \x -> delayedT x -:> x


builtinFunctions :: M.Map (Qualified Ident) (SourceType, NameKind, NameVisibility)
builtinFunctions = builtinCxt <&> \x -> (x, Public, Defined)

-- NOTE/REVIEW: I'm rendering all "Word8" types as tyInt for now.
--              I'm not sure whether that's correct
builtinCxt :: M.Map (Qualified Ident) SourceType
builtinCxt =
  M.fromList
    [ -- Integers
      PLC.I_addInteger #@ tyInt -:> tyInt -:> tyInt
    , PLC.I_subtractInteger #@ tyInt -:> tyInt -:> tyInt
    , PLC.I_multiplyInteger #@ tyInt -:> tyInt -:> tyInt
    , PLC.I_divideInteger #@ tyInt -:> tyInt -:> tyInt
    , PLC.I_quotientInteger #@ tyInt -:> tyInt -:> tyInt
    , PLC.I_remainderInteger #@ tyInt -:> tyInt -:> tyInt
    , PLC.I_modInteger #@ tyInt -:> tyInt -:> tyInt
    , PLC.I_equalsInteger #@ tyInt -:> tyInt -:> tyBoolean
    , PLC.I_lessThanInteger #@ tyInt -:> tyInt -:> tyBoolean
    , PLC.I_lessThanEqualsInteger #@ tyInt -:> tyInt -:> tyBoolean
    , -- ByteStrings
      PLC.I_appendByteString #@ tyByteString -:> tyByteString -:> tyByteString
    , -- \/ Check the implications of the variant semantics for this (https://github.com/IntersectMBO/plutus/blob/973e03bbccbe3b860e2c8bf70c2f49418811a6ce/plutus-core/plutus-core/src/PlutusCore/Default/Builtins.hs#L1179-L1207)
      PLC.I_consByteString #@ tyInt -:> tyByteString -:> tyByteString
    , PLC.I_sliceByteString #@ tyInt -:> tyInt -:> tyByteString -:> tyByteString
    , PLC.I_lengthOfByteString #@ tyByteString -:> tyInt
    , PLC.I_indexByteString #@ tyByteString -:> tyInt -:> tyInt
    , PLC.I_equalsByteString #@ tyByteString -:> tyByteString -:> tyBoolean
    , PLC.I_lessThanByteString #@ tyByteString -:> tyByteString -:> tyBoolean
    , PLC.I_lessThanEqualsByteString #@ tyByteString -:> tyByteString -:> tyBoolean
    , -- Cryptography
      PLC.I_sha2_256 #@ tyByteString -:> tyByteString
    , PLC.I_sha3_256 #@ tyByteString -:> tyByteString
    , PLC.I_blake2b_256 #@ tyByteString -:> tyByteString
    , PLC.I_verifyEd25519Signature #@ tyByteString -:> tyByteString -:> tyByteString -:> tyBoolean
    , PLC.I_verifyEcdsaSecp256k1Signature #@ tyByteString -:> tyByteString -:> tyByteString -:> tyBoolean
    , PLC.I_verifySchnorrSecp256k1Signature #@ tyByteString -:> tyByteString -:> tyByteString -:> tyBoolean
    , -- Strings
      PLC.I_appendString #@ tyString -:> tyString -:> tyString
    , PLC.I_equalsString #@ tyString -:> tyString -:> tyBoolean
   {- We have to disable these for now. There's a fundamental incompability between PSString and Text that
      we don't have the resources to address at this point in time.
   -}
    , PLC.I_encodeUtf8 #@ tyString -:> tyByteString
    , PLC.I_decodeUtf8 #@ tyByteString -:> tyString
    , -- Bool
      -- NOTE: Specializing this to "Type", which miiiight not be what we want depending on how we do the data encoding
      PLC.I_ifThenElse #@ forallT "x" $ \x -> tyBoolean -:> x -:> x -:> x
    , -- Unit
      PLC.I_chooseUnit #@ forallT "x" $ \x -> tyUnit -:> x -:> x
    , -- Tracing
      PLC.I_trace #@ forallT "x" $ \x -> tyString -:> x -:> x
    , -- Pairs
      PLC.I_fstPair #@ forallT "a" $ \a -> forallT "b" $ \b -> tyBuiltinPair a b -:> a
    , PLC.I_sndPair #@ forallT "a" $ \a -> forallT "b" $ \b -> tyBuiltinPair a b -:> b
    , -- Lists
      PLC.I_chooseList #@ forallT "a" $ \a -> forallT "b" $ \b -> tyBuiltinList a -:> b -:> b
    , PLC.I_mkCons #@ forallT "a" $ \a -> a -:> tyBuiltinList a -:> tyBuiltinList a
    , PLC.I_headList #@ forallT "a" $ \a -> tyBuiltinList a -:> a
    , PLC.I_tailList #@ forallT "a" $ \a -> tyBuiltinList a -:> tyBuiltinList a
    , PLC.I_nullList #@ forallT "a" $ \a -> tyBuiltinList a -:> tyBoolean
    , -- Data
      -- Construction
      PLC.I_chooseData #@ forallT "a" $ \a -> tyBuiltinData -:> a -:> a -:> a -:> a -:> a
    , PLC.I_constrData #@ tyInt -:> tyBuiltinList tyBuiltinData -:> tyBuiltinData
    , PLC.I_mapData #@ tyBuiltinList (tyBuiltinPair tyBuiltinData tyBuiltinData) -:> tyBuiltinData
    , PLC.I_listData #@ tyBuiltinList tyBuiltinData -:> tyBuiltinData
    , PLC.I_iData #@ tyInt -:> tyBuiltinData
    , PLC.I_bData #@ tyByteString -:> tyBuiltinData
    , -- Destruction
      PLC.I_unConstrData #@ tyBuiltinData -:> tyBuiltinPair tyInt (tyBuiltinList tyBuiltinData)
    , PLC.I_unMapData #@ tyBuiltinData -:> tyBuiltinList (tyBuiltinPair tyBuiltinData tyBuiltinData)
    , PLC.I_unListData #@ tyBuiltinData -:> tyBuiltinList tyBuiltinData
    , PLC.I_unIData #@ tyBuiltinData -:> tyInt
    , PLC.I_unBData #@ tyBuiltinData -:> tyByteString
    , -- Data Misc
      PLC.I_equalsData #@ tyBuiltinData -:> tyBuiltinData -:> tyBoolean
    , PLC.I_serialiseData #@ tyBuiltinData -:> tyByteString
    , -- Misc constructors
      PLC.I_mkPairData #@ tyBuiltinData -:> tyBuiltinData -:> tyBuiltinPair tyBuiltinData tyBuiltinData
    , PLC.I_mkNilData #@ tyUnit -:> tyBuiltinList tyBuiltinData
    , PLC.I_mkNilPairData #@ tyUnit -:> tyBuiltinList (tyBuiltinPair tyBuiltinData tyBuiltinData)
    , -- BLS primitives
      PLC.I_bls12_381_G1_add #@ tyElementG1 -:> tyElementG1 -:> tyElementG1
    , PLC.I_bls12_381_G1_neg #@ tyElementG1 -:> tyElementG1
    , PLC.I_bls12_381_G1_scalarMul #@ tyInt -:> tyElementG1 -:> tyElementG1
    , PLC.I_bls12_381_G1_compress #@ tyElementG1 -:> tyByteString
    , PLC.I_bls12_381_G1_uncompress #@ tyByteString -:> tyElementG1
    , PLC.I_bls12_381_G1_hashToGroup #@ tyByteString -:> tyByteString -:> tyElementG1
    , PLC.I_bls12_381_G1_equal #@ tyElementG1 -:> tyElementG1 -:> tyBoolean
    , PLC.I_bls12_381_G2_add #@ tyElementG2 -:> tyElementG2 -:> tyElementG2
    , PLC.I_bls12_381_G2_neg #@ tyElementG2 -:> tyElementG2
    , PLC.I_bls12_381_G2_scalarMul #@ tyInt -:> tyElementG2 -:> tyElementG2
    , PLC.I_bls12_381_G2_compress #@ tyElementG2 -:> tyByteString
    , PLC.I_bls12_381_G2_uncompress #@ tyByteString -:> tyElementG2
    , PLC.I_bls12_381_G2_hashToGroup #@ tyByteString -:> tyByteString -:> tyElementG2
    , PLC.I_bls12_381_G2_equal #@ tyElementG2 -:> tyElementG2 -:> tyBoolean
    , PLC.I_bls12_381_millerLoop #@ tyElementG1 -:> tyElementG2 -:> tyMlResult
    , PLC.I_bls12_381_mulMlResult #@ tyMlResult -:> tyMlResult -:> tyMlResult
    , PLC.I_bls12_381_finalVerify #@ tyMlResult -:> tyMlResult -:> tyBoolean
    , -- More hashes
      PLC.I_keccak_256 #@ tyByteString -:> tyByteString
    , PLC.I_blake2b_224 #@ tyByteString -:> tyByteString
    , -- Conversion
      PLC.I_integerToByteString #@ tyBoolean -:> tyInt -:> tyInt -:> tyByteString
    , PLC.I_byteStringToInteger #@ tyBoolean -:> tyByteString -:> tyInt
    ]

{- HLINT ignore "Use void" -}
{- HLINT ignore "Use <$" -}
{- HLINT ignore "Use <&>" -}
module Language.PureScript.CoreFn.Desugar.Utils where

import Prelude
import Protolude (MonadError (..), traverse_)

import Data.Function (on)
import Data.Tuple (swap)
import Data.Map qualified as M

import Language.PureScript.AST qualified as A
import Language.PureScript.AST.Literals (Literal(..))
import Language.PureScript.AST.SourcePos (pattern NullSourceSpan, SourceSpan(..))
import Language.PureScript.AST.Traversals (everythingOnValues, overTypes)
import Language.PureScript.CoreFn.Ann (Ann)
import Language.PureScript.CoreFn.Binders (Binder(..))
import Language.PureScript.CoreFn.Expr (Expr(..), PurusType)
import Language.PureScript.CoreFn.Meta (ConstructorType(..), Meta(..))
import Language.PureScript.Crash (internalError)
import Language.PureScript.Environment (
  pattern RecordT,
  DataDeclType(..),
  Environment(..),
  NameKind(..),
  lookupConstructor,
  lookupValue,
  NameVisibility (..),
  dictTypeName,
  TypeClassData (typeClassArguments),
  function,
  pattern (:->),
  isDictTypeName)
import Language.PureScript.Names (Ident(..), ModuleName, ProperName(..), ProperNameType(..), Qualified(..), QualifiedBy(..), getQual, runIdent, coerceProperName)
import Language.PureScript.Types (SourceType, Type(..), Constraint (..), srcTypeConstructor, srcTypeApp, rowToSortedList, RowListItem(..), replaceTypeVars, everywhereOnTypes)
import Control.Monad.Supply.Class (MonadSupply)
import Control.Monad.State.Strict (MonadState, gets, modify')
import Control.Monad.Writer.Class ( MonadWriter )
import Language.PureScript.TypeChecker.Types
    ( kindType )
import Language.PureScript.Errors
    ( MultipleErrors )
import Debug.Trace (traceM, trace)
import Data.Text qualified as T
import Text.Pretty.Simple (pShow)
import Data.Text.Lazy qualified as LT
import Language.PureScript.TypeChecker.Monad
    ( bindLocalVariables,
      getEnv,
      withScopedTypeVars,
      CheckState(checkCurrentModule, checkEnv), debugNames )
import Language.PureScript.PSString (PSString)
import Language.PureScript.Label (Label(..))
import Data.Bifunctor (Bifunctor(..))
import Data.List.NonEmpty qualified as NEL
import Language.PureScript.TypeClassDictionaries (NamedDict, TypeClassDictionaryInScope (..))
import Data.List (foldl')
import Language.PureScript.AST.Traversals (litM)
import Control.Lens.Plated
import Language.PureScript.Sugar (desugarGuardedExprs)
import Language.PureScript.AST.Declarations (declSourceSpan)


{- UTILITIES -}
--TODO: Explain purpose of every function


-- | Type synonym for a monad that has all of the required typechecker functionality
type M m = (MonadSupply m, MonadState CheckState m, MonadError MultipleErrors m, MonadWriter MultipleErrors m)



{- "Type Constructor analysis" machinery. (This requires some explaining)

   In the course of converting to typed CoreFn, we always proceed "top-down"
   from top-level declarations which must have a type annotation attached
   (their typechecker enforces this - it will add an inferred annotation if
   the user fails to annotate the type).

   Because not all sub-expression (specifically, "synthetic applications" where a type class
   dictionary constructor is applied to its argument in an instance declaration) are typed,
   we may run into situations where the inferred or reconstructed type for a sub-expression
   is universally quantified, even though we know (via our "top-down" approach) that the
   quantified type variables should be instantiated (either to concrete types or to
   type variables which are introduced in the outer lexical scope).

   An example (from test 4310) makes the problem clearer. Suppose we have:

   ```
   data Tuple a b = Tuple a b

   infixr 6 Tuple as /\
   infixr 6 type Tuple as /\

   mappend :: String -> String -> String
   mappend _ _ = "mappend"

   infixr 5 mappend as <>

   class Test a where
     runTest :: a -> String

   instance Test Int where
     runTest _ = "4"

   instance (Test a, Test b) => Test (a /\ b) where
     runTest (a /\ b) = runTest a <> runTest b

   ```

   The generated code for the typeclass declaration gives us (in part):

  ```
  Test$Dict :: forall a.  { runTest :: a -> String  }  -> { runTest :: a -> String }
  Test$Dict = \(x: { runTest :: a -> String} ) ->
            (x: { runTest :: a -> String} )

  runTest :: forall (@a :: Type). Test$Dict a -> a -> String
  runTest = \(dict: Test$Dict a) ->
          case (dict: Test$Dict a) of
            (Test$Dict v) -> (v: { runTest :: a -> String} ).runTest
  ```

  Because the Tuple instance for Test uses `runTest` (the function), and because
  `runTest` is universally quantified, if we did not instantiate those quantifiers,
  a new skolem scope will be introduced at each application of `runTest`, giving us
  type variables that cannot be unified with the outermost type variables.

  That is, without using this machiner (and `instantiate`), we end up with something like
  this for the tuple instance:

  ```
  test/\ :: forall (a :: Type) (b :: Type). Test$Dict a -> Test$Dict b -> Test$Dict (Tuple a b)
  test/\ = \(dictTest: Test$Dict a) ->
         \(dictTest1: Test$Dict b) ->
         (Test$Dict: { runTest :: a -> String} -> Test$Dict a ) { runTest: \(v: Tuple a0 b1) ->                                                                                                                                                                                                                                   }
           case (v: Tuple a0 b1) of
             (Tuple a b) ->
               ((mappend: String -> String -> String)
               (((runTest: forall (@a :: Type). Test$Dict a -> a -> String) (dictTest: Test$Dict a)) (a: t1)))
               (((runTest: forall (@a :: Type). Test$Dict a -> a -> String) (dictTest1: Test$Dict b)) (b: t2))
  ```

  By using this machinery in `inferBinder'`, we can instantiate the quantifiers to the
  lexically scoped type variables in the top-level signature, and get output that is properly typed:

  ```
  test/\ :: forall (a :: Type) (b :: Type). Test$Dict a -> Test$Dict b -> Test$Dict (Tuple a b)                                                                                                                                                                                                                                                      
  test/\ = \(dictTest: Test$Dict a) ->                                                                                                                                                                                                                                                                                                               
         \(dictTest1: Test$Dict b) ->
         (Test$Dict: { runTest :: Tuple a b -> String} -> Test$Dict (Tuple a b) ) { runTest: \(v: Tuple a b) ->                                                                                                                                                                                                                                   }
            case (v: Tuple a b) of
               (Tuple a b) ->
                ((mappend: String -> String -> String)
                (((runTest: forall (@a :: Type). Test$Dict a -> a -> String) (dictTest: Test$Dict a)) (a: a)))
                (((runTest: forall (@a :: Type). Test$Dict a -> a -> String) (dictTest1: Test$Dict b)) (b: b))

  ```

  We also use this in the branch of the `App` case of `exprToCoreFn` that handles dictionary applications
  (in the same manner and for the same purpose).

-}

-- Given a type (which we expect to be a TyCon applied to type args),
-- extract (TyCon,[Args]) (returning Nothing if the input type is not a TyCon)
analyzeCtor :: SourceType -> Maybe (SourceType,[SourceType])
analyzeCtor t = (,ctorArgs t) <$> ctorFun t

-- Extract all of the arguments to a type constructor
ctorArgs :: SourceType -> [SourceType]
ctorArgs (TypeApp _ t1 t2) = ctorArgs t1 <> [t2]
ctorArgs _  = []

-- Extract the TyCon ("function") part of an applied Type Constructor
ctorFun :: SourceType -> Maybe SourceType
ctorFun (TypeApp _ t1 _) = go t1
  where
    go (TypeApp _ tx _) = case ctorFun tx of
      Nothing -> Just tx
      Just tx' -> Just tx'
    go other = Just other
ctorFun _ = Nothing


{- Instantiation machinery. This differs from `instantiatePolyType` and
   `withInstantiatedFunType` in that those functions are used to "peek under"
   the quantifier in a universally quantified type (i.e. those functions
   *put the quantifier back* after temporarily instantiating the quantified variables
   *to type variables* for the purposes of type reconstruction).

   This instantiates a quantified type (the first arg) and *does not* replace the
   quantifier. This is primarily used when we encounter an expression with a universally
   quantified type (either as an annotation in a AST.TypedValue or as the result of looking up
   the type in the typechecking environment) in a context where we know (from our top-down approach)
   that the instantiated type must be instantiated to something "concrete" (where, again,
   a "concrete" type can either be an explicit type or a tyvar from the outer scope).
-}
instantiate :: SourceType -> [SourceType] -> SourceType
instantiate ty [] = ty
instantiate (ForAll _ _ var _ inner _) (t:ts) = replaceTypeVars var t $ instantiate inner ts
instantiate other _ = other

-- | Traverse a literal. Note that literals are usually have a type like `Literal (Expr a)`. That is: The `a` isn't typically an annotation, it's an expression type
traverseLit :: forall m a b. Applicative m => (a -> m b) -> Literal a -> m (Literal b)
traverseLit f = \case
  NumericLiteral x -> pure $ NumericLiteral x
  StringLiteral x -> pure $ StringLiteral x
  CharLiteral x -> pure $ CharLiteral x
  BooleanLiteral x -> pure $ BooleanLiteral x
  ArrayLiteral xs  -> ArrayLiteral <$> traverse f xs
  ObjectLiteral xs -> ObjectLiteral <$> traverse (\(str,x) -> (str,) <$> f x) xs


-- Wrapper around instantiatePolyType to provide a better interface
withInstantiatedFunType :: M m => ModuleName -> SourceType -> (SourceType -> SourceType -> m (Expr Ann)) -> m (Expr Ann)
withInstantiatedFunType mn ty act = case instantiatePolyType mn ty of
  (a :-> b, replaceForalls, bindAct) -> bindAct $ replaceForalls <$> act a b
  (other,_,_) -> let !showty = LT.unpack (pShow other)
                 in error $ "Internal error. Expected a function type, but got: " <> showty
{- This function more-or-less contains our strategy for handling polytypes (quantified or constrained types). It returns a tuple T such that:
     - T[0] is the inner type, where all of the quantifiers and constraints have been removed. We just instantiate the quantified type variables to themselves (I guess?) - the previous
       typchecker passes should ensure that quantifiers are all well scoped and that all essential renaming has been performed. Typically, the inner type should be a function.
       Constraints are eliminated by replacing the constraint argument w/ the appropriate dictionary type.

     - T[1] is a function to transform the eventual expression such that it is properly typed. Basically: It puts the quantifiers back, (hopefully) in the right order and with
       the correct visibility, skolem scope, etc.

     - T[2] is a monadic action which binds local variables or type variables so that we can use type inference machinery on the expression corresponding to this type.
       NOTE: The only local vars this will bind are "dict" identifiers introduced to type desguared typeclass constraints.
             That is: If you're using this on a function type, you'll still have to bind the antecedent type to the
                      identifier bound in the VarBinder.
-}
-- TODO: Explicitly return two sourcetypes for arg/return types
instantiatePolyType :: M m => ModuleName -> SourceType-> (SourceType, Expr b -> Expr b, m a -> m a)
instantiatePolyType mn = \case
  ForAll ann vis var mbk t mSkol -> case instantiatePolyType mn t of
    (inner,g,act) ->
      let f = \case
                Abs ann' ty' ident' expr' ->
                  Abs ann' (ForAll ann vis var (purusTy <$> mbk) (purusTy ty') mSkol) ident' expr'
                other -> other
          -- FIXME: kindType?
          act' ma = withScopedTypeVars mn [(var,kindType)] $ act ma -- NOTE: Might need to pattern match on mbk and use the real kind (though in practice this should always be of kind Type, I think?)
      in (inner, f . g, act')
  fun@(a :-> _) -> case analyzeCtor a of
      Just (TypeConstructor _ (Qualified _ nm), _) ->
        if isDictTypeName nm
          then
            let act' ma = bindLocalVariables [(NullSourceSpan,Ident "dict",a,Defined)] ma
            in (fun,id,act')
          else (fun,id,id)
      _ -> (fun,id,id)
  other -> (other,id,id)

-- In a context where we expect a Record type (object literals, etc), unwrap the record and get at the underlying rowlist
unwrapRecord :: Type a -> Either (Type a) [(PSString,Type a)]
unwrapRecord = \case
  RecordT lts -> Right $ go <$> fst (rowToSortedList lts)
  other -> Left other
 where
  go :: RowListItem a -> (PSString, Type a)
  go RowListItem{..} = (runLabel rowListLabel, rowListType)

traceNameTypes :: M m => m ()
traceNameTypes  = do
  nametypes <- getEnv >>= pure . debugNames
  traverse_ traceM nametypes

desugarCasesEverywhere :: M m => A.Declaration -> m (A.Declaration)
desugarCasesEverywhere d = traverseDeclBodies (transformM $ desugarGuardedExprs (declSourceSpan d )) d


traverseDeclBodies :: forall m. Applicative m =>  (A.Expr -> m A.Expr) -> A.Declaration -> m A.Declaration
traverseDeclBodies f = \case
  A.BindingGroupDeclaration decls ->
    A.BindingGroupDeclaration
      <$> traverse go {- (\(annIdent,nk,expr) -> f expr >>= \e -> pure (annIdent,nk,e)) -} decls
  A.ValueDecl ann name nk bs [A.MkUnguarded e] ->
      (\x -> A.ValueDecl ann name nk bs [A.MkUnguarded x]) <$> f e
  other -> pure other
 where
  go :: ((A.SourceAnn, Ident), NameKind, A.Expr) -> m ((A.SourceAnn, Ident), NameKind, A.Expr)
  go (annid, nk, e) = (\ex -> (annid, nk, ex)) <$> f e


instance Plated A.Expr where
  plate f = \case
    A.Literal ss litE -> A.Literal ss <$> traverseLit f litE
    A.UnaryMinus ss e -> A.UnaryMinus ss <$> f e
    A.BinaryNoParens a b c -> A.BinaryNoParens <$> f a <*> f b <*> f c
    A.Parens e -> A.Parens <$> f e
    A.Accessor str e -> A.Accessor str <$> f e
    A.ObjectUpdate e fs -> A.ObjectUpdate <$> f e <*> (traverse . traverse) f fs
    A.ObjectUpdateNested e pt -> A.ObjectUpdateNested <$> f e <*> traverse f pt
    A.Abs b e -> A.Abs b <$> f e
    A.App e1 e2 -> A.App <$> f e1 <*> f e2
    A.VisibleTypeApp e t -> (\ex -> A.VisibleTypeApp ex t) <$> f e
    A.Unused e -> A.Unused <$> f e
    A.Var ss i -> pure $ A.Var ss i
    A.Op ss nm -> pure $ A.Op ss nm
    A.IfThenElse c e1 e2 -> A.IfThenElse <$> f c <*> f e1 <*> f e2
    A.Constructor ss nm -> pure $ A.Constructor ss nm
    A.Case es alts -> A.Case <$> traverse f es <*> traverse goAlt alts
    A.TypedValue b e t -> (\ex -> A.TypedValue b ex t) <$> f e
    A.Let wp decls e -> A.Let wp <$> traverse (traverseDeclBodies f) decls <*> f e
    A.Do mn doElems -> A.Do mn <$> traverse goDoElem doElems
    A.Ado mn dElems e -> A.Ado mn <$> traverse goDoElem dElems <*> f e
    A.PositionedValue ss cs e -> A.PositionedValue ss cs <$> f e
    other -> pure other
   where
     goAlt = \case
       A.CaseAlternative bs ges -> A.CaseAlternative bs <$> traverse goGE ges
     goGE (A.GuardedExpr gs e) = A.GuardedExpr <$> traverse goGuard gs <*> f e
     goGuard = \case
       A.ConditionGuard e -> A.ConditionGuard <$> f e
       A.PatternGuard b e -> A.PatternGuard b <$> f e
     goDoElem = \case
       A.DoNotationValue e -> A.DoNotationValue <$> f e
       A.DoNotationBind b e -> A.DoNotationBind b <$> f e
       A.DoNotationLet decls -> A.DoNotationLet <$> traverse (traverseDeclBodies f) decls
       A.PositionedDoNotationElement ss cs de -> A.PositionedDoNotationElement ss cs <$> goDoElem de

     

 

{- Since we operate on an AST where constraints have been desugared to dictionaries at the *expr* level,
   using a typechecker context which contains ConstrainedTypes, looking up the type for a class method
   will always give us a "wrong" type. Let's try fixing them in the context!
-}
desugarConstraintType :: SourceType -> SourceType
desugarConstraintType = \case
  ForAll a vis var mbk t mSkol ->
    let t' = desugarConstraintType t
    in ForAll a vis var mbk t' mSkol
  ConstrainedType _ Constraint{..} t ->
    let inner = desugarConstraintType t
        dictTyName :: Qualified (ProperName 'TypeName) = dictTypeName . coerceProperName <$> constraintClass
        dictTyCon = srcTypeConstructor dictTyName
        dictTy = foldl srcTypeApp dictTyCon constraintArgs
    in function dictTy inner
  other -> other

desugarConstraintTypes :: M m =>  m ()
desugarConstraintTypes  = do
  env <- getEnv
  let f = everywhereOnTypes desugarConstraintType

      oldNameTypes = names env
      desugaredNameTypes = (\(st,nk,nv) -> (f st,nk,nv)) <$> oldNameTypes

      oldTypes = types env
      desugaredTypes = first f <$> oldTypes

      oldCtors = dataConstructors env
      desugaredCtors = (\(a,b,c,d) -> (a,b,f c,d)) <$> oldCtors

      oldSynonyms = typeSynonyms env
      desugaredSynonyms = second f <$> oldSynonyms

      newEnv = env { names = desugaredNameTypes
                   , types = desugaredTypes
                   , dataConstructors = desugaredCtors
                   , typeSynonyms = desugaredSynonyms }

  modify' $ \checkstate -> checkstate {checkEnv = newEnv}

desugarConstraintsInDecl :: A.Declaration -> A.Declaration
desugarConstraintsInDecl = \case
  A.BindingGroupDeclaration decls ->
    A.BindingGroupDeclaration
      $ (\(annIdent,nk,expr) -> (annIdent,nk,overTypes desugarConstraintType expr)) <$> decls
  A.ValueDecl ann name nk bs [A.MkUnguarded e] ->
     A.ValueDecl ann name nk bs [A.MkUnguarded $ overTypes desugarConstraintType e]
  A.DataDeclaration ann declTy tName args ctorDecs ->
    let fixCtor (A.DataConstructorDeclaration a nm fields)
          = A.DataConstructorDeclaration a nm (second (everywhereOnTypes desugarConstraintType) <$> fields)
    in A.DataDeclaration ann declTy tName args (fixCtor <$> ctorDecs)
  other -> other

-- Gives much more readable output (with colors for brackets/parens!) than plain old `show`
pTrace :: (Monad m, Show a) => a -> m ()
pTrace = traceM . LT.unpack . pShow

-- | Given a string and a monadic action, produce a trace with the given message before & after the action (with pretty lines to make it more readable)
wrapTrace :: Monad m => String -> m a -> m a
wrapTrace msg act = do
  traceM startMsg
  res <- act
  traceM endMsg
  pure res
 where
   padding = replicate 10 '='
   pad str = padding <> str <> padding
   startMsg = pad $ "BEGIN " <> msg
   endMsg = pad $ "END " <> msg

{-
  This is used to solve a problem that arises with re-exported instances.

  We diverge from PureScript by "desugaring" constrained types to types that contain
  explicit type class dictionaries. (We have to do this for PIR conversion - we have to type
  all nodes of the AST.)

  During PureScript's initial desugaring phase, type class declarations, instance declarations, and
  expressions that contain type class constaints are transformed into generated value declarations. For example:

  ```
    class Eq a where
      eq a :: a -> a -> Bool

    f :: forall a. Eq a => a -> a -> Boolean
    f x y = eq x y
  ```

  Is transformed into (something like, I'm ommitting the full generated code for brevity):

  ```
    Eq$Dict :: forall a. {eq :: a -> a -> Boolean } -> {eq :: a -> a -> Boolean}
    Eq$Dict x = x

    eq :: forall a. Eq$Dict a -> a -> a -> Boolean
    eq  = \dict ->  case dict of
      (v :: {eq :: a -> a -> Boolean}) -> v.eq

    f :: forall a. Eq a => a -> a -> Boolean
    f = \dict x y -> (eq dict) x y
  ```

  Three important things to note here:
   - PureScript does *not* transform constrained types into types that contain explicit dictionaries,
     even though the expressions are desugared to contain those dictionaries. (We do this ourselves
     after the PS typechecking phase)
   - Generated declarations for type classes and instances are not (and cannot be) exported,
     because typeclass desugaring takes place *after* import/export resolution
     in their desugaring pipeline. (This would be difficult to fix, each step of the desugaring pipeline
     expects input that conforms to the output of the previous step).
   - Generated code relating to typeclass dictionaries is ignored by the PureScript typechecker.
     Ordinarily, we can rely on the typechecker to insert the type annotation for most
     expressions, but we cannot do so here.

  These factors give rise to a problem: Our desugared constraint types (where we transform
  type annotations of the form `C a => (..)` into `C$Dict a -> (...)`) no longer contain constraints,
  and therefore we cannot use the constraint solving machinery directly to infer the types of
  identifiers that refer to type class dictionaries. Because generated type class code cannot be exported
  by the user in the source (and would not ordinarily be implicitly re-exported even if it could be exported),
  we cannot rely upon normal import resolution to provide the types corresponding to dictionary identifiers.

  This solves the problem. Because we use the same state/module scope as the PS typechecker, we
  have access to all of the type class dictionaries (including their identifiers) that are in scope.
  When we encounter an identifier that cannot be assigned a type by the normal type lookup process,
  we extract a map from identifiers to source types, and lookup the identifier in the map, allowing us to
  resolve the types of dictionary expressions.

  These identifiers are always qualified by module in the AST, so cannot clash with local definitions, which
  are qualified by SourcePos.

  NOTE: In theory (at least), this component of the type checker environment  can change if we
        make any calls to `infer` or any of the type checking functions in the
        TypeChecker.X namespace. So for now, we rebuild this map every time we fail to
        lookup the type for an identifier in the normal way. (Which is grossly
        inefficient)

        In principle, we should be able to totally reconstruct the types w/o making
        any calls to `infer` or the typechecker machinery. Once that is done, we can
        construct this map only once for each module, which will greatly improve performance.
-}
lookupDictType :: M m => Qualified Ident -> m (Maybe SourceType)
lookupDictType nm = do
  tyClassDicts <- typeClassDictionaries <$> getEnv
  let dictMap = dictionaryIdentMap tyClassDicts
  pure $ M.lookup nm dictMap
 where
  dictionaryIdentMap :: M.Map QualifiedBy (M.Map (Qualified (ProperName 'ClassName)) (M.Map (Qualified Ident) (NEL.NonEmpty NamedDict)))
                     -> M.Map (Qualified Ident) SourceType
  dictionaryIdentMap m =  foldl' go M.empty inner
    where
      -- duplicates?
      inner =  concatMap NEL.toList . M.elems $ M.unions $ concatMap M.elems $ M.elems m
      go :: M.Map (Qualified Ident) SourceType -> NamedDict -> M.Map (Qualified Ident) SourceType
      go acc TypeClassDictionaryInScope{..} = M.insert tcdValue dictTy acc
        where
          dictTy = foldl' srcTypeApp dictTyCon tcdInstanceTypes
          dictTyCon = srcTypeConstructor $ coerceProperName . dictTypeName <$> tcdClassName


(</>) :: String -> String -> String
x </> y = x <> "\n" <> y

-- We need a string for traces and readability is super important here
showIdent' :: Ident -> String
showIdent' = T.unpack . runIdent

-- | Turns a `Type a` into a `Type ()`. We shouldn't need source position information for types.
-- NOTE: Deprecated (probably)
purusTy :: SourceType -> PurusType
purusTy = id -- fmap (const ())

-- | Given a class name, return the TypeClassData associated with the name.
getTypeClassData :: M m => Qualified (ProperName 'ClassName) -> m TypeClassData
getTypeClassData nm = do
  env <- getEnv
  case M.lookup nm (typeClasses env) of
    Nothing -> error $ "No type class data for " </> show nm </> "  found in" </> show (typeClasses env)
    Just cls -> pure cls

-- | Given a class name, return the parameters to the class and their *kinds*. (Maybe SourceType is a kind. Type classes cannot be parameterized by anything other than type variables)
getTypeClassArgs :: M m => Qualified (ProperName 'ClassName) -> m [(T.Text,Maybe SourceType)]
getTypeClassArgs nm = getTypeClassData nm >>= (pure . typeClassArguments)


-- | Retrieves the current module name from the context. This should never fail (as we set the module name when we start converting a module)
getModuleName :: M m => m ModuleName
getModuleName = gets checkCurrentModule >>= \case
  Just mn -> pure mn
  Nothing -> error "No module name found in checkState"

-- Creates a map from a module name to the re-export references defined in
-- that module.
reExportsToCoreFn :: (ModuleName, A.DeclarationRef) -> M.Map ModuleName [Ident]
reExportsToCoreFn (mn', ref') = M.singleton mn' (exportToCoreFn ref')

toReExportRef :: A.DeclarationRef -> Maybe (ModuleName, A.DeclarationRef)
toReExportRef (A.ReExportRef _ src ref) =
      fmap
        (, ref)
        (A.exportSourceImportedFrom src)
toReExportRef _ = Nothing

-- Remove duplicate imports
dedupeImports :: [(Ann, ModuleName)] -> [(Ann, ModuleName)]
dedupeImports = fmap swap . M.toList . M.fromListWith const . fmap swap

-- | Create an Ann (with no comments or metadata) from a SourceSpan
ssA :: SourceSpan -> Ann
ssA ss = (ss, [], Nothing)

-- Gets metadata for let bindings.
getLetMeta :: A.WhereProvenance -> Maybe Meta
getLetMeta A.FromWhere = Just IsWhere
getLetMeta A.FromLet = Nothing

-- Gets metadata for values.
getValueMeta :: Environment -> Qualified Ident -> Maybe Meta
getValueMeta env name =
  case lookupValue env name of
    Just (_, External, _) -> Just IsForeign
    _ -> Nothing

-- Gets metadata for data constructors.
getConstructorMeta :: Environment -> Qualified (ProperName 'ConstructorName) -> Meta
getConstructorMeta env ctor =
  case lookupConstructor env ctor of
    (Newtype, _, _, _) -> IsNewtype
    dc@(Data, _, _, fields) ->
      let constructorType = if numConstructors (ctor, dc) == 1 then ProductType else SumType
      in IsConstructor constructorType fields
  where

  numConstructors
    :: (Qualified (ProperName 'ConstructorName), (DataDeclType, ProperName 'TypeName, SourceType, [Ident]))
    -> Int
  numConstructors ty = length $ filter (((==) `on` typeConstructor) ty) $ M.toList $ dataConstructors env

  typeConstructor
    :: (Qualified (ProperName 'ConstructorName), (DataDeclType, ProperName 'TypeName, SourceType, [Ident]))
    -> (ModuleName, ProperName 'TypeName)
  typeConstructor (Qualified (ByModuleName mn') _, (_, tyCtor, _, _)) = (mn', tyCtor)
  typeConstructor _ = internalError "Invalid argument to typeConstructor"

-- | Find module names from qualified references to values. This is used to
-- ensure instances are imported from any module that is referenced by the
-- current module, not just from those that are imported explicitly (#667).
findQualModules :: [A.Declaration] -> [ModuleName]
findQualModules decls =
 let (f, _, _, _, _) = everythingOnValues (++) fqDecls fqValues fqBinders (const []) (const [])
 in f `concatMap` decls

fqDecls :: A.Declaration -> [ModuleName]
fqDecls (A.TypeInstanceDeclaration _ _ _ _ _ _ q _ _) = getQual' q
fqDecls (A.ValueFixityDeclaration _ _ q _) = getQual' q
fqDecls (A.TypeFixityDeclaration _ _ q _) = getQual' q
fqDecls _ = []

fqValues :: A.Expr -> [ModuleName]
fqValues (A.Var _ q) = getQual' q
fqValues (A.Constructor _ q) = getQual' q
fqValues _ = []

fqBinders :: A.Binder -> [ModuleName]
fqBinders (A.ConstructorBinder _ q _) = getQual' q
fqBinders _ = []

getQual' :: Qualified a -> [ModuleName]
getQual' = maybe [] return . getQual

-- | Converts a ProperName to an Ident.
properToIdent :: ProperName a -> Ident
properToIdent = Ident . runProperName

-- "Pure" desugaring utils

-- Desugars case binders from AST to CoreFn representation. Doesn't need to be monadic / essentially the same as the old version.
binderToCoreFn ::  Environment -> ModuleName -> SourceSpan -> A.Binder -> Binder Ann
binderToCoreFn  env mn _ss (A.LiteralBinder ss lit) =
  let lit' = binderToCoreFn env mn ss <$> lit
  in  LiteralBinder (ss, [], Nothing) lit'
binderToCoreFn _ _ ss A.NullBinder =
  NullBinder (ss, [], Nothing)
binderToCoreFn _ _ _ss vb@(A.VarBinder ss name) = trace ("binderToCoreFn: " <> show vb ) $
  VarBinder (ss, [], Nothing) name
binderToCoreFn env mn _ss (A.ConstructorBinder ss dctor@(Qualified mn' _) bs) =
  let (_, tctor, _, _) = lookupConstructor env dctor
      args = binderToCoreFn env mn _ss <$> bs
  in  ConstructorBinder (ss, [], Just $ getConstructorMeta env dctor) (Qualified mn' tctor) dctor args
binderToCoreFn env mn _ss (A.NamedBinder ss name b) =
  let arg = binderToCoreFn env mn _ss b
  in  NamedBinder (ss, [], Nothing) name arg
binderToCoreFn env mn _ss (A.PositionedBinder ss _ b) =
  binderToCoreFn env mn ss  b
binderToCoreFn env mn ss  (A.TypedBinder _ b) =
  binderToCoreFn env mn ss  b
binderToCoreFn _ _ _  A.OpBinder{} =
  internalError "OpBinder should have been desugared before binderToCoreFn"
binderToCoreFn _ _ _  A.BinaryNoParensBinder{} =
  internalError "BinaryNoParensBinder should have been desugared before binderToCoreFn"
binderToCoreFn _ _ _  A.ParensInBinder{} =
  internalError "ParensInBinder should have been desugared before binderToCoreFn"



-- | Desugars import declarations from AST to CoreFn representation.
importToCoreFn :: A.Declaration -> Maybe (Ann, ModuleName)
-- TODO: We probably *DO* want types here
importToCoreFn (A.ImportDeclaration (ss, com) name _ _) = Just ((ss, com, Nothing), name)
importToCoreFn _ = Nothing

-- | Desugars foreign declarations from AST to CoreFn representation.
externToCoreFn :: A.Declaration -> Maybe Ident
externToCoreFn (A.ExternDeclaration _ name _) = Just name
externToCoreFn _ = Nothing

-- | Desugars export declarations references from AST to CoreFn representation.
-- CoreFn modules only export values, so all data constructors, instances and
-- values are flattened into one list.
exportToCoreFn :: A.DeclarationRef -> [Ident]
exportToCoreFn (A.TypeRef _ _ (Just dctors)) = fmap properToIdent dctors
exportToCoreFn (A.TypeRef _ _ Nothing) = []
exportToCoreFn (A.TypeOpRef _ _) = []
exportToCoreFn (A.ValueRef _ name) = [name]
exportToCoreFn (A.ValueOpRef _ _) = []
exportToCoreFn (A.TypeClassRef _ _) = []
exportToCoreFn (A.TypeInstanceRef _ name _) = [name]
exportToCoreFn (A.ModuleRef _ _) = []
exportToCoreFn (A.ReExportRef _ _ _) = []

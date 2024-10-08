module Language.PureScript.CoreFn.Desugar.TypeClasses where


import Prelude

import Control.Arrow (first, second)
import Control.Monad (unless)
import Control.Monad.Error.Class (MonadError (..))
import Control.Monad.State (MonadState (..), StateT, evalStateT, modify)
import Control.Monad.Supply.Class (MonadSupply)
import Data.Graph (SCC (..), stronglyConnComp)
import Data.List (find, partition)
import Data.List.NonEmpty (nonEmpty)
import Data.List.NonEmpty qualified as NEL
import Data.Map qualified as M
import Data.Maybe (catMaybes, isJust, mapMaybe)
import Data.Set qualified as S
import Data.Text (Text)
import Data.Traversable (for)
import Language.PureScript.Constants.Prim qualified as C
import Language.PureScript.Crash (internalError)
import Language.PureScript.Environment (isDictTypeName, DataDeclType (..), NameKind (..), TypeClassData (..), dictTypeName, function, makeTypeClassData, primClasses, primCoerceClasses, primIntClasses, primRowClasses, primRowListClasses, primSymbolClasses, primTypeErrorClasses, tyRecord, mkRecordT, Environment (..))
import Language.PureScript.Errors hiding (isExported, nonEmpty)
import Language.PureScript.Externs (ExternsDeclaration (..), ExternsFile (..))
import Language.PureScript.Label (Label (..))
import Language.PureScript.Names (Ident (..), ModuleName, Name (..), ProperName (..), ProperNameType (..), Qualified (..), QualifiedBy (..), coerceProperName, freshIdent, qualify, runIdent, pattern ByNullSourcePos, disqualify)
import Language.PureScript.PSString (mkString, PSString)
import Language.PureScript.Sugar.CaseDeclarations (desugarCases)
import Language.PureScript.TypeClassDictionaries (superclassName)
import Language.PureScript.Types
import Data.Text qualified as T
import Language.PureScript.TypeChecker.Monad (CheckState(..), getEnv)
import Language.PureScript.AST.Binders qualified as A
import Language.PureScript.AST.Declarations qualified as A
import Control.Monad.Writer.Class (MonadWriter)
import Control.Monad.State.Strict (modify')
import Data.Map (Map)
import Control.Lens.Plated
import Language.PureScript.CoreFn.Desugar.Utils

type TypeClasses = Map (Qualified (ProperName 'ClassName)) TypeClassData


lookupInRow :: Ident -> SourceType -> Maybe SourceType
lookupInRow (Label . mkString . runIdent -> lbl) row = case rowToList row of
  (items,_) -> rowListType <$> find (\x -> rowListLabel x == lbl) items

typeClassDictRecordTy :: M m => [SourceType] -> Qualified (ProperName 'ClassName) ->  m SourceType
typeClassDictRecordTy args qcn = mkRecordT . rowFromListSimple <$> mkTypeClassRowDict' args qcn

typeClassDictRecordTy' :: [SourceType] -> TypeClasses -> Qualified (ProperName 'ClassName) ->  SourceType
typeClassDictRecordTy' args scope qcn = mkRecordT . rowFromListSimple $ mkTypeClassRowDict args scope qcn

mkTypeClassRowDict' :: M m => [SourceType] -> Qualified (ProperName 'ClassName) -> m [(PSString,SourceType)]
mkTypeClassRowDict' args qcn = do
  s <- typeClasses <$> getEnv
  pure $ mkTypeClassRowDict args s qcn

rowFromListSimple :: [(PSString,SourceType)] -> SourceType
rowFromListSimple = foldr (\(str,ty) acc -> RCons NullSourceAnn (Label str) ty acc) (REmpty NullSourceAnn)

-- First argument is a list of arguments to the constraint or dictionary,
-- need this to ensure that we instantiate to the correct variables
mkTypeClassRowDict :: [SourceType] -> TypeClasses -> Qualified (ProperName 'ClassName) -> [(PSString,SourceType)]
mkTypeClassRowDict args scope cn = case M.lookup cn scope of
  Nothing -> error $ "Internal error: Could not generate a type class dictionary for " <> T.unpack (runProperName . disqualify $  cn)
                     <> "\nReason: No type class declaration for this class is in scope!"
  Just tcData ->
    let subs         = zip (fst <$> typeClassArguments tcData) args 
        superclasses = typeClassSuperclasses tcData
        theseMethods = (\(a,b,_) -> (mkString (runIdent a),b)) <$> typeClassMembers tcData
        superclassRows = mkSuperclassRowDict <$> superclasses
    in second (replaceAllTypeVars subs) <$> (theseMethods <> superclassRows)
 where
  -- TODO: Change this in TypeChecker.Entailment too (if it won't break anything)
  scName :: Qualified (ProperName 'ClassName) -> Text
  scName pn = runProperName (disqualify pn)

  {- I'm not sure if the PS ensures that the tyvar arguments are homogenized across classes
     so we need to make sure we replace the type variable args in the TypeClassData w/ the tv args
     from the SourceConstraint
  -}
  mkSuperclassRowDict :: SourceConstraint -> (PSString, SourceType)
  mkSuperclassRowDict (Constraint _ scn  kArgs args _) =
        let scRow    = rowFromListSimple $ mkTypeClassRowDict args scope scn
            scRecNm  = mkString $ scName scn
        in (scRecNm, mkRecordT scRow)



-- *THIS* is where we have to remove constraints everywhere in the module.
desugarConstraintType :: TypeClasses -> SourceType -> SourceType
desugarConstraintType dict = replaceDictionaryTypesWithRecords dict . transform go
 where
   go = \case
    ForAll a vis var mbk t mSkol ->
      let t' = desugarConstraintType dict t
       in ForAll a vis var mbk t' mSkol
    ConstrainedType _ (Constraint {..}) t ->
        let inner = desugarConstraintType dict t
            dictRecordTy = typeClassDictRecordTy' constraintArgs dict constraintClass
        in function dictRecordTy inner
    other -> other

desugarConstraintTypes :: (M m) => m ()
desugarConstraintTypes = do
  env <- getEnv
  let scope = typeClasses env
      f = everywhereOnTypes (desugarConstraintType scope)

      oldNameTypes = names env
      desugaredNameTypes = (\(st, nk, nv) -> (f st, nk, nv)) <$> oldNameTypes

      oldTypes = types env
      desugaredTypes = first f <$> oldTypes

      oldCtors = dataConstructors env
      desugaredCtors = (\(a, b, c, d) -> (a, b, f c, d)) <$> oldCtors

      oldSynonyms = typeSynonyms env
      desugaredSynonyms = second f <$> oldSynonyms

      newEnv =
        env
          { names = desugaredNameTypes
          , types = desugaredTypes
          , dataConstructors = desugaredCtors
          , typeSynonyms = desugaredSynonyms
          }

  modify' $ \checkstate -> checkstate {checkEnv = newEnv}

desugarConstraintsInBinder :: TypeClasses -> A.Binder -> A.Binder
desugarConstraintsInBinder scope = \case
  A.NullBinder -> A.NullBinder
  A.LiteralBinder ss lb -> A.LiteralBinder ss $ desugarConstraintsInBinder scope <$> lb
  A.VarBinder ss ident -> A.VarBinder ss ident
  A.ConstructorBinder ss qn bs -> A.ConstructorBinder ss qn $ desugarConstraintsInBinder scope <$> bs
  A.OpBinder ss qn -> A.OpBinder ss qn
  A.BinaryNoParensBinder a b c ->
    let f = desugarConstraintsInBinder scope
     in A.BinaryNoParensBinder (f a) (f b) (f c)
  A.ParensInBinder b -> A.ParensInBinder $ desugarConstraintsInBinder scope b
  A.NamedBinder ss ident b -> A.NamedBinder ss ident $ desugarConstraintsInBinder scope b
  A.PositionedBinder ss cs b -> A.PositionedBinder ss cs $ desugarConstraintsInBinder scope b
  A.TypedBinder ty b -> A.TypedBinder (desugarConstraintType scope ty) (desugarConstraintsInBinder scope b)

desugarConstraintsInDecl :: TypeClasses -> A.Declaration -> A.Declaration
desugarConstraintsInDecl scope = \case
  A.BindingGroupDeclaration decls ->
    A.BindingGroupDeclaration $
      (\(annIdent, nk, expr) -> (annIdent, nk,  overTypes (desugarConstraintType scope) $ eliminateDictionaryNewtypes expr)) <$> decls
  A.ValueDecl ann name nk bs [A.MkUnguarded e] ->
    let bs' = desugarConstraintsInBinder scope <$> bs
     in A.ValueDecl ann name nk bs' [A.MkUnguarded . overTypes (desugarConstraintType scope) $ eliminateDictionaryNewtypes e]
  A.DataDeclaration ann declTy tName args ctorDecs ->
    let fixCtor (A.DataConstructorDeclaration a nm fields) =
          A.DataConstructorDeclaration a nm (second (everywhereOnTypes (desugarConstraintType scope)) <$> fields)
     in A.DataDeclaration ann declTy tName args (fixCtor <$> ctorDecs)
  A.DataBindingGroupDeclaration ds -> A.DataBindingGroupDeclaration $ (desugarConstraintsInDecl scope) <$> ds
  other -> other

-- I guess we could/should eliminate *all* newtypes?
eliminateDictionaryNewtypes :: A.Expr -> A.Expr
eliminateDictionaryNewtypes = transform $ \case
  A.App  (A.Constructor ss cname) a | isDictTypeName (coerceProperName . disqualify $ cname) -> a
  other -> other

replaceDictionaryTypesWithRecords :: TypeClasses -> SourceType -> SourceType
replaceDictionaryTypesWithRecords scope  = transform $ \ty -> case analyzeCtor ty  of
  Just (TypeConstructor ss tname,args)
    | isDictTypeName (disqualify tname) ->
      let cname = dictTypeNameToClassName tname
      in  typeClassDictRecordTy' args scope cname
  _ -> ty
 where
   dictTypeNameToClassName :: Qualified (ProperName 'TypeName) -> Qualified (ProperName 'ClassName)
   dictTypeNameToClassName = fmap (ProperName . T.takeWhile (/= '$') . runProperName)


removeDictionaryDataDecls :: [Declaration] -> [Declaration]
removeDictionaryDataDecls = filter $ \case
  A.DataDeclaration _ _ tName _ _ -> not (isDictTypeName tName)
  _ -> True 

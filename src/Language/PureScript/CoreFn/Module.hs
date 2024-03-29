{-# LANGUAGE StandaloneDeriving, ScopedTypeVariables, StrictData  #-}
module Language.PureScript.CoreFn.Module where

import Prelude

import Data.Map.Strict (Map)
import Data.List (sort)

import Data.Text (Text)
import Language.PureScript.AST.SourcePos (SourceSpan)
import Language.PureScript.AST.Literals (Literal(..))
import Language.PureScript.Comments (Comment)
import Language.PureScript.CoreFn.Expr (Bind(..), Expr(..), CaseAlternative)
import Language.PureScript.CoreFn.Ann
import Language.PureScript.Names (Ident, ModuleName, ProperNameType (..), ProperName)
import Data.Bifunctor (second)
import Language.PureScript.AST.Declarations (DataConstructorDeclaration)
import Language.PureScript.Environment (DataDeclType)
import Language.PureScript.Types (SourceType)


-- |
-- The CoreFn module representation
--
data Module a = Module
  { moduleSourceSpan :: SourceSpan
  , moduleComments :: [Comment]
  , moduleName :: ModuleName
  , modulePath :: FilePath
  , moduleImports :: [(a, ModuleName)]
  , moduleExports :: [Ident]
  , moduleReExports :: Map ModuleName [Ident]
  , moduleForeign :: [Ident]
  , moduleDecls :: [Bind a]
  , moduleDataTypes :: Map (ProperName 'TypeName) (DataDeclType,[(Text, Maybe SourceType)],[DataConstructorDeclaration])
  } deriving (Functor, Show)

deriving instance Eq a => Eq (Module a)

data DiffResult a =
    DiffSourceSpan SourceSpan SourceSpan
  | DiffComments [Comment] [Comment]
  | DiffName ModuleName ModuleName
  | DiffPath FilePath FilePath
  | DiffImports [(a,ModuleName)] [(a,ModuleName)]
  | DiffReExports (Map ModuleName [Ident]) (Map ModuleName [Ident])
  | DiffExports [Ident] [Ident]
  | DiffForeign [Ident] [Ident]
  | DiffDecl (Maybe (Bind a)) (Maybe (Bind a))

deriving instance Eq a => Eq (DiffResult a)
deriving instance Ord a => Ord (DiffResult a)
deriving instance Show a => Show (DiffResult a)

-- TODO: Remove this? It's not that useful.

diffModule :: Module Ann -> Module Ann -> [DiffResult Ann]
diffModule m1 m2 = ezDiff DiffSourceSpan moduleSourceSpan
                   <> ezDiff DiffComments moduleComments
                   <> ezDiff DiffName moduleName
                   <> ezDiff DiffPath modulePath
                   <> ezDiff DiffImports moduleImports
                   <> ezDiff DiffReExports moduleReExports
                   <> ezDiff DiffExports moduleExports
                   <> ezDiff DiffForeign moduleForeign
                   <> diffDecls (sort $ fmap removeComments <$>  moduleDecls m1) (sort $ fmap removeComments <$> moduleDecls m2)
  where
    ezDiff :: Eq b => (b -> b -> DiffResult Ann) -> (Module Ann -> b) -> [DiffResult Ann]
    ezDiff f g
      | g m1 == g m2 = []
      | otherwise = [f (g m1) (g m2)]

    diffDecls :: [Bind Ann] -> [Bind Ann] -> [DiffResult Ann]
    diffDecls [] bs@(_:_) = map (DiffDecl Nothing . Just) bs
    diffDecls as@(_:_) [] = map (\a -> DiffDecl (Just a) Nothing) as
    diffDecls [] [] = []
    diffDecls (a:as) (b:bs)
      | a == b = diffDecls as bs
      | otherwise = DiffDecl (Just a) (Just b) : diffDecls as bs

canonicalizeModule :: Ord a => Module a -> Module a
canonicalizeModule (Module modSS modComments modName modPath modImports modExports modReExports modForeign modDecls modADTs)
  = Module modSS modComments' modName modPath modImports' modExports' modReExports' modForeign' modDecls' modADTs
 where
   modComments' = sort modComments
   modImports' = sort modImports
   modExports' = sort modExports
   modForeign' = sort modForeign
   modReExports' = sort <$> modReExports
   modDecls' = sort . map canonicalizeDecl $ modDecls

canonicalizeDecl :: Ord a => Bind a -> Bind a
canonicalizeDecl = \case
  NonRec ann ident expr -> NonRec ann ident (canonicalizeExpr expr)
  Rec recBindingGroup -> Rec . sort . fmap (second canonicalizeExpr) $ recBindingGroup

canonicalizeExpr :: Ord a => Expr a -> Expr a
canonicalizeExpr = \case
  Literal ann ty lit -> Literal ann ty (canonicalizeLit lit)
  Constructor a ty tName cName fields -> Constructor a ty tName cName fields
  Accessor a ty fieldName expr -> Accessor a ty fieldName (canonicalizeExpr expr)
  ObjectUpdate a ty origVal copyFields updateFields ->
    let updateFields' = sort $ second canonicalizeExpr <$> updateFields
        copyFields' = sort <$> copyFields
        origVal' = canonicalizeExpr origVal
    in ObjectUpdate a ty origVal' copyFields' updateFields'
  Abs a ty ident body -> Abs a ty ident (canonicalizeExpr body)
  App a  e1 e2 ->
    let e1' = canonicalizeExpr e1
        e2' = canonicalizeExpr e2
    in App a  e1' e2'
  Var a ty ident -> Var a ty ident
  -- This one is confusing. The order intrinsically matters. Can't sort at the top level. Not sure what to do about that.
  Case a ty es alts -> Case a ty (canonicalizeExpr <$> es) (canonicalizeAlt <$> alts)
  Let a binds expr ->
    let binds' = sort $ canonicalizeDecl <$> binds
        expr'   = canonicalizeExpr expr
    in Let a binds' expr'

canonicalizeAlt :: CaseAlternative a -> CaseAlternative a
canonicalizeAlt = id -- TODO

canonicalizeLit :: Literal (Expr a) -> Literal (Expr a)
canonicalizeLit = id

{-# LANGUAGE TemplateHaskell #-}

module Language.Purus.Make.Prim where

import Prelude

import Data.Map (Map)
import Data.Map qualified as M

import Data.Bifunctor ( Bifunctor(second, bimap) )

import Language.PureScript.CoreFn.Ann (Ann)
import Language.PureScript.CoreFn.Expr (Bind (..), PurusType, Expr (..))
import Language.PureScript.CoreFn.Utils (exprType)
import Language.PureScript.CoreFn.Module
import Language.PureScript.Names (
  ModuleName (..), Qualified (..), QualifiedBy (..), Ident
 )
import Language.PureScript.Types (Type(TypeConstructor))

import Language.Purus.Prim.Data (primDataPS)
import Language.Purus.TH (ctDecodeModule)
import Language.Purus.IR.Utils 

import Control.Lens.Combinators (transform, over)
import Control.Lens (_2)


replaceModuleNameInModule :: ModuleName -> -- old module name
                             ModuleName -> -- new module name
                             Module (Bind Ann) PurusType PurusType Ann ->
                             Module (Bind Ann) PurusType PurusType Ann
replaceModuleNameInModule oldMn newMn (Module srcSpan comments _oldName path imports exports reExports mForeign decls datatypes)
    = Module
        srcSpan
        comments
        (f _oldName)
        path
        imports
        exports
        reExports
        mForeign
        (renameDecl <$> decls)
        (renameDatatypes datatypes)
 where
   f :: ModuleName -> ModuleName
   f mn | mn == oldMn = newMn
        | otherwise   = mn

   goQualified :: forall (a :: *). Qualified a -> Qualified a
   goQualified = \case
     Qualified (ByModuleName mn) x -> Qualified (ByModuleName (f mn)) x
     other -> other

   renameDatatypes :: Datatypes PurusType PurusType -> Datatypes PurusType PurusType
   renameDatatypes (Datatypes typeDict ctDict) = Datatypes typeDict' ctorDict'
     where
       typeDict' = M.fromList
                   . fmap (bimap goQualified goDataDecl)
                   . M.toList
                   $ typeDict

       ctorDict' = M.fromList
                   . fmap (bimap goQualified goQualified)
                   . M.toList
                   $ ctDict

       goDataDecl :: DataDecl PurusType PurusType -> DataDecl PurusType PurusType
       goDataDecl (DataDecl ddTy ddDataTyName ddDataArgs ddDataCtors)
         = DataDecl ddTy (goQualified ddDataTyName) (second goType <$> ddDataArgs) (goCtorDecl <$> ddDataCtors)

       goCtorDecl :: CtorDecl PurusType -> CtorDecl PurusType
       goCtorDecl (CtorDecl cdName cdFields) = CtorDecl (goQualified cdName) (second goType <$> cdFields)

       goType :: PurusType -> PurusType
       goType = transform $ \case
         TypeConstructor ann qtn -> TypeConstructor ann (goQualified qtn)
         other -> other

   renameDecl :: Bind Ann -> Bind Ann
   renameDecl = \case
     NonRec ann ident e -> NonRec ann ident $ transform go e
     Rec bs -> Rec $ second (transform go) <$> bs
    where
      go :: Expr Ann -> Expr Ann
      go e = case e of
        Var ann ty qi -> Var ann ty (goQualified qi)
        other -> other

prelude :: Module (Bind Ann) PurusType PurusType Ann
prelude = $(ctDecodeModule "tests/purus/passing/prelude/output/Prelude/Prelude.cfn")

primModule :: Module (Bind Ann) PurusType PurusType Ann
primModule  = renamed {moduleDataTypes = newDatatypes}
 where
   renamed = replaceModuleNameInModule (ModuleName "Prelude") (ModuleName "Prim") prelude
   oldDatatypes = moduleDataTypes renamed
   newDatatypes = oldDatatypes <> primDataPS

primValueTypes :: Map (Qualified Ident) PurusType
primValueTypes = M.fromList . concatMap go $ moduleDecls primModule
  where
    q = Qualified (ByModuleName (ModuleName "Prim"))
    go :: Bind Ann -> [(Qualified Ident, PurusType)]
    go = \case
      NonRec _ ident expr -> [(q ident, exprType expr)]
      Rec xs -> map (\((_,ident),expr) -> (q ident, exprType expr)) xs  

   

{-
[NOTE 1]

  This function renames every Qualified Ident/TypeName/ProperName/etc everywhere in the module such that
  it has the new name provided as an argument.

  At the moment, it has to be Builtin or Prim if we want to use it as an imported-by-default or always-available
  module, because only those are properly supported in Language.PureScript.Sugar.Names.Env, though in principle we could have as many
  prim modules as we like.

-}

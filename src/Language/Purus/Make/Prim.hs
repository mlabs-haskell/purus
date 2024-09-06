{-# LANGUAGE TemplateHaskell #-}
module Language.Purus.Make.Prim where

import Prelude

import Data.Map (Map)
import Data.Map qualified as M

import Data.Bifunctor ( Bifunctor(second, bimap) )

import Language.PureScript.AST.SourcePos (pattern NullSourceSpan)
import Language.PureScript.CoreFn.Ann (Ann)
import Language.PureScript.CoreFn.Expr (Bind (..), PurusType, Expr (..))
import Language.PureScript.CoreFn.Utils (exprType)
import Language.PureScript.CoreFn.Module
    ( CtorDecl(CtorDecl),
      DataDecl(DataDecl),
      Datatypes(Datatypes),
      Module(Module, moduleName, moduleDataTypes, moduleDecls) )
import Language.PureScript.Names (
  ModuleName (..), Qualified (..), QualifiedBy (..), Ident
 )
import Language.PureScript.Types (Type(TypeConstructor))
import Language.Purus.Prim.Data (primDataPS)
import Language.Purus.Utils (decodeModuleBS)

import Control.Lens.Combinators (transform)
import System.IO.Unsafe (unsafePerformIO)
import Data.FileEmbed ( embedFile )
import Data.ByteString (ByteString)

primifyModule :: ModuleName -> -- old module name
                             ModuleName -> -- new module name
                             Module (Bind Ann) PurusType PurusType Ann ->
                             Module (Bind Ann) PurusType PurusType Ann
primifyModule oldMn newMn (Module srcSpan comments _oldName path imports exports reExports mForeign decls datatypes)
    = Module
        srcSpan
        comments
        (f _oldName)
        path
        (filter (isPrimModule . snd) imports)
        exports
        reExports
        mForeign
        (renameDecl <$> decls)
        (renameDatatypes datatypes)
 where
   isPrimModule :: ModuleName -> Bool
   isPrimModule (ModuleName x) = x == "Prim" || x == "Builtin"

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

prelude1Raw :: ByteString
prelude1Raw = $(embedFile "src/ps-libs/prelude/Prelude/Prelude.cfn")

prelude2Raw :: ByteString
prelude2Raw = $(embedFile "src/ps-libs/prelude/Prelude2/Prelude2.cfn")

prelude3Raw :: ByteString
prelude3Raw = $(embedFile "src/ps-libs/prelude/Prelude3/Prelude3.cfn")


{-# NOINLINE prelude1 #-}
prelude1 :: Module (Bind Ann) PurusType PurusType Ann
prelude1 = unsafePerformIO $ decodeModuleBS prelude1Raw

{-# NOINLINE prelude2 #-}
prelude2 :: Module (Bind Ann) PurusType PurusType Ann
prelude2 = unsafePerformIO $ decodeModuleBS prelude2Raw

{-# NOINLINE prelude3 #-}
prelude3 :: Module (Bind Ann) PurusType PurusType Ann
prelude3 = unsafePerformIO $ decodeModuleBS prelude3Raw


mkPrimModule :: Module (Bind Ann) PurusType PurusType Ann -> Module (Bind Ann) PurusType PurusType Ann
mkPrimModule mdl = renamed {moduleDataTypes = newDatatypes}
 where
   renamed = primifyModule (moduleName mdl) (ModuleName "Prim") mdl
   oldDatatypes = moduleDataTypes renamed
   newDatatypes = oldDatatypes <> primDataPS

primValueTypes :: Module (Bind Ann) PurusType PurusType Ann -> Map (Qualified Ident) PurusType
primValueTypes = M.fromList . concatMap go . moduleDecls
  where
    q = Qualified (ByModuleName (ModuleName "Prim"))
    go :: Bind Ann -> [(Qualified Ident, PurusType)]
    go = \case
      NonRec _ ident expr -> [(q ident, exprType expr)]
      Rec xs -> map (\((_,ident),expr) -> (q ident, exprType expr)) xs  

-- we assume that we've updated the names, and that e.g. both modules have the same ModuleName (likely "Prim")
-- there shouldn't be any re-exports or foreigns
-- we can ignore comments (I think?)
mergeModules :: Module (Bind Ann) PurusType PurusType Ann ->
                Module (Bind Ann) PurusType PurusType Ann ->
                Module (Bind Ann) PurusType PurusType Ann
mergeModules
  (Module _ss1 _ _  _    imports1 exports1 _ _ decls1 datatypes1)
  (Module _ss2 _ nm path _        exports2 _ _ decls2 datatypes2) =
    Module
      NullSourceSpan
      []
      nm
      path
      imports1 -- should only import Prim, I think?
      (exports1 <> exports2)
      M.empty -- no re exports
      [] -- no foreign
      (decls1 <> decls2) -- combine the decls
      (datatypes1 <> datatypes2) -- combine the datatypes


syntheticPrim :: Module (Bind Ann) PurusType PurusType Ann
syntheticPrim = prim1 `mergeModules` prim2 `mergeModules` prim3
  where
    prim1 = mkPrimModule prelude1
    prim2 = mkPrimModule prelude2
    prim3 = mkPrimModule prelude3

syntheticPrimValueTypes :: Map (Qualified Ident) PurusType
syntheticPrimValueTypes =  primValueTypes syntheticPrim

{-
[NOTE 1]

  This function renames every Qualified Ident/TypeName/ProperName/etc everywhere in the module such that
  it has the new name provided as an argument.

  At the moment, it has to be Builtin or Prim if we want to use it as an imported-by-default or always-available
  module, because only those are properly supported in Language.PureScript.Sugar.Names.Env, though in principle we could have as many
  prim modules as we like.

-}

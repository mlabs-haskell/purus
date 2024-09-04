module Language.Purus.Make.Prim where

import Prelude

import Control.Exception (throwIO)

import Data.Text (Text)
import Data.Text qualified as T

import Data.Map qualified as M
import Data.Set qualified as S

import Data.Function (on)

import Data.Foldable (foldrM)
import Data.List (delete, foldl', groupBy, sortBy, stripPrefix)

import System.FilePath (
  makeRelative,
  takeDirectory,
  takeExtensions,
  (</>),
 )

import Language.PureScript.CoreFn.Ann (Ann)
import Language.PureScript.CoreFn.Expr (Bind, PurusType)
import Language.PureScript.CoreFn.Module
import Language.PureScript.Names (
  Ident (Ident),
  ModuleName (..),
  runIdent,
  runModuleName,
 )
import Language.PureScript.Types (SourceType)

import Language.Purus.Eval
import Language.Purus.IR.Utils (IR_Decl, foldBinds)
import Language.Purus.Pipeline.CompileToPIR (compileToPIR)
import Language.Purus.Pipeline.DesugarCore (desugarCoreModule)
import Language.Purus.Pipeline.DesugarObjects (
  desugarObjects,
  desugarObjectsInDatatypes,
 )
import Language.Purus.Pipeline.EliminateCases (eliminateCases)
import Language.Purus.Pipeline.GenerateDatatypes (
  generateDatatypes,
 )
import Language.Purus.Pipeline.Inline (inline)
import Language.Purus.Pipeline.Instantiate (applyPolyRowArgs, instantiateTypes)
import Language.Purus.Pipeline.Lift (lift)
import Language.Purus.Pipeline.Monad (
  CounterT (runCounterT),
  DesugarCore,
  globalScope,
  runCounter,
  runDesugarCore,
  runInline,
  runPlutusContext,
 )
import Language.Purus.Pretty.Common (prettyStr)
import Language.Purus.Prim.Data (primDataPS)
import Language.Purus.Types (PIRTerm, PLCTerm, initDatatypeDict)
import Language.Purus.Utils (
  decodeModuleIO,
  findDeclBodyWithIndex,
 )

import Control.Monad.Except (MonadError (throwError))
import Control.Monad.State (evalStateT)

import Control.Lens (At (at))
import Control.Lens.Combinators (folded)
import Control.Lens.Operators ((^?))

{- You know, maybe it'd be better to do this *after* CoreFn desugaring?

synthesizeModule :: FilePath -- path to the .cfn CoreFn JSON module
                    {-  Optional datatype declarations to shim here. We expect these
                        to already have the Module Name provided as an argument here
                    -}
                 -> Datatypes SourceType SourceType
                 -> ModuleName -- the *new* name for the module, e.g. Prim. See [NOTE 1]
                 -> IO (Module (Bind Ann) PurusType PurusType Ann)
synthesizeModule path datatypes newName = do
  old <- decodeModuleIO path
  let renamed = renameEverything old
      newDataTypes = datatypes <> moduleDataTypes renamed
  pure $ renamed {moduleDataTypes = newDataTypes}
 where
   renameEverything :: Module (Bind Ann) PurusType PurusType Ann
                    -> Module (Bind Ann) PurusType PurusType Ann
   renameEverything (Module srcSpan comments _oldName _path imports exports reExports mForeign decls mDatatypes)
    = Module
        (renameSrcSpan srcSpan)
        (renameComments comments)
        newName
        path
        (renameImports imports)
        exports
        (renameReExports reExports)
        mForeign
        (renameDecls decls)

-}

{-
[NOTE 1]

  This function renames every Qualified Ident/TypeName/ProperName/etc everywhere in the module such that
  it has the new name provided as an argument.

  At the moment, it has to be Builtin or Prim if we want to use it as an imported-by-default or always-available
  module, because only those are properly supported in Language.PureScript.Sugar.Names.Env, though in principle we could have as many
  prim modules as we like.

-}

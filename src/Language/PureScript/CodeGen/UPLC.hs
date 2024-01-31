{-# OPTIONS_GHC -Wno-redundant-constraints #-}
module Language.PureScript.CodeGen.UPLC (moduleToUPLC, printUPLC) where

import Prelude ((.), ($))
import Protolude (print)
import Protolude.Error (error)

import Control.Monad.Except (MonadError)
import Control.Monad.Reader (MonadReader)
import Control.Monad.Supply.Class (MonadSupply)
import Control.Monad.IO.Class (MonadIO (liftIO))

import Language.PureScript.CoreFn (Ann, Module(..))
import Language.PureScript.Errors (MultipleErrors(..))
import Language.PureScript.Options (Options(..))


import UntypedPlutusCore
    ( DeBruijn, DefaultFun, DefaultUni, Program )
import PlutusCore.Pretty ( prettyPlcReadableDef )

-- Stolen from Ply, not 100% sure if this is what we want, i.e. maybe there should be an annotation?
type UPLCProgram = Program DeBruijn DefaultUni DefaultFun ()

moduleToUPLC :: forall m
              . (MonadReader Options m, MonadSupply m, MonadError MultipleErrors m)
             => Module Ann -> m UPLCProgram
moduleToUPLC = error "Error: UPLC Backend not yet implemented!"

printUPLC :: forall m. MonadIO m => UPLCProgram -> m ()
printUPLC program = liftIO . print $ prettyPlcReadableDef program

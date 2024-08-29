{- Various concrete monads used by the pipeline components. There's probably a more elegant solution,
   but this works well-enough for now.

   DesugarCore and Inline run in the monads with those names.

   Lift runs in the Inline monad.

   DesugarObjects runs in `Counter` (it only needs access to the stream of uniques)

   GenerateDatatypes, EliminateCases, and CompileToPIR run in the PlutusContext Monad.

   Intantiate is pure.
-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StarIsType #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Language.Purus.Pipeline.Monad where

import Prelude

import Data.Map (Map)
import Data.Map qualified as M

import Language.PureScript.CoreFn.Ann (Ann)
import Language.PureScript.CoreFn.Expr (PurusType)
import Language.PureScript.CoreFn.Module (Module)
import Language.PureScript.Names (Ident, ModuleName)
import Language.Purus.IR.Utils (IR_Decl)
import Language.Purus.Types (DatatypeDictionary)

import Control.Lens.Operators ((%=), (+=), (.=))
import Control.Lens.TH (makeLenses)
import Control.Monad.Except (MonadError)
import Control.Monad.Reader (MonadReader (..), MonadTrans (..))
import Control.Monad.State (
  MonadState (get, put),
  StateT (..),
  evalStateT,
  gets,
 )

import Prettyprinter (
  Pretty (pretty),
  align,
  hardline,
  indent,
  vcat,
  (<+>),
 )

newtype CounterT m a = CounterT {runCounterT :: StateT Int m a}
  deriving newtype (Functor, Applicative, Monad, MonadTrans)

deriving instance (MonadError e m) => MonadError e (CounterT m)
deriving instance (MonadReader r m) => MonadReader r (CounterT m)

instance (MonadState s m) => MonadState s (CounterT m) where
  get = CounterT $ runCounterT (lift get)
  put x = CounterT $ runCounterT (put x)

class MonadCounter (m :: * -> *) where
  next :: m Int

instance (Monad m) => MonadCounter (CounterT m) where
  next = CounterT $ do
    s <- get
    id += 1
    pure s

instance (Monad m) => MonadCounter (StateT s (CounterT m)) where
  next = lift next

{- The different compiler monads are primarily distinguished by their State type,
   this is a way to generalize that.
-}
newtype PurusM s a = PurusM {runPurusM :: StateT s (CounterT (Either String)) a}
  deriving newtype (Functor, Applicative, Monad, MonadCounter, MonadError String, MonadState s)

instance MonadReader r (PurusM r) where
  ask = get

  local f act = do
    s <- get
    id %= f
    res <- act
    id .= s
    pure res

  reader f = gets f

evalPurusM :: s -> PurusM s a -> CounterT (Either String) a
evalPurusM s pm = evalStateT (runPurusM pm) s

runStatePurusM :: s -> PurusM s a -> CounterT (Either String) (a, s)
runStatePurusM s pm = runStateT (runPurusM pm) s

{- See explanation in Language.Purus.Pipeline.Desugar for why we need two scopes -}
data DesugarContext = DesugarContext {_globalScope :: Map ModuleName (Map Ident Int), _localScope :: Map Ident Int}
  deriving (Show, Eq)

instance Pretty DesugarContext where
  pretty (DesugarContext globals locals) =
    let globals' =
          align
            . vcat
            . fmap (\(a, b) -> pretty a <+> ":=" <+> b <> hardline)
            . M.toList
            $ indent 2
              . align
              . vcat
              . map pretty
              . M.toList
              <$> globals

        locals' = align . vcat . map pretty . M.toList $ locals
     in "DesugarContext:"
          <> hardline
          <> "Globals:"
          <> indent 2 globals'
          <> hardline
          <> "Locals:"
          <> indent 2 locals'
          <> hardline

instance Semigroup DesugarContext where
  (DesugarContext gb1 lb1) <> (DesugarContext gb2 lb2) = DesugarContext (gb1 <> gb2) (lb1 <> lb2)

instance Monoid DesugarContext where
  mempty = DesugarContext M.empty M.empty

makeLenses ''DesugarContext

newtype DesugarCore a = DesugarCore (PurusM DesugarContext a)
  deriving newtype
    ( Functor
    , Applicative
    , Monad
    , MonadError String
    , MonadCounter
    , MonadState DesugarContext
    , MonadReader DesugarContext
    )

runDesugarCore :: DesugarCore a -> CounterT (Either String) (a, DesugarContext)
runDesugarCore (DesugarCore psm) = runStatePurusM mempty psm

newtype Inline a = Inline (PurusM (Module IR_Decl PurusType PurusType Ann) a)
  deriving newtype (Functor, Applicative, Monad, MonadError String, MonadCounter, MonadReader (Module IR_Decl PurusType PurusType Ann))

runInline :: Module IR_Decl PurusType PurusType Ann -> Inline a -> CounterT (Either String) a
runInline modl (Inline psm) = evalPurusM modl psm

newtype PlutusContext a = PlutusContext (PurusM DatatypeDictionary a)
  deriving newtype (Functor, Applicative, Monad, MonadError String, MonadCounter, MonadState DatatypeDictionary, MonadReader DatatypeDictionary)

runPlutusContext :: DatatypeDictionary -> PlutusContext a -> CounterT (Either String) a
runPlutusContext dtdict (PlutusContext psm) = evalPurusM dtdict psm

newtype Counter a = Counter (PurusM () a)
  deriving newtype (Functor, Applicative, Monad, MonadError String, MonadCounter)

runCounter :: Counter a -> CounterT (Either String) a
runCounter (Counter psm) = evalPurusM () psm

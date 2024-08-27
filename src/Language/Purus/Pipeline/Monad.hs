{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StarIsType #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TemplateHaskell #-}
module Language.Purus.Pipeline.Monad where

import Prelude


import Data.Map (Map)
import Data.Map qualified as M



import Language.PureScript.CoreFn (Ann)
import Language.PureScript.CoreFn.Expr
import Language.PureScript.CoreFn.Module
import Language.PureScript.Names
import Language.Purus.IR.Utils
import Language.Purus.Types

import Control.Monad.State
import Control.Monad.Except (MonadError)
import Control.Monad.Reader
import Control.Monad.Trans.Except (ExceptT)

import Control.Lens.TH (makeLenses)
import Control.Lens.Operators

import Prettyprinter

{- [Current Compilation Pipeline Structure]
     - That is, "current" before the `Language.Purus` reorganization.
     - It's a bit of a mess so I gotta remember how it all works before cleaning it up :p

DesugarCore ->
Lift ->
Inline ->
Instantiate ->
DesugarObjects ->
GenerateDatatypes ->
EliminateCaseExpressions ->
CompileToPIR

NOTE: ALL THE ERRORS are morally strings at this point

NOTE: I'm "factoring out" the Supply part of the types. *Everything* here needs
      access to that.

1. DesugarCore :: [Module (Bind Ann) PurusType PurusType Ann]
               -> State (Map Ident Int) (Module IR_Decl SourceType SourceType Ann)

   - NOTE: The state represents `unique` bindings for bound variables
   - NOTE: At present, this state gets passed to DesugarObjects.
           That's a bit weird. I don't think it needs to be passed to
           DesugarObjects now that we bind *everything* in DesugarCore?
           - If we don't need to pass it to DesugarObjects then the return type of
             this is just (Module IR_Decl SourceType SourceType Ann)
   - NOTE: I think we want to smash the in-scope data declarations into the
           `Module` returned by this, but if we do that we need to add that as an
           argument

-- NOTE: After core desugaring, we're only ever concerned with a single module
         (and its scope). We should make sure that dependencies (including datatypes)
         are resolved after this point.

2. Lift :: (Ident,Int)
        -> Exp WithObjects PurusType
        -> Reader
             (ModuleName, Module IR_Decl SourceType SourceType Ann)
             LiftResult

   - NOTE: This and inline run in the old Monomorphizer monad.
   - NOTE: It seems to *only* use the Module to get ahold of the declarations
           (this is likely also true for Inline as well)

3. Inline :: LiftResult
          -> Reader
               (ModuleName, Module IR_Decl SourceType SourceType Ann)
               Exp WithObjects PurusType

   - NOTE: Lift composes with this to give us `(Ident,Int) -> Exp WithObjects PurusType -> m Exp WithObjects PurusType `
   - NOTE: We don't seem to care about the module context anymore, as we've already
           extracted the used declarations in `Lift`. This just uses the
           unique counter portion of Monomorphizer.
           * So the return type of this can be `Exp WithObjects PurusType` (w/ the counter factored out)
   - NOTE: After this stage, we don't care about the module declarations or anything else,
           but we *do* care about the datatypes!

4. Instantiate :: Exp WithObjects PurusType -> Exp WithObjects PurusType

  - NOTE: Pure.
  - NOTE: We probably want to rewrite this to be generic over the XObjectFoo and Type representation. We'll have
          to do it again after or during object desugaring (but we *must* do it before then).

5. DesugarObjects :: Exp Withobjects PurusType -> State (Map Ident Int) (Exp WithoutObjects Ty)

  - NOTE: Shouldn't actually need to use the Map, and is pure modulo the unique counter afaict

-- NOTE: The rest of the steps run in `DataTypeM`, which is a state monad with a counter plus some bookkeeping
         stuff required for the final passes.

6. GenerateDataTypes :: Datatypes Kind Ty
                     -> Exp WithoutObjects Ty
                     -> State DatatypeDictionary ()
   - This gives us a state with a bunch of Maps from PS names to PIR Names (with uniques), for
     constructors, datatypes, type names, type variables, etc.

   - We need to (or should, at least) do this *before* case expression elimination because
     we need to know *what* the Unique is for the destructor functions (and also for the
     types that we mention in the destructor functions).

7. EliminateCaseExpressions :: Exp WithoutObjects Ty
                            -> State DatatypeDictionary (Exp WithoutObjects Ty)
  - This should be *reader* DatatypeDictionary b/c it never modifies it. I don't think CompileToPIR does either?

8. CompileToPIR :: DataTypes Kind Ty
                -> Exp WithoutObjects Ty
                -> PIRTerm

 - It *really* should not need those datatypes. I don't think it uses them for anything except constructor
   instantiation, which should happen *way* before this. I'm guessing that was a dirty hack to
   ensure that everything got instantiated again after desugarobjects?
 - This needs updated to use the Exp x t (Vars t) concrete style (which is vastly superior)
 - Need to split some of the PIR utils out into their own utility

Major changes to make while fixing all this:
  - Delete the "IR.Bindings" maps from let expressions and anywhere else I left them
  - Refactor using the new IR utils, especially toExp/fromExp/viaExp and foldBind/Alt/etc

So there are basically 4 different phases:

   - a) DesugarCore,  hich maintains an ident-int map local state (for initial index generation )

   - b) Lift and Inline read from, but do not modify, the set of in-scope expression declarations
        for the main module being compiled

   - c) Instantiate is pure. DesugarObjects *should* be pure modulo the counter

   - d) GenerateDatatypes makes the DatatypeDictionary which EliminateCaseExpressions, and CompileToPIR
        read from.

-}

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

runStatePurusM :: s -> PurusM s a -> CounterT (Either String) (a,s)
runStatePurusM s pm = runStateT (runPurusM pm) s

data DesugarContext = DesugarContext {_globalScope :: Map ModuleName (Map Ident Int), _localScope :: Map Ident Int}
  deriving (Show, Eq)

instance Pretty DesugarContext where
  pretty (DesugarContext globals locals) =
    let globals' = align
                   . vcat
                   . fmap (\ (a,b) -> pretty a <+> ":=" <+> b <> hardline)
                   . M.toList
                   $ indent 2
                     . align
                     . vcat
                     . map pretty
                     . M.toList
                     <$> globals

        locals' = align . vcat . map pretty . M.toList $ locals
    in "DesugarContext:" <> hardline
       <> "Globals:" <> indent 2 globals' <> hardline
       <> "Locals:" <> indent 2 locals' <> hardline 

instance Semigroup DesugarContext where
  (DesugarContext gb1 lb1) <> (DesugarContext gb2 lb2) = DesugarContext (gb1 <> gb2) (lb1 <> lb2)

instance Monoid DesugarContext where
  mempty = DesugarContext M.empty M.empty 

makeLenses ''DesugarContext

newtype DesugarCore a = DesugarCore (PurusM DesugarContext a)
  deriving newtype (Functor,
                    Applicative,
                    Monad,
                    MonadError String,
                    MonadCounter,
                    MonadState DesugarContext,
                    MonadReader DesugarContext)

runDesugarCore :: DesugarCore a -> CounterT (Either String) (a,DesugarContext)
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

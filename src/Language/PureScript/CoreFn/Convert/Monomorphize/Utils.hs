{-# OPTIONS_GHC -Wno-orphans #-} -- has to be here (more or less)
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}

module Language.PureScript.CoreFn.Convert.Monomorphize.Utils  where

import Prelude

import Language.PureScript.CoreFn.Expr (PurusType, Bind)
import Language.PureScript.CoreFn.Convert.IR (_V, Exp(..), FVar(..), BindE(..), BVar (..), flattenBind, abstractMany, mkBindings, Alt (..), Lit (..), expTy)
import Language.PureScript.Names (Ident(..), ModuleName (..), QualifiedBy (..), Qualified (..), pattern ByNullSourcePos)
import Language.PureScript.Types
    ( SourceType, RowListItem (..), rowToList, Type (..), Constraint(..) )
import Language.PureScript.CoreFn.FromJSON ()
import Data.Text qualified as T
import Data.Map (Map)
import Data.Map qualified as M
import Control.Lens ( (<&>), (^?) )
import Control.Monad.RWS.Class (gets, modify', MonadReader (..))
import Control.Monad.RWS (RWST(..))
import Control.Monad.Except (throwError)
import Data.Text (Text)
import Bound.Var (Var(..))
import Bound.Scope (Scope (..), abstractEither, toScope, fromScope, mapScope, mapBound, mapMBound)
import Data.Bifunctor (Bifunctor (..))
import Data.List (find)
import Control.Lens.Plated ( transform, Plated(..) )
import Language.PureScript.Environment (pattern (:->))
import Language.PureScript.CoreFn.Pretty (prettyTypeStr)
import Language.PureScript.AST.SourcePos ( SourceAnn )
import Language.PureScript.PSString (PSString)
import Language.PureScript.Label (Label(runLabel))
import Language.PureScript.CoreFn.Module ( Module(..) )
import Language.PureScript.CoreFn.Ann ( Ann )
import Language.PureScript.CoreFn.Convert.DesugarCore
    ( WithObjects )
import Data.Aeson qualified as Aeson
import GHC.IO (throwIO)

type IR_Decl = BindE PurusType (Exp WithObjects PurusType) (FVar PurusType)

{- Monomorphizer monad & related utilities -}

-- TODO: better error messages
newtype MonoError
 = MonoError String deriving (Show)

-- Just a newtype over `Int`
newtype MonoState = MonoState {
  unique :: Int
}

-- Reads (ModuleName,ModuleDecls), writes nothing (...yet), State is a newtype over Int for fresh names & etc
type Monomorphizer a = RWST (ModuleName, [BindE PurusType (Exp WithObjects PurusType) (FVar PurusType)]) () MonoState (Either MonoError)  a

getModName :: Monomorphizer ModuleName
getModName = ask <&> fst

getModBinds :: Monomorphizer [BindE PurusType (Exp WithObjects PurusType) (FVar PurusType)]
getModBinds = ask <&> snd

note ::  String -> Maybe b -> Monomorphizer b
note  err = \case
  Nothing -> throwError $ MonoError err
  Just x -> pure x

freshen :: Ident -> Monomorphizer Ident
freshen ident = do
  u <- gets unique
  modify' $ \(MonoState  _) -> MonoState  (u + 1)
  let uTxt = T.pack (show u)
  case ident of
    Ident t -> pure $ Ident $ t <> "_$$" <> uTxt
    GenIdent (Just t) i -> pure $ GenIdent (Just $ t <> "_$$" <> uTxt) i -- we only care about a unique ord property for the maps
    GenIdent Nothing i  -> pure $ GenIdent (Just $ "var_$$" <> uTxt) i
    -- other two shouldn't exist at this stage
    other -> pure other

freshBVar :: t -> Monomorphizer (BVar t)
freshBVar t = do
  u <- gets unique
  modify' $ \(MonoState  _) -> MonoState  (u + 1)
  let gIdent = Ident $ T.pack ("x_$$" <> show u)
  pure $ BVar u t gIdent

{-
   Misc utils for constructing/analyzing expressions
-}

qualifyNull :: a -> Qualified a
qualifyNull = Qualified ByNullSourcePos

-- Construct a Let expression from a list of BindEs and a scoped body
gLet ::
  [BindE PurusType (Exp WithObjects PurusType) (FVar PurusType)] ->
  Scope (BVar PurusType) (Exp WithObjects PurusType) (FVar PurusType) ->
  Exp WithObjects PurusType (FVar PurusType)
gLet binds e =  LetE bindings binds $ abstractEither abstr e'
  where
    e' = fromScope e
    bindings = mkBindings allBoundIdents
    allBoundIdents = uncurry (flip FVar . qualifyNull) <$> second (expTy F) <$> concatMap flattenBind binds

    abstr :: Var (BVar PurusType) (FVar PurusType) -> Either (BVar PurusType) (FVar PurusType)
    abstr = \case
      B bv -> Left bv
      F fv -> case abstractMany allBoundIdents fv of
        Nothing -> Right fv
        Just bv -> Left bv

-- Tools for updating variable types/names

-- REVIEW: Is this right? Do we really want to update bound & free var types at the same time like this?
updateVarTyS :: forall x t
              . BVar t
             -> t
             -> Scope (BVar t) (Exp x t) (FVar t)
             -> Scope (BVar t) (Exp x t) (FVar t)
updateVarTyS (BVar ix _ ident) ty scoped = mapScope goBound goFree scoped
  where
    goBound :: BVar t -> BVar t
    goBound bv@(BVar bvIx _ bvIdent)
      | bvIx == ix && bvIdent == ident = BVar bvIx ty ident
      | otherwise = bv

    goFree :: FVar t -> FVar t
    goFree fv@(FVar _ (Qualified q@(BySourcePos _) varId))
      | varId == ident = FVar ty (Qualified q varId)
      | otherwise = fv
    goFree other = other


-- doesn't change types!
renameBoundVar :: Ident
               -> Ident
               -> Scope (BVar t) (Exp WithObjects t) (FVar t)
               -> Scope (BVar t) (Exp WithObjects t) (FVar t)
renameBoundVar old new  = mapBound $ \case
  BVar bvIx bvTy bvIdent | bvIdent == old -> BVar bvIx bvTy new
  other -> other


{- Given a function and a list of arguments that hopefully
   match the type & number of the args in the functions signature,
   apply the function to all of the arguments.

   TODO: Eventually we shouldn't need this but it's useful to throw errors
         while debugging if we get something that's not a function
-}
unsafeApply ::
  Exp WithObjects PurusType (FVar PurusType) ->
  [Exp WithObjects PurusType (FVar PurusType)] ->
  Exp WithObjects PurusType (FVar PurusType)
unsafeApply e (arg:args)= case expTy F e of
  (_ :-> _) -> unsafeApply (AppE e arg) args
  other -> Prelude.error $ "Unexpected argument to unsafeApply:" <> prettyTypeStr other
unsafeApply e [] = e

{- Find the declaration *group* to which a given identifier belongs.
-}
findInlineDeclGroup ::
  Ident ->
  [BindE ty (Exp x ty) a] ->
  Maybe (BindE ty (Exp x ty) a)
findInlineDeclGroup _ [] = Nothing
findInlineDeclGroup ident (NonRecursive ident' expr:rest)
  | ident == ident' = Just $ NonRecursive ident' expr
  | otherwise = findInlineDeclGroup ident rest
findInlineDeclGroup ident (Recursive xs:rest) = case  find (\x -> fst x == ident) xs of
  Nothing -> findInlineDeclGroup ident rest
  Just _ -> Just (Recursive xs)

{- Find the body of a declaration with the given name in the given module.
-}
findDeclBody :: Text
             -> Module IR_Decl k t Ann
             -> Maybe (Exp WithObjects PurusType (FVar PurusType))
findDeclBody nm Module{..} = findDeclBody' (Ident nm) moduleDecls

findDeclBody' :: Ident -> [BindE ty (Exp x ty) a] -> Maybe (Exp x ty a)
findDeclBody' ident binds = case findInlineDeclGroup ident binds of
  Nothing -> Nothing
  Just decl -> case decl of
    NonRecursive _ e -> Just e
    Recursive xs -> snd <$> find (\x -> fst x == ident) xs
{- Turns a Row Type into a Map of field names to Row item data.

   NOTE: Be sure to unwrap the enclosing record if you're working w/ a
         record type.
-}
mkFieldMap :: SourceType -> M.Map PSString (RowListItem SourceAnn)
mkFieldMap fs = M.fromList $ (\x -> (runLabel (rowListLabel x),x)) <$> (fst . rowToList $ fs)

-- TODO: Doesn't have much of a purpose after the IR rework
extractAndFlattenAlts :: Alt x t (Exp x t) a -> [Scope (BVar t) (Exp x t) a]
extractAndFlattenAlts (UnguardedAlt _ _ res) = [res]


{- Updates the identifier and type of free variables using the provided Map

   Note that this erases original source position information, since it is meant to be
   used during inlining (and ergo the original source position may no longer be
   accurate or meaningful, e.g. in generated code)
-}
updateFreeVars :: Map Ident (Ident, SourceType)
               -> Exp WithObjects PurusType (FVar PurusType)
               -> Exp WithObjects PurusType (FVar PurusType)
updateFreeVars dict = transform updateFreeVar
  where
    updateFreeVar :: Exp WithObjects PurusType (FVar PurusType) -> Exp WithObjects PurusType (FVar PurusType)
    updateFreeVar  expr = case expr ^? _V of
     Just (FVar _ (Qualified (ByModuleName _) varId)) -> case M.lookup varId dict of
       Nothing -> expr
       Just (newId,newType) -> V (FVar newType (Qualified ByNullSourcePos newId))
     _ -> expr


{- IO utility. Reads a CoreFn module from a source file. Probably this should be somewhere else?

-}
decodeModuleIO :: FilePath -> IO (Module (Bind Ann) PurusType PurusType Ann)
decodeModuleIO path = Aeson.eitherDecodeFileStrict' path >>= \case
  Left err -> throwIO $ userError err
  Right modx -> pure modx

{-
   Bound utils
-}
transverseScopeAndVariables ::
  (Monad exp, Traversable exp, Applicative f) =>
  (exp a -> f fvar1) ->
  (Var bvar1 fvar1 -> exp (Var bvar2 fvar2)) ->
  Scope bvar1 exp a ->
  f (Scope bvar2 exp fvar2)
transverseScopeAndVariables f g expr = toScope . (g =<<) <$> traverse (traverse f) (unscope expr)

{- Like `transverseScope` but not polymorphic in the `a` and allows
   type changing.
-}
transverseScopeViaExp :: Applicative f
                      => (Exp x t a -> f (Exp x t b))
                      -> Scope (BVar t) (Exp x t) a
                      -> f (Scope (BVar t) (Exp x t) b)
transverseScopeViaExp f scope
  = let fromScoped = fromScope scope
        sequenced  = sequence fromScoped
        traversed  = traverse f sequenced
        hm         = sequence <$> traversed
    in toScope <$> hm

-- Type changing in the Objects / Type / Var "fields",
-- needed for DesugarObjects
transverseScopeViaExpX :: Monad f
                      => (Exp x t a -> f (Exp y t' b))
                      -> (BVar t -> f (BVar t'))
                      -> Scope (BVar t) (Exp x t) a
                      -> f (Scope (BVar t') (Exp y t') b)
transverseScopeViaExpX f bvf scope
  = let fromScoped = fromScope scope
        sequenced  = sequence fromScoped
        traversed  = traverse f sequenced
        hm         = sequence <$> traversed
    in mapMBound bvf =<< toScope <$> hm

{- Surprisingly this is useful. We often want to ignore bound variables
   when traversing the AST (esp during inlining, where they don't
   matter at all).
-}
transverseScopeViaExp' :: (Exp x t a -> b)
                       -> Scope (BVar t) (Exp x t) a
                       -> Var (BVar t) b
transverseScopeViaExp' f scope
  = let fromScoped = fromScope scope
        sequenced  = sequence fromScoped
    in f <$> sequenced

{- Mashup of `foldM` and `transverseScope`.
-}
foldMScopeViaExp :: Monad f
                  => b
                  -> (b -> Exp x t a -> f b)
                  -> [Scope (BVar t) (Exp x t) a]
                  -> f b
foldMScopeViaExp e _ [] = pure e
foldMScopeViaExp e f (x:xs) = case transverseScopeViaExp' (f e) x of
  B _ -> foldMScopeViaExp e f xs
  F act -> do
    e' <- act
    foldMScopeViaExp e' f xs


{- Useful for transform/rewrite/cosmos/etc -}
instance Plated (Exp x t a) where
  plate = go
   where
     go :: forall f
         . ( Applicative f)
        => (Exp x t a -> f  (Exp x t a))
        -> Exp x t a
        -> f (Exp x t  a)
     go  tfun = \case
      LamE t bv e ->  LamE t bv <$> helper e
      CaseE t es alts ->
        let goAlt ::  Alt x t (Exp x t) a -> f (Alt x t (Exp x t) a)
            goAlt (UnguardedAlt bs pats scoped) = UnguardedAlt bs pats <$> helper scoped
        in CaseE t <$>  tfun es <*>  traverse goAlt alts
      LetE binds decls scoped ->
        let goDecls :: BindE t (Exp x t) a -> f (BindE t (Exp x t) a)
            goDecls = \case
              NonRecursive ident expr ->
                NonRecursive ident <$> tfun expr
              Recursive xs ->
                Recursive <$> traverse (\(i,x) -> (i,) <$> tfun x) xs
        in LetE binds <$> traverse goDecls decls <*> helper scoped
      AppE e1 e2 -> AppE <$> tfun e1 <*> tfun e2
      AccessorE x t pss e -> AccessorE x t pss <$> tfun e
      ObjectUpdateE x t e cf fs -> (\e' fs' -> ObjectUpdateE x t e' cf fs')
                                   <$> tfun  e
                                   <*> traverse (\(nm,expr) -> (nm,) <$> tfun expr) fs
      LitE t lit -> LitE t <$> traverseLit lit
      other -> pure other
      where
        traverseLit :: Lit x (Exp x t a)
                    -> f (Lit x (Exp x t a))
        traverseLit  = \case
          IntL i -> pure $ IntL i
          NumL d -> pure $ NumL d
          StringL str -> pure $ StringL str
          CharL char -> pure $ CharL char
          ArrayL xs -> ArrayL <$> traverse tfun  xs
          ConstArrayL xs -> ConstArrayL <$> pure xs
          ObjectL x fs -> ObjectL x <$> traverse (\(str,e) -> (str,) <$> tfun e) fs

        helper ::  Scope (BVar t) (Exp x t) a -> f (Scope (BVar t) (Exp x t) a)
        helper = transverseScopeViaExp tfun

-- put this somewhere else

instance Plated SourceType where
  plate f = \case
    tu@(TUnknown _ _) -> pure tu
    tv@(TypeVar _ _ _) -> pure tv
    tstr@(TypeLevelString _ _) -> pure tstr
    tint@(TypeLevelInt _  _) -> pure tint
    twild@(TypeWildcard _ _) -> pure twild
    tcon@(TypeConstructor _ _) -> pure tcon
    top@(TypeOp _ _) -> pure top
    TypeApp a t1 t2 -> TypeApp a <$> f t1 <*> f t2
    KindApp a t1 t2 -> KindApp a <$> f t1 <*> f t2
    ForAll a vis var mk innerTy scop ->
      (\mk' innerTy' -> ForAll a vis var mk' innerTy' scop)
      <$>  f mk
      <*> f innerTy
    ConstrainedType a constraint t -> ConstrainedType a <$> goConstraint f constraint <*> f t
    Skolem a txt mk i scop -> (\mk' -> Skolem a txt mk' i scop) <$>  f mk
    REmpty a -> pure $ REmpty a
    RCons a l x xs -> RCons a l <$> f x <*> f xs
    KindedType a t1 t2 -> KindedType a <$> f t1 <*> f t2
    BinaryNoParensType a t1 t2 t3 -> BinaryNoParensType a <$> f t1 <*> f t2 <*> f t3
    ParensInType a t -> ParensInType a <$> f t
   where
    goConstraint :: forall f
                  . Applicative f
                 => (SourceType -> f SourceType)
                 -> Constraint SourceAnn
                 -> f (Constraint SourceAnn)
    goConstraint g (Constraint a cn kargs args cdata) =
      (\kargs' args' -> Constraint a cn kargs' args' cdata)
      <$> traverse g kargs
      <*> traverse g args

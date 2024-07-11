{-# OPTIONS_GHC -Wno-orphans #-} -- has to be here (more or less)
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
{-# LANGUAGE TypeApplications #-}

module Language.PureScript.CoreFn.Convert.Monomorphize.Utils  where

import Prelude

import Language.PureScript.CoreFn.Expr (PurusType, Bind)
import Language.PureScript.CoreFn.Convert.IR (_V, Exp(..), FVar(..), BindE(..), BVar (..), abstractMany, mkBindings, Alt (..), Lit (..), expTy, ppExp, expTy', Pat (..))
import Language.PureScript.Names (Ident(..), ModuleName (..), QualifiedBy (..), Qualified (..), pattern ByNullSourcePos)
import Language.PureScript.Types
    ( SourceType, RowListItem (..), rowToList, Type (..), Constraint(..) )
import Language.PureScript.CoreFn.FromJSON ()
import Data.Text qualified as T
import Data.Map (Map)
import Data.Map qualified as M
import Control.Lens ( (<&>), (^?), (^..) )
import Control.Monad.RWS.Class (gets, modify', MonadReader (..))
import Control.Monad.RWS (RWST(..))
import Control.Monad.Except (throwError)
import Data.Text (Text)
import Bound.Var (Var(..))
import Bound.Scope (Scope (..), toScope, fromScope, mapBound)
import Data.Bifunctor (Bifunctor (..))
import Data.List (find)
import Control.Lens.Plated ( transform, Plated(..), cosmos )
import Language.PureScript.Environment (pattern (:->), mkRecordT, pattern RecordT)
import Language.PureScript.CoreFn.Pretty (prettyTypeStr, prettyAsStr)
import Language.PureScript.AST.SourcePos ( SourceAnn, pattern NullSourceAnn )
import Language.PureScript.PSString (PSString)
import Language.PureScript.Label (Label(..))
import Language.PureScript.CoreFn.Module ( Module(..) )
import Language.PureScript.CoreFn.Ann ( Ann )
import Language.PureScript.CoreFn.Convert.DesugarCore
    ( WithObjects )
import Data.Aeson qualified as Aeson
import GHC.IO (throwIO)
import Control.Monad (join)
import Language.PureScript.CoreFn.TypeLike (TypeLike(..))
import Prettyprinter (Pretty)
import Data.Maybe (isJust)
import Bound (abstract)

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
type Monomorphizer a = RWST (ModuleName, [BindE PurusType (Exp WithObjects PurusType) (Var (BVar PurusType) (FVar PurusType))]) () MonoState (Either MonoError)  a

getModName :: Monomorphizer ModuleName
getModName = ask <&> fst

getModBinds :: Monomorphizer [BindE PurusType (Exp WithObjects PurusType) (Var (BVar PurusType) (FVar PurusType))]
getModBinds = ask <&> snd

note ::  String -> Maybe b -> Monomorphizer b
note  err = \case
  Nothing -> throwError $ MonoError err
  Just x -> pure x

freshUnique :: Monomorphizer Int
freshUnique = do
  u <- gets unique
  modify' $ \(MonoState  _) -> MonoState  (u + 1)
  pure u

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

-- REVIEW: IDK if this is right? Do we need to abstract here?
-- Construct a Let expression from a list of BindEs and a scoped body
gLet ::
  [BindE PurusType (Exp WithObjects PurusType) (Var (BVar PurusType) (FVar PurusType))] ->
  Scope (BVar PurusType) (Exp WithObjects PurusType) (Var (BVar PurusType) (FVar PurusType)) ->
  Exp WithObjects PurusType (Var (BVar PurusType) (FVar PurusType))
gLet binds e =  LetE M.empty binds e

-- Tools for updating variable types/names

-- REVIEW: Is this right? Do we really want to update bound & free var types at the same time like this?
updateVarTyS :: forall x
              . BVar SourceType
             -> SourceType
             -> Scope (BVar SourceType) (Exp x SourceType) (Var (BVar SourceType) (FVar SourceType))
             -> Scope (BVar SourceType) (Exp x SourceType) (Var (BVar SourceType) (FVar SourceType))
updateVarTyS (BVar bvIx _ bvIdent) ty scoped = toScope . fmap F . updateBVar bvIdent bvIx ty $ unscoped
  where
    scoped' = mapBound goBound scoped
    unscoped = join <$> fromScope scoped'
    goBound :: BVar SourceType -> BVar SourceType
    goBound bv@(BVar bvIx' _ bvIdent')
      | bvIx == bvIx' && bvIdent == bvIdent' = BVar bvIx ty bvIdent 
      | otherwise = bv


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
unsafeApply :: forall a x t.
  (TypeLike t, Pretty t) =>
  (a -> Var (BVar t) (FVar t)) ->
  Exp x t a ->
  [Exp x t a] ->
  Exp x t a
unsafeApply f e (arg:args)= case unFunction . snd . stripQuantifiers . expTy f $ e of
  Just _ -> unsafeApply f (AppE e arg) args
  other -> Prelude.error $ "Unexpected argument to unsafeApply:\n  "
                           <> "Fun Expression: " <> ppExp (f <$> e)
                           <> "\n  Arg: " <> ppExp (f <$> arg)
                           <> "\n  FunType: " <> prettyAsStr other
                           <> "\n  ArgType: " <> prettyAsStr (expTy f arg)
unsafeApply _ e [] = e

{- Find the declaration *group* to which a given identifier belongs.
-}
findInlineDeclGroup ::
  Ident ->
  [BindE ty (Exp x ty) a] ->
  Maybe (BindE ty (Exp x ty) a)
findInlineDeclGroup _ [] = Nothing
findInlineDeclGroup ident (NonRecursive ident' bvix expr:rest)
  | ident == ident' = Just $ NonRecursive ident' bvix expr
  | otherwise = findInlineDeclGroup ident rest
findInlineDeclGroup ident (Recursive xs:rest) = case  find (\x -> fst (fst x) == ident) xs of
  Nothing -> findInlineDeclGroup ident rest
  Just _ -> Just (Recursive xs)

{- Find the body of a declaration with the given name in the given module.
-}
findDeclBody :: Text
             -> Module IR_Decl k t Ann
             -> Maybe (Scope (BVar PurusType) (Exp WithObjects PurusType) (FVar PurusType))
findDeclBody nm Module{..} = findDeclBody' (Ident nm) moduleDecls

findDeclBody' :: Ident -> [BindE ty (Exp x ty) a] -> Maybe (Scope (BVar ty) (Exp x ty) a)
findDeclBody' ident binds = case findInlineDeclGroup ident binds of
  Nothing -> Nothing
  Just decl -> case decl of
    NonRecursive _ _ e -> Just e
    Recursive xs -> snd <$> find (\x -> fst (fst x) == ident) xs
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
               -> Exp WithObjects PurusType (Var (BVar PurusType) (FVar PurusType))
               -> Exp WithObjects PurusType (Var (BVar PurusType) (FVar PurusType))
updateFreeVars dict = transform updateFreeVar
  where
    updateFreeVar :: Exp WithObjects PurusType (Var (BVar PurusType) (FVar PurusType))
                  -> Exp WithObjects PurusType (Var (BVar PurusType) (FVar PurusType))
    updateFreeVar  expr = case expr ^? _V of
     Just (F (FVar _ (Qualified (ByModuleName _) varId))) -> case M.lookup varId dict of
       Nothing -> expr
       Just (newId,newType) -> V $ F (FVar newType (Qualified ByNullSourcePos newId))
     _ -> expr


{- IO utility. Reads a CoreFn module from a source file. Probably this should be somewhere else?

-}
decodeModuleIO :: FilePath -> IO (Module (Bind Ann) PurusType PurusType Ann)
decodeModuleIO path = Aeson.eitherDecodeFileStrict' path >>= \case
  Left err -> throwIO $ userError err
  Right modx -> pure modx






{- Mashup of `foldM` and `transverseScope`.
-}
foldMScopeViaExp :: Monad f
                  => b
                  -> (b -> Exp x t (Var (BVar t) a) -> f b)
                  -> [Scope (BVar t) (Exp x t) (Var (BVar t) a)]
                  -> f b
foldMScopeViaExp e _ [] = pure e
foldMScopeViaExp e f (x:xs) = do
  let unscopedX = join <$> fromScope x
  this <- f e unscopedX
  foldMScopeViaExp this f xs

-- Exp x t (Var l a) -> Var l (Exp x t a)

distributeExp :: Var l (Exp x t a) -> Exp x t (Var l a)
distributeExp = \case
  B bv -> pure (B bv)
  F fv -> F <$> fv


updateBVar :: forall x
            . Ident
           -> Int
           -> SourceType
           -> Exp x SourceType (Var (BVar SourceType) (FVar SourceType))
           -> Exp x SourceType (Var (BVar SourceType) (FVar SourceType))
updateBVar bvId bvIx newTy = \case
  V (B (BVar bvIx' _ bvId')) | bvId' == bvId && bvIx == bvIx' -> V . B $ BVar bvIx' newTy bvId'
  LamE t bv@(BVar bvIx' _ bvId') scoped ->
    let newLamTy1 = quantify $ funTy newTy (expTy' id scoped')
        newLamTy2 = quantify $ funTy t (expTy' id scoped')
        unscoped = join <$> fromScope scoped
        scoped'  = toScope . fmap F $ updateBVar bvId bvIx newTy unscoped
    in if bvIx' == bvIx && bvId' == bvId
       then LamE newLamTy1 (BVar bvIx newTy bvId') scoped'
       else LamE newLamTy2 bv scoped'
  LitE _ (ObjectL x fs) ->
    let fs' = map (second $ updateBVar bvId bvIx newTy) fs
        newLitTy = mkRecordT $ foldr (\(nm,e) acc -> RCons NullSourceAnn (Label nm) (expTy id e) acc) (REmpty NullSourceAnn) fs'
    in LitE newLitTy (ObjectL x fs')
  AppE e1 e2 -> AppE (updateBVar bvId bvIx newTy e1) (updateBVar bvId bvIx newTy e2)
  ObjectUpdateE x _t orig copy fs ->
    let fs' = map (second $ updateBVar bvId bvIx newTy) fs
        orig' = updateBVar bvId bvIx newTy orig
        newUpdTy = mkRecordT $ foldr (\(nm,e) acc -> RCons NullSourceAnn (Label nm) (expTy id e) acc) (REmpty NullSourceAnn) fs'
    in ObjectUpdateE x newUpdTy orig' copy fs'
  AccessorE x _t lbl e -> let e' = updateBVar bvId bvIx newTy e in
    case expTy id e' of
     RecordT row ->
      let rowList :: [RowListItem SourceAnn]
          rowList = fst $ rowToList row
          newAccTy = case find (\ri -> rowListLabel ri == Label lbl) rowList of
                       Nothing -> error $ "updateBVar: No row field for " <> prettyAsStr lbl <> "in row type " <> prettyAsStr row
                       Just t -> rowListType t
      in AccessorE x newAccTy lbl e'
     other -> error "updateBVar: Malformed object type"
  CaseE _ scrut alts ->
     let scrut' = updateBVar bvId bvIx newTy scrut
         alts' = map goAlt alts
         newResTy = case head alts' of
                     UnguardedAlt _ _ rhs -> expTy' id rhs
     in CaseE newResTy scrut' alts'
  LetE _ binders scoped ->
    let unscoped = join <$> fromScope scoped
        scoped'  = toScope . fmap F . updateBVar bvId bvIx newTy $ unscoped
        binders' = map goBinder binders
    in LetE M.empty binders' scoped'
  TyInstE unchanged e -> TyInstE unchanged $ updateBVar bvId bvIx newTy e
  other ->  other
 where
   goAlt :: Alt x SourceType (Exp x SourceType) (Var (BVar SourceType) (FVar SourceType))
         -> Alt x SourceType (Exp x SourceType) (Var (BVar SourceType) (FVar SourceType))
   goAlt (UnguardedAlt _ pat scoped) =
     let pat' = goPat pat
         unscoped = join <$> fromScope scoped
         scoped' = toScope . fmap F . updateBVar bvId bvIx newTy $ unscoped
     in UnguardedAlt M.empty pat' scoped'
   goPat = \case
     VarP bvId' bvIx' _t | bvId == bvId' && bvIx == bvIx' -> VarP bvId bvIx newTy
     LitP (ObjectL x fs) -> LitP . ObjectL x . map (second goPat) $ fs
     ConP tn cn inner -> ConP tn cn $ map goPat inner
     other -> other

   goBinder :: BindE SourceType (Exp x SourceType) (Var (BVar SourceType) (FVar SourceType))
            -> BindE SourceType (Exp x SourceType) (Var (BVar SourceType) (FVar SourceType))
   goBinder = \case
     NonRecursive idnt bvix body ->
       let bop = join <$> fromScope body
           body' =   toScope . fmap F $ updateBVar bvId bvIx newTy bop
       in NonRecursive idnt bvix body'
     Recursive xs ->
       let goRecBind (idnt,scoped) = (idnt, toScope . fmap F . updateBVar bvId bvIx newTy . fmap join . fromScope $ scoped)
       in Recursive $ map goRecBind  xs

   
-- something is wrong with my attempts to write a plated instance and I dunno how to fix it,
-- but specific traverals seem to work, so this should work?
transformExp :: forall x t f
              . Monad f
              => (Exp x t (Var (BVar t) (FVar t)) -> f (Exp x t (Var (BVar t) (FVar t))))
              -> Exp x t (Var (BVar t) (FVar t))
              -> f (Exp x t (Var (BVar t) (FVar t)))
transformExp  f = \case
      LamE t bv e -> LamE t bv <$> transformScope e 
      CaseE t e alts ->
        let goAlt ::  Alt x t (Exp x t) (Var (BVar t) (FVar t)) -> f (Alt x t (Exp x t) (Var (BVar t) (FVar t)))
            goAlt (UnguardedAlt bs pats scoped) =  UnguardedAlt bs pats <$> transformScope scoped
        in CaseE t <$>   runTransform e <*>  traverse goAlt alts
      LetE binds decls scoped ->
        let goDecls :: BindE t (Exp x t) (Var (BVar t) (FVar t)) -> f (BindE t (Exp x t) (Var (BVar t) (FVar t)))
            goDecls = \case
              NonRecursive ident bvix expr ->
                NonRecursive ident bvix <$> transformScope expr
              Recursive xs ->  Recursive <$> traverse (\(i,x) -> (i,) <$> transformScope x) xs
        in LetE binds <$> traverse goDecls decls <*> transformScope scoped
      AppE e1 e2 -> AppE <$> runTransform e1 <*> runTransform e2
      AccessorE x t pss e -> AccessorE x t pss <$> runTransform e
      ObjectUpdateE x t e cf fs -> (\e' fs' -> ObjectUpdateE x t e' cf fs')
                                   <$> runTransform  e
                                   <*> traverse (\(nm,expr) -> (nm,) <$> runTransform expr) fs
      LitE t lit -> LitE t <$> traverse runTransform lit
      V a -> pure (V a)
      TyInstE t e -> TyInstE t <$> runTransform e
  where
    runTransform :: Exp x t (Var (BVar t) (FVar t)) -> f (Exp x t (Var (BVar t) (FVar t)))
    runTransform x = transformExp f x >>= f

    transformScope :: Scope (BVar t) (Exp x t) (Var (BVar t) (FVar t))
                   -> f (Scope (BVar t) (Exp x t) (Var (BVar t) (FVar t)))
    transformScope scoped = do
      let unscoped = join <$> fromScope scoped
      transformed <- runTransform unscoped
      pure $ toScope (F <$> transformed)

{- Useful for transform/rewrite/cosmos/etc -}
instance Plated (Exp x t (Var (BVar t) (FVar t))) where
  plate = go
   where
     go :: forall f
         . ( Applicative f)
        => (Exp x t (Var (BVar t) (FVar t)) -> f  (Exp x t (Var (BVar t) (FVar t))))
        -> Exp x t (Var (BVar t) (FVar t))
        -> f (Exp x t (Var (BVar t) (FVar t)))
     go  tfun = \case
      LamE t bv e ->  LamE t bv <$> scopeHelper e
      CaseE t es alts ->
        let goAlt ::  Alt x t (Exp x t) (Var (BVar t) (FVar t)) -> f (Alt x t (Exp x t) (Var (BVar t) (FVar t)))
            goAlt (UnguardedAlt bs pats scoped) =
              UnguardedAlt bs pats <$> scopeHelper scoped
        in CaseE t <$>  tfun es <*>  traverse goAlt alts
      LetE binds decls scoped ->
        let goDecls :: BindE t (Exp x t) (Var (BVar t) (FVar t)) -> f (BindE t (Exp x t) (Var (BVar t) (FVar t)))
            goDecls = \case
              NonRecursive ident bvix expr ->
                NonRecursive ident bvix <$> scopeHelper expr
              Recursive xs ->
                Recursive <$> traverse (\(i,x) -> (i,) <$> scopeHelper x) xs

        in LetE binds <$> traverse goDecls decls <*> scopeHelper scoped
      AppE e1 e2 -> AppE <$> tfun e1 <*> tfun e2
      AccessorE x t pss e -> AccessorE x t pss <$> tfun e
      ObjectUpdateE x t e cf fs -> (\e' fs' -> ObjectUpdateE x t e' cf fs')
                                   <$> tfun  e
                                   <*> traverse (\(nm,expr) -> (nm,) <$> tfun expr) fs
      LitE t lit -> LitE t <$> traverseLit lit
      V a -> pure (V a)
      TyInstE t e -> TyInstE t <$> tfun e
      where
        scopeHelper :: Scope (BVar t) (Exp x t) (Var (BVar t) (FVar t))
                    -> f (Scope (BVar t) (Exp x t) (Var (BVar t) (FVar t)))
        scopeHelper scoped =
          let unscoped = join <$> fromScope scoped
              effed    = tfun unscoped
              abstr = abstract $ \case
               B bv -> Just bv
               _    -> Nothing
          in abstr <$> effed

        traverseLit  = \case
          IntL i -> pure $ IntL i
          -- NumL d -> pure $ NumL d
          StringL str -> pure $ StringL str
          CharL char -> pure $ CharL char
          -- ArrayL xs -> ArrayL <$> traverse tfun  xs
          -- ConstArrayL xs -> ConstArrayL <$> pure xs
          ObjectL x fs -> ObjectL x <$> traverse (\(str,e) -> (str,) <$> tfun e) fs

isSelfRecursiveNR :: BindE t (Exp x t) (Var (BVar t) (FVar t)) -> Bool
isSelfRecursiveNR (NonRecursive ident indx body) = containsBVar ident indx body
isSelfRecursiveNR _ = False

containsBVar :: Ident -> Int -> Scope (BVar t) (Exp x t) (Var (BVar t) (FVar t)) -> Bool
containsBVar idnt indx expr = any (\case
    V (B (BVar bvix _ bvident)) -> bvix == indx &&  idnt == bvident
    _ -> False) subExpressions 
 where
    subExpressions = (join <$> fromScope expr) ^.. cosmos

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

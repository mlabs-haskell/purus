{-# OPTIONS_GHC -Wno-orphans #-} -- has to be here (more or less)
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DeriveTraversable, DeriveAnyClass  #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}

module Language.PureScript.CoreFn.Convert.IR where

import Prelude
import Language.PureScript.Names (Ident(..), Qualified (..), QualifiedBy (..), ProperNameType (..), ProperName(..), disqualify, runModuleName, showIdent, runIdent, showQualified)
import Language.PureScript.Types
    ( SkolemScope, TypeVarVisibility (..), genPureName )
import Language.PureScript.CoreFn.FromJSON ()
import Data.Text qualified as T
import Data.Map qualified as M
import Language.PureScript.PSString (PSString, prettyPrintString, decodeStringWithReplacement)
import Data.Text (Text)
import Bound
import Data.Kind qualified as GHC
import Control.Monad
import Data.Functor.Classes
import Data.Bifunctor (Bifunctor(first))
import Data.Maybe (fromJust, fromMaybe)
import Text.Show.Deriving
import Prettyprinter
import Language.PureScript.Constants.Prim qualified as C
import Prettyprinter.Render.Text ( renderStrict )
import Data.Map (Map)
import Control.Lens.TH (makePrisms)
import Bound.Scope (instantiateEither)
import Protolude.List (ordNub)
import Data.List (elemIndex, sortOn)
import Control.Lens (view,_2)
import Language.PureScript.CoreFn.TypeLike
import Data.Void (Void)
import Control.Lens.Plated
import Language.PureScript.CoreFn.Pretty ((<::>))
import Language.PureScript.CoreFn.Pretty.Common (prettyAsStr)
import Language.PureScript.CoreFn.Convert.Debug (doTrace)

-- The final representation of types and terms, where all constructions that
-- *should* have been eliminated in previous steps are impossible
-- TODO: Make sure we error on exotic kinds

data Kind
  = KindType
  | KindArrow Kind Kind
  deriving (Show, Eq, Ord)

data Ty
  = TyVar Text Kind
  | TyCon (Qualified (ProperName 'TypeName))
  | TyApp Ty Ty
  | KApp Ty Ty
  | Forall TypeVarVisibility Text Kind Ty (Maybe SkolemScope)
  | KType Ty Ty
  deriving (Show, Eq, Ord)

pattern (:~>) :: Ty -> Ty -> Ty
pattern a :~> b = TyApp (TyApp (TyCon C.Function) a) b
infixr 0 :~>

instance Plated Ty where
  plate f = \case
    TyVar txt ki -> pure $ TyVar txt ki
    TyCon nm  -> pure $ TyCon nm
    TyApp t1 t2 -> TyApp <$> f t1 <*> f t2
    KApp t1 t2 -> KApp <$> f t1 <*> f t2
    Forall vis var k innerTy scop ->
      (\x -> Forall vis var k x scop )
      <$> f innerTy
    KType t1 t2 -> KType <$> f t1 <*> f t2

instance TypeLike Ty where
  type KindOf Ty = Kind
  applyType = TyApp

  stripQuantifiers = \case
     Forall vis var mk inner _ -> first ((vis,var,mk):) $ stripQuantifiers inner
     other -> ([],other)

  funTy a b = a :~> b

  funArgTypes = init . splitFunTyParts

  replaceAllTypeVars = go [] where
    go :: [Text] -> [(Text, Ty)] -> Ty -> Ty
    go _  m (TyVar v k) = fromMaybe (TyVar v k) (v `lookup` m)
    go bs m (TyApp t1 t2) = TyApp (go bs m t1) (go bs m t2)
    go bs m (KApp t1 t2) = KApp  (go bs m t1) (go bs m t2)
    go bs m (Forall vis v k t sco)
      | v `elem` keys = go bs (filter ((/= v) . fst) m) $ Forall vis v k t sco
      | (v,k) `elem` usedVars =
        let v' = genPureName v (keys ++ bs ++ (fst <$> usedVars))
            t' = go bs [(v, TyVar v' k)] t
        in Forall vis v' k (go (v' : bs) m t') sco
      | otherwise = Forall vis v k (go (v : bs) m t) sco
     where
      keys = map fst m
      usedVars = concatMap (usedTypeVariables . snd) m
    go bs m (KType t k) = KType (go bs m t) (go bs m k)
    go _  _ ty = ty

  splitFunTyParts = \case
    (a :~> b) -> a : splitFunTyParts b
    t         -> [t]

  quantify ty = foldr (\(arg,argKind) t -> Forall TypeVarInvisible arg argKind t Nothing) ty $ freeTypeVariables ty

  instantiates var x (TyVar y _) | y == var = Just x
  instantiates var (TyApp t1 t2) (TyApp t1' t2') = case instantiates var t1 t1' of
    Just x -> Just x
    Nothing -> instantiates var t2 t2'
  instantiates _ _ _ = Nothing

  freeTypeVariables = ordNub . fmap snd . sortOn fst . go 0 [] where
    -- Tracks kind levels so that variables appearing in kind annotations are listed first.
        go :: Int -> [Text] -> Ty -> [(Int, (Text,Kind))]
        go lvl bound (TyVar v k) | v `notElem` bound = [(lvl, (v,k))]
        go lvl bound (TyApp t1 t2) = go lvl bound t1 ++ go lvl bound t2
        go lvl bound (KApp t1 t2) = go lvl bound t1 ++ go (lvl - 1) bound t2
        go lvl bound (Forall _ v _ t _) =  go lvl (v : bound) t
        go lvl bound (KType t k) = go lvl bound t ++ go (lvl - 1) bound k
        go _ _ _ = []

  usedTypeVariables = ordNub . go
    where
      go :: Ty -> [(Text,Kind)]
      go (TyVar v k) = [(v,k)]
      go (TyApp t1 t2) = go t1 <> go t2
      go (KApp t1 t2)  = go t1 <> go t2
      go (KType t1 t2) = go t1 <> go t2
      go (Forall _ _ _ inner _) = go inner
      go (TyCon _) = []

  resultTy t = case snd $  stripQuantifiers t of
    (_ :~> b) -> resultTy b
    other -> other

  unTyVar = \case
    TyVar t k -> Just (t,k)
    _         -> Nothing

  instTy t = \case
    Forall _ var _k inner _ -> replaceAllTypeVars [(var,t)] inner
    other -> other

  unFunction = \case
    (a :~> b) -> Just (a,b)
    _ -> Nothing

  quantify1 nm kind inner = Forall TypeVarInvisible nm kind inner Nothing

-- HACK: Need this for pretty printer, refactor later
class FuncType ty where
  -- | Get first argument of a function, if function or act as identity otherwise
  headArg :: ty -> ty

instance FuncType Ty where
  headArg t = case snd $ stripQuantifiers t of
    (a :~> _) -> a
    other -> other

type Bindings ty = Map Int (FVar ty)

-- A Bound variable. Serves as a bridge between the textual representation and the named de bruijn we'll need for PIR
data BVar ty = BVar Int ty Ident deriving (Show, Eq, Ord) -- maybe BVar Int (FVar ty) ??

data FVar ty = FVar ty (Qualified Ident) deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

type VoidList :: GHC.Type -> GHC.Type -> GHC.Type
type family VoidList voidOrNot argTy where
  VoidList Void _x = Void
  VoidList _a    x = [x]

data Lit x a
  = IntL Integer
  -- | NumL Double
  | StringL PSString
  | CharL Char
  -- | ArrayL [a]
  -- | ConstArrayL [Lit x Void]
  | ObjectL !(XObjectLiteral x) [(PSString, a)]
  deriving (Functor, Foldable, Traversable)

deriving instance (Show a, Show (XObjectLiteral x)) => Show (Lit x a)
deriving instance (Eq a, Eq (XObjectLiteral x)) => Eq (Lit x a)
deriving instance (Ord a, Ord (XObjectLiteral x)) => Ord (Lit x a)

-- We're switching to a more "Haskell-like" representation (largely to avoid overlapping names)
data Pat x t (f :: GHC.Type -> GHC.Type) a
  = VarP Ident Int  t -- VarBinder
  | WildP -- NullBinder
  | LitP (Lit x (Pat x t f a)) -- LiteralBinder
  | ConP (Qualified (ProperName 'TypeName)) (Qualified (ProperName 'ConstructorName)) [Pat x t f a] -- CTor binder
  deriving (Functor, Foldable, Traversable)

deriving instance (Show a, Show t, Show (XObjectLiteral x)) => Show (Pat x t f a)
deriving instance (Eq t, Eq a, Eq (XObjectLiteral x)) => Eq (Pat x t f a)
deriving instance (Ord t, Ord a, Ord (XObjectLiteral x)) => Ord (Pat x t f a)

data Alt x ty f a
  = UnguardedAlt (Bindings ty) (Pat x ty f a) (Scope (BVar ty) f a)
  deriving (Functor, Foldable, Traversable)

deriving instance (Monad f, Show1 f, Show a, Show ty, Show (XObjectLiteral x)) => Show (Alt x ty f a)
deriving instance (Monad f, Eq1 f, Eq a, Eq ty, Eq (XObjectLiteral x)) => Eq (Alt x ty f a)
deriving instance (Monad f, Ord1 f, Ord a, Ord ty, Ord (XObjectLiteral x)) => Ord (Alt x ty f a)

getPat :: Alt x ty f a -> Pat x ty f a
getPat = \case
  UnguardedAlt _ ps _ -> ps

-- idk if we really need the identifiers?
data BindE ty (f :: GHC.Type -> GHC.Type) a
  = NonRecursive Ident Int (Scope (BVar ty) f a)
  | Recursive [((Ident,Int), Scope (BVar ty) f a)]
  deriving (Eq,Ord,Show,Functor,Foldable,Traversable)

type family XAccessor x
type family XObjectUpdate x
type family XObjectLiteral x

data Exp x ty a
 = V a -- let's see if this works
 | LitE ty (Lit x (Exp x ty a))
 | LamE (BVar ty) (Scope (BVar ty) (Exp x ty) a)
 | AppE (Exp x ty a) (Exp x ty a)
 | CaseE ty (Exp x ty a) [Alt x ty (Exp x ty) a]
 | LetE (Bindings ty) [BindE ty (Exp x ty) a] (Scope (BVar ty) (Exp x ty) a)
 | AccessorE !(XAccessor x) ty PSString (Exp x ty a)
 | ObjectUpdateE !(XObjectUpdate x) ty (Exp x ty a) (Maybe [PSString]) [(PSString, Exp x ty a)]
 | TyInstE ty (Exp x ty a)
 | TyAbs (BVar (KindOf ty)) (Exp x ty a)
 deriving (Functor,Foldable,Traversable)

deriving instance (Eq ty, Eq a, Eq (KindOf ty), Eq (XAccessor x), Eq (XObjectUpdate x), Eq (XObjectLiteral x)) => Eq (Exp x ty a)

instance (Eq ty, Eq (KindOf ty)) => Eq1 (Exp x ty) where
  liftEq eq (V a) (V b) = eq a b
  liftEq eq (LitE t1 l1) (LitE t2 l2) = t1 == t2 && liftEq (liftEq eq) l1 l2
  liftEq eq (LamE n1 e1) (LamE n2 e2) =  n1 == n2 && liftEq eq e1 e2
  liftEq eq (AppE l1 l2) (AppE r1 r2) = liftEq eq l1 r1 && liftEq eq l2 r2
  liftEq eq (CaseE t1 es1 as1) (CaseE t2 es2 as2) = t1 == t2 && liftEq eq es1 es2 && liftEq (liftEq eq) as1 as2
  liftEq eq (LetE n1 bs1 e1) (LetE n2 bs2 e2) = n1 == n2 && liftEq (liftEq eq) bs1 bs2 && liftEq eq e1 e2
  liftEq eq (TyInstE t1 e1) (TyInstE t2 e2) = t1 == t2 && liftEq eq e1 e2
  liftEq eq (TyAbs bv1 e1) (TyAbs bv2 e2) = bv1 == bv2 && liftEq eq e1 e2
  liftEq _ _ _ = False

instance Eq1 (Lit x) where
  liftEq _ (IntL i1) (IntL i2) = i1 == i2
  -- liftEq _ (NumL i1) (NumL i2) = i1 == i2
  liftEq _ (StringL i1) (StringL i2) = i1 == i2
  liftEq _ (CharL i1) (CharL i2) = i1 == i2
  -- liftEq eq (ArrayL xs) (ArrayL ys) = liftEq eq xs ys
  liftEq _ _ _ = False

instance (Eq1 f, Monad f, Eq t) => Eq1 (Pat x t f) where
  liftEq _ (VarP i1 n1 t1) (VarP i2 n2 t2) = i1 == i2 && n1 == n2 && t1 == t2
  liftEq _ WildP WildP = True
  liftEq eq (ConP tn1 cn1 ps1) (ConP tn2 cn2 ps2) = tn1 == tn2 && cn1 == cn2 && liftEq (liftEq eq) ps1 ps2
  liftEq eq (LitP l1) (LitP l2) =  liftEq (liftEq eq) l1 l2
  liftEq _ _ _ = False

instance (Eq1 f, Monad f, Eq ty) => Eq1 (BindE ty f) where
  liftEq eq (NonRecursive i1 ix1 b1) (NonRecursive i2 ix2 b2) = ix1 == ix2 && i1 == i2 && liftEq eq b1 b2
  liftEq eq (Recursive xs) (Recursive ys) = go eq xs ys
    where
      go :: forall a b
          . (a -> b -> Bool)
         -> [((Ident,Int), Scope (BVar ty) f a)]
         -> [((Ident,Int), Scope (BVar ty) f b)] -> Bool
      go f ((i1,x):xss) ((i2,y):yss) = i1 == i2 && liftEq f x y && go f xss yss
      go _ [] [] = True
      go _ _ _ = False
  liftEq _ _ _ = False

instance (Eq1 f, Monad f, Eq ty) => Eq1 (Alt x ty f) where
  liftEq eq (UnguardedAlt n1 ps1 e1) (UnguardedAlt n2 ps2 e2) = n1 == n2 && liftEq eq ps1 ps2 && liftEq eq e1 e2

instance Applicative (Exp x ty) where
  pure = V
  (<*>) = ap

instance Monad (Exp x ty) where
  return = pure
  V a           >>= f     = f a
  AppE e1 e2    >>=   f = AppE (e1 >>= f) (e2 >>= f)
  LamE i e   >>=    f = LamE i (e >>>= f)
  LetE n bs e >>=     f = LetE n (map (>>>= f) bs) (e >>>= f)
  CaseE t es alts >>=   f = CaseE t (es >>= f) (map (>>>= f) alts)
  LitE t lit        >>= f = LitE t $ goLit lit
    where
      goLit = \case
        IntL i      -> IntL i
        -- NumL d      -> NumL d
        StringL str -> StringL str
        CharL c     -> CharL c
        -- ArrayL xs   -> ArrayL $ map (\x -> x >>= f) xs
        -- ConstArrayL lits -> ConstArrayL  lits
        ObjectL ext obj -> ObjectL ext $ map (\(field, x) -> (field, x >>= f)) obj
  TyInstE t e >>= f = TyInstE t (e >>= f)
  TyAbs bv e >>= f = TyAbs bv (e >>= f)
  AccessorE ext ty field expr >>= f = AccessorE ext ty field (expr >>= f)
  ObjectUpdateE ext ty expr toCopy toUpdate >>= f =
    ObjectUpdateE ext ty (expr >>= f) toCopy (map (\(field, x) -> (field, x >>= f)) toUpdate)

instance Bound (Pat x t) where
  VarP i n t >>>= _      = VarP i n t
  WildP  >>>= _      = WildP
  ConP tn cn p >>>= f = ConP tn cn (map (>>>= f) p)
  LitP litP >>>= f    = LitP (goLit litP)
    where
      goLit = \case
        IntL i      -> IntL i
        -- NumL d      -> NumL d
        StringL str -> StringL str
        CharL c     -> CharL c
        -- ConstArrayL lits -> ConstArrayL lits
        -- ArrayL xs   -> ArrayL $ map (\x -> x >>>= f) xs
        ObjectL ext obj -> ObjectL ext $ map (\(field, x) -> (field, x >>>= f)) obj

instance Bound (Alt x ty) where
  UnguardedAlt i ps e >>>= f = UnguardedAlt i (ps >>>= f) (e >>>= f)

instance Bound (BindE ty) where
  NonRecursive i ix e >>>= f = NonRecursive i ix $ e >>>= f
  Recursive xs     >>>= f = Recursive $ go f xs
    where
      go :: forall a f c
          . Monad f
         => (a -> f c)
         -> [((Ident,Int), Scope (BVar ty) f a)]
         -> [((Ident,Int), Scope (BVar ty) f c)]
      go _ [] = []
      go g ((i,e):rest) =
        let e' = e >>>= g
            rest' = go g rest
        in (i,e') : rest'

instance Pretty ty => Pretty (BVar ty) where
  pretty (BVar n t i) =  align . parens
    $ pretty (showIdent i) <> "#" <> pretty n <+> "::" <+> pretty t 

instance Pretty ty => Pretty (FVar ty) where
  pretty (FVar _t i) =   align . parens
    $ pretty (showQualified showIdent i) <+> "::" <+> pretty _t

instance Pretty Kind where
  pretty = \case
    KindType -> "*"
    KindArrow k1 k2 -> parens (pretty k1 <+> "->" <+> pretty k2)
instance Pretty Ty where
  pretty = \case
    TyVar t k -> parens (pretty t <::> pretty k)
    TyCon (Qualified qb tn) -> case qb of
      ByModuleName mn ->
        let mn'  = runModuleName mn
            tn'  = runProperName tn
        in pretty mn' <> "." <> pretty tn'
      _ -> pretty (runProperName tn)
    TyApp t1 t2 -> goTypeApp t1 t2
    KApp t1 t2 -> pretty t1 <> ("@" <> pretty t2)
    Forall vis var mk inner _ -> case stripQuantifiers inner of
      (quantified,inner') -> goForall ((vis,var,mk): quantified) inner'
    KType ty kind -> parens $ pretty ty <> " :: " <> pretty kind
   where
     goTypeApp :: forall ann. Ty -> Ty -> Doc ann
     goTypeApp (TyApp  f a) b
       |  f == TyCon C.Function =
           let a' = pretty a
               b' = pretty b
           in parens $ hsep [a' <+> "->",b']
       | otherwise =
           let f' = goTypeApp f a
               b' =  pretty b
           in  parens $ f' <+> b'
     goTypeApp a b = hsep [pretty a, parens $ pretty b]

     goForall :: [(TypeVarVisibility,Text,Kind)] -> Ty -> Doc ann
     goForall xs inner =
       let boundVars = hsep $ renderBoundVar <$> xs
           inner'    = pretty inner
       in "forall" <+> boundVars <> "." <+> inner'
      where
        renderBoundVar :: (TypeVarVisibility, Text, Kind) -> Doc ann
        renderBoundVar (_, var, mk) =  parens (pretty var <+> "::" <+> pretty mk)

bvType :: BVar ty -> ty
bvType (BVar _ t _) = t

instance (Pretty a, Pretty ty, Pretty (KindOf ty), TypeLike ty) => Pretty (Exp x ty a) where
  pretty = \case
    V x -> pretty x
    LitE ty lit -> parens $ pretty lit <+> "::" <+> pretty ty
    LamE bv body' ->
      let unscoped = fromScope body'
      in group . align $ "\\" <> parens (align $ pretty bv)
         <+> "->" <> hardline
         <> indent 4 (pretty unscoped)
    appE@(AppE _ _) -> case unsafeAnalyzeApp appE of
      (fun,args) ->
        let applied = group . align . vsep $ pretty fun : fmap (indent 2 . ("#" <+>) . pretty) args
        in group . align $  applied
    CaseE _ es alts ->
      let scrutinees = pretty es
          branches = group . pretty <$> alts
      in "case" <+> scrutinees <+> "of"
           <+> hardline <+> indent 2 (vcat branches)
    LetE _ bound e ->
      let unscopedE = fromScope e
      in align $ vcat [
          "let",
          indent 2 . align . vcat $ pretty <$> bound,
          "in" <+> align (pretty unscopedE)
          ]
    AccessorE _ _ field expr -> parens (pretty expr) <> dot <> pretty field
    ObjectUpdateE _ _ _ _ _ -> "TODO: Implement ObjectUpdateE printer"
    TyInstE t e -> vsep [pretty e, "@" <> parens (pretty t)]
    TyAbs bv e -> "/\\" <+> align (pretty bv)
                  <+> "->" <> hardline
                  <> indent 4 (pretty e)

instance (Pretty a, Pretty ty, Pretty (KindOf ty), TypeLike ty) => Pretty (Alt x ty (Exp x ty) a) where
  pretty = \case
    UnguardedAlt _ ps body -> pretty ps <+> "->" <>
                              hardline <> indent 2 (pretty $ fromScope body)

instance (Pretty b, Pretty a) => Pretty (Var b a) where
  pretty = \case
    B b -> pretty b
    F a -> pretty a

instance Pretty a => Pretty (Lit x a) where
  pretty = \case
    IntL i -> pretty i
    -- NumL d -> pretty d
    StringL pss -> pretty . T.unpack $ prettyPrintString pss
    CharL c -> viaShow . show $ c
    -- ConstArrayL xs -> list $ pretty <$> xs
    -- ArrayL xs -> list $ pretty <$> xs
    ObjectL _ obj -> encloseSep "{" "}" ", "
      (map (\(field, expr) -> pretty (T.pack $ decodeStringWithReplacement field) <> ":" <+> pretty expr) obj)

instance (Pretty a, Pretty t) => Pretty (Pat x t (Exp x ty) a) where
  pretty = \case
    VarP i n t -> pretty (runIdent i) <> pretty n <> "@" <> parens (pretty t)
    WildP  -> "_"
    LitP lit -> case lit of
      IntL i -> pretty i
      -- NumL d -> pretty d
      StringL pss -> pretty . T.unpack $ prettyPrintString pss
      CharL c -> viaShow . show $ c
      -- ConstArrayL xs -> list $ pretty <$> xs
      -- ArrayL xs -> list $ pretty <$> xs
      ObjectL _ _obj -> "TODO: Implement ObjectL pattern printer"
    ConP cn _ ps -> pretty (runProperName . disqualify $ cn) <+> hsep (pretty <$> ps)

instance (Pretty a, Pretty (KindOf ty), Pretty ty, TypeLike ty, Pretty (Exp x ty a)) => Pretty (BindE ty (Exp x ty) a) where
  pretty = \case
    NonRecursive i ix e -> let e' = fromScope e in pretty (runIdent i) <> "#" <> pretty ix  <+> "=" <+> pretty e'
    Recursive es ->
      let go ((ident,ix),expr) =
            let expr' = fromScope expr
            in pretty (runIdent ident) <> "#" <> pretty ix <+> "=" <+> pretty expr'
      in align . vcat $ go <$> es

ppExp :: (Pretty a, Pretty ty, Pretty (KindOf ty), TypeLike ty) => Exp x ty a -> String
ppExp = T.unpack . renderStrict . layoutPretty defaultLayoutOptions . pretty

ppTy :: Ty -> String
ppTy = T.unpack . renderStrict . layoutPretty defaultLayoutOptions . pretty

unsafeAnalyzeApp :: forall x a ty. Exp x ty a -> (Exp x ty a, [Exp x ty a])
unsafeAnalyzeApp  = fromJust . analyzeApp

analyzeApp :: forall x a ty. Exp x ty a -> Maybe (Exp x ty a, [Exp x ty a])
analyzeApp e =  (,appArgs e) <$> appFun e
  where
    appArgs :: Exp x ty a -> [Exp x ty a]
    appArgs (AppE t1 t2) = appArgs t1 <> [t2]
    appArgs _ = []

    appFun :: Exp x ty a -> Maybe (Exp x ty a)
    appFun (AppE  t1 _) = go t1
      where
        go (AppE tx _) = case appFun tx of
          Nothing -> Just tx
          Just tx' -> Just tx'
        go other = Just other
    appFun _ = Nothing


expTy :: forall x t a. (TypeLike t, Pretty t) => (a -> Var (BVar t) (FVar t)) -> Exp x t a -> t
expTy f = \case
  V x -> case f x of
    B (BVar _ t _) -> t
    F (FVar t _) -> t
  LitE t _ -> t
  LamE (BVar _ t _) body -> t `funTy` expTy' f body
  AppE e1 e2 -> appType f e1 e2
  CaseE t _ _  -> t
  LetE _ _ e -> expTy' f e
  AccessorE _ t _ _ -> t
  ObjectUpdateE _ t _ _ _ -> t
  TyInstE t e -> instTy t $ expTy f e
  TyAbs (BVar _ k idnt) inner -> quantify1 (runIdent idnt) k (expTy f inner)

expTy' :: forall x t a. (TypeLike t, Pretty t) => (a -> Var (BVar t) (FVar t)) -> Scope (BVar t) (Exp x t) a -> t
expTy' f scoped = case instantiateEither (either (V . B) (V . F)) scoped of
  V x -> case x >>= f  of
    B (BVar _ t _) -> t
    F (FVar t _) -> t
  LitE t _ -> t
  LamE (BVar _ t _) body  ->
    let body' = join <$> fmap f <$> body
    in t `funTy` expTy' id body'
  AppE  e1 e2 -> appType (>>= f) e1 e2
  CaseE t _ _  -> t
  LetE  _ _ e -> expTy' (>>= f) e
  AccessorE _ t _ _ -> t
  ObjectUpdateE _ t _ _ _ -> t
  TyAbs (BVar _ k idnt) inner ->
    let e' = join <$> fmap f <$> inner
        innerT = expTy id e'
    in quantify1 (runIdent idnt) k innerT
  TyInstE t e ->
    let e' = join <$> fmap f <$> e
    in instTy t (expTy id e')

-- | Gets the type of an application expression.
--   (Name might be a bit confusing, does not apply types)
appType :: forall x t a. (TypeLike t, Pretty t) => (a -> Var (BVar t) (FVar t)) -> Exp x t a -> Exp x t a -> t
appType h fe ae = doTrace "appType" msg  result
  where
    errmsg = (
      "\nINPUT FUN:\n" <> prettyAsStr (expTy h fe) <>
      "\n\nINPUT ARGS:\n" <> prettyAsStr (expTy h ae))

    msg = errmsg <> "\n\nRESULT\n: " <> prettyAsStr result

    foldr1Trace f xs
      | null xs = error $ "appType\n\n" <> errmsg
      | otherwise = foldr1 f xs

    result = case unsafeAnalyzeApp (AppE fe ae) of
      (fe',ae') ->
          quantify
        . foldr1Trace funTy
        . drop (length ae')
        . splitFunTyParts
        . snd
        . stripQuantifiers
        $ instantiateWithArgs (expTy h fe') (expTy h <$> ae')

$(deriveShow1 ''BindE)

instance (Show (XObjectLiteral x)) => Show1 (Lit x) where
  liftShowsPrec = $(makeLiftShowsPrec ''Lit)

instance (Show (XObjectLiteral x), Show t) => Show1 (Pat x t f) where
  liftShowsPrec = $(makeLiftShowsPrec ''Pat)

instance (Show ty, Show (KindOf ty), Show (XAccessor x), Show (XObjectUpdate x), Show (XObjectLiteral x)) => Show1 (Exp x ty) where
  liftShowsPrec = $(makeLiftShowsPrec ''Exp)

-- TH cannot derive it because `f` is used in last and second last field and some machinery doesn't like that
instance (Show ty, Show (KindOf ty), Show (XAccessor x), Show (XObjectUpdate x), Show (XObjectLiteral x)) => Show1 (Alt x ty (Exp x ty)) where
  liftShowsPrec sp sl d (UnguardedAlt i ps e)
    = showString "UnGuardedAlt "
      . showsPrec d i
      . showString " "
      . showsPrec d (liftShowsPrec sp sl d ps "")
      . showString " "
      . liftShowsPrec sp sl d e

deriving instance (Show a, Show (KindOf ty), Show ty, Show1 (Exp x ty), Show (XAccessor x), Show (XObjectUpdate x), Show (XObjectLiteral x)) => Show (Exp x ty a)

makePrisms ''Ty
makePrisms ''Exp

mkBindings :: [FVar ty] -> Bindings ty
mkBindings = M.fromList . zip [0..]

abstractMany :: Eq ty => [FVar ty] -> FVar ty -> Maybe (BVar ty)
abstractMany xs v@(FVar t i) = (\index -> BVar index t $ disqualify i) <$> elemIndex v xs


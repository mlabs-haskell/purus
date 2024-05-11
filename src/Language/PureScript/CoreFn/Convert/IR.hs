{-# OPTIONS_GHC -Wno-orphans #-} -- has to be here (more or less)
{-# OPTIONS_GHC -Werror #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DeriveTraversable, DeriveAnyClass  #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Language.PureScript.CoreFn.Convert.IR where

import Prelude
import Language.PureScript.Names (Ident(..), Qualified (..), QualifiedBy (..), ProperNameType (..), ProperName(..), disqualify, runModuleName, showIdent, runIdent, moduleNameFromString, coerceProperName, showQualified)
import Language.PureScript.Types
    ( SkolemScope, TypeVarVisibility (..), genPureName )
import Language.PureScript.CoreFn (Binder(..), Literal (..))
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
-- The final representation of types and terms, where all constructions that
-- *should* have been eliminated in previous steps are impossible
-- TODO: Make sure we error on exotic kinds
data Ty
  = TyVar Text
  | TyCon (Qualified (ProperName 'TypeName))
  | TyApp Ty Ty
  | KApp Ty Ty
  | Forall TypeVarVisibility Text (Maybe Ty) Ty (Maybe SkolemScope)
  | KType Ty Ty
  deriving (Show, Eq, Ord)

pattern (:~>) :: Ty -> Ty -> Ty
pattern a :~> b = TyApp (TyApp (TyCon C.Function) a) b
infixr 0 :~>

instance TypeLike Ty where
  applyType = TyApp

  stripQuantifiers = \case
     Forall vis var mk inner _ -> first ((vis,var,mk):) $ stripQuantifiers inner
     other -> ([],other)

  funTy a b = a :~> b

  funArgTypes = init . splitFunTyParts

  replaceAllTypeVars = go [] where
    go :: [Text] -> [(Text, Ty)] -> Ty -> Ty
    go _  m (TyVar v) = fromMaybe (TyVar v) (v `lookup` m)
    go bs m (TyApp t1 t2) = TyApp (go bs m t1) (go bs m t2)
    go bs m (KApp t1 t2) = KApp  (go bs m t1) (go bs m t2)
    go bs m (Forall vis v mbK t sco)
      | v `elem` keys = go bs (filter ((/= v) . fst) m) $ Forall vis v mbK' t sco
      | v `elem` usedVars =
        let v' = genPureName v (keys ++ bs ++ usedVars)
            t' = go bs [(v, TyVar v')] t
        in Forall vis v' mbK' (go (v' : bs) m t') sco
      | otherwise = Forall vis v mbK' (go (v : bs) m t) sco
     where
      mbK' = go bs m <$> mbK
      keys = map fst m
      usedVars = concatMap (usedTypeVariables . snd) m
    go bs m (KType t k) = KType (go bs m t) (go bs m k)
    go _  _ ty = ty

  splitFunTyParts = \case
    (a :~> b) -> a : splitFunTyParts b
    t         -> [t]

  quantify ty = foldr (\arg t -> Forall TypeVarInvisible arg Nothing t Nothing) ty $ freeTypeVariables ty

  instantiates var x (TyVar y) | y == var = Just x
  instantiates var (TyApp t1 t2) (TyApp t1' t2') = case instantiates var t1 t1' of
    Just x -> Just x
    Nothing -> instantiates var t2 t2'
  instantiates _ _ _ = Nothing

  freeTypeVariables = ordNub . fmap snd . sortOn fst . go 0 [] where
    -- Tracks kind levels so that variables appearing in kind annotations are listed first.
        go :: Int -> [Text] -> Ty -> [(Int, Text)]
        go lvl bound (TyVar v) | v `notElem` bound = [(lvl, v)]
        go lvl bound (TyApp t1 t2) = go lvl bound t1 ++ go lvl bound t2
        go lvl bound (KApp t1 t2) = go lvl bound t1 ++ go (lvl - 1) bound t2
        go lvl bound (Forall _ v mbK t _) = foldMap (go (lvl - 1) bound) mbK ++ go lvl (v : bound) t
        go lvl bound (KType t k) = go lvl bound t ++ go (lvl - 1) bound k
        go _ _ _ = []

  usedTypeVariables = ordNub . go
    where
      go :: Ty -> [Text]
      go (TyVar v) = [v]
      go (TyApp t1 t2) = go t1 <> go t2
      go (KApp t1 t2)  = go t1 <> go t2
      go (KType t1 t2) = go t1 <> go t2
      go (Forall _ _ _ inner _) = go inner
      go (TyCon _) = error "iUsedTypeVariables: TODO: TyCon" -- not sure what it should be

  resultTy t = case snd $  stripQuantifiers t of
    (_ :~> b) -> resultTy b
    other -> other

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

data FVar ty = FVar ty (Qualified Ident) deriving (Show, Eq, Ord)

data Lit x a
  = IntL Integer
  | NumL Double
  | StringL PSString
  | CharL Char
  | BoolL Bool
  | ArrayL [a]
  | ConstArrayL [Lit x Void]
  | ObjectL !(XObjectLiteral x) [(PSString, a)]
  deriving (Functor, Foldable, Traversable)

deriving instance (Show a, Show (XObjectLiteral x)) => Show (Lit x a)
deriving instance (Eq a, Eq (XObjectLiteral x)) => Eq (Lit x a)
deriving instance (Ord a, Ord (XObjectLiteral x)) => Ord (Lit x a)

-- We're switching to a more "Haskell-like" representation (largely to avoid overlapping names)
data Pat x (f :: GHC.Type -> GHC.Type) a
  = VarP Ident -- VarBinder
  | WildP -- NullBinder
  | AsP Ident (Pat x f a) -- NamedBinder
  | LitP (Lit x (Pat x f a)) -- LiteralBinder
  | ConP (Qualified (ProperName 'TypeName)) (Qualified (ProperName 'ConstructorName)) [Pat x f a] -- CTor binder
  deriving (Functor, Foldable, Traversable)

deriving instance (Show a, Show (XObjectLiteral x)) => Show (Pat x f a)
deriving instance (Eq a, Eq (XObjectLiteral x)) => Eq (Pat x f a)
deriving instance (Ord a, Ord (XObjectLiteral x)) => Ord (Pat x f a)

data Alt x ty f a
  = UnguardedAlt (Bindings ty) [Pat x f a] (Scope (BVar ty) f a)
  deriving (Functor, Foldable, Traversable)

deriving instance (Monad f, Show1 f, Show a, Show ty, Show (XObjectLiteral x)) => Show (Alt x ty f a)
deriving instance (Monad f, Eq1 f, Eq a, Eq ty, Eq (XObjectLiteral x)) => Eq (Alt x ty f a)
deriving instance (Monad f, Ord1 f, Ord a, Ord ty, Ord (XObjectLiteral x)) => Ord (Alt x ty f a)

getPat :: Alt x ty f a -> [Pat x f a]
getPat = \case
  UnguardedAlt _ ps _ -> ps

-- idk if we really need the identifiers?
data BindE ty (f :: GHC.Type -> GHC.Type) a
  = NonRecursive Ident (Scope (BVar ty) f a)
  | Recursive [(Ident, Scope (BVar ty) f a)]
  deriving (Eq,Ord,Show,Functor,Foldable,Traversable)

flattenBind :: BindE ty f a -> [(Ident, Scope (BVar ty) f a)]
flattenBind = \case
  NonRecursive i e -> [(i,e)]
  Recursive xs -> xs

type family XAccessor x
type family XObjectUpdate x
type family XObjectLiteral x

data Exp x ty a
 = V a -- let's see if this works
 | LitE ty (Lit x (Exp x ty a))
 | CtorE ty (ProperName 'TypeName) (ProperName 'ConstructorName) [Ident]
 | LamE ty (BVar ty) (Scope (BVar ty) (Exp x ty) a)
 | AppE (Exp x ty a) (Exp x ty a)
 | CaseE ty [Exp x ty a] [Alt x ty (Exp x ty) a]
 | LetE (Bindings ty) [BindE ty (Exp x ty) a] (Scope (BVar ty) (Exp x ty) a)
 | AccessorE !(XAccessor x) ty PSString (Exp x ty a)
 | ObjectUpdateE !(XObjectUpdate x) ty (Exp x ty a) (Maybe [PSString]) [(PSString, Exp x ty a)]
 deriving (Functor,Foldable,Traversable)

deriving instance (Eq ty, Eq a, Eq (XAccessor x), Eq (XObjectUpdate x), Eq (XObjectLiteral x)) => Eq (Exp x ty a)

instance Eq ty => Eq1 (Exp x ty) where
  liftEq eq (V a) (V b) = eq a b
  liftEq eq (LitE t1 l1) (LitE t2 l2) = t1 == t2 && liftEq (liftEq eq) l1 l2
  liftEq _ (CtorE t1 tn1 cn1 fs1) (CtorE t2 tn2 cn2 fs2)  = t1 == t2 && tn1 == tn2 && cn1 == cn2 && fs1 == fs2
  liftEq eq (LamE t1 n1 e1) (LamE t2 n2 e2) = t1 == t2 && n1 == n2 && liftEq eq e1 e2
  liftEq eq (AppE l1 l2) (AppE r1 r2) = liftEq eq l1 r1 && liftEq eq l2 r2
  liftEq eq (CaseE t1 es1 as1) (CaseE t2 es2 as2) = t1 == t2 && liftEq (liftEq eq) es1 es2 && liftEq (liftEq eq) as1 as2
  liftEq eq (LetE n1 bs1 e1) (LetE n2 bs2 e2) = n1 == n2 && liftEq (liftEq eq) bs1 bs2 && liftEq eq e1 e2
  liftEq _ _ _ = False

instance Eq1 (Lit x) where
  liftEq _ (IntL i1) (IntL i2) = i1 == i2
  liftEq _ (NumL i1) (NumL i2) = i1 == i2
  liftEq _ (StringL i1) (StringL i2) = i1 == i2
  liftEq _ (CharL i1) (CharL i2) = i1 == i2
  liftEq _ (BoolL i1) (BoolL i2) = i1 == i2
  liftEq eq (ArrayL xs) (ArrayL ys) = liftEq eq xs ys
  liftEq _ _ _ = False

instance (Eq1 f, Monad f) => Eq1 (Pat x f) where
  liftEq _ (VarP i1) (VarP i2) = i1 == i2
  liftEq _ WildP WildP = True
  liftEq eq (AsP i1 p1) (AsP i2 p2) = i1 == i2 && liftEq eq p1 p2
  liftEq eq (ConP tn1 cn1 ps1) (ConP tn2 cn2 ps2) = tn1 == tn2 && cn1 == cn2 && liftEq (liftEq eq) ps1 ps2
  liftEq eq (LitP l1) (LitP l2) =  liftEq (liftEq eq) l1 l2
  liftEq _ _ _ = False

instance (Eq1 f, Monad f, Eq ty) => Eq1 (BindE ty f) where
  liftEq eq (NonRecursive i1 b1) (NonRecursive i2 b2) = i1 == i2 && liftEq eq b1 b2
  liftEq eq (Recursive xs) (Recursive ys) = go eq xs ys
    where
      go :: forall a b. (a -> b -> Bool) -> [(Ident, Scope (BVar ty) f a)] -> [(Ident, Scope (BVar ty) f b)] -> Bool
      go f ((i1,x):xss) ((i2,y):yss) = i1 == i2 && liftEq f x y && go f xss yss
      go _ [] [] = True
      go _ _ _ = False
  liftEq _ _ _ = False

instance (Eq1 f, Monad f, Eq ty) => Eq1 (Alt x ty f) where
  liftEq eq (UnguardedAlt n1 ps1 e1) (UnguardedAlt n2 ps2 e2) = n1 == n2 && liftEq (liftEq eq) ps1 ps2 && liftEq eq e1 e2

instance Applicative (Exp x ty) where
  pure = V
  (<*>) = ap

instance Monad (Exp x ty) where
  return = pure
  V a           >>= f     = f a
  CtorE t tn cn fs >>= _  = CtorE t tn cn fs
  AppE e1 e2    >>=   f = AppE (e1 >>= f) (e2 >>= f)
  LamE t i e   >>=    f = LamE t i (e >>>= f)
  LetE n bs e >>=     f = LetE n (map (>>>= f) bs) (e >>>= f)
  CaseE t es alts >>=   f = CaseE t (map (\x -> x >>= f) es) (map (>>>= f) alts)
  LitE t lit        >>= f = LitE t $ goLit lit
    where
      goLit = \case
        IntL i      -> IntL i
        NumL d      -> NumL d
        StringL str -> StringL str
        CharL c     -> CharL c
        BoolL b     -> BoolL b
        ArrayL xs   -> ArrayL $ map (\x -> x >>= f) xs
        ObjectL ext obj -> ObjectL ext $ map (\(field, x) -> (field, x >>= f)) obj
  AccessorE ext ty field expr >>= f = AccessorE ext ty field (expr >>= f)
  ObjectUpdateE ext ty expr toCopy toUpdate >>= f =
    ObjectUpdateE ext ty (expr >>= f) toCopy (map (\(field, x) -> (field, x >>= f)) toUpdate)

instance Bound (Pat x) where
  VarP i  >>>= _      = VarP i
  WildP   >>>= _      = WildP
  AsP i p   >>>= f      = AsP i (p >>>= f)
  ConP tn cn p >>>= f = ConP tn cn (map (>>>= f) p)
  LitP litP >>>= f    = LitP (goLit litP)
    where
      goLit = \case
        IntL i      -> IntL i
        NumL d      -> NumL d
        StringL str -> StringL str
        CharL c     -> CharL c
        BoolL b     -> BoolL b
        ArrayL xs   -> ArrayL $ map (\x -> x >>>= f) xs
        ObjectL ext obj -> ObjectL ext $ map (\(field, x) -> (field, x >>>= f)) obj

instance Bound (Alt x ty) where
  UnguardedAlt i ps e >>>= f = UnguardedAlt i (map (>>>= f) ps) (e >>>= f)

instance Bound (BindE ty) where
  NonRecursive i e >>>= f = NonRecursive i $ e >>>= f
  Recursive xs     >>>= f = Recursive $ go f xs
    where
      go :: forall a f c. Monad f => (a -> f c) -> [(Ident, Scope (BVar ty) f a)] -> [(Ident, Scope (BVar ty) f c)]
      go _ [] = []
      go g ((i,e):rest) =
        let e' = e >>>= g
            rest' = go g rest
        in (i,e') : rest'

instance Pretty (BVar ty) where
  pretty (BVar n _t i) =   pretty (showIdent i) <> pretty n -- <+> "::" <+> pretty t

instance Pretty (FVar ty) where
  pretty (FVar _t i) =  pretty (showQualified showIdent i) -- <+> "::" <+> pretty t)

instance Pretty Ty where
  pretty = \case
    TyVar t -> pretty t
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
           in hsep [a' <+> "->",b']
       | otherwise =
           let f' = goTypeApp f a
               b' = pretty b
           in  f' <+> b'
     goTypeApp a b = hsep [pretty a, pretty b]

     goForall :: [(TypeVarVisibility,Text,Maybe Ty)] -> Ty -> Doc ann
     goForall xs inner =
       let boundVars = hsep $ renderBoundVar <$> xs
           inner'    = pretty inner
       in "forall" <+> boundVars <> "." <+> inner'
      where
        renderBoundVar :: (TypeVarVisibility, Text, Maybe Ty) -> Doc ann
        renderBoundVar (_, var, mk) = case mk of
          Nothing -> pretty var
          Just k  -> parens (pretty var <+> "::" <+> pretty k)

instance (Pretty a, Pretty ty, FuncType ty) => Pretty (Exp x ty a) where
  pretty = \case
    V x -> pretty x
    LitE ty lit -> parens $ pretty lit <+> "::" <+> pretty ty
    CtorE _ _ cname _ -> pretty $ runProperName cname
    LamE ty bv body' ->
      let unscoped = fromScope body'
      in "\\" <> parens (align $ pretty bv <+> "::" <+> pretty (headArg ty))
         <+> "->" <> hardline
         <> indent 2 (pretty unscoped)
    appE@(AppE _ _) -> case unsafeAnalyzeApp appE of
      (fun,args) ->
        let applied = group . align . hsep $ parens . pretty <$> (fun:args)
        in group . align $ parens applied
    CaseE _ es alts ->
      let scrutinees = group $ hsep (pretty <$> es)
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

instance (Pretty a, Pretty ty, FuncType ty) => Pretty (Alt x ty (Exp x ty) a) where
  pretty = \case
    UnguardedAlt _ ps body -> hcat (pretty <$> ps) <+> "->" <>
                              hardline <> indent 2 (pretty $ fromScope body)

instance (Pretty b, Pretty a) => Pretty (Var b a) where
  pretty = \case
    B b -> pretty b
    F a -> pretty a

instance Pretty a => Pretty (Lit x a) where
  pretty = \case
    IntL i -> pretty i
    NumL d -> pretty d
    StringL pss -> pretty . T.unpack $ prettyPrintString pss
    CharL c -> viaShow . show $ c
    BoolL b -> if b then "true" else "false"
    ArrayL xs -> list $ pretty <$> xs
    ObjectL _ obj -> encloseSep "{" "}" ", "
      (map (\(field, expr) -> (pretty $ T.pack $ decodeStringWithReplacement field) <> ":" <+> pretty expr) obj)

instance Pretty a => Pretty (Pat x (Exp x ty) a) where
  pretty = \case
    VarP i -> pretty (runIdent i)
    WildP -> "_"
    AsP i pat -> pretty (runIdent i) <> pretty pat
    LitP lit -> case lit of
      IntL i -> pretty i
      NumL d -> pretty d
      StringL pss -> pretty . T.unpack $ prettyPrintString pss
      CharL c -> viaShow . show $ c
      BoolL b -> if b then "true" else "false"
      ArrayL xs -> list $ pretty <$> xs
      ObjectL _ _obj -> "TODO: Implement ObjectL pattern printer"
    ConP cn _ ps -> pretty (runProperName . disqualify $ cn) <+> hsep (pretty <$> ps)

instance (Pretty a, Pretty ty, FuncType ty) => Pretty (BindE ty (Exp x ty) a) where
  pretty = \case
    NonRecursive i e -> pretty (runIdent i) <+> "=" <+> pretty (fromScope e)
    Recursive es -> align . vcat $ pretty . uncurry NonRecursive <$> es

ppExp :: (Pretty a, Pretty ty, FuncType ty) => Exp x ty a -> String
ppExp = T.unpack . renderStrict . layoutPretty defaultLayoutOptions . pretty

ppTy :: Ty -> String
ppTy = T.unpack . renderStrict . layoutPretty defaultLayoutOptions . pretty

unsafeAnalyzeApp :: forall x a ty. Exp x ty a -> (Exp x ty a, [Exp x ty a])
unsafeAnalyzeApp e = fromJust $ (,appArgs e) <$> appFun e
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

expTy :: forall x t a. TypeLike t => (a -> Var (BVar t) (FVar t)) -> Exp x t a -> t
expTy f = \case
  V x -> case f x of
    B (BVar _ t _) -> t
    F (FVar t _) -> t
  LitE t _ -> t
  CtorE t _ _ _  -> t
  LamE t _ _ -> t
  AppE e1 e2 -> appType f e1 e2
  CaseE t _ _  -> t
  LetE _ _ e -> expTy' f e
  AccessorE _ t _ _ -> t
  ObjectUpdateE _ t _ _ _ -> t

expTy' :: forall x t a. TypeLike t => (a -> Var (BVar t) (FVar t)) -> Scope (BVar t) (Exp x t) a -> t
expTy' f scoped = case instantiateEither (either (V . B) (V . F)) scoped of
  V x -> case x >>= f  of
    B (BVar _ t _) -> t
    F (FVar t _) -> t
  LitE t _ -> t
  CtorE t _ _ _  -> t
  LamE t _ _ -> t
  AppE  e1 e2 -> appType (>>= f) e1 e2
  CaseE t _ _  -> t
  LetE  _ _ e -> expTy' (>>= f) e
  AccessorE _ t _ _ -> t
  ObjectUpdateE _ t _ _ _ -> t

-- | Gets the type of an application expression.
--   (Name might be a bit confusing, does not apply types)
appType :: forall x t a. TypeLike t => (a -> Var (BVar t) (FVar t)) -> Exp x t a -> Exp x t a -> t
appType h fe ae = case stripQuantifiers funT of
   ([],ft) ->
     let numArgs = length argTypes
     in foldl1 funTy . drop numArgs . splitFunTyParts $ ft
   (xs,ft) ->
     let funArgs = funArgTypes ft
         dict    = mkInstanceMap M.empty (view _2 <$> xs) argTypes funArgs
         numArgs = length argTypes
     in quantify
        . foldl1 funTy
        . drop numArgs
        . splitFunTyParts
        . replaceAllTypeVars (M.toList dict)
        $ ft
  where
    (f,args) = appFunArgs fe ae
    funT    = expTy h f
    argTypes = expTy h <$> args

    mkInstanceMap :: Map Text t -> [Text] -> [t] -> [t] -> Map Text t
    mkInstanceMap acc [] _ _ = acc
    mkInstanceMap acc _ [] _ = acc
    mkInstanceMap acc _ _ [] = acc
    mkInstanceMap acc (var:vars) (mt:mts) (pt:pts) = case instantiates var mt pt of
      Nothing -> mkInstanceMap acc [var] mts pts
                 <> mkInstanceMap M.empty vars (mt:mts) (pt:pts)
      Just t  -> mkInstanceMap (M.insert var t acc) vars (mt:mts) (pt:pts)

appFunArgs :: forall x ty a.  Exp x ty a -> Exp x ty a -> (Exp x ty a, [Exp x ty a])
appFunArgs f args = (appFun f, appArgs f args)
  where
    appArgs :: Exp x ty a -> Exp x ty a -> [Exp x ty a]
    appArgs (AppE t1 t2) t3 = appArgs t1 t2 <> [t3]
    appArgs _  t3 = [t3]

    appFun :: Exp x ty a -> Exp x ty a
    appFun (AppE t1 _) = appFun t1
    appFun res            = res


$(deriveShow1 ''BindE)

instance (Show (XObjectLiteral x)) => Show1 (Lit x) where
  liftShowsPrec = $(makeLiftShowsPrec ''Lit)

instance (Show (XObjectLiteral x)) => Show1 (Pat x f) where
  liftShowsPrec = $(makeLiftShowsPrec ''Pat)

instance (Show ty, Show (XAccessor x), Show (XObjectUpdate x), Show (XObjectLiteral x)) => Show1 (Exp x ty) where
  liftShowsPrec = $(makeLiftShowsPrec ''Exp)

-- TH cannot derive it because `f` is used in last and second last field and some machinery doesn't like that
instance (Show ty, Show (XAccessor x), Show (XObjectUpdate x), Show (XObjectLiteral x)) => Show1 (Alt x ty (Exp x ty)) where
  liftShowsPrec sp sl d (UnguardedAlt i ps e)
    = showString "UnGuardedAlt "
      . showsPrec d i
      . showString " "
      . showsPrec d (fmap (\x -> liftShowsPrec sp sl d x "") ps)
      . showString " "
      . liftShowsPrec sp sl d e

deriving instance (Show a, Show ty, Show1 (Exp x ty), Show (XAccessor x), Show (XObjectUpdate x), Show (XObjectLiteral x)) => Show (Exp x ty a)

makePrisms ''Ty
makePrisms ''Exp

-- REVIEW: This *MIGHT* not be right. I'm not 1000% sure what the PS criteria for a mutually rec group are
--         First arg threads the FVars that correspond to the already-processed binds
--         through the rest of the conversion. I think that's right - earlier bindings
--         should be available to later bindings
assembleBindEs :: Eq ty =>  [FVar ty] -> [[(FVar ty ,Exp x ty (FVar ty))]] -> [BindE ty (Exp x ty) (FVar ty)]
assembleBindEs _ [] = []
assembleBindEs dict ([]:rest) = assembleBindEs dict rest -- shouldn't happen but w/e
assembleBindEs dict ([(fv@(FVar _tx ix),e)]:rest) =
  let dict' = fv:dict
      abstr = abstract (abstractMany dict')
  in NonRecursive (disqualify ix) (abstr e) : assembleBindEs dict' rest
assembleBindEs dict (xsRec:rest) =
  let (dict', recBind) = assembleRec dict xsRec
  in recBind : assembleBindEs dict' rest

assembleRec :: Eq ty => [FVar ty] -> [(FVar ty, Exp x ty (FVar ty))] -> ([FVar ty], BindE ty (Exp x ty) (FVar ty))
assembleRec dict xs =
  let dict' = dict <> (fst <$> xs)
      abstr = abstract (abstractMany dict')
      recBind = Recursive
                . map (uncurry $ \(FVar _ ixx) xp -> (disqualify ixx, abstr xp))
                $ xs
  in (dict', recBind)

mkBindings :: [FVar ty] -> Bindings ty
mkBindings = M.fromList . zip [0..]

abstractMany :: Eq ty => [FVar ty] -> FVar ty -> Maybe (BVar ty)
abstractMany xs v@(FVar t i) = (\index -> BVar index t $ disqualify i) <$> elemIndex v xs

toPat :: Binder ann -> Pat x (Exp x ty) (FVar ty)
toPat = \case
  NullBinder _ -> WildP
  VarBinder _ i  ->  VarP i
  ConstructorBinder _ tn cn bs -> ConP tn cn $ map toPat bs
  NamedBinder _ nm b ->  AsP nm $ toPat b
  LiteralBinder _ lp -> case lp of
    NumericLiteral (Left i) -> LitP $ IntL i
    NumericLiteral (Right d) -> LitP $ NumL d
    StringLiteral pss -> LitP $ StringL pss
    CharLiteral c -> LitP $ CharL c
    BooleanLiteral b -> LitP $ BoolL b
    ArrayLiteral as -> LitP $ ArrayL $ map toPat as
    ObjectLiteral fs' -> do
      -- this isn't right, we need to make sure the positions of the binders are correct,
      -- since (I think?) you can use an Obj binder w/o using all of the fields
      let fs = sortOn fst fs'
          len = length fs
          fakeCName = mkFakeCName len
          fakeTName = mkFakeTName len
          inner = map (toPat . snd) fs
        in ConP fakeTName fakeCName inner

mkFakeCName :: Int -> Qualified (ProperName 'ConstructorName)
mkFakeCName x =
  Qualified
    (ByModuleName $ moduleNameFromString "$GEN")
    (ProperName $ "~TUPLE_" <> T.pack (show x))

mkFakeTName :: Int -> Qualified (ProperName 'TypeName)
mkFakeTName x = coerceProperName @_ @'TypeName <$> mkFakeCName x

{-# OPTIONS_GHC -Wno-orphans #-} -- has to be here (more or less)
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DeriveTraversable, DeriveAnyClass  #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Language.PureScript.CoreFn.Convert.IR where

import Prelude
import Language.PureScript.CoreFn.Expr
    ( _Var,
      eType,
      exprType,
      Bind(..),
      CaseAlternative(CaseAlternative),
      Expr(..) )
import Language.PureScript.Names (Ident(..), Qualified (..), QualifiedBy (..), pattern ByNullSourcePos, ProperNameType (..), ProperName(..), moduleNameFromString, coerceProperName, disqualify)
import Language.PureScript.Types
    ( SourceType, Type(..), SkolemScope, TypeVarVisibility, srcTypeConstructor, srcTypeApp, RowListItem (rowListType) )
import Language.PureScript.Environment (pattern (:->), pattern RecordT, function)
import Language.PureScript.CoreFn.Pretty
    ( prettyTypeStr, renderExprStr )
import Language.PureScript.CoreFn.Ann (Ann)
import Language.PureScript.CoreFn.FromJSON ()
import Data.Text qualified as T
import Data.List (find, elemIndex, sortOn, foldl')
import Language.PureScript.AST.Literals (Literal(..))
import Data.Map qualified as M
import Language.PureScript.PSString (PSString)
import Language.PureScript.AST.SourcePos
    ( pattern NullSourceAnn )
import Control.Lens.IndexedPlated
import Control.Lens ( ix )
import Language.PureScript.CoreFn.Convert.Monomorphize
    ( stripQuantifiers, nullAnn, mkFieldMap )
import GHC.Natural (Natural)
import Data.Text (Text)
import Bound
import Data.Kind qualified as GHC
import Control.Monad
import Data.Functor.Classes
import Data.Bifunctor (Bifunctor(bimap, first, second))
import Control.Lens.Combinators (to)
import Language.PureScript.CoreFn (Binder(..))
import Data.Maybe (mapMaybe)
import Control.Lens.Operators
import Text.Show.Deriving

-- The final representation of types and terms, where all constructions that
-- *should* have been eliminated in previous steps are impossible
-- Arguably we shouldn't strip the annotations here, but
-- I don't have time to write another prettyprinter and
-- we can always put them back later
data Ty
  = TyVar Text
  | TyCon (Qualified (ProperName 'TypeName))
  | TyApp Ty Ty
  | KApp Ty Ty
  | Forall TypeVarVisibility Text (Maybe Ty) Ty (Maybe SkolemScope)
  | KType Ty Ty
  deriving (Show, Eq)

data Lit a
  = IntL Integer
  | NumL Double
  | StringL PSString
  | CharL Char
  | BoolL Bool
  | ArrayL [a]
  deriving (Eq,Ord,Show,Functor,Foldable,Traversable)

-- We're switching to a more "Haskell-like" representation (largely to avoid overlapping names)
data Pat (f :: GHC.Type -> GHC.Type) a
  = VarP -- VarBinder
  | WildP -- NullBinder
  | AsP (Pat f a) -- NamedBinder
  | LitP (Lit (Pat f a)) -- LiteralBinder
  | ConP (Qualified (ProperName 'TypeName)) (Qualified (ProperName 'ConstructorName)) [Pat f a] -- CTor binder
  deriving (Eq,Ord,Show,Functor,Foldable,Traversable)

data Alt f a
  = UnguardedAlt {-# UNPACK #-} !Int [Pat f a] (Scope Int f a)
  | GuardedAlt {-# UNPACK #-} !Int [Pat f a] [(Scope Int f a, Scope Int f a)]
  deriving (Eq,Ord,Show,Functor,Foldable,Traversable)

-- idk if we really need the identifiers?
data BindE a
  = NonRecursive Ident (Scope Int Exp a)
  | Recursive [(Ident,Scope Int Exp a)]

data Exp a
 = V a -- let's see if this works
 | LitE Ty (Lit (Exp a))
 | CtorE Ty (ProperName 'TypeName) (ProperName 'ConstructorName) [Ident]
 | LamE Ty {-# UNPACK #-} !Int (Pat Exp a) (Scope Int Exp a)
 | AppE Ty (Exp a) (Exp a)
 | CaseE Ty [Exp a] [Alt Exp a]
 | LetE Ty {-# UNPACK #-} !Int [Scope Int Exp a] (Scope Int Exp a)
 deriving (Eq,Functor,Foldable,Traversable)

instance Eq1 Exp where
  liftEq eq (V a) (V b) = eq a b
  liftEq eq (LitE t1 l1) (LitE t2 l2) = t1 == t2 && liftEq (liftEq eq) l1 l2
  liftEq _ (CtorE t1 tn1 cn1 fs1) (CtorE t2 tn2 cn2 fs2)  = t1 == t2 && tn1 == tn2 && cn1 == cn2 && fs1 == fs2
  liftEq eq (LamE t1 n1 p1 e1) (LamE t2 n2 p2 e2) = t1 == t2 && n1 == n2 && liftEq eq p1 p2 && liftEq eq e1 e2
  liftEq eq (AppE t1 l1 l2) (AppE t2 r1 r2) = t1 == t2 && liftEq eq l1 r1 && liftEq eq l2 r2
  liftEq eq (CaseE t1 es1 as1) (CaseE t2 es2 as2) = t1 == t2 && liftEq (liftEq eq) es1 es2 && liftEq (liftEq eq) as1 as2
  liftEq eq (LetE t1 n1 bs1 e1) (LetE t2 n2 bs2 e2) = t1 == t2 && n1 == n2 && liftEq (liftEq eq) bs1 bs2 && liftEq eq e1 e2
  liftEq _ _ _ = False

instance Eq1 Lit where
  liftEq _ (IntL i1) (IntL i2) = i1 == i2
  liftEq _ (NumL i1) (NumL i2) = i1 == i2
  liftEq _ (StringL i1) (StringL i2) = i1 == i2
  liftEq _ (CharL i1) (CharL i2) = i1 == i2
  liftEq _ (BoolL i1) (BoolL i2) = i1 == i2
  liftEq eq (ArrayL xs) (ArrayL ys) = liftEq eq xs ys
  liftEq _ _ _ = False

instance (Eq1 f, Monad f) => Eq1 (Pat f) where
  liftEq _ VarP VarP = True
  liftEq _ WildP WildP = True
  liftEq eq (AsP p1) (AsP p2) = liftEq eq p1 p2
  liftEq eq (ConP tn1 cn1 ps1) (ConP tn2 cn2 ps2) = tn1 == tn2 && cn1 == cn2 && liftEq (liftEq eq) ps1 ps2
  liftEq eq (LitP l1) (LitP l2) =  liftEq (liftEq eq) l1 l2
  liftEq _ _ _ = False

instance (Eq1 f, Monad f) => Eq1 (Alt f) where
  liftEq eq (UnguardedAlt n1 ps1 e1) (UnguardedAlt n2 ps2 e2) = n1 == n2 && liftEq (liftEq eq) ps1 ps2 && liftEq eq e1 e2
  liftEq eq (GuardedAlt n1 ps1 e1) (GuardedAlt n2 ps2 e2) = n1 == n2 && liftEq (liftEq eq) ps1 ps2 && go eq e1 e2
    where
      go :: forall a b. (a -> b -> Bool) -> [(Scope Int f a, Scope Int f a)] -> [(Scope Int f b, Scope Int f b)] -> Bool
      go _ [] [] = True
      go f ((g1,ex1):xs) ((g2,ex2):ys) = liftEq f g1 g2 && liftEq f ex1 ex2  && go f xs ys
      go _ _ _ = False
  liftEq _ _ _ = False

instance Applicative Exp where
  pure = V
  (<*>) = ap

instance Monad Exp where
  return = pure
  V a           >>= f     = f a
  CtorE t tn cn fs >>= _  = CtorE t tn cn fs
  AppE t e1 e2    >>=   f = AppE t (e1 >>= f) (e2 >>= f)
  LamE t n p e   >>=    f = LamE t n (p >>>= f) (e >>>= f)
  LetE t n bs e >>=     f = LetE t n (map (>>>= f) bs) (e >>>= f)
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

instance Bound Pat where
  VarP    >>>= _      = VarP
  WildP   >>>= _      = WildP
  AsP p   >>>= f      = AsP (p >>>= f)
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

instance Bound Alt where
  UnguardedAlt i ps e >>>= f = UnguardedAlt i (map (>>>= f) ps) (e >>>= f)
  GuardedAlt i ps es  >>>= f = GuardedAlt i (map (>>>= f) ps) (map (bimap (>>>= f) (>>>= f)) es)

data VarBox = VarBox Ty Ident deriving (Show, Eq)

$(deriveShow1 ''Lit)
$(deriveShow1 ''Pat)
$(deriveShow1 ''Exp)

instance Show1 (Alt Exp) where
  liftShowsPrec sp sl d (UnguardedAlt i ps e)
    = showString "UnGuardedAlt "
      . showsPrec d i
      . showString " "
      . showsPrec d (fmap (\x -> liftShowsPrec sp sl d x "") ps)
      . showString " "
      . liftShowsPrec sp sl d e
  liftShowsPrec sp sl d (GuardedAlt i ps e)
    =  showString "GuardedAlt "
       . showsPrec d i
       . showString " "
       . ps'
       . showString " "
       . e'
   where
      ps' = showsPrec d (fmap (\x -> liftShowsPrec sp sl d x $ "") ps)
      e' =  showsPrec d $ fmap (\(x,y) ->
                    let f z = liftShowsPrec sp sl d z $ ""
                    in   (f x, f y)) e
deriving instance Show a => Show (Exp a)

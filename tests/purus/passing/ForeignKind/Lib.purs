module ForeignKinds.Lib where -- (Nat, Kinded, Zero, Succ, N0, N1, N2, N3, NatProxy(..), class AddNat, addNat, proxy1, proxy2) where

-- declaration

data Nat

-- use in foreign data

foreign import data Zero :: Nat
foreign import data Succ :: Nat -> Nat

-- use in data

data NatProxy (t :: Nat) = NatProxy

-- use in type sig

succProxy :: forall (n :: Nat). NatProxy n -> NatProxy (Succ n)
succProxy _ = NatProxy

-- use in alias

type Kinded (f :: Nat) = f :: Nat

type KindedZero = Kinded Zero

type N0 = Zero
type N1 = Succ N0
type N2 = Succ N1
type N3 = Succ N2

-- use of alias

proxy0 :: NatProxy N0
proxy0 = NatProxy

proxy1 :: NatProxy N1
proxy1 = NatProxy

proxy2 :: NatProxy N2
proxy2 = NatProxy

proxy3 :: NatProxy N3
proxy3 = NatProxy

-- use in class

-- TODO: Don't require annotations in fundep
class AddNat (l :: Nat) (r :: Nat) (o :: Nat) | l -> r o

instance forall (r :: Nat). AddNat Zero r r


-- TODO: Bind kinds
instance forall (l :: Nat) (r :: Nat) (o :: Nat). AddNat l r o
  => AddNat (Succ l) r (Succ o)

addNat :: forall (l :: Nat) (r :: Nat) (o :: Nat). AddNat l r o => NatProxy l -> NatProxy r -> NatProxy o
addNat _ _ = NatProxy

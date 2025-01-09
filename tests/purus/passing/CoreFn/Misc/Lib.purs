module Lib where

import Prim


testCons :: List Int
testCons = Prim.Cons 1 Nil


class TestClass (a :: Type) where
  testMethod :: a -> Boolean

instance TestClass Int where
  testMethod x = True 

testTestClass :: Boolean
testTestClass = testMethod 3 

class Eq (a :: Type) where
  eq :: a -> a -> Boolean

minus :: Int -> Int -> Int
minus _ _ = 42

instance Eq Int where
  eq _ _ = true

brokenEven :: Int -> Int
brokenEven n =
    if n `eq` 0 then 1
    else brokenEven (n `minus` 2)


data Option (a :: Type) = Some a | Nada

opt2Int :: Option Int -> Int
opt2Int = case _ of
  Some i -> i
  Nada -> 0

testOpt2Int :: Int
testOpt2Int = opt2Int (Some 3)

data Identitee (a :: Type) = Identitee a

unIdentitee :: Identitee Int -> Int
unIdentitee = case _ of
  Identitee x -> x

testIdentitee = unIdentitee (Identitee 101)
-- Multi Param
class Eq2 (a :: Type) (b :: Type) where
  eq2 :: a -> b -> Boolean

instance Eq2 Int Boolean where
  eq2 _ _ = true

testEq2 :: Boolean
testEq2 = eq2 101 false

{- Binders (also tests a bunch of other things by happenstance) -}

-- Unit test type for inferBinder'
data TestBinderSum =
    ConInt Int
 --  | ConInts (List Int)
 --  | ConBoolean Boolean
  | ConString String
  | ConChar Char
  | ConNested TestBinderSum
  | ConQuantified (forall (x :: Type). x -> Int)
  | ConConstrained (forall (x :: Type). Eq x => x -> Int) -- kind of nonsensical
  | ConObject {objField :: Int}
  | ConObjectQuantified {objFieldQ :: forall (x :: Type). x -> Int}

testBinders :: TestBinderSum  -> Int
testBinders x = case x of
  ConInt a -> a -- ConstructorBinder enclosing VarBinder
  ConChar _ -> 5 -- Char LitBinder
  ConNested conNest -> case conNest of  -- Nested ConstructorBinders
    ConInt n -> n
    _ -> 2 
  ConQuantified f -> f "hello"
  ConConstrained g -> g 2
  ConNested other -> 7
  ConObject obj -> obj.objField
  ConObjectQuantified objQ -> objQ.objFieldQ "world"
  ConObject objs -> case objs of
    {objField: f} -> f
  other         -> 0

testBindersCase :: Int
testBindersCase = testBinders (ConInt 2)

{- Binding groups (with and w/o type anns) -}

{- Disabling these for now because they don't terminate

 TODO: Break out into a "compiles to PIR but can't evaluate" set of tests
       (these would actually be useful, we want to know that functions like this
        compile to sensible PIR even if they never terminate)

mutuallyRecursiveBindingGroup :: Int
mutuallyRecursiveBindingGroup =
  let f :: Int -> Int
      f x = g 2
      h :: Int -> Int -> Int
      h x y = f y
      g :: Int -> Int
      g y = h (f y) 3
  in g 3

mutuallyRecursiveBindingGroupNoTypes :: Int
mutuallyRecursiveBindingGroupNoTypes =
  let f' x = g' 2
      h' x y = y
      g' y = h' (f' y) 3
  in g' 3
-}
nestedBinds :: Int
nestedBinds =
  let  f :: Int -> Int
       f _ = 4

       g :: forall (a :: Type). a -> Int
       g _ = 5

       h = let i = g "hello"
               j = f i
           in f j
  in h

{- Data declarations -}
data ADataRec = ADataRec {hello :: Int, world :: Boolean}

newtype ANewtypeRec = ANewTypeRec {foo :: Int}

data ASum = Constr1 Int | Constr2 Boolean

{- lits -}
anIntLit :: Int
anIntLit = 1

aStringLit :: String
aStringLit = "woop"

aVal :: Int
aVal = 1

testasum :: ASum -> Int
testasum x = case x of
  Constr1 y -> 1
  Constr2 z -> 2

aBool :: Boolean
aBool = true

aList :: List Int
aList = [1,2,3,4,5]

aList2 :: List Int
aList2 = Cons 1 (Cons 2 Nil)

{- Functions -}

aFunction :: forall (x :: Type). x -> (forall (y :: Type). y -> Int) -> Int
aFunction any f = f any

aFunction2 :: Int -> List Int
aFunction2 x = [x,1]

aFunction3 :: Int -> Int
aFunction3 x = if (eq x 2) then 4 else 1

{- this can't compile to PIR (but it's useful to test the CoreFn codegen)
aFunction4 :: forall (r :: Row Type). {a :: Int | r} -> Int
aFunction4 r = r.a

aFunction5 :: Int
aFunction5 = aFunction4 {a: 2}
-}

{- TODO/FIXME: This should be fixable
aFunction6 :: Int
aFunction6 = aFunction [] go
  where
    go :: forall (z :: Type). z -> Int
    go _ = 10
-}
-- main = aFunction4 {a: 2, b: 3}

{- NOTE: These compile fine but don't terminate during evaluation so they're off for now

recF1 :: forall (x :: Type). x -> Int
recF1 x = recG1 x

recG1 :: forall (x :: Type). x -> Int
recG1 x = recF1 x
-}
testBuiltin :: Int
testBuiltin = Builtin.addInteger 1 2
 
main = aFunction4 {a: 101, b: "hello"}
  where
    aFunction4 :: forall (r :: Row Type). {a :: Int | r} -> Int
    aFunction4 r = r.a
-- main2 = ConBoolean true

plus :: Int -> Int -> Int
plus a b = Builtin.addInteger a b

infixr 5 plus as +

infixr 5 Builtin.multiplyInteger as *

fakeLT :: Int -> Int -> Boolean
fakeLT _ _ = True

infixr 5 fakeLT as <=

testPlus = plus 1 1

-- main = plus 1 1

guardedCase :: Int -> Int -> Int
guardedCase w x = case w, x of
  y, z | eq y 2
       , aPred y
       , eq z 0
       , eq y nestedBinds -> 2
  _, _ -> 0

nestedApplications :: Int
nestedApplications = i (f (g (h 2))) 4
  where
    i x _ = x
    f x = x
    g _ = 5
    h = case _ of
      2 -> 3
      _ -> 5

{- Objects -}

anObj :: {foo :: Int}
anObj = {foo: 3}

objUpdate :: {foo :: Int}
objUpdate = anObj {foo = 4}

polyInObj :: {bar :: forall (x :: Type). x -> Int, baz :: Int}
polyInObj = {bar: go, baz : 100}
  where
    go :: forall (y :: Type). y -> Int
    go _ = 5

polyInObjMatch :: Int
polyInObjMatch = case polyInObj of
  {bar: f, baz: _} -> f "hello"

aPred :: Int -> Boolean
aPred _ = true

cons :: forall (a :: Type). a -> List a -> List a
cons x xs = [x]

{-
emptyList :: forall (x :: Type). List x
emptyList = []
-}

consEmptyList1 = cons 1 []

consEmptyList2 = cons "hello" []

id :: forall (t :: Type). t -> t
id x = x

testId = id 2

objForall :: forall (a :: Type) (b :: Type). {getIdA :: a -> a, getIdB :: b -> b}
objForall = {getIdA: id, getIdB: id}

arrForall ::  List (forall (a :: Type). a -> a)
arrForall = [id]

guardedCase2 :: Int
guardedCase2 = case polyInObj of
  {bar: _, baz: x}
              | eq @Int x 4 -> x
  _ -> 0

{-

-- Works with signature, throws without-- 
inner :: { getId :: forall a. a -> a}
inner = {getId: id}-}


class Eq a <= Ord (a :: Type) where
  compare :: a -> a -> Int

instance Ord Int where
  compare _ _ = 42

testEqViaOrd :: forall (a :: Type). Ord a => a -> a -> Boolean
testEqViaOrd a b = eq a b

testSuperClass :: Boolean
testSuperClass = testEqViaOrd 1 2

testValidator :: forall (a :: Type) (b :: Type) (c :: Type)
              . a -> b -> c -> Boolean
testValidator datum redeemer context =  True

testValidatorApplied :: Boolean
testValidatorApplied = testValidator "datum" "redeemer" "context"

testForLift :: Int -> Boolean
testForLift x = h x 3
  where
    h a b = g a <= j 4 b
    j c d = c + g d
    g a = if h a x then j x 1 else x * x

{- TODO: Doesn't terminate, move somewhere else where we won't evaluate it
testForLiftApplied :: Boolean
testForLiftApplied = testForLift 2
-}
testForLiftPoly :: forall (a :: Type). a -> Boolean
testForLiftPoly x = h x True
  where
    q :: a 
    q = x

    h :: a -> Boolean -> Boolean
    h a b = if g a then i q  else j a b
      where
        i :: forall (b :: Type). b -> Boolean
        i z  = False

    j :: a -> Boolean -> Boolean
    j c d = if d then d else g c

    g :: a -> Boolean
    g y = True

testForLiftPolyApplied = testForLiftPoly "hello"

or :: Boolean -> Boolean -> Boolean
or b1 b2 = if b1 then True else b2

infixr 5 or as ||

not :: Boolean -> Boolean
not b = if b then False else True

and :: Boolean -> Boolean -> Boolean
and p q = not (not p || not q) -- i think? rly tired atm

infixr 6 and as &&

iff :: Boolean -> Boolean -> Boolean
iff p q = (p && q) || (not p && not q)

infix 5 iff as ===

{- TODO: Doesn't terminate 
ghcInlinerPaperEx :: Boolean
ghcInlinerPaperEx = p
  where
    p = q False
    h x = f True && x
    g x = h x === False
    f x = g (not x)
    q x = g (x=== not x)
-}

{- TODO: Doesn't terminate 
kozsTwoSCCEx :: Boolean
kozsTwoSCCEx =
  let z = True
      a x =  b x && x
      b x =  x &&  c x && h x
      c x =  not (f x && b x)
      f x = not (g x)
      g x =  not (h x) && not x
      h x = not (f x)
  in a z && b z && c z && f z && g z && h z
-}
testLedgerTypes :: DCert
testLedgerTypes = DCertMir

litPattern :: Int -> Boolean
litPattern n = case n of
  0 -> False
  1 -> True
  2 -> True
  3 -> True
  4 -> True
  _ -> False

litPatternApplied :: Boolean
litPatternApplied = litPattern 5

irrPattern :: Int -> Int
irrPattern n = case n of
  _ -> 2

someData :: Builtin.BuiltinData
someData = Builtin.iData 1

testPrelude1 :: Int
testPrelude1 = deserializeInt someData

someDataList :: Builtin.BuiltinList Builtin.BuiltinData
someDataList = Builtin.mkCons someData (Builtin.mkNilData Prim.unit)

isNullSomeDataList :: Boolean
isNullSomeDataList = Builtin.nullList someDataList 
{-
plutusIFTE :: Builtin.BuiltinData
plutusIFTE = Builtin.ifThenElse True someData (Builtin.trace "BOOM!" someData)
-}

identitea :: forall (x :: Type). x -> x
identitea x = x

apIdentitea :: Int
apIdentitea = identitea 2 

testIdConst :: Int
testIdConst = identitea (const 5 2)
  where
    const :: forall (a :: Type) (b :: Type). a -> b -> a
    const p q = p 

testForLift' :: Int -> Boolean
testForLift' x = h x 3
  where
    h a b = g a <= 4
    g a = if h a x then x + x else x * x

{- TODO: Doesn't terminate, enable execution budget or move
testForLiftApplied' :: Boolean
testForLiftApplied' = testForLift' 101
-}

-- Multi case elimination test 
data C (a :: Type) (b :: Type) (c :: Type) = C a b c


-- iffbool I guess
eqBool :: Boolean -> Boolean -> Boolean
eqBool True True = True
eqBool False False = True
eqBool _ _ = False


equalsC :: C Int String (Maybe Boolean) -> C Int String (Maybe Boolean) -> Boolean
equalsC (C i1 s1 (Just b1)) (C i2 s2 (Just b2)) = and (Builtin.equalsInteger i1 i2)
                                                  (and (Builtin.equalsString s1 s2)
                                                       (eqBool b1 b2)
                                                  )
equalsC (C i1 s1 Nothing) (C i2 s2 Nothing) = and (Builtin.equalsInteger i1 i2) (Builtin.equalsString s1 s2)
equalsC _ _ = False

testMultiCaseSimple :: Maybe Int -> Maybe Int -> Int
testMultiCaseSimple Nothing Nothing = 0
testMultiCaseSimple Nothing (Just y) = y
testMultiCaseSimple (Just x) Nothing = x
testMultiCaseSimple (Just x) (Just y) = x + y

-- redundant args in case expressions / unreachable case winnowing

testRedundantCtors :: Maybe Int -> Prim.Unit
testRedundantCtors x = case x of
  Just 1 -> unit
  --Just 1 -> unit
  Just x -> unit
  --Just y -> unit
  Nothing -> unit
  --Nothing -> unit

testBrokenCollapse :: Identitee Int -> Prim.Unit
testBrokenCollapse = case _ of
  Identitee 1 -> unit
  Identitee x -> unit 

testRedundantLit :: Int -> Int 
testRedundantLit x = case x of
  1 -> 1
  1 -> 2
  1 -> 3
  _ -> 4
  x -> 5

-- NESTED CONSTRUCTORS?!

testNested :: Maybe (Maybe (Maybe Int)) -> Int
testNested = case _ of
  Nothing -> 0
  Just Nothing -> 1
  Just (Just Nothing) -> 2
  Just (Just (Just x)) -> x

testNestedSmaller :: Maybe (Maybe Int) -> Int
testNestedSmaller = case _ of
  Nothing -> 0
  Just Nothing -> 1
  Just (Just x) -> x


-- error (compiler magic)

testError :: Int -> Int
testError = case _ of
  0 -> error @Int   
  other -> other 

testError' :: Int -> Int
testError' = case _ of
  0 -> error
  other -> other

testDelay :: Delayed Int
testDelay = delay 2

testForce :: Int
testForce = force testDelay

testLazy :: Boolean
testLazy = force (
  if True
  then delay True
  else delay error)

testLazy' :: Boolean
testLazy' = force (case True of
  True -> delay True
  False -> delay error)


data AB = A | B

testAB1 :: AB -> String
testAB1 = case _ of
  A -> "A"
  B -> "B"

testAB2 ::  String
testAB2 = case A of
  A -> "A"
  B -> "B"

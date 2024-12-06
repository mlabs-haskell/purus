module TestInliner where


-- Basic logical & arithmetical operators for less trivial examples

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

fakeLT :: Int -> Int -> Boolean
fakeLT _ _ = True

infixr 5 fakeLT as <=

plus :: Int -> Int -> Int
plus a b = Builtin.addInteger a b

infixr 5 plus as +

infixr 5 Builtin.multiplyInteger as *

-- Test cases for inliner/lifter

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

{- TODO: Doesn't terminate
ghcInlinerPaperEx :: Boolean
ghcInlinerPaperEx = p
  where
    p = q False
    h x = f True && x
    g x = h x === False
    f x = g (not x)
    q x = g (x=== not x)


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
testForLift :: Int -> Boolean
testForLift x = h x 3
  where
    h a b = g a <= j 4 b
    j c d = c + g d
    g a = if h a x then j x 1 else x * x

{-TODO: Doesn't termiante
testForLiftApplied :: Boolean
testForLiftApplied = testForLift 2
-}
testForLift' :: Int -> Boolean
testForLift' x = h x 3
  where
    h a b = g a <= 4
    g a = if h a x then x + x else x * x

{- TODO: Doesn't terminate
testForLiftApplied' :: Boolean
testForLiftApplied' = testForLift' 101
-}

module ValidPatterns where

data Tuple (a :: Type) (b :: Type) = Tuple a b

infixr 6 Tuple as /\
infixr 6 type Tuple as /\

data Constructors (a :: Type) (b :: Type) (c :: Type)
    = A a b c
    | B b c
    | C c
    | D

myConstructorPatterns :: forall (a :: Type) (b :: Type) (c :: Type). Constructors a b c -> Boolean
myConstructorPatterns (A _ _ _) = True
myConstructorPatterns (A a _ _) = True
myConstructorPatterns (A a b _) = True
myConstructorPatterns (A a b c) = True
myConstructorPatterns (A _ b c) = True
myConstructorPatterns (B _ _) = True
myConstructorPatterns (B b _) = True
myConstructorPatterns (B b c) = True
myConstructorPatterns (C _) = True
myConstructorPatterns (C c) = True
myConstructorPatterns D = True
myConstructorPatterns (D) = True
-- Add some useless brackets:
myConstructorPatterns (A (_) _ _) = True
myConstructorPatterns (A (a) _ _) = True
myConstructorPatterns (A (a) b _) = True
myConstructorPatterns (A (a) b c) = True
myConstructorPatterns (A (_) b c) = True
myConstructorPatterns (B (_) _) = True
myConstructorPatterns (B (b) _) = True
myConstructorPatterns (B (b) c) = True
myConstructorPatterns (C (_)) = True
myConstructorPatterns (C (c)) = True
myConstructorPatterns ((D)) = True

myBinaryOpPattern :: forall (a :: Type) (b :: Type). Tuple a b -> Boolean
myBinaryOpPattern (_ /\ _) = True
myBinaryOpPattern (a /\ b) = True
myBinaryOpPattern (_ /\ b) = True
myBinaryOpPattern (a /\ _) = True
-- Add some useless brackets:
myBinaryOpPattern ((_) /\ _) = True
myBinaryOpPattern ((a) /\ b) = True
myBinaryOpPattern ((_) /\ b) = True
myBinaryOpPattern ((a) /\ _) = True
myBinaryOpPattern ((_) /\ (_)) = True
myBinaryOpPattern ((a) /\ (b)) = True
myBinaryOpPattern ((_) /\ (b)) = True
myBinaryOpPattern ((a) /\ (_)) = True
-- Add more useless brackets:
myBinaryOpPattern (((_) /\ (_))) = True
myBinaryOpPattern (((a) /\ (b))) = True
myBinaryOpPattern (((_) /\ (b))) = True
myBinaryOpPattern (((a) /\ (_))) = True
myBinaryOpPattern _ = True


myNumericLiteralPattern :: Int -> Boolean
myNumericLiteralPattern 0 = True
myNumericLiteralPattern (0) = True
myNumericLiteralPattern _ = True
myNumericLiteralPattern a = True
myNumericLiteralPattern (a) = True

myCharLiteralPattern :: Char -> Boolean
myCharLiteralPattern 'a' = True
myCharLiteralPattern ('b') = True
myCharLiteralPattern _ = True
myCharLiteralPattern a = True
myCharLiteralPattern (a) = True

someBinOpCases :: Boolean
someBinOpCases = 
    let someTuple = 'a' /\ 'b'
    in case someTuple of
        a /\ b -> True
        _ /\ _ -> True
        (_ /\ _) -> True

someConstructorCases :: Int
someConstructorCases = 
    let someConstructor = A 'a' 'b' 'c'
    in case someConstructor of
        A _ _ _   -> 1
        (A _ _ _) -> 2
        B _ (_)   -> 3
        (C (_))   -> 4
        (C _)     -> 5
        C _       -> 6
        D         -> 7
        (D)       -> 8

someNumericLiteralCases :: Boolean
someNumericLiteralCases = case 0 of
    1 -> True
    1 -> True
    2 -> True
    (3) -> True
    (-3) -> True
    _ -> True

someNumericLiteralPatterns :: Int -> Boolean
someNumericLiteralPatterns (-3) =  True
someNumericLiteralPatterns 0 =  True
someNumericLiteralPatterns 1 =  True
someNumericLiteralPatterns _ =  True

someCharLiteralCases :: Boolean
someCharLiteralCases = case 'a' of
    'a' -> True
    'b' -> True
    ('c') -> True
    _ -> True

someStringLiteralPatterns :: String -> Boolean
someStringLiteralPatterns "" = True
someStringLiteralPatterns "a" = True
someStringLiteralPatterns "banana" = True
someStringLiteralPatterns ("") = True
someStringLiteralPatterns (("a")) = True
someStringLiteralPatterns ("banana") = True
someStringLiteralPatterns _ = True

someStringLiteralCases :: Boolean
someStringLiteralCases = case "" of
    "" -> True
    "a" -> True
    ("b") -> True
    ("ccc") -> True
    _ -> True

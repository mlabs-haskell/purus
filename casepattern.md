Case Desugaring: Temporary Patterns

data ADT
  = C0
  | C1 Int
  | C2 ADT Bool

-- f :: Bool -> Int

example :: ADT -> Int
example = \case
i.    C0 -> 0
ii.   three@(C1 x) -> x
iii.  C1 2 -> 2
iv.   C2 C0 True -> 0
v.    C2 (C1 x) _ -> f x
vi.   C2 x y -> example x + f y


1) Calculate the temporary SOP type for matching. Each case branch is a "constructor" and each bound variable is an argument to the product. Call this `mkTmpSop`

mkTmpSOP example  =[
i.     [],
ii.    [ADT,Int],
iii.   [],
iv.    [],
v.     [Int],
vi.    [ADT,Int]
]

2) Calculate the variables bound in the patterns of each branch, and their types:

example Bindings = [
i.      [],
ii.     [three :: ADT, x :: Int],
iii.    [],
iv.     [x :: Int]
v.      [x :: ADT, y :: Bool]
]

3) Write a function that checks whether a scrutinee matches a pattern. Call this `matches`
  - we need this when we have nested patterns, like in ii. We need to know whether the inner pattern (`C1 x`) matches before binding any variables

'#' is a PIR-level application operator

Procedure:
  - VarP/WildP always match, so for these patterns the function is: \_ -> True
  - AsP matches if the inner pattern matches
  - LitPs match if the literals in the pattern are equal to the expression, so for iii the function is: \(x :: Int) -> x == 2
  - Constructor patterns match if:
     1. The scrutinee variant is the same variant as the constructor in the pattern (i.e. the constructor indices match)
     2. All of the arguments to the constructor expression match their respective arguments (if there aren't any arguments, i.e. if the ctor is unary, 1. is the only test)
    So in PIR-ish notation, we get:
      i. C0 ==> \scrut ->
                  case scrut [ True -- C0 case
                             , False
                             , False
                             ]
      ii. C1 x ==> \scrut ->
                     case scrut [ False,
                                , \_ -> True -- C1 has 1 arg and it's a VarP so it matches any Int
                                , \_ _ -> False -- C2 has 2 args but they don't matter here
                                ]
      iii. C1 2 ==> \scrut ->
                      case scrut [ False
                                 , \scrut0 -> scrut0 == 2
                                 , \_ _ -> False
                                      ]
      iv. C2 C0 True ==> \scrut ->
                           case scrut [ False
                                      , \_ -> False
                                      , \scrut0 scrut1 -> (matches C0 # scrut0) && (matches True # scrut1)
                                      ]
      ...etc

4) Write a function that maps the scrutinee to the Tmp Pattern ADT for each alt branch. call this `toPatSOP`

Preliminary: For a list of alternatives (alt:alts), we often need `toPatsSOP alts` to construct the `alt` case. Call this function `fallThrough`

Procedure:

  - VarP always binds a var succeeds. So if we have a TOP LEVEL VarP pattern `x` for an alternative at index `n` toPatSOP `x` = \scrut -> Constr n [scrut]
  - If we have a nested VarP (i.e. inside of a constructor pattern), we don't map to a Constr (see below)
  - WildP doesn't bind any vars and maps to an empty list of fields at the current alternative index: _ ==> \_ -> Constr n []
  - Literal patterns don't bind any variables and are just used to construct the `matches` functions.
    -  E.g. a top level `1` pattern ==> \scrut -> if (matches `1` # scrut) (Constr n []) else (fallThrough # scrut)
  - Constructor patterns map the variables bound in their argument patterns to a Constructor in the temporary ADT. Example:
    iv. C2 (C1 x) True  ==> \scrut -> case scrut [ fallThrough # scrut
                                             , \_ -> fallThrough # scrut
                                             , \(arg0 :: ADT) (arg1 :: Bool) ->
                                                 if (matches (C1 x) # arg0) && (matches True # arg1)
                                                 then case arg0 [ error
                                                                , \xInt -> Constr n xInt
                                                                , error
                                                                ]
                                                 else (fallThrough # scrut)
                                             ]
    Another example w/ multiple bound vars: 
    ??. C2 (C2 x y) z ==> \scrut -> case scrut [ fallThrough # scrut
                                             , \_ -> fallThrough # scrut 
                                             , \(arg0 :: ADT) (arg1 :: Bool) -> 
                                                 if (matches (C1 x y) # arg0) && (matches y # arg1)
                                                 then let res0 = case arg0 [error 
                                                                           ,\xADT _ -> xInt
                                                                           ,error]
                                                          res1 =  case arg0 [error 
                                                                           ,\_ yADT-> xInt
                                                                           ,error]
                                                      in Constr n [res0,res1]
                                                 else (fallThrough # scrut)
                                             ]

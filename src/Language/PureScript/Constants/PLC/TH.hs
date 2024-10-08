{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

module Language.PureScript.Constants.PLC.TH where

import Prelude

import Data.Char (toLower)
import Data.Functor ((<&>))
import Data.Map qualified as M
import Language.Haskell.TH (
  Body (NormalB),
  Clause (Clause),
  Dec (FunD),
  Exp (AppE, ConE, ListE, LitE, TupE),
  Lit (StringL),
  Name,
  Q,
  mkName,
  nameBase,
 )
import Language.Haskell.TH.Datatype (
  ConstructorInfo (ConstructorInfo, constructorName),
  ConstructorVariant (NormalConstructor),
  DatatypeInfo (
    DatatypeInfo,
    datatypeCons,
    datatypeContext,
    datatypeInstTypes,
    datatypeName,
    datatypeVariant,
    datatypeVars
  ),
  reifyDatatype,
 )

isNormalNullaryCtor :: ConstructorInfo -> Bool
isNormalNullaryCtor (ConstructorInfo _ [] [] [] [] NormalConstructor) = True
isNormalNullaryCtor _ = False

lowerName :: Name -> String
lowerName nm = case nameBase nm of
  (x : xs) -> toLower x : xs
  other -> other

ctorBaseNames :: Name -> Q [String]
ctorBaseNames nm = do
  DatatypeInfo {..} <- reifyDatatype nm
  pure $ lowerName . constructorName <$> datatypeCons

{- This takes the name of a Sum type w/ only Nullary constructors (t) and
   creates a `Map String t` from the name (w/ a lowercase'd first char to make PS
   TH machinery happy) to the corresponding constructor.

   We need this to convert to PIR. Builtin functions are free variables, and
   the only information we can embed in a Var is the qualified name. During
   conversion to PIR, we have to be able to lookup the Builtin that correspond to
   the string in the Var.
-}
mkBuiltinMap :: Name -> Q [Dec]
mkBuiltinMap nm = do
  DatatypeInfo {..} <- reifyDatatype nm
  let ctors = datatypeCons
  if all isNormalNullaryCtor ctors
    then do
      let ctorNames = constructorName <$> ctors
          baseAndQualified =
            ctorNames <&> \x ->
              TupE
                [ Just . LitE . StringL . lowerName $ x
                , Just (ConE x)
                ]
      fromListE <- [e|M.fromList|]
      let body = AppE fromListE (ListE baseAndQualified)

      pure [FunD (mkName $ lowerName nm <> "Map") [Clause [] (NormalB body) []]]
    else
      fail $
        "Cannot construct a Map for type "
          <> show nm
          <> " because at least one ctor is not a normal, nullary ctor"

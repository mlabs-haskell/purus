-- @shouldFailWith UndefinedTypeVariable
module Main where

type T = forall a. List a

foo :: T
foo = bar where
  bar :: List a
  bar = []

-- @shouldWarnWith MissingTypeDeclaration
module Main where

import Effect.Console

type T = forall a. List a

-- | Note: This should not raise a `ShadowedTypeVar` warning as the 
-- | type `a` introduced in `T` should not be in scope 
-- | in the definition of `bar`.
foo :: T
foo = bar where
  bar :: forall a. List a
  bar = []

main = log "Done"

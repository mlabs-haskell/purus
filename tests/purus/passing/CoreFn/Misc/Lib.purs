module Lib where

import Prim

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

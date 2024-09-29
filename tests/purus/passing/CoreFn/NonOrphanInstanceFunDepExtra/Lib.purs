module Lib where
-- covering sets: {{f, l}}
class C (f :: Type) (l :: Type) (r :: Type) | l -> r
data L

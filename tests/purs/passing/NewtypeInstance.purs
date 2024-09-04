module Main where

import Prelude
import Effect
import Effect.Console
import Data.Monoid
import Data.Tuple

type MyString = String 

newtype X = X MyString

derive newtype instance showX :: Show X
derive newtype instance eqX :: Eq X
derive newtype instance ordX :: Ord X

newtype Y a = Y (List a)

derive newtype instance showY :: Show (Y String)

class Singleton a b where
  singleton :: a -> b

instance singletonList :: Singleton a (List a) where
  singleton x = [x]

derive newtype instance singletonY :: Singleton a (Y a)

newtype MyList a = MyList (List a)

derive newtype instance showMyList :: Show a => Show (MyList a)
derive newtype instance functorMyList :: Functor MyList

newtype ProxyList x a = ProxyList (List a)

derive newtype instance functorProxyList :: Functor (ProxyList x)

class (Monad m, Monoid w) <= MonadWriter w m | m -> w where
  tell :: w -> m Unit

instance monadWriterTuple :: Monoid w => MonadWriter w (Tuple w) where
  tell w = Tuple w unit

newtype MyWriter w a = MyWriter (Tuple w a)

derive newtype instance functorMyWriter :: Functor (MyWriter w)
derive newtype instance applyMyWriter :: Semigroup w => Apply (MyWriter w)
derive newtype instance applicativeMyWriter :: Monoid w => Applicative (MyWriter w)
derive newtype instance bindMyWriter :: Semigroup w => Bind (MyWriter w)
derive newtype instance monadMyWriter :: Monoid w => Monad (MyWriter w)
derive newtype instance monadWriterMyWriter :: Monoid w => MonadWriter w (MyWriter w)

type Syn' w a = MyWriter w a
newtype Syn a = Syn (Syn' (MyList Int) a)
derive newtype instance functorSyn :: Functor Syn

data Proxy2 a b = Proxy2
derive instance Functor (Proxy2 x)

newtype Foo :: forall k. k -> Type
newtype Foo a = Foo (Proxy2 k a)
derive newtype instance Functor Foo

main = do
  logShow (X "test")
  logShow (singleton "test" :: Y String)
  logShow (map show (MyList [1, 2, 3]))
  log "Done"

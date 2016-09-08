{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

import Data.Proxy
import Data.Monoid
import GHC.Types
import Control.Monad.Identity
import Data.Function



-- | Takes something of any kind and makes a term out of it
--  We only product with Proxy to maintain injectivity

class Reifiable k (a :: k) | a -> k where
  type Reify k (a :: k) = r | r -> k a

instance Reifiable Type a where
  type Reify Type a = (Proxy Type, a)

-- | Discharge Proxy

type family  LoseProxy (a :: k) :: k where
  LoseProxy (Proxy k, a) = a


-- | Polykinded Existentials

class Existential k (e :: Type) | e -> k where
  type ConstraintOf e :: k -> Constraint

  pack :: forall (a :: k). (ConstraintOf e) a => LoseProxy (Reify k a) -> e
  modify :: (forall (a :: k). (ConstraintOf e) a => LoseProxy (Reify k a) -> LoseProxy (Reify k a)) -> e -> e
  apply :: forall b. (forall (a :: k). (ConstraintOf e) a => LoseProxy (Reify k a) -> b) -> e -> b


-- | DeMorgan's Law: exists a. a = ~forall a. ~a
data Exists c where
  Exists :: (forall b. (forall a. c a => LoseProxy (Reify Type a) -> b) -> b) -> Exists c

instance Existential Type (Exists c) where
  type ConstraintOf (Exists c) = c
  pack x = Exists (\f -> f x)
  modify g (Exists a) =  Exists $ \f -> a (\x -> f . g $ x)
  apply f (Exists a) = a f


-- | Let's test it on existential types:

class CounterLike a where
  zero :: a
  incr :: a -> a
  toInt :: a -> Int

instance CounterLike Int where
  zero = 0
  incr = (+1)
  toInt = id

type UnaryCounter = [()]
instance CounterLike UnaryCounter where
  zero = []
  incr = (():)
  toInt = length


myCounters :: [Exists CounterLike]
myCounters = [ pack (zero :: Int)
             , pack (zero :: UnaryCounter)
               & modify incr
               & modify incr
             ]

sumMyCounters :: Int
sumMyCounters = sum . (apply toInt<$>) $ myCounters


-- | Now lets test it on existential functors:

class Foo f where
  unFoo :: f a -> Int


{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeApplications #-}

import Data.Proxy
import Data.Monoid
import GHC.Types

type UnaryCounter = [()]

class CounterLike a where
  zero :: a
  incr :: a -> a
  toInt :: a -> Int

instance CounterLike Int where
  zero = 0
  incr = (+1)
  toInt = id

instance CounterLike UnaryCounter where
  zero = []
  incr = (():)
  toInt = length

-- DeMorgan's Law 

newtype Exists c = Exists (forall b. (forall a. c a => a -> b) -> b)

pack :: forall c a. c a => a -> Exists c
modify :: forall c. (forall a. c a => a -> a) -> Exists c -> Exists c
apply :: forall c b. (forall a. c a => a -> b) -> Exists c -> b

pack x = Exists (\f -> f x)
modify g (Exists k) =  Exists (\f -> k (\x -> f . g $ x))
apply f (Exists k) = k f


mylilcounter :: Exists CounterLike
mylilcounter = pack (100 :: Int)

double :: Exists CounterLike -> Exists CounterLike
double = modify (\x -> d x x)
  where
    d x = appEndo . mconcat $ replicate (toInt x) (Endo incr)

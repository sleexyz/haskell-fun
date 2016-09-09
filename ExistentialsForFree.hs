{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ConstraintKinds #-}

import Data.Proxy
import Data.Monoid

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

-- demorgan's law
--
-- exists x. px = !forall x. !px

type Exists c = (forall b. (forall a. c a => a -> b) -> b)

pack :: 
  forall c a. c a => 
  Proxy c ->  
  a -> 
  Exists c
pack p x f = f x

modify :: forall c. Proxy c -> (forall a. c a => a -> a) -> Exists c -> Exists c
modify _ g k =  \f -> k (\x -> f . g $ x)







mylilcounter :: Exists CounterLike
mylilcounter = pack (Proxy :: Proxy CounterLike) (zero :: Int)

double :: Exists CounterLike -> Exists CounterLike
double k = \f -> k (\x -> f . d x $ x)
  where
    d x = appEndo . mconcat $ replicate (toInt x) (Endo incr)

double' :: Exists CounterLike -> Exists CounterLike
double' = modify (Proxy :: Proxy CounterLike) (\x -> d x x)
  where
    d x = appEndo . mconcat $ replicate (toInt x) (Endo incr)

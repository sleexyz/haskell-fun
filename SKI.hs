module SKI where

import Prelude hiding (Functor, Applicative)

class Functor f where
  fmap :: (a -> b) -> f a -> f b

class (Functor f) => Applicative f where
  pure :: a -> f a
  (<*>) :: f (a -> b) -> f a -> f b



i x = x
s x t = x
k f g t = (f t) (g t)

-- instance Functor ((->) t) where
--   fmap =  k

-- instance Applicative ((->) t) where
--   pure = s
--   (<*>) = k

{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Alternative applicative presentation
--
-- http://blog.ezyang.com/2012/08/applicative-functors/
module Applicative where


class Applicative' f where
  unit :: f ()
  (***) :: f a -> f b -> f (a, b)


instance (Functor f, Applicative' f) => Applicative f where
  pure :: a -> f a
  pure x = const x <$> unit

  (<*>) :: f (a -> b) -> f a -> f b
  f <*> x = (\(l, r) -> l r) <$> (f *** x)





{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- Freer Free monoids
-- Frozen Free monoids

import Data.Monoid
import GHC.TypeLits
import GHC.Types
import Data.Proxy
import Data.Profunctor
import Control.Monad

-- | first level monoid scope
-- data MonoidScope m  = MonoidScope [m] m

-- instance Monoid m => Monoid (MonoidScope m) where
--   mempty = MonoidScope [] mempty
--   mappend (MonoidScope ms m) (MonoidScope ns n) = MonoidScope (mappend ms ns) (mappend m n)

-- liftMonoidScope :: m -> MonoidScope m
-- liftMonoidScope x = MonoidScope [] x

data N = Z | S N

-- | A frozen free monad; indexed by depth

data family IterateN (n :: N) (f :: Type -> Type) a
data instance IterateN Z f a = Pure a
data instance IterateN (S n) f a = In (f (IterateN n f a))

unpure :: IterateN Z f a -> a
unpure (Pure a) = a

unin :: IterateN (S n) f a -> f (IterateN n f a)
unin (In a) = a


instance (Functor f) => Functor (IterateN Z f) where
  fmap = dimap unpure Pure
instance (Functor f, Functor (IterateN n f)) => Functor (IterateN (S n) f) where
  fmap =  dimap unin In . fmap . fmap

instance (Applicative f) => Applicative (IterateN Z f) where
  pure = Pure
  (Pure f) <*> (Pure x) = Pure (f x)

instance (Applicative f, Applicative (IterateN n f)) => Applicative (IterateN (S n) f) where
  pure = In . pure . pure
  (In f) <*> (In x) = In (((<*>) . (fmap(<*>))) f x)

instance Monad f => Monad (IterateN Z f) where
  (Pure x) >>= f = f x

instance (Monad f, Monad (IterateN n f)) => Monad (IterateN (S n) f) where
  x >>= f = joinScope (f <$> x)
    where
      joinScope :: Monad f => IterateN ('S n) f (IterateN ('S n) f b) -> IterateN ('S n) f b
      joinScope (In x) = In (joinScope' $  x)

      joinScope' :: Monad f => f (IterateN n f (IterateN ('S n) f b1)) -> f (IterateN n f b1)
      joinScope' x = (fmap . _) joinScope x



-- Does this call for plated?

-- Telescope

infixr :--
data Telescope n a where
  Start :: a -> Telescope Z a
  (:--) :: IterateN (S n) [] a -> Telescope n a -> Telescope (S n) a

unstart :: Telescope Z a -> a
unstart (Start x) = x

instance Functor (Telescope Z) where
  fmap = dimap unstart Start

instance (Functor (IterateN (S n) []), Functor (Telescope n)) => Functor (Telescope (S n)) where
  fmap f (x :-- tail) = fmap f x :-- fmap f tail

instance Applicative (Telescope Z) where
  pure = Start
  (Start f) <*> (Start x) = Start (f x)

-- instance (Applicative (Telescope n),)Applicative (Telescope Z) where
--   pure = Start
--   (Start f) <*> (Start x) = Start (f x)

instance Monoid m => Monoid (Telescope Z m) where
  mempty = Start mempty
  mappend (Start x) (Start y) = Start (mappend x y)

instance (Monoid (Telescope n m), Monoid (IterateN (S n) [] m)) => Monoid (Telescope (S n) m) where
  mempty = mempty :-- mempty
  mappend (x :-- xs) (y :-- ys) = (mappend x y) :-- (mappend xs ys)

-- type family (ShowScope n a) where
--   ShowScope 0 a = Show a
--   ShowScope n a = (Show (IterateN n ([]) a), ShowScope (n-1) a)

-- showScope :: ShowableScope n a -> String
-- showScope (Pure m) = "Pure " <> show m
-- showScope (m :-- rest) = show m <> " :-- " <> showScope rest

-- instance Show (ShowableScope n a) where
--   show m = "(" <> showScope m <> ")"

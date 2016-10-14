{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}

-- | What do you know, it's impossible!

module ExistentialInstance where

import Data.Monoid



newtype Exists c = Exists (forall b. (forall a. c a => a -> b) -> b)
-- deriving instance c (Exists c)

pack :: forall c a. c a => a -> Exists c
pack x = Exists (\f -> f x)

modify :: forall c. (forall a. c a => a -> a) -> Exists c -> Exists c
modify g (Exists k) = Exists (\f -> k (\x -> f (g x)))

apply :: forall c b. (forall a. c a => a -> b) -> Exists c -> b
apply f (Exists k) = k f




instance Show (Exists Show) where
  show = apply show

unit :: Exists Show
unit = pack ()

things :: [Exists Show]
things = [pack (), pack "hello", pack (1 :: Int)]

foo :: Monoid m => m -> m
foo _ = mempty

instance Monoid (Exists Monoid) where
  mempty :: Exists Monoid
  mempty = error "impossible"

  mappend :: Exists Monoid -> Exists Monoid -> Exists Monoid
  mappend = error "impossible"

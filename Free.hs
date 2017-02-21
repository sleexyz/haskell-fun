{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE LambdaCase #-}

module Free where

import Data.Function

data Free f a where
  Pure :: a -> Free f a
  Wrap :: f (Free f a) -> Free f a

instance (Functor f) => Functor (Free f) where
  fmap :: (a -> b) -> Free f a -> Free f b
  fmap g = \case
    Pure x -> Pure (g x)
    Wrap prev -> Wrap ((fmap . fmap) g prev)

instance (Functor f) => Applicative (Free f) where
  pure :: a -> Free f a
  pure = Pure
  (<*>) :: Free f (a -> b) -> Free f a -> Free f b
  (Pure f) <*> (Pure x) = Pure (f x)
  (Pure f) <*> (Wrap prev) = Wrap ((fmap . fmap) f prev)
  (Wrap f) <*> prev = Wrap $ fmap (<*> prev) f

instance (Functor f) => Monad (Free f) where
  return :: a -> Free f a
  return = Pure

  (>>=) :: (Functor f) => Free f a -> (a -> Free f b) -> Free f b
  (Pure x) >>= k = k x
  (Wrap x) >>= k = Wrap $ fmap (>>= k) x

class Monad m => MonadFree f m where
  wrap :: f (m a) -> m a

instance (Functor f) => MonadFree f (Free f) where
  wrap :: f (Free f a) -> Free f a
  wrap = Wrap

data Op next =
    Exit
  | Get Int (String -> next)
  | Modify Int (String -> String) (String -> next)
  deriving (Functor)

v1 :: Op (Op (Op next))
v1 = Modify 0 (const "hello")
  $ \x -> Get 1
  $ \x -> Exit

v2 :: Free Op next
v2 = Wrap $ Modify 0 (const "hello")
  $ \x -> Wrap $ Get 1
  $ \x -> Wrap $ Exit

v3 :: Free Op next
v3 = do
  x <- Wrap $ Modify 0 (const "hello") Pure
  y <- Wrap $ Get 1 Pure
  Wrap Exit


liftF :: (Functor f) => f a -> Free f a
liftF = Wrap . fmap Pure

v4 :: Free Op next
v4 = do
  x <- liftF $ Modify 0 (const "hello") id
  y <- liftF $ Get 1 id
  liftF Exit

class (Monad m) => OpRunner m where
  get :: Int -> m String
  modify :: Int -> (String -> String) -> m String
  exit :: m ()

instance OpRunner (Free Op) where
  get i = Wrap $ Get i Pure
  modify i f = Wrap $ Modify i f Pure
  exit = Wrap Exit

v5 :: (OpRunner m) => m ()
v5 = do
  x <- modify 0 (const "hello")
  y <- get 1
  exit


-- hrmm... can we use generic programming to derive lifted version of these
-- functions as methods?

{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UndecidableInstances #-}

module Free where

import Data.Function
import Test.Hspec
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Control.Concurrent.MVar
import Data.Maybe

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

-- | Why "free"? Free, because its part of a free/forgetful adjunction
--
-- which means we need a universal property. For a fixed functor f, it needs to be
-- the "most general" monad such that functor morphisms from f
-- can be factored by some functor morphism from f*

-- TODO: investigate the following claim:
--
-- A free monad is a monad where there exists functions 
--
-- emb :: f a -> Free f a
--
-- and 
--
-- interp :: (Monad m) => (forall a. f a -> m a) -> Free f a -> m a
-- 
-- such that for all monads m and functor morphisms i :: f a -> m a, interp i is the unique
--
-- monad morphism with the property (interp i) . emb = i


-- Lifts a morphism from category of functors to morphism in category of monads
-- Control.Free calls this `foldFree`
interp :: (Functor f, Monad m) => (forall a. f a -> m a) -> Free f a -> m a
interp i (Pure x) =  return x
interp i (Wrap f) = i f >>= interp i

-- Morphism in category of functors
-- Control.Free calls this `liftF`
emb :: (Functor f) => f a -> Free f a
emb = Wrap . fmap Pure

-- unfortunately, free monads have a downfall;
-- for each branch, our `next` parameter
-- must appear in the positive position
-- for interp, aka foldFree, to work

data Op next =
    Get Int (String -> next)
  | Set Int String (String -> next)
  deriving (Functor)

v1 :: Op (Op String)
v1 = Set 0 "hello"
  $ \x -> Get 1 id

class (Monad m) => OpRunner m where
  get :: Int -> m String
  set :: Int -> String -> m String

instance OpRunner (Free Op) where
  get i = Wrap $ Get i Pure
  set i s = Wrap $ Set i s Pure

spec :: Spec
spec = do
  describe "free" $ do
    let
      mkRunOp :: MVar (IntMap String) -> Op a -> IO a
      mkRunOp ref = \case
        Get n k -> do
          -- putStrLn $ "...getting " ++ show n
          dict <- readMVar ref
          return . k $ dict IntMap.! n
        Set n v k -> do
          -- putStrLn $ "...setting " ++ show n ++ " to " ++ v
          modifyMVar_ ref (return . IntMap.insert n v)
          dict <- readMVar ref
          return . k $ dict IntMap.! n

    it "works without do notation" $ do
      let
        example :: Free Op String
        example = Wrap $ Set 0 "hello"
          $ \x -> Wrap $ Get 0 Pure

      ref <- newMVar $ IntMap.fromList []
      example & interp (mkRunOp ref)
      test <- takeMVar ref & (fmap (fromMaybe "" . IntMap.lookup 0))
      test `shouldBe` "hello"

    it "works with do notation" $ do
      let
        example :: Free Op String
        example = do
          Wrap $ Set 0 "hello" Pure
          Wrap $ Get 0 Pure

      ref <- newMVar $ IntMap.fromList []
      example & interp (mkRunOp ref)
      test <- takeMVar ref & (fmap (fromMaybe "" . IntMap.lookup 0))
      test `shouldBe` "hello"

    it "works with emb" $ do
      let
        example :: Free Op String
        example = do
          x <- emb $ Set 0 "hello" id
          emb $ Get 1 id

      ref <- newMVar $ IntMap.fromList []
      example & interp (mkRunOp ref)
      test <- takeMVar ref & (fmap (fromMaybe "" . IntMap.lookup 0))
      test `shouldBe` "hello"

    it "works with classy Monads" $ do
      let
        example :: (OpRunner m) => m String
        example = do
          x <- set 0 "hello"
          get 1

      ref <- newMVar $ IntMap.fromList []
      example & interp (mkRunOp ref)
      test <- takeMVar ref & (fmap (fromMaybe "" . IntMap.lookup 0))
      test `shouldBe` "hello"

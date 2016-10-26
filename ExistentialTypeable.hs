{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ConstraintKinds #-}

module ExistentialInstance where

import Data.Maybe
import Data.Typeable



newtype Exists c = Exists (forall b. (forall a. (Typeable a, c a) => a -> b) -> b)

pack :: forall c a. (Typeable a, c a) => a -> Exists c
pack x = Exists (\f -> f x)

modify :: forall c. (forall a. (Typeable a, c a) => a -> a) -> Exists c -> Exists c
modify g (Exists k) = Exists (\f -> k (\x -> f (g x)))

apply :: forall c b. (forall a. (Typeable a, c a) => a -> b) -> Exists c -> b
apply f (Exists k) = k f

project :: (Typeable a, c a) => Exists c -> Maybe a
project  = apply cast

-- * Example:

things :: [Exists Show]
things = [pack "foo", pack (), pack "bar", pack (1 :: Int), pack "baz"]

onlyStrings :: [String]
onlyStrings = catMaybes . fmap project $ things

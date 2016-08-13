{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Existential where

data Showable where
  Pack :: forall a. (Show a) => a -> Showable



unpackInt :: Showable -> Maybe Int
unpackInt (Pack (a :: Int)) = Just a
unpackInt _ = Nothing

-- instance Show Showable where
--   show = show . unpack


bar :: [Showable]
bar = [Pack 1, Pack "hello"]

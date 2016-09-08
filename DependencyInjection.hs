{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}


-- encapsulated dependency injection
-- via existential types,
-- and readers (co-presheaves)

import Control.Monad.Reader
import Data.Monoid
import GHC.Types
import Data.Proxy



data Packed c where
  Pack :: forall c a. c a =>  a -> Packed c

class Existential e where
  type ConstraintOf e :: * -> Constraint
  modify :: (forall c a. (ConstraintOf e) a => a -> a) -> e -> e
  apply :: forall b. (forall c a. (ConstraintOf e) a => a -> b) -> e -> b

instance Existential (Packed c) where
  type ConstraintOf (Packed c) = c
  modify f (Pack x) = Pack (f x)
  apply f (Pack x) =  f x




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

type NeedsCounterLike a = Reader (Packed CounterLike) a

add10 :: NeedsCounterLike Int
add10 =
  local (appEndo . mconcat $ replicate 10 (Endo (modify incr))) $ do
  getInt

getInt :: NeedsCounterLike Int
getInt = (\(Pack x) -> toInt x)  <$> ask

-- | Lets say, for production
runWithInt :: NeedsCounterLike a -> a
runWithInt dependent = runReader dependent (Pack (zero :: Int))

-- -- | Lets say, for testing
runWithUnary :: NeedsCounterLike a -> a
runWithUnary dependent = runReader dependent (Pack (zero :: UnaryCounter))

foo :: Int
foo = runWithInt add10

foo' :: Int
foo' = runWithUnary add10



-- what can yoneda give us?

pass :: forall c a. c a => a -> (forall b. (Packed c -> b) -> b)
pass x f = f (Pack x)

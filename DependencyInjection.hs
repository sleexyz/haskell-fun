{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}


import Control.Monad.Reader

-- We're going to fake existential types

type UnaryCounter = [()]

class CounterLike c where
  zero :: c
  incr :: c -> c
  toInt :: c -> Int

instance CounterLike Int where
  zero = 0
  incr = (+1)
  toInt = id

instance CounterLike UnaryCounter where
  zero = []
  incr = (():)
  toInt = length


type NeedsRealCounterLike m a = ReaderT Int m a
type NeedsCounterLike m a = (forall c. CounterLike c => ReaderT c m a)


getInt :: NeedsCounterLike IO Int
getInt = toInt <$> ask

-- | Lets say, for production
runWithInt :: NeedsCounterLike m a -> m a
runWithInt dependent = runReaderT dependent (zero :: Int)

-- | Lets say, for testing
runWithUnary :: NeedsCounterLike m a -> m a
runWithUnary dependent = runReaderT dependent (zero :: UnaryCounter)

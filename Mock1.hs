{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TypeOperators #-}

-- | Existentials, without typeclasses!
-- Featuring two examples of safe unpacking

module Mock1 where

import Data.Proxy
import Data.Function
import Data.Monoid
import GHC.Types
import GHC.Generics
import Control.Concurrent.MVar

type a ~> b = forall x. a x -> b x

-- DeMorgan's Law

newtype Exists p = Exists (forall b. (forall a. p a -> b) -> b)

pack :: forall p a. p a -> Exists p
modify :: forall p. (forall a. p a -> p a) -> Exists p -> Exists p
apply :: forall p b. (forall a. p a -> b) -> Exists p -> b

pack x = Exists (\f -> f x)
modify g (Exists k) =  Exists (\f -> k (\x -> f . g $ x))
apply f (Exists k) = k f

-- | Here we store the state purely in the first argument
data Counter a = Counter
  { val :: a
  , incr :: a -> a
  , toInt :: a -> Int
  }
  deriving (Generic)

intCounter :: Counter Int
intCounter = Counter
  { val = 0
  , incr = (+1)
  , toInt = id
  }

unaryCounter :: Counter [()]
unaryCounter = Counter
  { val = []
  , incr = (():)
  , toInt = length
  }

-- | This unpacks our counter
unpackCounter :: Exists Counter ->  Counter (Exists Counter)
unpackCounter c = apply (\Counter{..} -> Counter
                                         { val = c
                                         , incr = modify $ \c@Counter{..} ->
                                             c { val = incr $  val }
                                         , toInt = apply $ \c@Counter {..} ->
                                             toInt val
                                         }
                        ) c

myLilCounter :: Exists Counter
myLilCounter = pack intCounter

add2 = modify $ \c@Counter{..} ->
  c { val = incr . incr $  val }

double = modify $ \c@Counter{..} ->
  let
    d x = appEndo . mconcat $ replicate (toInt x) (Endo incr)
  in
    c { val = d val val }

counter :: Counter (Exists Counter)
counter = unpackCounter myLilCounter


-- | Should be equal to 2
cool ::  Int
cool = val & incr & incr & toInt
  where
    Counter{..} = counter




-- | Stateful existential type!

data StatefulCounter a = StatefulCounter
  { incr :: IO ()
  , value :: IO a
  , toInt :: IO Int
  }


unpackStatefulCounter :: Exists StatefulCounter -> StatefulCounter (Exists StatefulCounter)
unpackStatefulCounter c = apply (\StatefulCounter{..} -> StatefulCounter
                                  { incr = incr
                                  , value = return c
                                  , toInt = toInt
                                  }
                                ) c

myStatefulCounter :: IO (Exists StatefulCounter)
myStatefulCounter = do
  ref <- newMVar 0
  return . pack $ StatefulCounter
    { incr = modifyMVar_  ref (return . (+1))
    , value = readMVar ref
    , toInt = readMVar ref
    }

main :: IO ()
main = do
  counter <- myStatefulCounter
  apply (\StatefulCounter{..} -> incr >> incr >> incr >> incr) counter
  val <- apply (\StatefulCounter{..} -> toInt) counter
  print val

main' :: IO ()
main' = do
  StatefulCounter{..} <- unpackStatefulCounter <$> myStatefulCounter
  return ()
  incr
  incr
  incr
  incr
  val <- toInt
  print val

mkDebug :: StatefulCounter ~> StatefulCounter
mkDebug StatefulCounter{..} =
  StatefulCounter
    { incr = incr >> (toInt >>= print . ("Value: "++) . show)
    , value = value
    , toInt = toInt
    }


main'' :: IO ()
main'' = do
  packedCounter <- myStatefulCounter
  let StatefulCounter{..} = mkDebug $ unpackStatefulCounter packedCounter
  return ()
  incr
  incr
  incr
  incr
  i <- toInt
  print i

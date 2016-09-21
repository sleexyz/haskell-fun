{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ViewPatterns #-}

-- | Existentials, without typeclasses!

import Data.Proxy
import Data.Monoid
import GHC.Types
import GHC.Generics
import Control.Concurrent.MVar

-- DeMorgan's Law 

newtype Exists p = Exists (forall b. (forall a. p a -> b) -> b)

pack :: forall p a. p a -> Exists p
modify :: forall p. (forall a. p a -> p a) -> Exists p -> Exists p
apply :: forall p b. (forall a. p a -> b) -> Exists p -> b

pack x = Exists (\f -> f x)
modify g (Exists k) =  Exists (\f -> k (\x -> f . g $ x))
apply f (Exists k) = k f

data Counter a = Counter
  { val :: a
  , incr :: a -> a
  , toInt :: a -> Int
  }
  deriving (Generic)

intCounter = Counter
  { val = 0
  , incr = (+1)
  , toInt = id
  }

unaryCounter = Counter
  { val = []
  , incr = (():)
  , toInt = length
  }


myLilCounter = pack intCounter

add2 = modify $ \c@Counter{..} ->
  c { val = incr . incr $  val }

double = modify $ \c@Counter{..} ->
  let 
    d x = appEndo . mconcat $ replicate (toInt x) (Endo incr)
  in    
    c { val = d val val }



-- | Stateful existential type!
-- (we don't need a type parameter if our parameter is phantom)

data StatefulCounter a = StatefulCounter
  { incr :: IO ()
  , toInt :: IO Int
  }
  deriving (Generic1)


unpackStatefulCounter :: Exists StatefulCounter -> StatefulCounter (Exists StatefulCounter)
unpackStatefulCounter = apply (\StatefulCounter{..} -> StatefulCounter{..})


myStatefulCounter :: IO (Exists StatefulCounter)
myStatefulCounter = do
  ref <- newMVar 0
  return . pack $ StatefulCounter
    { incr = modifyMVar_  ref (return . (+1))
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

mkDebug :: Exists StatefulCounter -> Exists StatefulCounter
mkDebug = modify (\StatefulCounter{..} ->
  StatefulCounter
    { incr = incr >> (toInt >>= print . ("Value: "++) . show)
    , toInt = toInt
    }
  )

-- unpack :: (Generic1 p) => Exists p -> p (Exists p)
-- unpack = apply (\x -> to1 (from1 x))

main'' :: IO ()
main'' = do
  packedCounter <- mkDebug <$> myStatefulCounter
  let StatefulCounter{..} = unpackStatefulCounter packedCounter
  return ()
  incr
  incr
  incr
  incr
  val <- toInt
  print val

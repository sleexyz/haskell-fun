{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}

import Data.Proxy
import Data.Monoid
import GHC.Types
import GHC.Generics

-- DeMorgan's Law 

newtype Exists c = Exists (forall b. (forall a. c a => a -> b) -> b)

pack :: forall c a. c a => a -> Exists c
modify :: forall c. (forall a. c a => a -> a) -> Exists c -> Exists c
apply :: forall c b. (forall a. c a => a -> b) -> Exists c -> b

pack x = Exists (\f -> f x)
modify g (Exists k) =  Exists (\f -> k (\x -> f . g $ x))
apply f (Exists k) = k f


class HasCounter a where
  counter :: Counter a

data Counter a = Counter
  { _zero :: a
  , _incr :: a -> a
  , _toInt :: a -> Int
  }
  deriving (Generic)

instance HasCounter Int where
  counter = Counter
    { _zero = 0
    , _incr = (+1)
    , _toInt = id
    }

instance HasCounter [()] where
  counter = Counter
    { _zero = []
    , _incr = (():)
    , _toInt = length
    }


mylilcounter :: Exists HasCounter
mylilcounter = pack (100 :: Int)

add2 :: Exists HasCounter -> Exists HasCounter
add2 = modify $ \x ->
  let 
    Counter{..} = counter
  in    
    _incr . _incr $ x

double :: Exists HasCounter -> Exists HasCounter
double = modify $ \x ->
  let 
    Counter{..} = counter
    d x = appEndo . mconcat $ replicate (_toInt x) (Endo _incr)
  in    
    d x x

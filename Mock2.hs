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
{-# LANGUAGE NamedFieldPuns #-}

-- | Goal: Generic safe unpacking of existentials

module Mock2 where

import Data.Function
import GHC.Generics
import Test.Hspec

type a ~> b = forall x. a x -> b x

newtype Exists p = Exists (forall b. (forall a. p a -> b) -> b)
pack :: forall p a. p a -> Exists p
modify :: forall p. (forall a. p a -> p a) -> Exists p -> Exists p
apply :: forall p b. (forall a. p a -> b) -> Exists p -> b
pack x = Exists (\f -> f x)
modify g (Exists k) =  Exists (\f -> k (\x -> f . g $ x))
apply f (Exists k) = k f

-- | We give our type a proof of a
data WithPoint f a = WithPoint { point :: a, functions :: f a }

forgetPoint :: WithPoint f ~> f
forgetPoint = functions

instance Functor f => Functor (WithPoint f) where
  fmap f = \WithPoint{point, functions} -> WithPoint
    { point = f point
    , functions = fmap f functions
    }

data Counter a = Counter
  { zero :: a
  , incr :: a -> a
  , toInt :: a -> Int
  }
  deriving (Generic)

intCounter :: Counter Int
intCounter = Counter
  { zero = 0
  , incr = (+1)
  , toInt = id
  }

unpackCounter :: Exists Counter -> Counter (Exists (WithPoint Counter))
unpackCounter = apply $ \c@Counter{zero} -> Counter
  { zero = pack (WithPoint zero c)
  , incr = modify (\p@(WithPoint point Counter{incr}) ->
                      p {point = incr point}
                  )
  , toInt = apply (\(WithPoint point Counter{toInt}) ->
                      toInt point
                  )
  }

-- | Question is: can we write a generic unpack,
-- that traverses our data structure generically?
unpack :: Exists f -> f (Exists (WithPoint f))
unpack = apply hide

-- If a shows up covariantly, pack
-- If a shows up invariantly, modify
-- If a shows up contravariantly, apply
hide :: f a -> f (Exists (WithPoint f))
hide x = _ (from x)



main :: IO ()
main = hspec spec

spec :: Spec
spec = do

  describe "unpackCounter" $ do
    it "works" $ do
      let Counter{..} = unpackCounter . pack $ intCounter
          val = zero
            & incr
            & incr
            & toInt
      val `shouldBe` 2

  describe "unpack" $ do
    it "works" $ do
      pending


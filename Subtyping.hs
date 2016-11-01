{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE StandaloneDeriving #-}

module Subtyping where

import Test.Hspec
import GHC.Types

-- * Plumbing

type family Map (f :: j -> k) (list :: [j]) where
  Map _ '[] = '[]
  Map f (x : xs) = f x ': Map f xs

type family ConcatConstraints (cs ::[Constraint]) :: Constraint where
  ConcatConstraints '[] = ()
  ConcatConstraints (x ': xs) = (x, ConcatConstraints xs)

-- * Focus:

class KnownCoerce a b where
  coerce :: a -> b

instance {-# INCOHERENT #-} KnownCoerce a a where
  coerce = id

-- * We can write our subtyping relation manually:

data Object = Object
  deriving (Show, Eq)
data Pig = Pig
  deriving (Show, Eq)

instance KnownCoerce Pig Object where
  coerce Pig = Object

-- * Or we can derive our subtyping relation from the structure of our type:

data HList :: [*] -> * where
  Nil :: HList '[]
  (:+) :: a -> HList as -> HList (a ': as)
deriving instance (ConcatConstraints (Map Show as)) => Show (HList as)
deriving instance (ConcatConstraints (Map Eq as)) => Eq (HList as)

infixr 6 :+

instance KnownCoerce (HList xs) (HList '[]) where
  coerce _ = Nil

instance (KnownCoerce y x, KnownCoerce (HList ys) (HList xs)) => KnownCoerce (HList (y : ys)) (HList (x : xs)) where
  coerce (y :+ xs) = (coerce y) :+ (coerce xs)

-- fixme: implement type level sets

spec = do
  describe "pigs and objects" $ do
    it "coerces pigs and objects properly" $ do
      coerce Pig `shouldBe` Pig
      coerce Pig `shouldBe` Object

  describe "HList" $ do
    it "coerces HList properly" $ do
      coerce Nil                    `shouldBe` Nil
      coerce (Pig :+ Nil)           `shouldBe` Pig :+ Nil
      coerce (Pig :+ Nil)           `shouldBe` Object :+ Nil
      coerce (Object :+ Pig :+ Nil) `shouldBe` Object :+ Pig :+ Nil
      coerce (Pig :+ Pig :+ Nil)    `shouldBe` Object :+ Pig :+ Nil

    it "even when lengths are off" $ do
      coerce (Pig :+ Nil)           `shouldBe` Nil
      coerce (Pig :+ Pig :+ Nil)    `shouldBe` Nil
      coerce (Pig :+ Pig :+ Nil)    `shouldBe` Object :+ Nil

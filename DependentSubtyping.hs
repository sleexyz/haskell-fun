{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleContexts #-}

module Subtyping where

import Test.Hspec
import GHC.Types

type family Π = (r :: k -> *) | r -> k

class KnownCoerce (a :: k) (b :: k) where
  coerce :: Π a -> Π b

-- fixme: use tyfun

-- type family Map (f :: j -> k) (list :: [j]) where
--   Map _ '[] = '[]
--   Map f (x : xs) = f x ': Map f xs

-- type family ConcatConstraints (cs ::[Constraint]) :: Constraint where
--   ConcatConstraints '[] = ()
--   ConcatConstraints (x ': xs) = (x, ConcatConstraints xs)

-- * We can write our subtyping relation manually:

-- | Specify a universe of types:
data Thing = Object | Pig

-- | Define an interpretation
data SThing :: Thing -> * where
  SObject :: SThing Object
  SPig :: SThing Pig
deriving instance (Show (SThing a))
deriving instance (Eq (SThing a))
type instance Π = SThing

instance KnownCoerce (a :: Thing) (a :: Thing) where
  coerce = id

instance KnownCoerce Pig Object where
  coerce SPig = SObject

-- * Or we can derive our subtyping relation from the structure of our type:

data SList :: [k] -> * where
  SNil :: SList '[]
  SCons :: (Eq (Π a), Show (Π a)) => Π a -> SList xs -> SList (a ': xs)
-- deriving instance (ConcatConstraints (Map Show (Π as))) => Show (SList as)
-- deriving instance (ConcatConstraints (Map Eq (Π as))) => Eq (SList as)

deriving instance Show (SList a)
deriving instance Eq (SList a)

type instance Π = SList

type family x ∈ ys where
  x ∈ '[] = False
  x ∈ (x ': ys) = True
  x ∈ (y ': ys) = x ∈ ys

instance KnownCoerce ys '[] where
  coerce _ = SNil

instance (Eq (Π x), Show (Π x), KnownCoerce y x, KnownCoerce ys xs) => KnownCoerce (y : ys) (x : xs) where
  coerce (SCons y xs) = SCons (coerce y) (coerce xs)

spec = do
  describe "Thing" $ do
    it "coerces Thing properly" $ do
      let wilbur :: Π Pig
          wilbur = SPig

          wilburAsObject :: Π Object
          wilburAsObject = coerce wilbur

      show wilbur `shouldBe` "SPig"
      show wilburAsObject `shouldBe` "SObject"

  describe "List" $ do
    it "coerces List properly" $ do
      coerce SNil `shouldBe` SNil
      coerce (SCons SPig SNil) `shouldBe` SNil
      coerce (SCons SPig SNil) `shouldBe` SCons SPig SNil
      coerce (SCons SPig SNil) `shouldBe` SCons SObject SNil
      coerce ((SCons SObject (SCons SPig SNil))) `shouldBe` SCons SObject  (SCons SPig SNil)
      coerce ((SCons SPig (SCons SPig SNil))) `shouldBe` SCons SObject  (SCons SPig SNil)

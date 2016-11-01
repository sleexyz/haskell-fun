{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE ConstraintKinds #-}

module Subtyping where

import Test.Hspec
import GHC.Types

-- data a <: b = Wrap {unwrap :: a -> b}

-- data Vec1 = Vec1 { x :: Int }
-- data Vec2 = Vec2 { x :: Int, y :: Int }
-- data Vec3 = Vec3 { x :: Int, y :: Int, z :: Int }

-- v3tov2 :: Vec3 <: Vec2
-- v3tov2 = Wrap $ \Vec3{..} -> Vec2{..}

-- v3tov1 :: Vec3 <: Vec1
-- v3tov1 = Wrap $ \Vec3{..} -> Vec1{..}

-- v2tov1 :: Vec2 <: Vec1
-- v2tov1 = Wrap $ \Vec2{..} -> Vec1{..}

-- | Type-level function wrapper (TyFun)
data (~>) :: * -> * -> *
type family ($) (f :: (k ~> l) -> *) (a :: k) :: l

-- | Either define your poset manually

data Subtype :: (k -> *) -> (k -> k -> *) -> (k -> k -> Constraint) -> * where
  WrapCoerce :: (forall a b. (knownsub a b) => (sub a b) -> el a -> el b) -> Subtype el sub knownsub

unwrapCoerce ::
  Subtype el sub knownsub ->
  (forall a b. (knownsub a b) => (sub a b) -> el a -> el b)
unwrapCoerce (WrapCoerce x) = x


data Thing = Object | Animal | Pig | Pants

data Thing' :: Thing -> * where
  Object' :: Thing' Object
  Animal' :: Thing' Animal
  Pig' :: Thing' Pig
  Pants' :: Thing' Pants

data SubThing :: Thing -> Thing -> * where
  ThingId :: forall a. SubThing a a
  ThingCompose :: forall a b c. SubThing b c -> SubThing a b -> SubThing a c

class KnownSubThing (a :: Thing) (b :: Thing) where
  coerceThing :: SubThing a b -> Thing' a -> Thing' b

instance KnownSubThing Pig Pig where
  coerceThing _ Pig' = Pig'

subthings :: Subtype Thing' SubThing KnownSubThing
subthings = WrapCoerce coerceThing


asdf = (unwrapCoerce subthings) ThingId Pig'

spec = it "" $ do
  x <- return $ subthings
  shouldBe 1 1

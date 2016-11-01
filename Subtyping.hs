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
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE StandaloneDeriving #-}

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

type family Π (a :: k) = (r :: *) | r -> k

class (a :: k) <: (b :: k) where
  coerce :: Π a -> Π b


data Thing = Object | Animal | Pig | Pants
data Thing' :: Thing -> * where
  Object' :: Thing' Object
  Animal' :: Thing' Animal
  Pig' :: String -> Thing' Pig
  Pants' :: Thing' Pants
deriving instance (Show (Thing' t))
type instance Π (a :: Thing) = Thing' a


instance a <: a where
  coerce x = x

instance a <: Object where
  coerce _ = Object'

instance Pig <: Animal where
  coerce (Pig' st) = Animal'


spec = it "coerces properly" $ do
  let wilbur = Pig' "wilbur"
      wilburAsAnimal = coerce @Thing @Pig @Animal wilbur
  show wilbur `shouldBe` "Pig' \"wilbur\""
  show wilburAsAnimal `shouldBe` "Animal'"

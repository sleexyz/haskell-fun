{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilyDependencies #-}

-- | An experiment with Functor instances
--
-- We show that functors are morphisms of Cat.

module FunctorCategories where

import Data.Kind
import Prelude hiding (map, id, fmap, Functor, (.))
import qualified Prelude
import Test.Hspec
import Control.Arrow
import Control.Category
import GHC.Exts

-- | We use a Wrapped kind to store type information for a hom
data Wrapped k = Wrap (k -> k -> *) k k
type family Unwrap h = r | r -> h where Unwrap (Wrap hom a b) = hom a b
type family Source h where Source (Wrap hom a b) = a
type family Target h where Target (Wrap hom a b) = b

-- | A Functor is a hom in Cat
data Functor (homC :: Wrapped j) (homD :: Wrapped k) =
  Functor (Unwrap homC -> Unwrap homD)

-- | Cat is a Category!
instance Category Functor where
  id :: Functor homC homC
  id = Functor id

  (.) :: Functor homB homC -> Functor homA homB -> Functor homA homC
  (Functor f) . (Functor g) = Functor (f . g)

-- | Discharge a functor
mapWith :: Functor homC homD -> Unwrap homC -> Unwrap homD
mapWith (Functor map) = map

-- | We can recover good ol' fmap, this time polykinded
class HasFunctor f where
  canonicalFunctor ::
    Functor homC (Wrap target (f (Source homC)) (f (Target homC)))
  canonicalFunctor = Functor fmap

  fmap :: Unwrap homC -> target (f (Source homC)) (f (Target homC))
  fmap = mapWith canonicalFunctor

  {-# MINIMAL (fmap | canonicalFunctor) #-}


-- -- * Opposite functors

newtype Op cat b a = Op { unOp :: cat a b }

type family WrapOp h = r | r -> h where
  WrapOp (Wrap homC a b) = Wrap (Op homC) b a

instance Category cat => Category (Op cat) where
  id = Op id
  Op b . Op a = Op (a . b)

type (<--) a b = Op (->) a b

opF :: Functor (Wrap hom a b) (WrapOp (Wrap hom a b))
opF = Functor Op

-- * Kleisli lifts

-- fixme: write tests
liftKleisliF :: (Monad m) => Functor (Wrap (->) a b) (Wrap (Kleisli m) a b)
liftKleisliF = Functor (\f -> Kleisli (pure . f))


-- * Monads!

-- | A natural transformation is a morphism between Functors
-- type Nat homC homD = Functor homC homD -> Functor homC homD

-- class TrueMonad f = identityF ->


spec :: Spec
spec = do
  describe "Category" $ do
    it "works for identity functors" $ do
      pending
      let length' :: String -> Int
          length' = mapWith id length
      length' "Hello" `shouldBe` 5

    it "works for functors to opposite category" $ do
      let length' :: Int <-- String
          length' = mapWith opF length
      unOp length' "hello" `shouldBe` 5

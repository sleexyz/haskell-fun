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
{-# LANGUAGE ScopedTypeVariables #-}

-- | An experiment with a reformulation of
-- functors as parametrized on source and target categories.
--
-- We construct
-- - a formulation of Cat where Functors themselves are morphisms
--   - a true identity functor, i.e. without newtyping
--   - true functor composition
--
-- - a faithful recovering of Prelude.Functor, only this time category-agnostic

module FunctorCategories where

import Data.Kind
import Prelude hiding (map, id, fmap, Functor, (.))
import qualified Prelude
import Test.Hspec
import Control.Arrow
import Control.Category
import GHC.Exts
import Data.Proxy

-- | We use a Wrapped kind constructor to store type information for a morphism.
--
-- In particular, we wrap:
-- - The category, which is characterized by its hom set (in a Hask enriched category)
-- - The source object
-- - The target object
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

-- | We can recover good ol' Prelude.Functor, this time category-agnostic.
class HasFunctor f where
  canonicalFunctor ::
    Functor homC (Wrap target (f (Source homC)) (f (Target homC)))
  canonicalFunctor = Functor fmap

  fmap :: Unwrap homC -> target (f (Source homC)) (f (Target homC))
  fmap = mapWith canonicalFunctor

  {-# MINIMAL (fmap | canonicalFunctor) #-}

-- * Identity functor

-- | Proof by parametricity that the identity functor preserves identity on Hask for all a.
proofIdentityF :: forall a. a -> a
proofIdentityF = mapWith identityFunctor (id :: a -> a)
  where
    identityFunctor :: Functor (Wrap (->) a a) (Wrap (->) a a)
    identityFunctor = id

-- | Proof that identity is idempotent
proofIdentityFIdempotent :: forall a. a -> a
proofIdentityFIdempotent = mapWith (id . id . id) id

-- * Opposite functors

newtype Op cat b a = Op { unOp :: cat a b }

instance Category cat => Category (Op cat) where
  id = Op id
  Op b . Op a = Op (a . b)

type (<--) a b = Op (->) a b

opF :: Functor (Wrap hom b a) (Wrap (Op hom) a b)
opF = Functor Op


-- -- * Kleisli lifts
-- -- fixme: write tests
-- liftKleisliF :: (Monad m) => Functor (Wrap (->) a b) (Wrap (Kleisli m) a b)
-- liftKleisliF = Functor (\f -> Kleisli (pure . f))


-- * Functor Categories

-- | A natural transformation is a morphism between Functors
type Nat homC homD = Functor homC homD -> Functor homC homD

-- | covariant hom-functor in Hask-enriched category:
-- For some object a,
-- sends an object b from C to C(a, b)
covariantHom :: (Category homC) => Functor (Wrap homC b c) (Wrap (->) (homC a b) (homC a c))
covariantHom = Functor (<<<)

-- | contravariant hom-functor in Hask-enriched category:
-- For some object c,
-- sends an object b from C to C(b, c)
contravariantHom :: (Category homC) => Functor (Wrap homC a b) (Wrap (->) (homC b c) (homC a c))
contravariantHom = Functor (>>>)

-- yoneda ::
--   (((a -> b) -> f a -> f b) -> ((a -> b) -> g a -> g b)) ->
--   Functor 


-- yoneda functor = mapWith functor



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

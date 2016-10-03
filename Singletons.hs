{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Singletons where

import GHC.TypeLits
import GHC.Types
import Data.Proxy
import Test.Hspec


-- | Demote should be injective
type family Demote (k :: Type) = r | r -> k

type family TypeOf (a :: k) where TypeOf (a :: k) = k
reify :: forall a. Reflect a => Demote (TypeOf a)
reify = reflect (Proxy @a)

class Reflect (a :: k) where
  reflect :: Proxy a -> Demote k

type instance Demote () = ()
instance Reflect '() where
  reflect _ = ()

type instance Demote Symbol = String
instance (KnownSymbol s) => Reflect (s :: Symbol) where
  reflect p = symbolVal p

type instance Demote Nat = Integer
instance (KnownNat n) => Reflect (n :: Nat) where
  reflect p = natVal p

type instance Demote (Maybe k) = Maybe (Demote k)
instance (Reflect x) => Reflect (Just x :: Maybe a) where
  reflect _ = Just (reflect (Proxy @x))
instance Reflect (Nothing :: Maybe a) where
  reflect _ = Nothing

type instance Demote (Either j k) = Either (Demote j) (Demote k)
instance (Reflect x) => Reflect (Left x :: Either a b) where
  reflect _ = Left (reflect (Proxy @x))
instance (Reflect y) => Reflect (Right y :: Either a b) where
  reflect _ = Right (reflect (Proxy @y))

type instance Demote (a, b) = ((Demote a), (Demote b))
instance (Reflect x, Reflect y) => Reflect ('(x, y) :: (a, b)) where
  reflect _ = (reflect (Proxy @x), reflect (Proxy @y))

spec = do
  describe "reify" $ do
    it "works for Unit" $ do
      reify @'() `shouldBe` ()

    it "works for Symbol" $ do
      reify @"hello" `shouldBe` "hello"

    it "works for Maybe" $ do
      reify @(Just "hello") `shouldBe` Just "hello"
      reify @(Nothing :: Maybe Symbol) `shouldBe` Nothing
      reify @(Nothing :: Maybe (Maybe Symbol)) `shouldBe` Nothing

    it "works for Either" $ do
      reify @(Left "hello" :: Either Symbol Nat) `shouldBe` Left "hello"
      reify @(Right 123 :: Either Symbol Nat) `shouldBe` Right 123

    it "works for Tuples" $ do
      reify @'("hello", 123) `shouldBe` ("hello", 123)
      reify @'( '(123, "hello"), '()) `shouldBe` ((123, "hello"), ())

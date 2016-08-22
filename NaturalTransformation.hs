{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

import Control.Monad
-- infixl 1 &
-- (&) ∷ ∀a. a → (∀b. (a -> b) → b)
-- x & f = f x

infixl 5 <&>
(<&>) ∷ ∀f. (Functor f) ⇒ ∀a. f a → (∀b. (a -> b) → f b)
x <&> f = f <$> x

newtype Id a = Id {unId ∷ a}
  deriving Functor

-- A natural transformation has a component at all objects in the category
-- so forall a, we have a morphism from f a to g a

-- data f ≡> g = (Functor f, Functor g) ⇒ Nat {unNat ∷ (∀a. f a → g a)}
type (≡>) f g a = (∀a. f a → g a)

-- | if only we had impredicative polymorphism :p
-- type ((g :: * -> *) <<< (f :: * -> *)) = (forall a. g (f a))

newtype ((g ∷ * → *) ∘ (f :: * → *)) a = Chain {unChain ∷ (g (f a))}
  deriving (Functor, Show)


-- class Functor f ⇒ MMonad f where
--   eta ∷ (Id ≡> f)
--   mu ∷ (f ∘ f ≡> f)

class Functor f ⇒ MMonad f where
  eta ∷ (Id ≡> f) a
  mu ∷ (f ∘ f ≡> f) a

data List a = Nil | Cons a (List a)
  deriving (Functor, Foldable, Show)

instance Monoid (List a) where
  Nil `mappend` y = y
  (Cons x xs) `mappend` y = Cons x (xs `mappend` y)
  mempty = Nil


instance MMonad List where
  eta = (\x → Cons x Nil) . unId
  mu = foldl (mappend) Nil . unChain

join' = mu . Chain
pure' = eta. Id

instance (Functor f, MMonad f) ⇒ Applicative f where
  pure = pure'
  f <*> x = join' $ (<$>x) <$> f



instance MMonad f ⇒ Monad f where
  return = pure'
  x >>= f = join' (x <&> f)

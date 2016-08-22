{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE LambdaCase #-}

module Yoneda2 where


import Control.Comonad
import Control.Monad

infixr 9 ∘
(∘) = (.)


data a ≡ b = Isomorphism (a → b) (b → a)

-- class (Monad f, Monad g) ⇒ Iso f g where
--   to ∷ f a → g a
--   from ∷ g a → f a


-- The continuation operator

infixl 1 &
(&) ∷ ∀a.
      a
    → (∀b. (a → b) → b)
(&) x f = f x

toY ∷ ∀a. a → (∀ b. (a → b) → b)
toY = (&)

fromY ∷ ∀a. (∀ b. (a → b) → b) → a
fromY = (id&)

-- | we can't write the following b/c ghc doesn't support impredicative polymorphism, aka first-class polymorphism
-- iso ∷ Isomorphism a (∀ b. (a → b) → b)
-- iso = iso (&) (id&)


id' ∷ ∀a. a → a
id' x = fromY (toY x)

huh ∷ ∀a. (∀b. (a → b) → b) → (∀b. (a → b) → b)
huh x = toY (fromY x)



newtype Yoneda a = Yoneda (∀b. (a → b) → b)

lift ∷ a → Yoneda a
lift x = Yoneda (x&)

lower ∷ Yoneda a → a
lower (Yoneda x) = x id

iso ∷ a ≡ Yoneda a
iso = Isomorphism lift lower

id'' ∷ ∀a. a → a
id'' = lower . lift

id''' ∷ ∀a. Yoneda a → Yoneda a
id''' = lift . lower



instance Monad Yoneda where
  x >>= f
    = x
      & extract
      & f

instance Applicative Yoneda where
  pure = lift
  f <*> x
    = x
      & extract
      & (extract f)
      & pure

instance Functor Yoneda where
  fmap f x
    = x
    & extract
    & f
    & pure


instance Comonad Yoneda where
  extract = lower
  extend f x
    = x
      & f
      & pure



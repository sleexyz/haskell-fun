{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module DeriveApplicativeFromMonad where

import Control.Monad


infixl 1 &
(&) ∷ ∀a. a → (∀b. (a -> b) → b)
x & f = f x

infixl 5 <&>
(<&>) ∷ ∀f. (Functor f) ⇒ ∀a. f a → (∀b. (a -> b) → f b)
x <&> f = f <$> x

bar x y = join $ (flip $ (<$>) . (<&>)) x y


class Applicative' f where
  (<**>) :: f (a -> b) -> f a -> f b

instance Monad f => Applicative' f where
  f <**> x = join $ (<$>x) <$> f


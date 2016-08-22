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
{-# LANGUAGE PolyKinds #-}

import Control.Monad

infixl 8 &
(&) ∷ ∀a. a → (∀b. (a -> b) → b)
x & f = f x

infixl 5 <&>
(<&>) ∷ ∀f. (Functor f) ⇒ ∀a. f a → (∀b. (a -> b) → f b)
x <&> f = f <$> x

newtype Id a = Id {unId ∷ a}
  deriving Functor

class Trans (f ∷ * → *) (g ∷ * → *) where
  comp ∷ f ~> g

-- 2-D: morphisms in functor category
type (~>)
     (f ∷ * → *)
     (g ∷ * → *)
  = ∀(a ∷ *). f a → g a

-- type (>>>)
--      (f ∷ k)
--      (g ∷ k)
--   = (f → g)

newtype ((g ∷ * → *) ∘ (f :: * → *)) a = Chain {unChain ∷ (g (f a))}
  deriving (Functor, Show)

class Functor f ⇒ MMonad f where
  eta ∷ (Id ~> f)
  mu ∷ (f ∘ f ~> f)

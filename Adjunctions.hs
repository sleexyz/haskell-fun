{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE FlexibleContexts #-}

-- Here I show an isomorphism between the Hom isomorphism and the unit/counit
-- encodings of adjunctions
module Adjunctions where

-- * Hom isomorphism encoding

data Adjunction l r = MkAdjunction {
  to ∷ ∀a b. (Functor l, Functor r) ⇒ (l a → b) → (a → r b),
  from ∷ ∀a b. (Functor l, Functor r) ⇒ (a → r b) → (l a → b)
}

-- Unfortuntely I can't partially apply the type constructor out of order
-- so bear the "flip"
tensorHom ∷ ∀s. Adjunction ((,) s) ((→) s)
tensorHom = MkAdjunction { to, from }
  where
    to ∷ ∀a b. ((s, a) → b) → (a → s → b)
    to = flip . curry

    from ∷ ∀a b. (a → s → b) → ((s, a) → b)
    from = uncurry . flip

tensorHomLeftId ∷ ∀s a b. ((s, a) → b) → ((s, a) → b)
tensorHomLeftId = from . to
  where
    MkAdjunction { to, from } = tensorHom

tensorHomRightId ∷ ∀s a b. (a → s → b) → (a → s → b)
tensorHomRightId = to . from
  where
    MkAdjunction { to, from } = tensorHom


data Adjunction2 l r = MkAdjunction2 {
  unit ∷ ∀a. (Functor l, Functor r) ⇒ a → r (l a),
  counit ∷ ∀a. (Functor l, Functor r) ⇒ l (r a) → a
}

tensorHom2 ∷ ∀s. Adjunction2 ((,) s) ((→) s)
tensorHom2 = MkAdjunction2 { unit, counit }
  where
    unit ∷ ∀a. a → (s → (s, a))
    unit = flip (,)

    counit ∷ ∀a. (s, s → a) → a
    counit (x, f) = f x

convertTo ∷ ∀l r. Adjunction l r → Adjunction2 l r
convertTo MkAdjunction { to, from } = MkAdjunction2 { unit, counit }
  where
    unit ∷ ∀a. (Functor r, Functor l) ⇒ a → r (l a)
    unit = to id

    counit ∷ ∀a. (Functor r, Functor l) ⇒ (l (r a)) → a
    counit = from id

convertFrom ∷ ∀l r. Adjunction2 l r → Adjunction l r
convertFrom MkAdjunction2 { unit, counit } = MkAdjunction { to, from }
  where
    to ∷ ∀a b. (Functor r, Functor l) ⇒ (l a → b) → (a → r b)
    to f = fmap f . unit

    from ∷ ∀a b. (Functor r, Functor l) ⇒ (a → r b) → (l a → b)
    from g = counit . fmap g

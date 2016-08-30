{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Rank2Types #-}


module NotionOfComputationAsMonoids where

import Prelude hiding (Monoid)
import Control.Lens

-- MONOIDS!

infixl <>

class Monoid a where
  zero :: a
  (<>) :: a -> a -> a


data Endo a = Endo {getEndo :: a -> a}

instance (Monoid m) => (Monoid (m -> m)) where
  zero = id
  f <> g = f . g

instance Monoid [a] where
  zero = []
  (<>) = (++)

-- For every set a we can construct the monoid of endomorphisms
instance (Monoid m) => (Monoid (Endo m)) where
  zero = Endo id
  (Endo f) <> (Endo g) = Endo (f . g)



-- Cayley representation!

data Pair a b = Pair { rep :: a -> b, unrep :: b -> a }

cayleyMonoidPair :: (Monoid m) => Pair m (m -> m)
cayleyMonoidPair = Pair (<>) ($zero)

injectionIfIdentity :: Pair a b -> (a -> a)
injectionIfIdentity (Pair unrep rep) = rep . unrep


-- Example application of Cayley representation: difference lists

toDifferenceList :: [a] -> ([a] -> [a])
toDifferenceList = rep cayleyMonoidPair

fromDifferenceList :: ([a] -> [a]) -> [a]
fromDifferenceList = unrep cayleyMonoidPair

exampleDifferenceList :: String
exampleDifferenceList
  = ["hello"," my name is", " sean"]
    <&> toDifferenceList               -- we convert our Strings to their cayley representation
    & mconcat                          -- compose our endomorphisms
    & fromDifferenceList               -- and recover the list





-- Zoom ahead...
-- A Monad is a monoid  in the Monoidal category of endofunctor,
-- with endofunctor composition as tensor and
-- the identity functor as unit

-- Now we'll try to unpack the cayley representation of a monad

-- we get this: from yoneda somehow...

data Exp f g x = Exp (forall y. (x -> f y) -> g y)

-- Note: When f == g == id, then Exp is the Yoneda Lemma applied of x:
-- Yoneda Lemma (f a == (forall b. (a -> b) -> f b))
-- Corollary of Yoneda Lemma for Id functor: (a == (forall b. (a -> b) -> b))
-- which is the continuation
-- Exp composes f with the covariant hom functor from x


-- the derivation escapes me. I have to return from another time.


-- Exponential in endofunctor category
type Rep f = Exp f f

instance Monad (Rep f) where
  return x = Exp (\h -> h x)
  (Exp m) >>= f = Exp (\h -> m (\x -> let Exp t = f x in t h))

-- instance Applicative (Rep f) where
--   pure x = Exp (\h -> h x)


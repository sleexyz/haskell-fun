{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}

-- forget functions!
-- we're going to flip all our arrows

-- functions as dependent computations
-- time travelling

infixl 8 &
x & f = f x

infixl 5 <&>
x <&> f = f <$> x

-- readers of readers?

type Reader a = ((->) a)

cont :: a -> forall b. (a -> b) -> b
cont = (&)

type Eta f g = forall a. f a -> g a

yoneda :: (Functor f) => f a -> Eta (Reader a) f
yoneda = (<&>)


-- encode dependent shit
-- a
-- a -> b
-- a -> b -> c  = (reader a) x (reader b) $ c
-- (a -> b) -> c  = (reader (reader a b)) c
-- (a -> b) -> b = (reader (reader a b)) b

-- a = forall b. (reader (reader a b) b)
-- a reader is working in your world without something.

-- so reader a b expresses a computation of b with some dependency on a

-- whats a reader of a reader?

-- (reader (reader a b) b) expreses a computation of b with some dependency on some dependent computation of b

-- so I can do my computation of b while pretending I have a way to get b from a
-- I never ask for a.

-- forall b. (a -> b) -> b
-- I can do my computation of anything, while waiting on some computation of that thing which is waiting on a



-- a -> b
-- a computation of b waiting on a

-- a value of b (depending on an a)



-- function are too low-level


-- (a -> b) -> c : a computatation of c, waiting for a computation of b waiting for a
-- a -> b -> c : a computation of (a computation of c, waiting for b), waiting for a
-- (axb) -> c : a computation of c waiting for a, b



-- Flipping arrows to express dependencies

length' :: Int `DependingOn` String
length' = length


type DependingOn b a = a -> b

-- every a is isomorphic to

-- a computation of b depending on (a way to get from a to b)
cont'' :: a -> forall b. b `DependingOn` (a -> b)
cont'' = cont


-- encode dependency on a
-- (a, a -> b, a -> c)


-- under curry howard, (propositions as types, proofs as terms), we have
-- https://ncatlab.org/nlab/show/dependent+product

-- "generalization of the internal hom"

-- so if a function is a dependent computation,
-- a "dependent product" is a dependent proposition

type family B a where
  B Int = ()
  B String = ()

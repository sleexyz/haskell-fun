{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE TypeOperators #-}

module Representable where

class Functor f where
  map :: (a -> b) -> (f a -> f b)

type Y x = (->) x

instance Functor (Y x) where
  map :: (a -> b) -> Y x a -> Y x b
  map f x p = f (x p)

covariantHomFunctorIsFunctorial :: (a -> b) -> Y x a -> Y x b
covariantHomFunctorIsFunctorial = map


-- Yoneda says there is a isomorphism of hom sets
-- [C_op, Set](Y(a), F) ~= F(a)
--
-- where a is an object of C
-- where F is a presheaf
-- and Y is defined as the yoneda embedding, a functor
-- from C to its presheaf category [C_op, Set]
-- which maps a to Y(a), its contravariant hom functor
-- i.e Y(a) = Hom(-, a)


-- The contravariant hom functor is non computable, hence
-- from here we work with the covariant hom functor, and then the
-- covariant yoneda embedding
-- i.e Y(a) = Hom(a, -)
-- which is itself a contravariant functor
--
-- We can exploit duality to reformulate this corollary Yoneda's Lemma for
-- c
--
-- In other words,
-- [C_op, Set](Y(a), F) ~= F(a)
--
-- In other words,
-- [C, Set](F, Y(a)) ~= F(a)
--

-- A corollary of this "contravariant yoneda lemma" is that
-- the yoneda embedding is full and faithful.
--
-- If we take another hom functor, yoneda says
-- [C, Set](Y(b), Y(a)) ~= Y(a)(b)
--
-- In other words,
-- [C, Set](Y(b), Y(a)) ~= a -> b

-- Natural transformation
type (~>) f g = forall x. f x -> g x

yonedaFullFaithfulForwards :: (Y a b) -> (Y b ~> Y a)
yonedaFullFaithfulForwards f yb p = yb (f p)

yonedaFullFaithfulBackwards :: (Y b ~> Y a) -> Y a b
yonedaFullFaithfulBackwards nat x = nat (\x -> x) x

-- proof that yoneda embedding is fully faithful by parametricity
id' :: (a -> b) -> (a -> b)
id' f = yonedaFullFaithfulBackwards (yonedaFullFaithfulForwards f)

-- What does this translate in Haskell?
-- Every morphism in Hask maps contravariantly to
-- a category of reader monads by the contravariant yoneda embedding
toReaderNatTrans :: (a -> b) -> (forall x. (b -> x) -> (a -> x))
toReaderNatTrans = yonedaFullFaithfulForwards

fromReaderNatTrans :: (forall x. (b -> x) -> (a -> x)) -> (a -> b)
fromReaderNatTrans = yonedaFullFaithfulBackwards

-- Let's derive the cps transform

cpsIsh :: (() -> a) -> (forall x. (a -> x) -> () -> x)
cpsIsh = yonedaFullFaithfulForwards

cps :: a -> (forall x. (a -> x) -> x)
cps x f = yonedaFullFaithfulForwards (\_ -> x) f ()

-- and the inverse cps transform

uncpsish :: (forall x. (a -> x) -> () -> x) -> (() -> a)
uncpsish = yonedaFullFaithfulBackwards

uncps :: (forall x. (a -> x) -> x) -> a
uncps k = yonedaFullFaithfulBackwards (\f _ -> k f) ()


-- Let's prove the Yoneda lemma
-- [C_op, Set](Y(a), F) = F(a)
yonedaFwd :: (Functor f) => f a -> (forall x. (a -> x) -> f x)
yonedaFwd as f = map f as

yonedaBwd :: (Functor f) => (forall x. (a -> x) -> f x) -> f a
yonedaBwd nat = nat (\x -> x)

-- Proof of Yoneda's Lemma by parametricity
yonedaProof :: (Functor f) => f a -> f a
yonedaProof x =  yonedaBwd (yonedaFwd x)

data Identity a = MkIdentity {runIdentity :: a}
instance Functor Identity where
  map f (MkIdentity x) = MkIdentity (f x)

-- Derive the cps transform again!
-- The CPS transform is just the yoneda lemma applied to the Identity functor
cpsAgain :: a -> (forall x. (a -> x) -> x)
cpsAgain x f = runIdentity (yonedaFwd (MkIdentity x) f) 



-- We got to the CPS isomorphism two ways.
-- First was through applying the contravariant lemma to another (covariant) hom functor from Unit
-- Second was through apply the contravariant lemma to the identity functor
-- This suggests that the identity functor is isomorphic to Reader ()
identityIsoReaderUnitFwd :: (a -> b) -> (Y () a -> Y () b)
identityIsoReaderUnitFwd = map

identityIsoReaderUnitBwd :: (Y () a -> Y () b) -> (a -> b)
identityIsoReaderUnitBwd f x = f (\_ -> x) ()

identityIsoReaderUnitProof :: (a -> b) -> (a -> b)
identityIsoReaderUnitProof x = identityIsoReaderUnitBwd (identityIsoReaderUnitFwd x)

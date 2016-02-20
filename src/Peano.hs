{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}


module Peano
       where


 -- Heavily inspired from PeanoWitnesses and others
 -- https://hackage.haskell.org/package/PeanoWitnesses-0.1.0.0/docs/src/Data-Numeric-Witness-Peano.html
 -- http://stackoverflow.com/questions/19800664/is-it-possible-to-write-a-function-int-natsing-n-where-natsing-is-a-singleto
-- Here we explore a slew of language extensions:
-- DataKinds, Generalized Algebraic Datatypes, Type families, and ConstraintKinds!
--

-- ## STEP 1: Peano numbers
-- First we denote a kind Nat that includes our Peano numbers.
-- We do this via promoting a datatype into a datakind via the DataKinds extension



-- What does DataKinds give us?
-- DataKinds promotes our datatype from the type level to the kind level
-- and promotes its constructors from the term level to the type level


data Nat :: *     -- Nat is a type in this case, but DataKinds promotes Nat from a type to a kind itself
  where
    Zero :: Nat 
    Succ :: Nat -> Nat
deriving instance Show Nat -- standalone deriving syntax

data Nat' = Zero' | Succ' Nat' -- This is equivalent Haskell98 syntax. IMO, unclear, too sugary.
          deriving (Show)

-- So here,
-- Nat exists both as kind and a type (:: *)
-- And Zero exists both as a type (with kind Nat) and a term (with type Nat)
-- And Succ exists both as a type (with kind Nat -> Nat) and a term (with type Nat -> Nat)

-- Datakinds are Haskell's first step toward universe polymorphism!
-- Data constructors, normally polytypic function terms, get promoted polyuniversally
-- as polytypic families, ie



-- Step
-- Now we 


-- What's so Generalized about Generalized Algebraic Datatypes?
-- We can have constructors with richer return types!
-- (from
--       https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/data-type-extensions.html#gadt
-- )

-- Pattern matching on GADT's causes type refinement!
-- So we can define constructors with a different kind
--
-- ie.
-- -- NatSing is a higher-kinded type with kind Nat -> *
-- NatSing :: Nat -> *
--
-- -- ZeroSing is a constructor (Polytypic Function) with type NatSing 'Zero
-- -- Where 'Zero is the promoted 'Zero type
--
-- ZeroSing :: (NatSing `Zero` :: *)

data NatSing (n :: Nat) :: *
  where
    ZeroSing            :: (NatSing 'Zero                  :: *)
    SuccSing            :: NatSing n -> NatSing ('Succ n)  -- TODO: put a type on this!
deriving instance Show (NatSing n)

-- Notice that NatSing is unpromotable, as its a higher-kinded data constructor (with a promoted type!)
-- I.e. NatSing :: Nat -> *
-- Nat was promotable because Nat :: *
--
-- So ZeroSing exists monouniversally at the term level as a data constructor
-- And SuccSing exists monouniversally at the term level as a data constructor

-- reify terms from the aether

class ReifyNat n                             where reifyNat :: NatSing n
instance ReifyNat 'Zero                      where reifyNat = ZeroSing
instance (ReifyNat n) => ReifyNat ('Succ n)  where reifyNat = SuccSing (reifyNat :: NatSing n)

type family x + y where
  x + 'Zero = x
  x + 'Succ y = 'Succ (x + y)

type family x - y where
  x - 'Zero = x
  'Succ x - 'Succ y = x - y


type One = 'Succ 'Zero
type Two = 'Succ One
type Three = 'Succ Two
type Four = 'Succ Three

-- |ideal:
-- (n ::Int) -> Nat

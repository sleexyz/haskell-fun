{-# LANGUAGE DataKinds #-}
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

-- Heavily inspired from PeanoWitnesses and others
-- https://hackage.haskell.org/package/PeanoWitnesses-0.1.0.0/docs/src/Data-Numeric-Witness-Peano.html
-- http://stackoverflow.com/questions/19800664/is-it-possible-to-write-a-function-int-natsing-n-where-natsing-is-a-singleto

module Peano
       where

data Nat = Zero | Succ Nat

 -- TODO: figure out why I can't put a kind signature on Nat n

-- GADTs allow us return Nat Zeros from Nat n
-- GADT

data NatSing (n :: Nat) where
  ZeroSing :: NatSing 'Zero
  SuccSing :: NatSing n -> NatSing ('Succ n)
deriving instance Show (NatSing n)

-- reify terms from the aether

class ReifyNat n where
  reifyNat :: NatSing n
instance ReifyNat 'Zero where
  reifyNat = ZeroSing
instance (ReifyNat n) => ReifyNat ('Succ n)  where
  reifyNat = SuccSing (reifyNat :: NatSing n)

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

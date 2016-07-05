{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

-- A functor  takes a module and returns
-- ie. (Constraint -> Constraint)

-- Here we can get higher order functors!
-- ie. the moral equivalent (Constraint -> Constraint -> Constraint)
-- 
-- We make a Semiring out of our modules

module MLModules where

import Prelude hiding (Monoid, mempty)

class Monoid m where
  type T m
  (<>) ::  (?m :: m) => T m -> T m -> T m
  mempty ::  (?m :: m) => T m


data Add = Add
instance Monoid Add where
  type T Add = Int
  (<>) = (+)
  mempty = 0

data Mul = Mul
instance Monoid Mul where
  type T Mul = Int
  (<>) = (*)
  mempty = 1



class (Monoid (Additive m), Monoid (Multiplicative m)) => Semiring m where
  type Additive m
  proofAdd :: (?m :: m) => Additive m

  type Multiplicative m
  proofMul :: (?m :: m) => Multiplicative m

  plus :: (?m :: m) => T (Additive m) -> T (Additive m) -> T (Additive m)
  plus = (<>)
    where
      ?m = proofAdd

  mul :: (?m :: m) => T (Multiplicative m) -> T (Multiplicative m) -> T (Multiplicative m)
  mul = (<>)
    where
      ?m = proofMul

  zero :: (?m :: m) => T (Additive m)
  zero = (mempty)
    where
      ?m = proofAdd

  one :: (?m :: m) => T (Multiplicative m)
  one = (mempty)
    where
      ?m = proofMul



data Nat = Nat
instance Semiring Nat where
  type Additive Nat = Add
  proofAdd = Add where ?m = Nat

  type Multiplicative Nat = Mul
  proofMul = Mul where ?m = Nat




foo :: Int
foo = plus (mul 10 10) 100
  where
    ?m = Nat

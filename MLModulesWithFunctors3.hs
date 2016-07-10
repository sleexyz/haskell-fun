{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ConstrainedClassMethods #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

-- We can do better syntactically, with symbols...
-- I guess there's a tradeoff.
-- Now I need to be explicit that everything is a symbol.

module MLModulesWithFunctors2 where

import Prelude hiding (Monoid, mempty)
import GHC.TypeLits


data M (x :: Symbol) = M

class Monoid (m :: Symbol) where
  type T (m :: Symbol) :: *
  (<>) ::  (?m :: M m) => T m -> T m -> T m
  mempty ::  (?m :: M m) => T m

instance Monoid "Add" where
  type T "Add" = Int
  (<>) = (+)
  mempty = 0

instance Monoid "Mul" where
  type T "Mul" = Int
  (<>) = (*)
  mempty = 1



class (Monoid (Additive m), Monoid (Multiplicative m)) => Semiring (m :: Symbol) where
  type Additive m  :: Symbol
  type Multiplicative m :: Symbol

  plus :: (?m :: M m) => T (Additive m) -> T (Additive m) -> T (Additive m)
  plus = (<>)
    where ?m = M @ (Additive m)

  mul :: (?m :: M m) => T (Multiplicative m) -> T (Multiplicative m) -> T (Multiplicative m)
  mul = (<>)
    where ?m = M @ (Multiplicative m)

  zero :: (?m :: M m) => T (Additive m)
  zero = mempty
    where ?m = M @ (Additive m)

  one :: (?m :: M m) => T (Multiplicative m)
  one = mempty
    where ?m = M @ (Multiplicative m)



instance Semiring "Natural" where
  type Additive "Natural" = "Add"
  type Multiplicative "Natural" = "Mul"




foo :: Int
foo = plus (mul 10 10) 100
  where
    ?m = M @ "Natural"

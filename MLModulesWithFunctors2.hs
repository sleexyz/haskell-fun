{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ConstrainedClassMethods #-}
{-# LANGUAGE OverloadedStrings #-}

-- We can do better syntactically!

module MLModulesWithFunctors2 where

import Prelude hiding (Monoid, mempty)
import GHC.TypeLits

class Module m where
  proof :: m

data M (x :: Symbol) = M
instance Module (M  a) where proof = M

class Module m => Monoid m where
  type T m :: *
  (<>) ::  (?m :: m) => T m -> T m -> T m
  mempty ::  (?m :: m) => T m

instance Monoid (M "Add") where
  type T (M "Add") = Int
  (<>) = (+)
  mempty = 0

instance Monoid (M "Mul") where
  type T (M "Mul") = Int
  (<>) = (*)
  mempty = 1



class (Monoid (Additive m), Monoid (Multiplicative m)) => Semiring m where
  type Additive m
  type Multiplicative m

  plus :: (?m :: m) => T (Additive m) -> T (Additive m) -> T (Additive m)
  plus = (<>)
    where
      ?m = proof :: Additive m

  mul :: (?m :: m) => T (Multiplicative m) -> T (Multiplicative m) -> T (Multiplicative m)
  mul = (<>)
    where
      ?m = proof :: Multiplicative m

  zero :: (?m :: m) => T (Additive m)
  zero = (mempty)
    where
      ?m = proof :: Additive m

  one :: (?m :: m) => T (Multiplicative m)
  one = (mempty)
    where
      ?m = proof :: Multiplicative m



instance Semiring (M "Natural")where
  type Additive (M "Natural") = M "Add"
  type Multiplicative (M "Natural") = M "Mul"




foo :: Int
foo = plus (mul 10 10) 100
  where
    ?m = M @ "Natural"

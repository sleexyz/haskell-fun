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
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}


 {-|
 = Peano numbers!

We explore a slew of language extensions with the goal of creating type-level natural numbers.

* Datatype promotion via __<https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/promotion.html DataKinds>__
* Generalized algebraic datatypes via __<https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/data-type-extensions.html#gadt-style GADTs>__
* Type-level functions via __<https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/type-families.html TypeFamilies>__
* Constraint kinds via __<https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/type-families.html ConstraintKinds>__

-}

module Peano where

{- $step1
== STEP 1: Peano number types

How would you make Peano number /term/?

/Before we can make a term-level natural, we need to make a type-level set of all naturals, aka a type.../

...

How would you make a Peano number /type/?

/Before we can make a type-level natural, we need to make a kind-level set of all naturals, aka a kind.../

...

So we make a Peano number /type/ by promoting a datatype of naturals from the type-level to the kind-level, via the __DataKinds__ extension.

-}

{-|

/via data promotion:/


__Nat__ exists both as a __kind__ and a __type__ /(of kind *)/

__Zero__ exists both as a __type__ /(of kind Nat)/ and a __term__ /(of type Nat)/

__Succ__ exists both as a __type__ /(of kind Nat)/ and a __term__ /(of type Nat)/
-}
data Nat where
  Zero :: Nat
  Succ :: Nat -> Nat
deriving instance Show Nat


{- $after

=== GADT syntax:
GADT syntax:

@
data Nat where
  Zero :: Nat
  Succ :: Nat -> Nat
deriving instance Show Nat
@

Haskell98 syntax:

@
data Nat = Zero | Succ Nat
         deriving (Show)
@

In this case, we don't necessitate the use of GADT syntax, but IMO GADT syntax is just clearer. Type signatures clarify everything! Haskell98 datatype syntax is just too sugary.

=== Inter-universal constructs:
DataKinds is an example of an /inter-universal/ construct in Haskell. (Or /universe-polymorphic/, if you prefer)

==== Single-kinded datatypes become single-sorted datakinds

* A datatype gets promoted to a datakind
  * ie @Nat :: *@ becomes @Nat :: '*@, where @'*@ is psuedo-haskell for Kind, ie. the sort of all kinds, ie. the type of all types of all types, which must exist at a universe level higher than kinds.
* A term of a datatype gets promoted to type of datakind

==== Higher-kinded datatypes become higher-sorted datakinds

* A higher-kinded datatype gets promoted to a higher-sorted datakind
* A polytypic datatype constructor (of that higher-kinded datatype) gets promoted to a polykinded datakind constructor (of that higher-sorted datakind)

-}


{- $step2
== STEP 2: Create singleton types for proxied monotypic reification
So now we have a Nat kind, along with Zero and Succ types.

But currently they are inaccessible; Zero and Succ are empty types, in the sense that they contain no elements. I might be wrong here, but they are empty by necessity, since we created them out of thin air (ie. from the kind-level downwards, instead of from terms upwards).

How do we reify (to the term level) these empty types? For all I know, we can't, because we created them from thin air.

We can't reify @'Zero :: Nat@ or @'Succ 'Zero :: 'Nat@, but we can reify

So we do exactly that, via a GADT that takes a type of kind Nat as a type parameter
These singleton instances are terms that "hang" from the type level
-}


data NatSing (n :: Nat) :: *
  where
    ZeroSing            :: (NatSing 'Zero                  :: *)
    SuccSing            :: NatSing n -> NatSing ('Succ n)  -- TODO: put a kind on this!
deriving instance Show (NatSing n)

-- Notice that NatSing is unpromotable, as its a higher-kinded data constructor (with a promoted type!)
-- I.e. NatSing :: Nat -> *
-- So NatSing exists monouniversally at the type level
--
-- And ZeroSing exists monouniversally at the term level as a data constructor
-- And SuccSing exists monouniversally at the term level as a data constructor


-- ## STEP 3: Polytypic reification
-- Right now we have monotypic singleton terms that hang from Nat types from a thread.
-- We can create a polytypic term, that given a type, will return the appropriate term.
-- In essence, we create the equivalent of a *dependent* function;
-- Given a type, we return an appropriate term.
-- Given a Natural number at the type level, we return a natural number at the term level.


class ReifiableNat (n :: Nat)                            where reifyNat :: NatSing n
instance ReifiableNat 'Zero                      where reifyNat = ZeroSing
instance (ReifiableNat n) => ReifiableNat ('Succ n)  where reifyNat = SuccSing (reifyNat :: NatSing n)

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

{- $references
== References

* https://hackage.haskell.org/package/PeanoWitnesses-0.1.0.0/docs/src/Data-Numeric-Witness-Peano.html
* http://stackoverflow.com/questions/19800664/is-it-possible-to-write-a-function-int-natsing-n-where-natsing-is-a-singleto

-}

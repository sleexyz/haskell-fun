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
  * ie @Nat :: *@ becomes @Nat :: '*@, where @'*@ is psuedo-haskell for Kind, ie. the "sort" of all kinds, ie. the type of all types of all types, which must exist at a universe level higher than kinds.
* A term of a datatype gets promoted to type of datakind

==== Higher-kinded datatypes become higher-sorted datakinds

* A higher-kinded datatype gets promoted to a higher-sorted datakind
* A polytypic datatype constructor (of that higher-kinded datatype) gets promoted to a polykinded datakind constructor (of that higher-sorted datakind)

-}


{- $step2
== STEP 2: Create singleton types for proxied monotypic reification
So now we have a Nat kind, along with 'Zero and 'Succ types.

But currently they are inaccessible; 'Zero and 'Succ are empty types, in the sense that they contain no elements. I might be wrong here, but they are empty by necessity, since we created them out of thin air (ie. from the kind-level downwards, instead of from terms upwards).

How do we reify (to the term level) these empty types? For all I know, we can't, because we created them from thin air.

We can't reify @'Zero :: Nat@ or @'Succ 'Zero :: Nat@. However, we can reify a higher-kinded "proxy" type that is /parametrized/ on Nat. We call it a proxy type, because we aren't reifying to terms a reified @Nat@, i.e. reifying @'Zero@, but actually reifying to terms a reified @Nat -> *@, ie. reifying @'NatSing 'Zero@

We also call these proxy types "singleton" types, because they have a single inhabitant, as defined by our data constructors.
-}


data                       NatSing (n :: Nat)              :: *
  where
    ZeroSing            :: (NatSing 'Zero                  :: *)
    SuccSing            :: NatSing n -> NatSing ('Succ n)  -- TODO: put a kind on this!
deriving instance Show (NatSing n)

{- $notice
=== What's so GADTy about GADTs?
Generalized Algebraic types are generalized because they allow constructor terms with differently kinded types.

@
             NatSing                         :: Nat -> *
ZeroSing :: (NatSing 'Zero                   :: *)
SuccSing :: (NatSing n -> NatSing ('Succ n)  :: Nat -> *)
@
Notice how we were allowed to refine types from kind @Nat -> *@ to kind @*@ in the case of ZeroSing.

=== Is NatSing promotable?
Notice that NatSing is unpromotable, as it's a higher-kinded data constructor. Also, it is parametrized with a promoted type! So if NatSing was to be promoted to the kind level, then Nat would have to be promoted to the Sort level (one level above Kind). As of GHC 7.10, datakinds only promote to one level higher.

i.e. @NatSing :: Nat -> *@

* So NatSing exists at the type level as a higher kinded type
* And ZeroSing exists at the term level as a nullary function term
* And SuccSing exists at the term level as a polytypic unary function term
-}


{- $step3

== STEP 3: Polytypic reification
Right now we have monotypic singleton terms that are serve as proxied reified @Nat@s.
One cool thing we can do is create a polytypic term, that given a type (monomorphised), will return the appropriate monotypic term.


-}

class ReifiableNat (n :: Nat)                            where reifyNat :: NatSing n
instance ReifiableNat 'Zero                      where reifyNat = ZeroSing
instance (ReifiableNat n) => ReifiableNat ('Succ n)  where reifyNat = SuccSing (reifyNat :: NatSing n)

{- $step3cont

With a typeclass, we can create a polytypic function over our Nat types. We define that polytypic function as @reifyNat@, which is implemented for all @ReifiableNat@s.

* We make @'Zero@ a ReifiableNat, by implementing reifyNat, by returning @ZeroSing@, the proxied singleton term.
* We make @'Succ n@ a ReifiableNat given than @n@ is a ReifiableNat, by implementing @reifyNat@, by returning @SuccSing@ of @reifyNat@ of type @NatSing n@


What's the big deal? In essence, we've created the equivalent of a /dependent/ function:

* Given a type, we return a monotypic term.
* Given a natural number at the type level, we return a natural number at the term level.

-}

type family x + y :: Nat where
  x + 'Zero = x
  x + 'Succ y = 'Succ (x + y)

type family x - y :: Nat where
  x - 'Zero = x
  'Succ x - 'Succ y = x - y


type One =   ('Succ 'Zero  :: Nat)
type Two =   ('Succ One    :: Nat)
type Three = ('Succ Two    :: Nat)
type Four =  ('Succ Three  :: Nat)

-- |ideal:
-- (n ::Int) -> Nat

{- $references
== References

* https://hackage.haskell.org/package/PeanoWitnesses-0.1.0.0/docs/src/Data-Numeric-Witness-Peano.html
* http://stackoverflow.com/questions/19800664/is-it-possible-to-write-a-function-int-natsing-n-where-natsing-is-a-singleto

-}

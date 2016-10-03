{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ConstraintKinds #-}

-- | Usage:
-- f :: HList '[b, c] -> d
-- genCurry f :: b -> c -> d
--
module Variadic where

import GHC.TypeLits hiding ((*))
import GHC.Exts (Constraint)
import Data.Proxy

-- | Arbitrarily sized products encoded in a GADT.
-- aka an Hlist
data HList (k :: [*]) where
  Nil :: HList '[]
  (:+) :: x -> HList xs -> HList (x ':xs)

-- | Generalized curry for arbitrary sized products
class Curryable k o where
  type Curried k o
  genCurry :: (HList k -> o) -> Curried k o

-- | base case
instance Curryable '[] o where
  type Curried '[] o = o
  genCurry f = f Nil

-- | inductive case
instance (Curryable as o) => Curryable (a ': as) o where
  type Curried (a ': as) o = (a -> Curried as o)
  genCurry f head = genCurry g
    where
      g tail = f (head :+ tail)


type family ConstrainAll constraint list :: Constraint where
  ConstrainAll constraint '[] = ()
  ConstrainAll constraint (x ': xs) = (constraint x, ConstrainAll constraint xs)

homogenize :: forall b constraint list.
  (ConstrainAll constraint list) =>
  Proxy constraint ->
  (forall a. constraint a => a -> b) ->
  HList list ->
  [b]
homogenize _ f Nil = []
homogenize proxy f (head :+ tail) = f head : (homogenize proxy f tail)

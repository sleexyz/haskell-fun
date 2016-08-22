{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE InstanceSigs #-}

import GHC.Types
import Data.Proxy
import GHC.TypeLits

-- 1-D: morphisms in category
type (-->)
     (a :: *)
     (b :: *)
  = a -> b -- 2d

-- 2-D: morphisms in functor category
type (~>)
     (f :: * -> *)
     (g :: * -> *)
  = forall (a :: *). f a -> g a

-- 3-D: morphisms in natural transformation category
type (~~>)
     (s :: (* -> *) -> (* -> *))
     (t :: (* -> *) -> (* -> *))
  = (forall (f :: * -> *). s f ~> t f)


data N = Z | S N

data Morph (n :: N) a where
  Lift :: a -> Morph Z a
  Morph :: (Morph n a -> Morph n b) -> Morph (S n) (a -> b)

unLift :: Morph Z a -> a
unLift (Lift x) = x

unMorph :: Morph (S n) (a -> b) -> Morph (n) a -> Morph n b
unMorph (Morph f) = f

class ReifyMorph n where
  type Pure n a
  reify :: Morph n a -> Pure n a
  abstract :: Pure n a -> Morph n a


instance ReifyMorph Z where
  type Pure Z a = a
  reify (Lift x) = x
  abstract x = Lift x

instance ReifyMorph n => ReifyMorph (S n) where
  type Pure (S n) (a -> b) = Pure n a -> Pure n a

  reify (Morph f) = reify . f . abstract
  abstract f = Morph $ abstract . f . reify

foo :: Morph Z Int
foo = Lift 1


bar :: Morph (S Z) (String -> Int)
bar = Morph (Lift . (length). unLift)

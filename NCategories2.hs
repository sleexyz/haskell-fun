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
{-# LANGUAGE RankNTypes #-}

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

data Morph (n :: N) f g where
  Lift :: (forall a. f a -> g a) -> Morph Z f g
  Morph :: (Morph n f g -> Morph n f g) -> Morph (S n) f g


class ReifyMorph n where
  type Pure n (f :: * -> *) (g :: * -> *)
  -- reify :: Morph n f g -> (forall a. Pure n f g a -> Pure n f g a)
  reify :: Morph n f g -> Pure n f g
  abstract :: Pure n f g -> Morph n f g


instance ReifyMorph Z where
  type Pure Z f g = f ~> g
  reify (Lift x) = x
  abstract x = Lift x

-- instance ReifyMorph n => ReifyMorph (S n) where
--   type Pure (S n) f g a = Pure n f g a -> Pure n f g a
--   reify (Morph f) = \x -> f  reify x

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
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeInType #-}

import GHC.Types
import Data.Proxy
import GHC.TypeLits

-- -- 1-D: morphisms in category
-- type (-->)
--      (a :: *)
--      (b :: *)
--   = a -> b -- 2d

-- -- 2-D: morphisms in functor category
-- type (~>)
--      (f :: * -> *)
--      (g :: * -> *)
--   = forall (a :: *). f a -> g a

-- -- 3-D: morphisms in natural transformation category
-- type (~~>)
--      (s :: (* -> *) -> (* -> *))
--      (t :: (* -> *) -> (* -> *))
--   = (forall (f :: * -> *). s f ~> t f)


data N = Z | S N

type family Morph_ (n :: N) where
  Morph_ Z = Type
  Morph_ (S n) =  Morph_ n -> Morph_ n


type family Nat2N (n :: Nat) where
  Nat2N 0 = Z
  Nat2N n = S (Nat2N (n - 1))

type Morph (n :: Nat) = Morph_ (Nat2N n)


-- 1-D: morphisms in category
type (-->)
     (a :: Morph 0)
     (b :: Morph 0)
  = a -> b -- 2d

-- 2-D: morphisms in functor category
type (~>)
     (f :: Morph 1)
     (g :: Morph 1)
  = forall (a :: Morph 0). f a -> g a

-- -- 3-D: morphisms in natural transformation category
type (~~>)
     (s :: Morph 2)
     (t :: Morph 2)
  = (forall (f :: Morph 1). s f ~> t f)

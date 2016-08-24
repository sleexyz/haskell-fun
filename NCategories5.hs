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
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FunctionalDependencies #-}

import Prelude hiding (map)
import GHC.Types
import Data.Proxy
import GHC.TypeLits

data N = Z | S N

type family Morph_ (n :: N) where
  Morph_ Z = Type
  Morph_ (S n) =  Morph_ n -> Morph_ n


type family Nat2N (n :: Nat) where
  Nat2N 0 = Z
  Nat2N n = S (Nat2N (n - 1))

type Morph (n :: Nat) = Morph_ (Nat2N n)


-- Hom in a category
data family Hom (k :: Type) (f :: k) (g :: k)

-- Homs of Hask are functions terms
data instance Hom Type a b = Function { unFunction :: a -> b }
data instance Hom (k -> k) f g = Extend { unExtend :: forall (a :: k). Hom k (f a) (g a) }

-- Homs of [Hask -> Hask] are natural transformations
-- data instance Hom (Morph 1 Type) f g = Extend { unExtend :: forall (a :: Morph 0 Type). f a -> g a }
-- data instance Hom (Morph 2 Type) f g = Extend { unExtend :: forall (a :: Morph 1 Type). f a -> g a }



class Functor' (s :: Type) (t :: Type) (f :: s -> t) where
  map :: forall (a :: s) (b :: s). (Hom s a b) -> (Hom t (f a) (f b))

instance Functor f => Functor' Type Type f where
  map = Function . fmap . unFunction


class Natural (f :: Type -> Type) (t :: (Type -> Type) -> (Type -> Type)) where
  ffmap :: (f a -> t f a)

data Compose (g :: Type -> Type) (f :: Type -> Type) a = Compose { reifyCompose :: g (f a) }

instance Functor f => Functor (Compose f f) where
  fmap f (Compose a) = Compose $ (fmap . fmap) f a

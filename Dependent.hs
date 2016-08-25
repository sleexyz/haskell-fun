
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}


import Data.Kind
import Data.Proxy

data N = Z | S N

data Equal (k :: Type)  (a :: k) (b :: k) where
  Refl :: forall (k :: Type) (a :: k). Equal k a a

type family (a :: N) + (b :: N)  where
  Z + b = b
  (S a) + b = S (a + b)

type family Sym (k :: Type) (eq :: Equal k a b) :: (Equal k b a) where
  Sym k (Refl :: Equal k a a) = Refl

-- type family Cong (k :: Type) (s :: Type) (f :: k -> s) (a :: k) (b :: k)
--   (eq :: Equal k a b) :: Equal s (f a) (f b)
--   where
--     Cong f a b refl = refl



type family ZeroZero :: (Equal N (Z + Z) Z) where
  ZeroZero = Refl


type family ZeroPlus (a :: N) :: (Equal N (Z + a) a) where
  ZeroPlus Z = Refl
  ZeroPlus (S n) = Refl
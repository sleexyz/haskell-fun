{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}

module VectorTest where

import Data.Proxy
import Data.Kind
import Data.Type.Equality
import Unsafe.Coerce


-- type family Sing (p :: (k -> *)) = r | r -> k
-- type Π = Sing

-- type family Sing (p :: (k -> *)) = r | r -> k
-- type Π = Sing

-- data Π (k :: *) :: (k -> *) -> * where
--   V :: k


-- * Nat

data Nat :: * where
  Zero :: Nat
  Succ :: Nat -> Nat

instance Num Nat where
  (+) = error "crap"
  (*) = error "crap"
  abs = error "crap"
  signum = error "crap"
  negate = error "crap"
  fromInteger 0 = Zero
  fromInteger x = Succ (fromInteger (x - 1))

-- plus_succ :: Proxy n -> Proxy m -> (n + Succ m) :~: (Succ n + m)
-- plus_succ _ _ = _




data SNat :: Nat -> * where
  SZero :: SNat Zero
  SSucc :: SNat n -> SNat (Succ n)
deriving instance Show (SNat n)

data Exists (k :: *) :: (k -> *) -> * where
  Pack :: forall (p :: k -> *) (a :: k). p a -> Exists k p
deriving instance Show (Exists Nat SNat)
deriving instance (Show a) => Show (Exists Nat (Vec a))

-- deriving instance (Show (p a)) => Show (Exists n p)


type family Foo :: (* -> Constraint) -> Constraint

modify :: forall k (p :: k -> *) (f :: k -> k).
  (forall a. p a -> p (f a)) ->
  Exists k p -> Exists k p
modify f (Pack x) = (Pack (f x))

-- | index-preserving modification
modify' :: forall k (p :: k -> *).
  (forall a. p a -> p a) ->
  Exists k p -> Exists k p
modify' f (Pack x) = (Pack (f x))


refineNat :: Nat -> Exists Nat SNat
refineNat Zero = Pack SZero
refineNat (Succ n) = modify SSucc (refineNat n)

data Vec :: * -> Nat -> * where
  Nil :: Vec a Zero
  (:.) :: a -> Vec a n -> Vec a (Succ n)
infixr 6 :.
deriving instance (Show a) => Show (Vec a n)

refineList :: [a] -> Exists Nat (Vec a)
refineList [] = Pack Nil
refineList (x : xs) = modify (x :.) (refineList xs)

type family (n :: Nat) + (m :: Nat) where
  Zero + y = y
  (Succ x) + y = Succ (x + y)

concatVec :: Vec a n -> Vec a m -> Vec a (n + m)
concatVec Nil ys = ys
concatVec (x :. xs) ys = x :. (concatVec xs ys)


-- fixme: make safe
reverseVec :: Vec a n -> Vec a n
reverseVec x = unsafeCoerce $ reverseVecAux x Nil

-- fixme: make safe
reverseVecAux :: forall a n m. Vec a n ->  Vec a m -> Vec a (n + m)
reverseVecAux Nil ys = ys
reverseVecAux (x :. xs) ys = unsafeCoerce $ reverseVecAux xs (x :. ys)



-- * test

test :: (Show a) => String -> a -> IO ()
test str x = putStrLn (" ∘ " ++ str ++ "\n") >> print x >> putStrLn "\n"

main :: IO ()
main = do
  test "refineNat:" $ refineNat 42
  test "refineList:" $ refineList [1, 2, 3, 4, 5]

  test "concatVec: (with Nil)" $ modify' (concatVec Nil) (refineList [1, 2, 3, 4, 5])
  test "concatVec: (with (1 :. Nil))" $ modify (concatVec (1 :. Nil)) (refineList [1, 2, 3, 4, 5])

  test "reverseVec:" $ modify' reverseVec (refineList [1, 2, 3, 4, 5])

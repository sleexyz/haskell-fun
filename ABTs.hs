{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Dependently-typed type-safe ABTs
-- See PFPL, Chapter 1
-- Defunctionalization idea taken from Vinyl
module ASTs where

import Test.Hspec
import GHC.Types
import qualified GHC.TypeLits as TypeLits

-- * Some dependent type stuff:

-- | Type-level function wrapper (TyFun)
data (~>) :: * -> * -> *
type family (f :: (k ~> l) -> *) $ (a :: k) :: l

-- | Type level map
type family (f :: (k ~> l) -> *) <$> (xs :: [k]) :: [l] where
  f <$> '[] = '[]
  f <$> (x ': xs) = ((f $ x) ': (f <$> xs))

-- | HList
data Vec :: [*] -> * where
  Nil :: Vec '[]
  (:+) :: x -> Vec xs -> Vec (x ': xs)
infixr 6 :+

type family (xs :: [k]) ++ (ys :: [k]) :: [k] where
  '[] ++ ys = ys
  (x ': xs) ++ ys = x ': (xs ++ ys)

-- fixme: implement type error
type family xs !! n where
  (x ': xs) !! 0 = x
  (_ ': xs) !! x = xs !! (x TypeLits.- 1)


data V (i :: Nat)  = V

data Valence k = [k] :. k

data ABT k (fv :: [k]) (sort :: k) where
  Prim :: (Lang k) => Primitive k sort -> ABT k fv sort
  Op :: (Lang k) => Operator k i o -> Vec ((Foo k fv) <$> i) -> ABT k fv o

  -- fixme : implement constrant on n and fv
  Var :: (Lang k) => V n -> ABT k fv (fv !! n)

data Foo k (fv :: [k]) :: Valence k ~> * -> *
type instance Foo k fv $ (v :. o) = ABT k (v ++ fv) o

class Lang k where
  type Primitive k = (r :: k -> *) | r -> k
  type Operator k = (r :: [Valence k] -> k -> *) | r -> k

-- | A set of sorts
data Arith = Number
data ArithV a where
  NumberV :: Int -> ArithV Number
data ArithOp i o where
  Plus :: ArithOp '[ '[] :. Number,  '[] :. Number] Number
  Let :: ArithOp '[ '[] :. Number, '[Number] :. Number] Number

instance Lang Arith where
  type Primitive Arith = ArithV
  type Operator Arith = ArithOp



-- infixr 1 :\


spec :: Spec
spec = do
  describe "ASTs" $ do
    let
      x :: ABT Arith '[] Number
      x = Op Plus (Prim (NumberV 2) :+ Prim (NumberV 3) :+ Nil)
      -- x = Op Let (NumberV 1) (Op Plus ((V @0) :+ Prim (NumberV 2) :+ Nil))

    it "works" $ do
      1 `shouldBe` 1

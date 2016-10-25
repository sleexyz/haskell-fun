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

-- | Typed ABTs
-- Implemented via type-level DeBrujin indices
-- Defunctionalization idea taken from Vinyl
module ASTs where

import Test.Hspec
import GHC.Types
import qualified GHC.TypeLits as TypeLits -- fixme: unqualify

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
  (:+) :: !x -> !(Vec xs) -> Vec (x ': xs)
infixr 6 :+

type family (xs :: [k]) ++ (ys :: [k]) :: [k] where
  '[] ++ ys = ys
  (x ': xs) ++ ys = x ': (xs ++ ys)

-- fixme: implement type error
type family xs !! n where
  (x ': xs) !! 0 = x
  (_ ': xs) !! x = xs !! (x TypeLits.- 1)
  '[] !! x = TypeLits.TypeError (TypeLits.Text "Invalid index.")

type family Length (xs :: [k]) where
  Length '[] = 0
  Length (x ': xs) = 1 TypeLits.+ Length xs


data V (i :: Nat)  = V

data Valence k = [k] :. k

-- fixme: rename to u
data ABT
  k
  (primitive :: k -> *)
  (operator :: [Valence k] -> k -> *)
  (fv :: [k])
  (sort :: k) where
  Prim ::
    primitive sort ->
    ABT k primitive operator fv sort
  Op ::
    operator i o ->
    Vec (Foo k primitive operator fv <$> i) ->
    ABT k primitive operator fv o
  Var :: (n TypeLits.<= Length fv) =>
    V n ->
    ABT k primitive operator fv (fv !! n)

data Foo k primitive operator (fv :: [k]) :: Valence k ~> * -> *
type instance Foo k primitive operator fv $ (v :. o) = ABT k primitive operator (v ++ fv) o


-- * Example
data ArithU = Number

data ArithV a where
  NumberV :: Int -> ArithV Number

data ArithOp i o where
  Plus :: ArithOp '[ '[] :. Number,  '[] :. Number] Number
  Let :: ArithOp '[ '[] :. Number, '[Number] :. Number] Number

type Arith = ABT ArithU ArithV ArithOp

-- * Spec

spec :: Spec
spec = do
  describe "ASTs" $ do
    let
      x :: Arith '[] Number
      x = Op Plus $ Prim (NumberV 4) :+ Prim (NumberV 2) :+ Nil

      y :: Arith '[Number] Number
      y = Var (V @0)

      z :: Arith '[] Number
      z = Op Let $ Prim (NumberV 2) :+ Var (V @0) :+ Nil

    it "works" $ do
      1 `shouldBe` 1

-- | fixme: write tests for substitution

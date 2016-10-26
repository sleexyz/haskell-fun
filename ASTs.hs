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

-- | Dependently-typed type-safe ASTs
-- See PFPL, Chapter 1
module ASTs where

import Test.Hspec
import GHC.Types

-- * Some dependent type stuff:

type family Map (f :: k -> l) (xs :: [k]) :: [l] where
  Map f '[] = '[]
  Map f (x ': xs) = (f x ': Map f xs)

-- | HList
data Vec (xs :: [*]) where
  Nil :: Vec '[]
  (:+) :: x -> Vec xs -> Vec (x ': xs)

infixr 6 :+


-- * Firstly, type-safe AST's:

data AST k (sort :: k) where
  Var :: (Lang k) => Variable k sort -> AST k sort
  Op :: (Lang k) => Operator k i o -> Vec (Map (AST k) i) -> AST k o

class Lang k where
  type Variable k = (r :: k -> *) | r -> k
  type Operator k = (r :: [k] -> k -> *) | r -> k

-- | A set of sorts
data Arith = Number
data ArithV a where
  NumberV :: Int -> ArithV Number
data ArithOp i o where
  Plus :: ArithOp '[Number, Number] Number

instance Lang Arith where
  type Variable Arith = ArithV
  type Operator Arith = ArithOp


spec :: Spec
spec = do
  describe "Vec" $ do
    let
      _ = Nil :: Vec '[]
      _ = (NumberV 1) :+  Nil :: Vec '[ArithV Number]

    it "works" $ do
      1 `shouldBe` 1

  describe "ASTs" $ do
    let
      x :: AST Arith Number
      x = Var (NumberV 1)

      y :: AST Arith Number
      y = Var (NumberV 2)

      sum :: AST Arith Number
      sum = Op Plus (x :+ y :+ Nil)

    it "works" $ do
      1 `shouldBe` 1

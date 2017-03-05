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
{-# LANGUAGE LambdaCase #-}

-- | Typed ABTs
-- Literal translation of PFPL, chapter 1.2
-- Implemented via type-level DeBrujin indices
-- Defunctionalization idea taken from Vinyl
--
-- This is difficult, because I'm essentially simultaneously
-- encoding proofs on specific ABTs or ABTs in general.
--
-- TODO: is this related to colored operads?
-- TODO: Try again, this time be less generic, and go for
-- just first order substitution
module ASTs where

import Test.Hspec
import GHC.Types
import GHC.TypeLits

-- * Some dependent type stuff:

-- | Type-level function wrapper (TyFun)
data (~>) :: Type -> Type -> Type
type family (f :: (k ~> l) -> Type) $ (a :: k) :: l

-- | Type level map
type family (f :: (k ~> l) -> Type) <$> (xs :: [k]) :: [l] where
  f <$> '[] = '[]
  f <$> (x ': xs) = ((f $ x) ': (f <$> xs))

-- | HList
data Vec :: [Type] -> Type where
  Nil :: Vec '[]
  (:+) :: !x -> !(Vec xs) -> Vec (x ': xs)
infixr 6 :+

type family (xs :: [k]) ++ (ys :: [k]) :: [k] where
  '[] ++ ys = ys
  (x ': xs) ++ ys = x ': (xs ++ ys)

type family xs !! n where
  (x ': xs) !! 0 = x
  (_ ': xs) !! x = xs !! (x - 1)
  '[] !! x = TypeError (Text "Invalid index.")

type family Length (xs :: [k]) where
  Length '[] = 0
  Length (x ': xs) = 1 + Length xs


-- a Nat proxy
data V (i :: Nat)  = V

-- Valence is a property of arguments to operator
-- and is composed of two parts:
--
-- 1. a set of sorts of additional free variables
-- 2. the output type
data Valence u = [u] :. u

-- This formulation of an ABT is parametrized on
-- 5 parameters. Note the dependent typing.
--
-- (some universe of sorts)
-- u :: Type
--
-- (a set of "value" types, indexed by sort)
-- reify :: u -> Type
--
-- (a set of "operator" types, each indexed by a finite sequence of valences and by sort)
-- op :: [Valence u] -> u -> Type
--
-- (a set of sorts of free variables)
-- fv :: [u]
--
-- (the current type)
-- sort :: u
--
data ABT u :: (u -> Type) -> ([Valence u] -> u -> Type) -> [u] -> u -> Type where
  Prim :: reify sort -> ABT u reify op fv sort
  Op :: op valences sort -> Vec (CombineContexts u reify op fv <$> valences) -> ABT u reify op fv sort
  Var :: (n <= Length fv) => V n -> ABT u reify op fv (fv !! n)

-- A type family that adds the parent binding context to the child binding context
data CombineContexts u reify op (fv :: [k]) :: Valence k ~> Type -> Type
type instance CombineContexts u reify op fv $ (bv :. o) = ABT u reify op (fv ++ bv) o


-- first-order substitution
subst ::
     ABT u reify op fv i
  -> ABT u reify op (i : fv) o
  -> ABT u reify op fv o
subst x = \case
  Prim n -> Prim n
  Op o v -> Op o _ -- FIXME: some recursive case
  -- Var (n :: V 0) -> Var n -- FIXME: We can't currently pattern match at the term level
  Var n -> _

-- * Example

data ArithSort = Number

data ArithVal :: ArithSort -> Type where
  NumberV :: Int -> ArithVal Number

data ArithOp :: [Valence ArithSort] -> ArithSort -> Type where
  Plus :: ArithOp '[ '[] :. Number,  '[] :. Number] Number
  Let :: ArithOp '[ '[] :. Number, '[Number] :. Number] Number

type Arith = ABT ArithSort ArithVal ArithOp

plus :: Arith '[] Number -> Arith '[] Number -> Arith '[] Number
plus x y = Op Plus $ x :+ y :+ Nil

reduce :: Arith '[] Number -> Arith '[] Number
reduce = \case
  Prim (NumberV n) -> Prim (NumberV n)
  Op Plus (x :+ y :+ Nil) -> Op Plus (x :+ y :+ Nil) 
  Op Let (x :+ y :+ Nil) -> subst x y

-- * Spec

spec :: Spec
spec = do
  describe "ABTs" $ do
    let
      x :: Arith '[] Number
      x = Prim (NumberV 4)

      y :: Arith '[] Number
      y = Op Plus 
        $ Prim (NumberV 4) 
        :+ Prim (NumberV 2) 
        :+ Nil

      z :: Arith '[] Number
      z = Op Let 
        $ Prim (NumberV 2) 
        :+ Var (V @0) 
        :+ Nil

      w :: Arith '[] Number
      w = Op Let 
        $ Prim (NumberV 2) 
        :+ (
          Op Plus 
            $ Var (V @0) 
            :+ (
              Op Let
                $ Var (V @0)
                :+ Var (V @1)
                :+ Nil
            )
            :+ Nil
        )
        :+ Nil

    it "works" $ do
      pending

-- | fixme: write tests for substitution

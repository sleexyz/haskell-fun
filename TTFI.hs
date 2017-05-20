-- http://okmij.org/ftp/tagless-final/course/optimizations.html
--
-- TODO: Categorify

{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE PartialTypeSignatures #-}
module TTFI where

import Data.Functor.Const
import Data.Monoid
import Test.Hspec

-- Objects of Sym are Symantics r 's
data Symantics (r :: * -> *) = MkSymantics {
    int :: Int -> r Int,
    add :: r Int -> r Int -> r Int
  }

showInterpreter :: Symantics (Const String)
showInterpreter = MkSymantics {
    int = \x -> Const (show x),
    add = \x y -> Const ("(" <> getConst x <> " + " <> getConst y <> ")")
  }

-- morphisms
type Opt r s = Symantics r -> Symantics s

-- Optimization defines a pair of dual endofunctors in Sym
data Optimization f r = MkOptimization {
  fwd :: forall a. r a -> f r a,
  bwd :: forall a. f r a -> r a
}

class IsOptimization f where
  mkOpt :: forall r. Symantics r -> Optimization f r

liftPass :: forall r f. (IsOptimization f) => Symantics r -> Symantics (f r)
liftPass sym =
  let
    MkOptimization { fwd, bwd } = mkOpt sym :: Optimization f r
    MkSymantics { .. } = sym
  in
    MkSymantics {
      int = fwd . int,
      add = \x y -> fwd (add (bwd x) (bwd y))
    }

data IsInt r a where
  WrapInt :: Int -> IsInt r Int
  WrapUnknown :: r a -> IsInt r a

instance IsOptimization IsInt where
  mkOpt MkSymantics{..} = MkOptimization {
    fwd = WrapUnknown,
    bwd = \case 
      WrapInt x -> int x
      WrapUnknown x -> x
  }

-- TODO: make a composable type instead
type Pass f r = forall a. (Symantics (f r) -> f r a) -> r a

constantFold :: forall r. Symantics r -> Pass IsInt r
constantFold sym = 
  let 
    MkOptimization { fwd, bwd } = mkOpt sym :: Optimization IsInt r
    base :: Symantics (IsInt r)
    base@MkSymantics{..} = liftPass sym
    nextSym :: Symantics (IsInt r)
    nextSym = base {
      int = WrapInt,
      add = \x y -> case (x, y) of
        (WrapInt x, WrapInt y) -> WrapInt (x + y)
        (x, y) -> add x y
    }
  in
    \program -> bwd (program nextSym)

spec :: Spec
spec = do
  describe "Typed Tagless Final Interpreters" $ do
    describe "showInterpreter" $ do
      it "works" $ do
        let program sym = 
              let MkSymantics { .. } = sym in
              add (add (int 10) (int 0)) (add (int 20) (int (-5)))
        let str = getConst (program (showInterpreter))
        str `shouldBe` "((10 + 0) + (20 + -5))"

    describe "constantFold(showInterpreter)" $ do
      it "works" $ do
        let program sym = 
              let MkSymantics { .. } = sym in
              add (add (int 10) (int 0)) (add (int 20) (int (-5)))
        let str = getConst (constantFold showInterpreter program)
        str `shouldBe` "25"

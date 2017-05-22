-- Typed Tagless Final Interpreters in haskell
-- Based on Oleg's exposition at http://okmij.org/ftp/tagless-final/course/optimizations.html
--
-- TODO: explore a non module solution (i.e. more typeclassy))

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

data Symantics (r :: * -> *) = MkSymantics {
    int :: Int -> r Int,
    add :: r Int -> r Int -> r Int
  }

showInterpreter :: Symantics (Const String)
showInterpreter = MkSymantics {
    int = \x -> Const (show x),
    add = \x y -> Const ("(" <> getConst x <> " + " <> getConst y <> ")")
  }

type Program r a = Symantics r -> r a

type Transformation r s = forall a. Program r a -> Program s a

type Optimization f = forall r. Transformation (f r) r

data OptData f r = MkOptData {
  bwd :: forall a. (f r) a -> r a,
  fwd :: forall a. r a -> (f r) a
}

class IsOptimization f where
  getOptData :: forall r. Symantics r -> OptData f r

data IsInt (r :: * -> *) :: * -> * where
  WrapInt :: Int -> IsInt r Int
  WrapUnknown :: r a -> IsInt r a

instance IsOptimization IsInt where
  getOptData :: forall r. Symantics r -> OptData IsInt r
  getOptData MkSymantics{..} = MkOptData { fwd, bwd }
    where
      fwd :: r a -> IsInt r a
      fwd = WrapUnknown

      bwd :: IsInt r a -> r a
      bwd = \case
        WrapInt x -> int x
        WrapUnknown x -> x

getBaseOpt :: (IsOptimization f) => forall r. Symantics r -> Symantics (f r)
getBaseOpt sym =
  let
    MkOptData { .. } = getOptData sym
    MkSymantics { .. } = sym
  in
    MkSymantics {
      int = fwd . int,
      add = \x y -> fwd (add (bwd x) (bwd y))
    }

constantFoldPass :: Optimization IsInt
constantFoldPass program sym = bwd (program nextSym)
  where
    MkOptData { fwd, bwd } = getOptData sym
    baseOpt@MkSymantics { .. } = getBaseOpt sym
    nextSym = baseOpt {
      int = WrapInt,
      add = \x y -> case (x, y) of
        (WrapInt x, WrapInt y) -> WrapInt (x + y)
        (x, y) -> add x y
    }

data IsZero (r :: * -> *) :: * -> * where
  WrapZero :: IsZero r Int
  WrapUnknown' :: r a -> IsZero r a

instance IsOptimization IsZero where
  getOptData :: forall r. Symantics r -> OptData IsZero r
  getOptData MkSymantics {..} = MkOptData { fwd, bwd }
    where
      fwd :: r a -> IsZero r a
      fwd = WrapUnknown'

      bwd :: IsZero r a -> r a
      bwd = \case
        WrapZero -> int 0
        WrapUnknown' x -> x

zeroAddPass :: Optimization IsZero
zeroAddPass program sym = bwd (program nextSym)
  where
    MkOptData { fwd, bwd } = getOptData sym
    baseOpt@MkSymantics { .. } = getBaseOpt sym
    nextSym = baseOpt {
      int = \x -> if x == 0 then WrapZero else int x,
      add = \x y -> case (x, y) of
        (WrapZero, x) -> x
        (x, WrapZero) -> x
        (x, y) -> add x y
    }

spec :: Spec
spec = do
  describe "Typed Tagless Final Interpreters" $ do
    let program sym =
          let MkSymantics { .. } = sym in
          add (add (int 10) (int 0)) (add (int 20) (int (-5)))

    describe "showInterpreter" $ do
      it "works" $ do
        let str = getConst $ program showInterpreter
        str `shouldBe` "((10 + 0) + (20 + -5))"

    describe "optimizations" $ do
      describe "constantFoldPass(showInterpreter)" $ do
        it "works" $ do
          let optimize = constantFoldPass
          let str = getConst $ optimize program showInterpreter
          str `shouldBe` "25"

        it "is composable with function composition" $ do
          let optimize = constantFoldPass . constantFoldPass
          let str = getConst $ optimize  program showInterpreter
          str `shouldBe` "25"

      describe "zeroAddPass(showInterpreter)" $ do
        it "works" $ do
          let optimize = zeroAddPass
          let str = getConst $ optimize program showInterpreter
          str `shouldBe` "(10 + (20 + -5))"

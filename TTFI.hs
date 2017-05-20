-- An attempt at categorifying Typed Tagless Final Interpreters
-- Based on Oleg's exposition at http://okmij.org/ftp/tagless-final/course/optimizations.html

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
import Data.Proxy

-- We have a category Sym, where objects are types "Symantics r"
-- and morphisms are functions "Symantics r -> Symantics s"
data Symantics (r :: * -> *) = MkSymantics {
    int :: Int -> r Int,
    add :: r Int -> r Int -> r Int
  }

showInterpreter :: Symantics (Const String)
showInterpreter = MkSymantics {
    int = \x -> Const (show x),
    add = \x y -> Const ("(" <> getConst x <> " + " <> getConst y <> ")")
  }

-- Programs define a subset of functors from Sym to Hask.
-- In particular, they are type preserving functors.
-- Programs make a category themselves, where morphisms are program transformations.
-- We denote Prog ⊆ [Sym, Hask] to denote that Prog is a subcategory of the functor category [Sym, Hask]
type Program r a = Symantics r -> r a

-- Transformations are morphisms in the category Prog.
type Transformation r s = forall a. Program r a -> Program s a

-- Optimizations are algebras in the category Prog
-- We will define a class of nice to program optimizations
type Optimization f = forall r. Transformation (f r) r

data OptData f r = MkOptData {
  bwd :: forall a. (f r) a -> r a,
  fwd :: forall a. r a -> (f r) a,
  defaultNextSym :: Symantics (f r)
}

class IsOptimization f where
  getOptData :: forall r. Symantics r -> OptData f r

data IsInt r a where
  WrapInt :: Int -> IsInt r Int
  WrapUnknown :: r a -> IsInt r a

instance IsOptimization IsInt where
  getOptData :: forall r. Symantics r -> OptData IsInt r
  getOptData MkSymantics{..} = MkOptData { fwd, bwd, defaultNextSym }
    where
      fwd :: r a -> IsInt r a
      fwd = WrapUnknown

      bwd :: IsInt r a -> r a
      bwd = \case
        WrapInt x -> int x
        WrapUnknown x -> x

      defaultNextSym = MkSymantics {
        int = fwd . int,
        add = \x y -> fwd (add (bwd x) (bwd y))
      }

constantFold :: Optimization IsInt
constantFold program sym = bwd (program nextSym)
  where
    MkOptData { fwd, bwd, defaultNextSym } = getOptData sym
    MkSymantics { .. } = defaultNextSym
    nextSym = defaultNextSym {
      int = WrapInt,
      add = \x y -> case (x, y) of
        (WrapInt x, WrapInt y) -> WrapInt (x + y)
        (x, y) -> add x y
    }

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
        let str = getConst (constantFold program showInterpreter)
        str `shouldBe` "25"

      it "is chainable" $ do
        let program sym =
              let MkSymantics { .. } = sym in
              add (add (int 10) (int 0)) (add (int 20) (int (-5)))
        let str = getConst ((constantFold . constantFold) program showInterpreter)
        str `shouldBe` "25"

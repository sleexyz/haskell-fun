{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE PatternSynonyms #-}

-- | An embedded stack language in Haskell
module EmbeddedStackLang where

import Test.Hspec
import Data.Kind
import Data.Function
import Prelude hiding (head)

infixr 5 :+

-- | A stack is an HList
data Stack (k :: [*]) where
  Nil :: Stack '[]
  (:+) :: x -> Stack xs -> Stack (x ':xs)

push = (:+)



type family CanStep (s :: [*]) :: Bool where
  CanStep ((a -> b) : a : tail) =  True

class KnownStack (s :: [*]) where
  canStep :: Stack s -> Bool

instance KnownStack ((a -> b) : a : tail) where
  canStep _ = True

instance {-# INCOHERENT #-} KnownStack tail where
  canStep _ = False

-- * Step

step :: Stack ((a -> b) : a : tail) -> Stack (b : tail)
step (f :+ x :+ tail) = f x :+ tail


-- * Run

type family Normalize (s :: [*]) :: [*] where
  Normalize ((a -> b) : a : tail) = Normalize (b : tail)
  Normalize tail = tail

class Runnable tail where
  run :: Stack tail -> Stack (Normalize tail)

instance Runnable (b : tail) => Runnable ((a -> b) : a : tail) where
  run :: Stack ((a -> b) : a : tail) -> Stack (Normalize (b : tail))
  run x = run (step x)

instance (Normalize tail ~ tail) => Runnable tail where
  run :: Stack tail -> Stack tail
  run x = x


-- * utils

head :: Stack (a ': tail) -> a
head (x :+ _) = x

-- * Tests

spec = describe "stack language:" $ do
  describe "step" $ do
    it "single step" $ do
      Nil
        & push "Hello"
        & push (++"World")
        & step
        & head & shouldBe "HelloWorld"

      Nil
        & push ()
        & push "Hello"
        & push (++"World")
        & step
        & head & shouldBe "HelloWorld"

    it "multiple steps" $ do
      Nil
        & push 1
        & push 2
        & push (+)
        & step
        & step
        & head & shouldBe 3

  describe "run" $ do
    it "single step" $ do
      Nil
        & push "Hello"
        & push (++"World")
        & run
        & head & shouldBe "HelloWorld"

      Nil
        & push ()
        & push "Hello"
        & push (++"World")
        & run
        & head & shouldBe "HelloWorld"

    it "multiple steps (monomorphic)" $ do
      Nil
        & push "World"
        & push "Hello"
        & push ((++) :: String -> String -> String)
        & run
        & head & shouldBe "HelloWorld"

    -- | Fails to typecheck
    -- it "multiple steps (polymorphic)" $ do
    --   Nil
    --     & push "World"
    --     & push "Hello"
    --     & push (++)
    --     & run
    --     & head & shouldBe "HelloWorld"

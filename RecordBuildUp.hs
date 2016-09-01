{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE Rank2Types #-}

import Data.Function

import GHC.TypeLits


-- Foo Constructor

data FooC (k :: [Symbol])  =
  FooC { a :: Int
       , b :: Int
       , c :: Int
       }
  deriving (Show)

def :: FooC '[]
def = FooC undefined undefined undefined


-- Foo

data Foo = Foo !Int !Int !Int
  deriving (Show)


-- Make a Foo from a Foo Constructor

class Fooable a where
  mkFoo :: a -> Foo

instance FooComplete k => Fooable (FooC k) where
  mkFoo FooC{..} = Foo a b c

class FooComplete (k :: [Symbol])
instance FooComplete ["a", "b", "c"]
instance FooComplete ["a", "c", "b"]
instance FooComplete ["b", "a", "c"]
instance FooComplete ["b", "c", "a"]
instance FooComplete ["c", "b", "a"]
instance FooComplete ["c", "a", "b"]





-- Setters

_a :: Int -> FooC k -> FooC ("a"':k)
_a x fooC = fooC {a=x}

_b :: Int -> FooC k -> FooC ("b"':k)
_b x fooC = fooC {b=x}

_c :: Int -> FooC k -> FooC ("c"':k)
_c x fooC = fooC {c=x}



-- it works!

fooExample :: Foo
fooExample = def
  & _a 1
  & _b 2
  & _c 3
  & mkFoo


-- Can we make a state monad that builds up a value?

-- data StateC (c :: [Symbol] -> *) a = StateC (forall k1 k2. c k1 -> (c k2, a))

-- fooState :: StateC FooC ()
-- fooState = StateC (\FooC{..} -> (FooC a b c, ()))

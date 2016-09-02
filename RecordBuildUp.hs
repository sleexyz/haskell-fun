{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE RebindableSyntax #-}

import Data.Function

import GHC.TypeLits
import Data.Type.Bool
import Control.Monad.Indexed.State
import Control.Monad.Indexed
import Prelude hiding ((>>), (>>=), return)


-- Type level utils

type family (l :: [Symbol]) :< (r :: [Symbol]) where
  (l ': ls) :< r = Elem l r && (ls :< r)
  ('[]) :< r = True

type family Elem (k :: Symbol) (l :: [Symbol]) where
  Elem k '[] = False
  Elem k (k ':_) = True
  Elem k (j ':xs) = Elem k xs


type family (l :: [Symbol]) <> (r :: [Symbol]) where
  -- (l ': ls) <> r = ls <> If (Elem l r) (r) (l ': r)
  (l ': ls) <> r = ls <> (l ': r)
  '[] <> r =  r





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

instance (["a", "b", "c"] :< k ~ True) => Fooable (FooC k) where
  mkFoo FooC{..} = Foo a b c



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


-- if you really want a state monad, you can use the Indexed State Monad

return = ireturn
(>>=) = (>>>=)
a >> b = a >>= (\_ -> b)

runConstruct :: forall (k :: [ Symbol ]) a. Fooable (FooC k) => IxState (FooC '[ ]) (FooC k) a -> Foo
runConstruct x = mkFoo . snd $ runIxState x def

fooStateExample :: Foo
fooStateExample = runConstruct $ do
  imodify (_a 1)
  imodify (_b 2)
  imodify (_c 3)

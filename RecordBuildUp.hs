{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}

import Data.Function

import GHC.TypeLits
import Data.Type.Bool
import Control.Monad.Indexed.State
import Control.Monad.Indexed.Trans
import Control.Monad.Indexed
import Control.Monad.IO.Class
import Prelude hiding ((>>), (>>=), return)
import qualified Prelude as P

-- Type-safe composable constructors!

-- The problem: normal record update syntax gives you no
-- compile-time guarantees that your lazy record
-- is completely defined.


-- Solution: Carry around a type-level list parameter
-- that you grow.


-- Type level utils:

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

class Default a where
  def :: a




-- Constructor type

data FooC (k :: [Symbol])  =
  FooC { a :: Int
       , b :: Int
       , c :: Int
       }
  deriving (Show)

fooC :: FooC '[]
fooC = FooC undefined undefined undefined


-- Our final type:

data Foo = Foo !Int !Int !Int
  deriving (Show)


-- Make a Foo from a Foo Constructor

class Fooable a where
  mkFoo :: a -> Foo

instance (["a", "b", "c"] :< k ~ True) => Fooable (FooC k) where
  mkFoo FooC{..} = Foo a b c



-- Setters

_a :: Int -> FooC k -> FooC ("a"':k)
_a x fooC' = fooC' {a=x}

_b :: Int -> FooC k -> FooC ("b"':k)
_b x fooC' = fooC' {b=x}

_c :: Int -> FooC k -> FooC ("c"':k)
_c x fooC' = fooC' {c=x}



fooExample :: Foo
fooExample = fooC
  & _a 1
  & _b 2
  & _c 3
  & mkFoo

-- The following doesn't typecheck (as expected):

-- fooExample' :: Foo
-- fooExample' = def
--   & _a 1
--   & _b 2
--   & mkFoo


-- Now, if you really want a state monad, you can use the Indexed State Monad!
-- We can use do notation on Indexed monads with RebindableSyntax:
-- We can use RecordWildcards and DuplicateRecordFields to tame our syntax rebinding:

data UseMonad m =
  UseMonad { return :: forall a. a -> m a
           , (>>=) :: forall a b. m a -> (a -> m b) -> m b
           , (>>) :: forall a b. m a -> m b -> m b
           }

instance (Monad m) => Default (UseMonad m) where
  def = UseMonad { return = P.return
                 , (>>=) = (P.>>=)
                 , (>>) = (P.>>)
                 }

data UseIxMonad (m :: k -> k -> * -> *) =
  UseIxMonad { return :: forall a i. a -> m i i a
             , (>>=) :: forall a b i j k. m i j a -> (a -> m j k b) -> m i k b
             , (>>) :: forall a b i j k. m i j a -> m j k b -> m i k b
             }

instance (IxMonad m) => Default (UseIxMonad m) where
  def = UseIxMonad { return = ireturn
                   , (>>=) = (>>>=)
                   , (>>) = \a b -> a >>>= (\_ -> b)
                   }

runConstruct :: forall (k :: [ Symbol ]) a. Fooable (FooC k) => IxState (FooC '[ ]) (FooC k) a -> Foo
runConstruct x = mkFoo . snd $ runIxState x fooC

fooStateExample :: Foo
fooStateExample =
  let UseIxMonad{..} = def
  in runConstruct $ do
    imodify (_a 1)
    imodify (_b 2)
    imodify (_c 3)

-- The following doesn't type check (as expected):

-- fooStateExample' :: Foo
-- fooStateExample' =
--   let UseIxMonad{..} = def
--   in runConstruct $ do
--     imodify (_a 1)
--     imodify (_b 2)



-- If you want to get really crazy: let's use monad transformers:

type FooCM i j a = IxStateT IO (FooC i) (FooC j) a

runConstructT :: forall (k :: [ Symbol ]) a. Fooable (FooC k) => FooCM '[ ] k a -> IO Foo
runConstructT x = mkFoo . snd <$> runIxStateT x fooC

fooStateTExample :: IO Foo
fooStateTExample =
  let UseIxMonad{..} = def
  in runConstructT $ do
    a <- ilift . liftIO $ ( let UseMonad{..} = def
                            in do
                              putStrLn "hello look at me type-safe composable constructors in an indexed-state monad"
                              putStrLn "gimme three integers separated by newlines:"
                              readLn :: IO Int
                          )
    imodify (_a a)
    b <- ilift . liftIO $ (readLn :: IO Int)
    imodify (_b b)
    c <- ilift . liftIO $ (readLn :: IO Int)
    imodify (_c c)

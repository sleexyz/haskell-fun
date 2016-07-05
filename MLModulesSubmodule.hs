{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImplicitParams #-}

module MLModules where

import Prelude hiding (Monoid)

-- We can represent a module dependencies with our fake modules!
-- This means submodules!
--
-- The only thing is I cannot have type associators be named the same thing
-- These type families are globally scoped.

class Semigroup m where
  type T m
  (<>) ::  (?m :: m) => T m -> T m -> T m

-- | Monoid is a submodule of Semigroup
class Semigroup m => Monoid m where
  type T' m 
  zero ::  (?m :: m) => T' m



data Add = Add
instance Semigroup Add where
  type T Add = Int
  (<>) = (+)

instance Monoid Add where
  type T' Add = Int
  zero = 0



foo :: Int
foo = zero <> zero
  where
    ?m = Add

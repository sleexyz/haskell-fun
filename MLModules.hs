{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ConstrainedClassMethods #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImplicitParams #-}

-- Here we use implicit parameters to achieve ML style modules!

module MLModules where

class MLMonoid m where
  type T m
  zero ::  (?m :: m) => T m
  (<>) ::  (?m :: m) => T m -> T m -> T m


data AddMon = AddMon
instance MLMonoid AddMon where
  type T AddMon = Int

  zero = 0
  (<>) = (+)

foo :: Int
foo = zero <> zero
  where
    ?m = AddMon

-- Can we get ML functors though?

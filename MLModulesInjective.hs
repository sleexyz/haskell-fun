{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilyDependencies #-}

module MLModules where

-- | We can do it with associated datatypes/newtypes...

-- seems like our associated type cannot appear in negative position if they are non-injective
-- If they are injective, then we can have them in negative position with type-family dependencies. 

class MLMonoid m where
  type T m

  zero :: T m
  (<>) :: T m -> T m -> T m


data AddMon = AddMon

instance MLMonoid AddMon where
  type T AddMon = Int

  getZero _ = 0
  getAppend _ = (+) 

foo :: Int
foo = zero <> zero

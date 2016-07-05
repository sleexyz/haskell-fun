{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving#-}
-- {-# LANGUAGE InjectiveTypeFamilies #-}

module MLModules where

-- | We can do it with associated datatypes/newtypes...

class MLMonoid m where
  data T m :: *

  zero :: T m
  (<>) :: T m -> T m -> T m

data AddMon= AddMon

instance MLMonoid AddMon where
  newtype T AddMon = AM Int
    deriving (Num, Show)

  zero = 0
  (<>) = (+) 

foo :: T AddMon
foo = zero

-- | But how do we do with just types?

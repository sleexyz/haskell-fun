{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

module MLModules where

-- So we force them into positive position.
-- We can do better though!

class MLMonoid m where
  type T m

  getZero :: m -> T m
  getAppend :: m -> T m -> T m -> T m

unpack :: (MLMonoid m) => m -> (T m, T m -> T m -> T m)
unpack m = (getZero m, getAppend m)


data AddMon = AddMon

instance MLMonoid AddMon where
  type T AddMon = Int

  getZero _ = 0
  getAppend _ = (+) 

data MulMon = MulMon

instance MLMonoid MulMon where
  type T MulMon = Int

  getZero _ = 1
  getAppend _ = (*)



foo :: Int
foo = zero <> zero
  where
    (zero, (<>)) = unpack AddMon

bar :: Int
bar = zero <> zero
  where
    (zero, (<>)) = unpack MulMon

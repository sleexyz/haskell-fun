{-# LANGUAGE GADTSyntax #-}

-- | 2016-06-29
-- | OPLSS 2016
-- | Derivation of fix without general recursion

module AnnotatedFix where

newtype Self t where
  Fold :: (Self t -> t) -> Self t

unfold :: Self t -> (Self t -> t)
unfold (Fold x) = x

fix :: (a -> a) -> a
fix f = unroll . Fold $ f . unroll

unroll :: Self t -> t
unroll e = unfold e e
-- unroll (Self f) = f (Self f)



-- example
fac :: Int -> Int
fac = fix g
  where
    g :: (Int -> Int) -> (Int -> Int)
    g f x
      | x == 0    = 1
      | otherwise = x * f (x - 1)

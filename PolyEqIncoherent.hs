{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

-- | NOTE: This method cannot handle terms with type variables.

module PolyEqIncoherent where


infix 4 ===

class PolyEq a b where
  (===) :: a -> b -> Bool

instance PolyEq a b where
  _ === _ = False

instance {-# INCOHERENT #-} (Eq a) => PolyEq a a where
  (===) = (==)


test0 = () === ()                         -- true
test1 = 1 === 1                           -- false!!!
test2 = (1 :: Int) === (1 :: Int)         -- true
test3 = (1 :: Int) === (1 + 0 :: Int)     -- true
test4 = "hello" === 1                     -- false
test5 = [] === []                         -- false!!!
test6 = ([] :: [Int]) === ([] :: [Int])   -- true
test7 = ([] :: [Float]) === ([] :: [Int]) -- true

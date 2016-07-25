module PolyEqTypeable where

import Data.Typeable

infix 4 ===
(===) :: (Eq a, Typeable a, Typeable b) => a -> b -> Bool
x === y = case cast y of
  Just y' -> x == y'
  Nothing -> False

test0 = () === ()                         -- true
test1 = 1 === 1                           -- true
test2 = (1 :: Int) === (1 :: Int)         -- true
test3 = (1 :: Int) === (1 + 0 :: Int)     -- true
test4 = "hello" === 1                     -- false
-- test5 = [] === []                         -- false!!!
test6 = ([] :: [Int]) === ([] :: [Int])   -- true
test7 = ([] :: [Float]) === ([] :: [Int]) -- false

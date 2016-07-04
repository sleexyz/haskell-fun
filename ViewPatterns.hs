{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE PatternSynonyms #-}

module ViewPatterns where

data Nat = Z | S Nat
  deriving Show

toNat :: Int -> Nat
toNat 0 = Z
toNat x = S . toNat $ x - 1

fromNat :: Nat -> Int
fromNat Z = 0
fromNat (S n)  = 1 + fromNat n


pattern Even :: Int
pattern Even <- (even -> True)

pattern Zero :: Int
pattern Zero <- (toNat -> Z)

pattern One :: Int
pattern One <- (toNat -> S Z)

pattern Succ :: Nat -> Int
pattern Succ n <- (toNat -> S n)

fib :: Int -> Int
fib (toNat -> Z) = 0
fib (toNat -> S Z) = 1
fib (toNat -> S (S n)) = fib (fromNat $ S n) + fib (fromNat $ n)

fib' :: Int -> Int
fib' (Zero) = 0
fib' (One) = 1
fib' (Succ (S n)) = fib' (fromNat $ S n) + fib' (fromNat $ n)


pattern ToNat i <- (toNat ->  i)
pattern FromNat n <- (fromNat -> n)

fib'' :: Int -> Int
fib'' (ToNat Z) = 0
fib'' (ToNat ( S Z)) = 1
fib'' (ToNat ( S (S n))) = fib'' (fromNat $ S n) + fib'' (fromNat $ n)
-- but we need a fromNat!


-- Can we define a infix thing?

pattern Nop :: a -> a
pattern Nop x <- (id -> x)

plus :: Int -> Int -> Int
plus (Nop 0) n = n  -- lol nop
plus (ToNat (S (FromNat m))) n = 1 + plus m n -- here I go in and out

isTrue :: Bool -> Bool
isTrue (Nop (Nop (Nop True))) = True
isTrue _ = False



-- pattern App :: (a -> b) -> a -> b
-- pattern App f y <- (($) -> f) -> y


{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module Defunctionalization where
import Util as Ut

-- turn recursion into iteration
-- Turn interpretation into abstract machines

-- https://en.wikipedia.org/wiki/Defunctionalization

-- cons :: a -> [a] -> [a]
-- cons x xs = x : xs

-- compose :: (b -> c) -> (a -> b) -> a -> c
-- compose f g x = f (g x)

-- flatten :: Tree t -> [t]
-- flatten t = walk t []

-- walk :: Tree t -> [t] -> [t]
-- walk (Leaf x) = cons x
-- walk (Node t1 t2) = compose (walk t1) (walk t2)



-- DEFUNCTIONALIZATION
-- Higher order functions are translated to datatypes to be pattern-matched on
-- Big-step semantics

-- type State a = (Expr a, [a])

-- data Expr a = Cons a
--             | Compose (Expr a) (Expr a)

-- apply :: State a -> [a]
-- apply (Cons x, xs)        = x : xs
-- apply (Compose f1 f2, xs) = apply (f1, (apply (f2, xs)))

-- flattenDef :: Tree t -> [t]
-- flattenDef t = apply (walkDef t, [])

-- walkDef :: Tree t -> Expr t
-- walkDef (Leaf x) = Cons x
-- walkDef (Node t1 t2) = Compose (walkDef t1) (walkDef t2)


-- DEFUNCTIONALIZATION WITH CONTINUATIONS:
-- Abstract machines!

type State a = (Expr a, [a], Cont a)

instance {-# OVERLAPPING #-} (Show a) => Show (State a) where
  show (expr, list, cont)
    = (Ut.color Ut.Blue $ show expr)
    ++ " "
    ++ (Ut.color Ut.Yellow $ show list)
    ++ " "
    ++ (Ut.color Ut.Black $ show cont)

data Cont a = Done
            | Eval (Expr a) (Cont a)
            deriving (Show, Eq)

data Expr a = Cons a
           | Compose (Expr a) (Expr a)
           | Finished
           deriving (Show, Eq)

step :: State a -> State a
-- On Cons, cons to your list and continue.
step ( Cons x, xs, Done) = ( Finished, x:xs, Done)

step ( Cons x
     , xs
     , Eval next nextCont
     )
  = ( next
    , x:xs
    , nextCont
    )

-- On Compose
step ( Compose f1 f2
     , xs
     , cont
     )
  = ( f2
    , xs
    , Eval f1 cont
    )

inject :: Expr a -> State a
inject expr = (expr, [], Done)

isDone :: State a -> Bool
isDone ( Finished, _ , Done) = True
isDone _             = False


evaluate :: Expr a -> State a
evaluate = until isDone step . inject

stepTrace :: (Show a) => State a -> IO (State a)
stepTrace state = fmap step (printAndReturn state)
  where
    printAndReturn x = (putStrLn . (++"\n"). show) x >> return x

-- TODO: Use a free monad instead

evaluateTrace :: (Show a) => Expr a -> IO (State a)
evaluateTrace expr = iterateUntilM isDone stepTrace (inject expr)
  where
    iterateUntilM :: (a -> Bool) -> (a -> IO a) -> (a -> IO a)
    iterateUntilM p kf x
      | p x = return x
      | otherwise = kf x >>= iterateUntilM p kf


-- Walking on Trees

data Tree a = Leaf a
            | Node (Tree a) (Tree a)
            deriving (Show, Eq)

-- A natural transformation
convert :: Tree a -> Expr a
convert (Leaf a) =   Cons a
convert (Node x y) = Compose (convert x) (convert y)


walkTreeTrace :: (Show a) => Tree a -> IO (State a)
walkTreeTrace = evaluateTrace . convert

testTree :: Tree Int
testTree = Node (Node (Leaf 1) (Leaf 2)) (Leaf 3)

{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}

module CEK where

type Index = Int
type Name  = String

-- Goal: small step interpretation
-- http://matt.might.net/articles/cek-machines/

type Var = String
data Lambda = Var :=> Exp
              deriving (Show, Eq)

data Exp
  = Lam Lambda
  | Ref Var
  | Exp :@ Exp
    deriving (Show, Eq)


-- A state is a triple
type State = (Exp, Env, Kont)


data D = Clo (Lambda, Env)
type Env = Var -> D


data Kont = Mt
          | Ar (Exp, Env, Kont)
          | Fn (Lambda, Env, Kont)

-- Deriving
instance Show Env where
  show _ = "<Env>"

deriving instance Show Kont

-- Machine functions

inject :: Exp -> State
inject e = (e, env0, Mt)
  where
    env0 :: Env
    env0 = \x -> error $ "no binding for " ++ x

step :: State -> State

-- Evaluating reference? Look up in environment
step (Ref x, env, k)
  = (Lam lam, env', k) where Clo (lam, env') = env x


-- Evaluate function application? First evaluate the function.
step (f :@ e, env, k)
  = (f, env, Ar (e, env, k))

-- Evaluated the function? Go evaluate the argument
step (Lam lam, env, Ar(e, env', k))
  = (e, env', Fn (lam, env, k))

-- Evaluating
step (Lam lam, env, Fn(x :=> e, env', k))
  = (e, env' // [x ==> Clo (lam, env)], k)

(==>) :: a -> b -> (a, b)
(==>) x y = (x, y)

(//) :: Eq a => (a -> b) -> [(a, b)] -> (a -> b)
(//) f [(x, y)] = \x' ->
  if (x == x')
  then y
  else f x'

terminal :: (State -> State) -> (State -> Bool) -> (State -> State)
terminal step isFinal c0 | isFinal c0 = c0
                         | otherwise = terminal step isFinal (step c0)

isFinal :: State -> Bool
isFinal (Lam _, env, Mt) = True
isFinal _                = False

evaluate :: Exp -> State
evaluate pr = terminal step isFinal (inject pr)


icomb :: Exp
icomb = Lam ("x" :=> Ref "x")

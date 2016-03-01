{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TypeSynonymInstances #-}

module CEKSemantics where

import           Prelude          hiding (until)
import qualified Text.Show.Pretty as Pr
import           Util             as Ut


type Index = Int
type Name  = String

-- Goal: small step interpretation
-- http://matt.might.net/articles/cek-machines/

type Var = String
data Lambda = Var :=> Expr
              deriving (Show, Eq)

data Expr
  = Lam Lambda
  | Ref Var
  | Expr :@ Expr
    deriving (Show, Eq)

-- A  CEK machine state has the current expression, an environment, and a continuation.
type State = (Expr, Env, Kont)

instance {-# OVERLAPPING #-} Show State where
  show (expr, env, kont)
    = (Ut.color Ut.Blue $ show expr)
    ++ " "
    ++ (Ut.color Ut.Yellow $ show env)
    ++ " "
    ++ (Ut.color Ut.Black $ show kont)


data Clo = Clo Lambda Env

type Env = Var -> Clo
instance Show Env where
  show _ = "<Env>"

data Kont = Done
          | EvalArg Expr Env Kont
          | EvalFn Lambda Env Kont
          deriving (Show)



-- Machine functions
step :: State -> State

-- |Evaluating a reference? Look up in environment.
step ( Ref name
      , env
      , kont
      )
  = ( Lam lam
    , env'
    , kont
    )
  where
    Clo lam env' = env name

-- |Evaluating a function application? First evaluate the function.
step ( f :@ arg
     , env
     , kont
     )
  = ( f
    , env
    , EvalArg arg env kont -- we pass the
    )

-- |Evaluated the function? Go evaluate the argument
step ( Lam lam
     , env
     , EvalArg arg env' kont
     )
  = ( arg
    , env'
    , EvalFn lam env kont
    )

-- |Evaluated the argument? Now perform the application.
step ( Lam lam
     , env
     , EvalFn (name :=> expr) env' kont
     )
  = (expr
    , env' // (name, Clo lam env)
    , kont
    )
  where
    -- What is this? alpha renaming?
    (//) :: (Var -> Clo) -> (Var, Clo) -> (Var -> Clo)
    (//) f (envName, clo) = \name ->
      if (envName == name)
      then clo
      else f name


inject :: Expr -> State
inject e = (e, env0, Done)
  where
    env0 :: Env
    env0 x =  error $ "no binding for " ++ x

-- |We know we're done when we have an empty continuation
isDone :: State -> Bool
isDone (Lam _, _, Done) = True
isDone _                = False

-- Evaluation

evaluate :: Expr -> State
evaluate expr = until isDone step (inject expr)
  where
    until :: (a -> Bool) -> (a -> a) -> (a -> a)
    until p f x
      | p x = x
      | otherwise = until p f (f x)


-- Evaluation with Trace

stepTrace :: State -> IO State
stepTrace state = fmap step (printAndReturn state)
  where
    printAndReturn x = (putStrLn . (++"\n"). show) x >> return x

evaluateTrace :: Expr -> IO State
evaluateTrace expr = iterateUntilM isDone stepTrace (inject expr)
  where
    iterateUntilM :: (a -> Bool) -> (a -> IO a) -> (a -> IO a)
    iterateUntilM p kf x
      | p x = return x
      | otherwise = kf x >>= iterateUntilM p kf


-- Testing

icomb :: Expr
icomb = Lam ("x" :=> Ref "x")

-- TODO: defunctionalize env into a map

{-# LANGUAGE GADTs               #-}
{-# LANGUAGE ScopedTypeVariables #-}

module TypedLambda where

import qualified Data.Map.Lazy as Map

-- data Sym = MakeInt Int
--            deriving (Show, Eq, Ord)

-- data Expr a where
--   Const :: Expr Sym
--   Lam :: Expr Sym -> Expr a


-- eval :: Expr a -> Expr a
-- eval Const =


-- one = MakeInt 1
-- two = MakeInt 2


type Context = Map.Map String Expr

data Expr = Var String
          | Lambda String Expr
          | Const Int
          | Error String
          | Add Expr Expr
          | Eval Expr Expr
            deriving (Show)




defaultContext :: Map.Map String Expr
defaultContext =
    Map.insert "1" (Const 1)
  $ Map.insert "2" (Const 2)
  $ Map.insert "undefined" (Error "Not found!")
  $ Map.empty




sorry :: forall a. a
sorry = error "didn't work yo"




eval :: Expr -> Context -> Expr

eval (Add x y) ctx =  add (eval x ctx) (eval y ctx)
  where
    add (Const x) (Const y) = Const $ x + y
    add _ _ = sorry


eval (Const x) ctx =  Const x
eval (Lambda tag expr) ctx =  Lambda tag expr

eval (Error str) ctx = sorry

eval (Var tag) ctx =  f $ Map.lookup tag ctx
    where
      f (Just x) = x
      f Nothing = Error "Not found!"

eval (Eval left right) ctx = eval' (eval left ctx) (eval right ctx)
  where
    eval' (Lambda tag f) expr = eval f $ Map.insert tag expr ctx
    eval' _ _ = sorry



testExpr = Eval (Lambda "x" (Add (Var "x") (Const 2))) (Const 3)
testExpr' = Add (Const 2) (Const 2)
testExpr'' = Add (Add (Var "2") (Var "2")) (Var "2")

test :: Expr -> Expr
test input = eval input defaultContext

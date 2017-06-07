{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StandaloneDeriving #-}

-- | DeBrujin
module LetLang where

import Test.Hspec
import Data.Function

data AST where
  Var :: Int -> AST
  Let :: AST -> AST -> AST 
  Prim :: Bool -> AST
deriving instance (Eq AST)
deriving instance (Show AST)

isValidExpr :: AST -> Bool
isValidExpr = isValidExprGivenMax (-1)
  where
    isValidExprGivenMax :: Int -> AST -> Bool
    isValidExprGivenMax i = \case
      Var j | j > i -> False
      Var j | otherwise -> True
      Let y rest -> isValidExprGivenMax i y && isValidExprGivenMax (i + 1) rest
      Prim x -> True

subst :: Int -> AST -> AST -> AST
subst i x = \case
  Var j | j == i ->  x
  Var j | j > i ->  Var (j - 1)
  Var j | otherwise -> Var j
  Let y rest -> Let (subst i x y) (subst i x rest)
  Prim x -> Prim x

reduce :: AST -> AST
reduce = id
  & handleLet
  where
    handleLet :: (AST -> AST) -> (AST -> AST)
    handleLet fallback = \case
      Let y rest -> reduce $ subst 0 y rest
      x -> fallback x

spec :: Spec
spec = do
  describe "isValidExpr" $ do
    it "should work" $ do
      isValidExpr (Prim True) `shouldBe` True
      isValidExpr (Let (Prim True) (Var 0)) `shouldBe` True
      isValidExpr (Let (Prim True) (Prim True)) `shouldBe` True
      isValidExpr (Let (Let (Prim True) (Var 0)) (Prim True)) `shouldBe` True
      isValidExpr (Let (Prim True) (Let (Prim True) (Var 1))) `shouldBe` True

      isValidExpr (Var 0) `shouldBe` False
      isValidExpr (Var 1) `shouldBe` False
      isValidExpr (Let (Prim True) (Var 1)) `shouldBe` False
      isValidExpr (Let (Var 0) (Prim True)) `shouldBe` False
      isValidExpr (Let (Var 1) (Prim True)) `shouldBe` False
      isValidExpr (Let (Let (Prim True) (Var 1)) (Prim True)) `shouldBe` False
      isValidExpr (Let (Prim True) (Let (Prim True) (Var 2))) `shouldBe` False

  describe "reduce" $ do
    it "should work" $ do
      reduce (Prim True) `shouldBe` Prim True
      reduce (Let (Prim True) (Prim True)) `shouldBe` Prim True
      reduce (Let (Prim True) (Var 0)) `shouldBe` Prim True
      reduce (Let (Prim True) (Var 0)) `shouldBe` Prim True
      reduce (Let (Prim True) (Let (Var 0) (Prim True))) `shouldBe` Prim True
      reduce (Let (Prim True) (Let (Prim False) (Var 0))) `shouldBe` Prim True
      reduce (Let (Prim True) (Let (Prim False) (Var 1))) `shouldBe` Prim False

{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StandaloneDeriving #-}

-- | This time we give our AST enough information
-- to be bidirectional
module LetLang2 where

import Test.Hspec
import Data.Function

data AST where
  Var :: String -> Int -> AST
  Let :: String -> AST -> AST -> AST 
  Prim :: Bool -> AST
deriving instance (Eq AST)
deriving instance (Show AST)

-- FIXME: check for more than just validity of debrujin indicies
isValidExpr :: AST -> Bool
isValidExpr = isValidExprGivenMax (-1)
  where
    isValidExprGivenMax :: Int -> AST -> Bool
    isValidExprGivenMax i = \case
      Var _ j | j > i -> False
      Var _ j | otherwise -> True
      Let _ y rest -> isValidExprGivenMax i y && isValidExprGivenMax (i + 1) rest
      Prim x -> True

subst :: Int -> AST -> AST -> AST
subst i x = \case
  Var _ j | j == i ->  x
  Var n j | j > i ->  Var n (j - 1)
  Var n j | otherwise -> Var n j
  Let n y rest -> Let n (subst i x y) (subst i x rest)
  Prim x -> Prim x

reduce :: AST -> AST
reduce = id
  & handleLet
  where
    handleLet :: (AST -> AST) -> (AST -> AST)
    handleLet fallback = \case
      Let _ y rest -> reduce $ subst 0 y rest
      x -> fallback x

printAST :: AST -> String
printAST = \case
  Var n _ -> n
  Let n x y  -> "let " ++ n ++ " = " ++ printAST x ++ " in\n" ++ printAST y
  Prim True -> "True"
  Prim False -> "False"

spec :: Spec
spec = do
  describe "isValidExpr" $ do
    it "should work" $ do
      isValidExpr (Prim True) `shouldBe` True
      isValidExpr (Let "x" (Prim True) (Var "x" 0)) `shouldBe` True
      isValidExpr (Let "x" (Prim True) (Prim True)) `shouldBe` True
      isValidExpr (Let "x" (Let "y" (Prim True) (Var "x" 0)) (Prim True)) `shouldBe` True
      isValidExpr (Let "x" (Prim True) (Let "y" (Prim True) (Var "x" 1))) `shouldBe` True

      isValidExpr (Var "x" 0) `shouldBe` False
      isValidExpr (Var "x" 1) `shouldBe` False
      isValidExpr (Let "x" (Prim True) (Var "x" 1)) `shouldBe` False
      isValidExpr (Let "x" (Var "x" 0) (Prim True)) `shouldBe` False
      isValidExpr (Let "x" (Var "x" 1) (Prim True)) `shouldBe` False
      isValidExpr (Let "x" (Let "y" (Prim True) (Var "x" 1)) (Prim True)) `shouldBe` False
      isValidExpr (Let "x" (Prim True) (Let "x" (Prim True) (Var "x" 2))) `shouldBe` False

  describe "reduce" $ do
    it "should work" $ do
      reduce (Prim True) `shouldBe` Prim True
      reduce (Let "x" (Prim True) (Prim True)) `shouldBe` Prim True
      reduce (Let "x" (Prim True) (Var "x" 0)) `shouldBe` Prim True
      reduce (Let "x" (Prim True) (Var "x" 0)) `shouldBe` Prim True
      reduce (Let "x" (Prim True) (Let "y" (Var "x" 0) (Prim True))) `shouldBe` Prim True
      reduce (Let "x" (Prim True) (Let "y" (Prim False) (Var "x" 0))) `shouldBe` Prim True
      reduce (Let "x" (Prim True) (Let "y" (Prim False) (Var "y" 1))) `shouldBe` Prim False

  describe "printAST" $ do
    it "should work" $ do
      printAST (Prim True) `shouldBe` "True"
      printAST (Let "x" (Prim True) (Let "y" (Prim False) (Var "y" 1))) 
        `shouldBe` "let x = True in\nlet y = False in\ny"

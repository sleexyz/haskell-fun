{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}

-- | We use Atkey-style indexed state monads 
-- i.e. Hoare monads
-- to represent morphisms in the arrow category of Hask
module Meta where

import Control.Monad.Indexed
import Control.Monad.Indexed.State
import Test.Hspec
import Data.Default

import Indexed

data TType = TUnit | TInt | TType :-> TType

-- | Typed program AST
data AST  (t :: TType) where
  PUnit :: AST TUnit
  PInt :: Int -> AST TInt
  PIncr :: AST (TInt :-> TInt)
  PApp :: AST (TInt :-> TInt) -> AST TInt -> AST TInt

deriving instance (Show (AST t))


-- | An act of Programming represents an arrrow
type P (a :: TType) (b :: TType) x = IxState (AST a) (AST b) x

evalInt :: AST TInt -> AST TInt
evalInt = PInt . evalInt'
  where
    evalInt' :: AST TInt -> Int
    evalInt' = \case
      PInt a -> a
      PApp PIncr a -> 1 + evalInt' a
    



spec :: Spec
spec = do
  describe "evalInt" $ do
    it "works" $ do
      show (evalInt (PInt 100)) `shouldBe` show (evalInt (PInt 100))


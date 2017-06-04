{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
module FreeState where

import Control.Monad
import Control.Monad.Free
import Control.Monad.Except
import Test.Hspec

-- data StateF next = 
--     Get (Int -> next)
--   | Set Int next
  
data StateF state next where
  Get :: (state -> next) -> StateF state next
  Set :: state -> next -> StateF state next
deriving instance Functor (StateF state)

type FunctorMorphism f g = forall a. f a -> g a

-- forall a. Either a -> Maybe a
--
-- foldFreeMonoid :: (Monoid b) => forall a. (a -> b) -> [a] -> b
-- foldFreeMonoid = foldMap
-- foldFreeMonoid f xs = foldr (\x acc -> f x `mappend` acc) mempty

-- foldFree' :: (forall x. f x -> m x) -> (Free f a -> m a)



-- type TheirInterpreter f m where
--   interpret :: f (m a) -> m a

-- type OurInterpreter f m where
--   interpret :: f a -> m a

interpretIntState :: StateF Int a -> IO a
interpretIntState (Get cont) = print "getting" >> pure (cont 0)
interpretIntState (Set i next) = print ("setting to " ++ show i) >> pure next

runState :: Free (StateF Int) a -> IO a
runState = foldFree interpretIntState


data FailureF next = Pass next | Fail String
  deriving Functor

interpretFailure :: forall a. FailureF a -> ExceptT String IO a
interpretFailure (Pass next) = return next
interpretFailure (Fail str) = throwError str

spec :: Spec
spec = do
  describe "interpretIntState" $ do
    -- let
    --   interpreter :: Free (StateF Int) a -> ExceptT String IO a
    --   interpreter = foldFree interpretIntState

    it "" pending

  describe "interpretFailure" $ do
    let
      interpreter :: Free FailureF a -> ExceptT String IO a
      interpreter = foldFree interpretFailure

    it "catches errors" $ do
      let
        testProg :: Free FailureF Int
        testProg = do
          x <- Free (Pass (Pure 100))
          Free (Fail "error!!")
          y <- Free (Pass (Pure 50))
          return (x + y)
      result <- runExceptT (interpreter testProg)
      result `shouldBe` Left "error!!"

    it "allows passing computations" $ do
      let
        testProg :: Free FailureF Int
        testProg = do
          x <- Free (Pass (Pure 100))
          y <- Free (Pass (Pure 50))
          return (x + y)

      result <- runExceptT (interpreter testProg)
      result `shouldBe` Right 150


main :: IO ()
main = hspec spec



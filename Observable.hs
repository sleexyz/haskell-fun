{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE KindSignatures #-}

-- | An attempt at modeling observables in Haskell.
module Observable where

import Prelude hiding (init)
import Data.IORef
import Control.Monad.ST
import Data.STRef
import Test.Hspec
import GHC.Types

class (Monad m) => Stateful m where
  init :: state -> m (MutableRef m state)

data MutableRef m state = 
  MkMutableRef (m state) (state -> m ())

get :: MutableRef m state -> m state
get (MkMutableRef getter _) = getter

set :: state -> MutableRef m state -> m ()
set state (MkMutableRef _ setter) = setter state

instance Stateful IO where
  init :: forall state. state -> IO (MutableRef IO state)
  init s = do
    ref <- newIORef s
    let getter = readIORef ref
        setter = writeIORef ref
    return $ MkMutableRef getter setter

instance Stateful (ST t) where
  init :: state -> (ST t) (MutableRef (ST t) state)
  init s = do
    ref <- newSTRef s
    let get = readSTRef ref
        set = writeSTRef ref
    return $ MkMutableRef get set

-- | Naive solution: 
--
-- An observable is mutable, mutable reference, in which the setter is mutated.
--
-- Issues:
--  - Cannot deregister callbacks
--  - Unwieldy (to get value, one must (get >=> get))
registerNaive :: 
  forall (m :: Type -> Type) (state :: Type).
  (Stateful m) =>  
  MutableRef m (MutableRef m state) -> 
  (state -> m ()) -> 
  m ()
registerNaive refref cb =  do
  let MkMutableRef getRef setRef = refref
  MkMutableRef oldGet oldSet <- getRef
  setRef $ MkMutableRef oldGet (\state -> oldSet state >> cb state)

runTest  :: forall a. (Eq a, Show a) => (forall m. (Stateful m) => m a) -> a -> Spec
runTest test expectedResult = do
    it "works for ST" $ do
      let result = runST test
      result `shouldBe` expectedResult

    it "works for IO" $ do
      result <- test
      result `shouldBe` expectedResult
  
spec :: Spec
spec = do
  describe "Stateful" $ do
    let test :: (Stateful m) => m String
        test = do
          ref <- init "hello"
          set "world" ref
          get ref
    let expectedResult = "world"
    runTest test expectedResult

  describe "Stateful: Pointer to pointer" $ do
    let test :: (Monad m, Stateful m) => m String
        test = do
          refref <- init =<< init "hello" 
          return refref >>= get >>= get
    let expectedResult = "hello"
    runTest test expectedResult

  describe "registerNaive" $ do
    context "doesn't trigger cb when not supposed to" $ do
      let test :: forall m. (Monad m, Stateful m) => m Bool
          test = do
            refref <- init =<< init "hello" 
            isModifiedRef <- init False
            registerNaive refref $ const $ set True isModifiedRef
            get isModifiedRef
      let expectedResult = False
      runTest test expectedResult

    context "triggers cb when supposed to" $ do
      let test :: forall m. (Monad m, Stateful m) => m Bool
          test = do
            refref <- return "hello" >>= init >>= init
            isModifiedRef <- init False
            registerNaive refref $ const $ set True isModifiedRef
            return refref >>= get >>= set "asdf"
            get isModifiedRef
      let expectedResult = True
      runTest test expectedResult

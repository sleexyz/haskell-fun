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

import Prelude hiding (initMutable)
import Data.IORef
import Control.Monad
import Control.Monad.ST
import Data.STRef
import Test.Hspec
import GHC.Types
import qualified Data.IntMap.Strict as Map

class (Monad m) => Stateful m where
  initMutable :: state -> m (MutableRef m state)

class Mutable ref where
  get :: ref m state -> m state
  set :: ref m state -> state -> m ()

data MutableRef m state = MkMutableRef {
  getter :: m state,
  setter :: state -> m ()
}

instance Mutable MutableRef where
  get (MkMutableRef getter _) = getter
  set (MkMutableRef _ setter) = setter

instance Stateful IO where
  initMutable :: forall state. state -> IO (MutableRef IO state)
  initMutable s = do
    ref <- newIORef s
    let getter = readIORef ref
        setter = writeIORef ref
    return $ MkMutableRef getter setter

instance Stateful (ST t) where
  initMutable :: state -> (ST t) (MutableRef (ST t) state)
  initMutable s = do
    ref <- newSTRef s
    let get = readSTRef ref
        set = writeSTRef ref
    return $ MkMutableRef get set

-- | Take 1: 
--
-- An observable is mutable, mutable reference, in which the setter is mutated.
--
-- Issues:
--  - Cannot deregister callbacks
--  - Unwieldy (to get value, one must (get >=> get))
registerTake1 :: forall (m :: Type -> Type) (state :: Type). (Stateful m) =>  
  MutableRef m (MutableRef m state) -> 
  (state -> m ()) -> 
  m ()
registerTake1 refref cb =  do
  let MkMutableRef getRef setRef = refref
  MkMutableRef oldGet oldSet <- getRef
  setRef $ MkMutableRef oldGet (\state -> oldSet state >> cb state)


-- | Take 2: 
--
-- An observable is mutable reference of some state, along with some mutable reference of a collection of callbacks.
--
-- Issues:
--  - Memory leaks
data ObservableRef m state = MkObservableRef {
  getter :: m state,
  setter :: state -> m (),
  registerer :: (state -> m ()) -> m (m ())
}

class Mutable ref => Observable ref where
  register :: ref m state -> (state -> m ()) -> m (m ())

makeObservable :: forall m state. Stateful m => 
 MutableRef m state -> 
 m (ObservableRef m state)
makeObservable (MkMutableRef originalGetter originalSetter) = do
    idRef <- initMutable (0 :: Int)
    let freshId :: m Int
        freshId = do
          newId <- get idRef
          set idRef (newId + 1)
          return newId

    callbacksRef <- initMutable (Map.fromList []  :: Map.IntMap (state -> m ()))
    let newSetter :: state -> m ()
        newSetter state = do
          originalSetter state
          callbacks <- get callbacksRef
          mapM_ ($state) $ fmap snd $ Map.toList callbacks

        register :: (state -> m ()) -> m (m ())
        register cb = do
          callbacks <- get callbacksRef
          id <- freshId
          set callbacksRef (Map.insert id cb callbacks)
          let deregister = do
                callbacks <- get callbacksRef
                set callbacksRef (Map.delete id callbacks)
          return deregister
          
    return $ MkObservableRef originalGetter newSetter register

instance Mutable ObservableRef where
  get (MkObservableRef getter _ _) = getter
  set (MkObservableRef _ setter _) = setter
  
instance Observable ObservableRef where
  register (MkObservableRef _ _ register) = register

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
    let test :: (Stateful m) => m Bool
        test = do
          ref <- initMutable False
          set ref True
          get ref
    runTest test True

  describe "Observable: Take 1" $ do
    context "doesn't trigger cb when not supposed to" $ do
      let test :: forall m. (Stateful m) => m Bool
          test = do
            refref <- initMutable =<< initMutable "hello" 
            isModifiedRef <- initMutable False
            registerTake1 refref $ const $ set isModifiedRef True
            get isModifiedRef
      runTest test False

    context "triggers cb when supposed to" $ do
      let test :: forall m. (Stateful m) => m Bool
          test = do
            refref <- return "hello" 
              >>= initMutable 
              >>= initMutable
            isModifiedRef <- initMutable False
            registerTake1 refref $ const $ set isModifiedRef True
            (get refref) >>= flip set "asdf"
            get isModifiedRef
      runTest test True

  describe "Observable: Take 2" $ do
    context "doesn't trigger cb when not supposed to" $ do
      let test :: forall m. (Stateful m) => m Bool
          test = do
            ref <- return "hello"
              >>= initMutable
              >>= makeObservable
            isModifiedRef <- initMutable False
            _ <- register ref (const $ set isModifiedRef True)
            get isModifiedRef
      runTest test False

    context "can trigger cb when supposed to" $ do
      let test :: forall m. (Stateful m) => m Bool
          test = do
            ref <- return "hello"
              >>= initMutable
              >>= makeObservable
            isModifiedRef <- initMutable False
            _ <- register ref (const $ set isModifiedRef True)
            set ref "world"
            get isModifiedRef
      runTest test True

    context "can trigger multiple cb's when supposed to" $ do
      let test :: forall m. (Stateful m) => m Bool
          test = do
            ref <- return "hello"
              >>= initMutable
              >>= makeObservable
            isModifiedRef1 <- initMutable False
            _ <- register ref (const $ set isModifiedRef1 True)
            isModifiedRef2 <- initMutable False
            _ <- register ref (const $ set isModifiedRef2 True)
            set ref "world"
            (&&) <$> get isModifiedRef1 <*> get isModifiedRef2
      runTest test True

    context "can trigger cb's multiple times" $ do
      let test :: forall m. (Stateful m) => m Int
          test = do
            ref <- return ()
              >>= initMutable
              >>= makeObservable
            counter <- initMutable (0 :: Int)
            let incr = get counter >>= \i -> set counter (i + 1)
            _ <- register ref $ const incr
            set ref ()
            set ref ()
            set ref ()
            set ref ()
            set ref ()
            get counter
      runTest test 5

    context "can deregister cb" $ do
      let test :: forall m. (Stateful m) => m Bool
          test = do
            ref <- return "hello"
              >>= initMutable
              >>= makeObservable
            isModifiedRef <- initMutable False
            deregister <- register ref (const $ set isModifiedRef True)
            deregister
            set ref "world"
            get isModifiedRef
      runTest test False

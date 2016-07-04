module STM where

import Control.Concurrent.STM
import Control.Concurrent
import Data.IORef
import Control.Monad

foo :: TVar Int -> STM ()
foo counterRef = do
  modifyTVar counterRef (+1)
  retry

work :: Int -> TVar Int -> STM ()
work m counterRef = do
  i <- readTVar counterRef
  if i `mod` 2 == m
    then modifyTVar counterRef $ (+1)
    else retry

test1 :: IO ()
test1 = do
  counterRef  <- atomically $ newTVar 0
  forkIO $ forever . atomically $ work 0 counterRef
  forkIO $ do
    threadDelay (10^7)
    forever . atomically $ work 1 counterRef

  forever $ do
    (atomically . readTVar $ counterRef) >>= print
    threadDelay (10^6)


type GameState = Int
type MoveCounter = Int

wok :: TVar MoveCounter -> STM ()
wok mcRef = do
  mc <- readTVar mcRef
  if mc == 20
    then retry
    else do
    modifyTVar mcRef (+1)

test2 :: IO ()
test2 = do
  mcRef <- atomically $ newTVar 0

  forkIO $ forever $ do
    atomically $ wok mcRef
    threadDelay (10^5)

  forkIO $ forever $ do
    atomically $ swapTVar mcRef 0
    threadDelay (3 * 10^6)

  forever $ do
    let thing n = mconcat . replicate n $ "o"
    (atomically $ readTVar mcRef)  >>= (putStrLn . thing)
    threadDelay (10^5)
  return ()


whileM_ :: Monad m => m Bool -> m a -> m ()
whileM_ mp mx = do
  p <- mp
  when p $ do
    void mx
    whileM_ mp mx



-- if move counter is different, then I want the game

test3 :: IO ()
test3 = do
  mcRef <- atomically $ newTVar 0
  gsRef <- atomically $ newTVar 0

  _ <- forkIO $ forever $ do
    mc <- atomically $ readTVar mcRef
    whileM_ ((atomically $ readTVar mcRef) >>= (pure . (==mc))) $ do
      atomically $ modifyTVar gsRef (+1)
      threadDelay (10^6)
    atomically $ modifyTVar gsRef (const 0)

  forkIO $ forever $ do
    atomically $ modifyTVar mcRef (+1)
    threadDelay (5 * 10^6)

  forever $ do
    gs <- atomically $ readTVar gsRef
    print gs
    threadDelay (10^5)
  return ()



test4 :: IO ()
test4 = do
  mcRef <- newIORef 0
  gsRef <- newIORef 0

  _ <- forkIO $ forever $ do
    mc <- readIORef mcRef
    whileM_ ((readIORef mcRef) >>= (pure . (==mc))) $ do
      modifyIORef gsRef (+1)
      threadDelay (10^6)
    modifyIORef gsRef (const 0)

  forkIO $ forever $ do
    modifyIORef mcRef (+1)
    threadDelay (5 * 10^6)

  forever $ do
    gs <- readIORef gsRef
    print gs
    threadDelay (10^5)
  return ()

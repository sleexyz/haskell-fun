module Deadlock where

import Test.Hspec
import Control.Concurrent


-- shown to me by Soenke Hahn

spec = do
  describe "1 mvar" $ do
    it "" $ do
      foo <- newMVar "asdf"
      forkIO $ do
        val <- takeMVar foo
        threadDelay 1000000
        putStrLn val
        putMVar foo "qwer"
      print "forked"
      val <- takeMVar foo
      putStrLn val
      putMVar foo "asdf"
      return ()

  describe "2 mvars" $ do
    it "" $ do
      foo <- newMVar "asdf"
      bar <- newMVar "ASDF"
      forkIO $ do
        fooval <- takeMVar foo
        threadDelay 1000000
        barval <- takeMVar bar
        threadDelay 1000000
        putStrLn fooval
        putMVar bar barval
        putMVar foo "qwer"
      print "forked"
      takeMVar bar
      threadDelay 500000
      val <- takeMVar foo
      putStrLn val
      putMVar foo "asdf"
      putMVar bar "asdf"
      return ()

      -- solution: enforce ordering for mvar taking (by convention, not statically)

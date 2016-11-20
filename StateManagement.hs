{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | State management API with no new types.
--
-- Inspired by Wai, Redux
module StateManagement where

import Control.Monad.Trans
import Data.IORef
import Control.Monad
import Control.Concurrent

type Reducer m state = state → (state → m ()) → m ()

runApp ∷ ∀ m s. (MonadIO m) ⇒ s → Reducer m s → m ()
runApp initial reducer = do
  ref <- liftIO $ newIORef initial
  let setState ∷ s → m ()
      setState newState = liftIO $ writeIORef ref newState

  forever $ do
    state ← liftIO $ readIORef ref
    reducer state setState

pureAction ∷ ∀ m state. state → (state → m ()) → m ()
pureAction state setState = setState state


main ∷ IO ()
main = runApp 1 $ \state setState → do
  threadDelay (10^6 `div` 60)
  setState (state + 1)
  print (state + 1)

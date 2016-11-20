{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Exploration of using RankNTypes to write a generic type-safe
-- state management API
--
-- Inspired by Wai, redux
module StateManagementExploration where

import Control.Monad.Trans
import Data.IORef
import Control.Monad
import Control.Concurrent

-- * Thought process:

-- | Given old state, return new state
reducer0 ∷ ∀ state. state → state
reducer0 s = s


-- | Given old state, return capability to return new State
reducer1  ∷ ∀ state. state → (∀ b. (state → b) → b)
reducer1 s = \setState → setState s

reducer2  ∷ ∀ state. state → (∀ b. (state → (state, b)) → (state, b))
reducer2 s = \setState -> setState s

reducer3 ∷ ∀ m state. (Monad m) ⇒ state → (∀ b. (state → m b) → m b)
reducer3 s = \setState -> setState s

-- | Given some state, return the capability to set the state
reducer4 ∷ ∀ m state. (Monad m) ⇒ state → (∀ b. (state → m b) → m ())
reducer4 s = \setState -> setState s >> pure ()


-- * I like reducer 4...

-- | A generic reducer. To use, provide a setState.
type ReducerSimple m state =
  state → (∀ b. (state → m b) → m b)

flipSwitchSimple ∷ ∀m. ReducerSimple m Bool
flipSwitchSimple state setState = do
  case state of
    True → setState False
    False → setState True


-- | A generic reducer
type Reducer m state = state → (state → m ()) → m ()

flipSwitch ∷ ∀m. (Monad m) ⇒ Reducer m Bool
flipSwitch state setState = do
  void $ case state of
    True → setState False
    False → setState True

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

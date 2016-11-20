{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}

-- | Safe State management API.
--
-- Inspired by Wai, Redux
module SafeStateManagement where

import Control.Monad
import Control.Monad.Indexed
import Control.Monad.Indexed.State
import Control.Monad.Indexed.Trans
import Control.Monad.IO.Class
import Data.Default
import Prelude hiding ((>>), (>>=), return)
import qualified Language.Haskell.Rebind as Use

type family Π = (r ∷ k → *) | r → k

data SBool ∷ Bool → * where
  STrue ∷ SBool True
  SFalse ∷ SBool False
deriving instance (Show (SBool b))

type instance Π = SBool

type family Not b where
  Not True = False
  Not False = True

sNot ∷ SBool t → SBool (Not t)
sNot STrue = SFalse
sNot SFalse = STrue

type DynState i j a = IxStateT IO i j a

lio :: IO a -> IxStateT IO i i a
lio = ilift . liftIO

main :: IO ()
main = void $ flip runIxStateT STrue $
  let Use.IxMonad{..} = def in do
    imodify sNot
    lio . print =<< iget

    imodify sNot
    lio . print =<< iget

    imodify sNot
    lio . print =<< iget


-- import Control.Monad.Trans
-- import Data.IORef
-- import Control.Monad
-- import Control.Concurrent

-- -- Idea: ensure setState is only called once.

-- type Reducer m state = state → (state → m ()) → m ()

-- runApp ∷ ∀ m s. (MonadIO m) ⇒ s → Reducer m s → m ()
-- runApp initial reducer = do
--   ref <- liftIO $ newIORef initial
--   let setState ∷ s → m ()
--       setState newState = liftIO $ writeIORef ref newState

--   forever $ do
--     state ← liftIO $ readIORef ref
--     reducer state setState

-- pureAction ∷ ∀ m state. state → (state → m ()) → m ()
-- pureAction state setState = setState state



-- main ∷ IO ()
-- main = runApp 1 $ \state setState → do
--   threadDelay (10^6 `div` 60)
--   setState (state + 1)
--   print (state + 1)

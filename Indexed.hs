{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}

import Data.Function

import Control.Monad.Indexed.State
import Control.Monad.Indexed.Trans
import Control.Monad.Indexed
import Control.Monad.IO.Class
import Prelude hiding ((>>), (>>=), return)
import qualified Prelude as P

class Default a where
  def :: a

data UseMonad m =
  UseMonad { return :: forall a. a -> m a
           , (>>=) :: forall a b. m a -> (a -> m b) -> m b
           , (>>) :: forall a b. m a -> m b -> m b
           }
data UseIxMonad (m :: k -> k -> * -> *) =
  UseIxMonad { return :: forall a i. a -> m i i a
             , (>>=) :: forall a b i j k. m i j a -> (a -> m j k b) -> m i k b
             , (>>) :: forall a b i j k. m i j a -> m j k b -> m i k b
             }

instance (Monad m) => Default (UseMonad m) where
  def = UseMonad { return = P.return
                 , (>>=) = (P.>>=)
                 , (>>) = (P.>>)
                 }


instance (IxMonad m) => Default (UseIxMonad m) where
  def = UseIxMonad { return = ireturn
                   , (>>=) = (>>>=)
                   , (>>) = \a b -> a >>>= (\_ -> b)
                   }



type DynState i j a = IxStateT IO i j a

runConstructT :: DynState () j a -> IO j
runConstructT x = snd <$> runIxStateT x ()


lio :: IO a -> IxStateT IO i i a
lio = ilift . liftIO

dynStateExample :: IO ()
dynStateExample =
  let UseIxMonad{..} = def
  in runConstructT $ do
    lio $ putStrLn "hello"
    imodify (const "hello")
    imodify (const ())
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
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE GADTs #-}

-- | Monad telescope
-- | Failed attempt!

module Telescope where

import Data.Function
import Control.Monad.IO.Class
import Prelude hiding ((>>), (>>=), return)
import qualified Prelude as P
import GHC.Types

-- | Polykinded indexed
class IxFunctor f where
  imap :: forall j k a b. (a -> b) -> f j k a -> f j k b

class IxFunctor m => IxMonad m where
  ipure :: a -> m j j a
  ibind :: (a -> m j k b) -> m i j a -> m i k b

infixl 1 >>>=

(>>>=) :: IxMonad m => m i j a -> (a -> m j k b) -> m i k b
m >>>= k = ibind k m

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
  def = UseIxMonad { return = ipure
                   , (>>=) = (>>>=)
                   , (>>) = \a b -> a >>>= (\_ -> b)
                   }


data Telescope (m :: [ Type -> Type ]) (n :: [ Type -> Type ]) a where
  Pure :: forall j a. a -> Telescope j j a
  Cons :: forall f j k a. Monad f => Telescope j k (f a) -> Telescope j (f ': k) a


instance IxFunctor Telescope where
  imap f (Pure x) = Pure (f x)
  imap f (Cons x) = Cons (imap (f<$>) x)


-- class Joinable i j k where
--   ijoin :: Telescope i j (Telescope j k a) -> Telescope i k a

-- instance Joinable j j k where
--   -- ijoin :: Telescope j j (Telescope j k a) -> Telescope j k a
--   ijoin (Pure x) = x

-- instance Joinable j (f ': j) k where
--   -- ijoin :: Telescope j (f ': j) (Telescope (f ': j) k a) -> Telescope j k a
--   ijoin (Cons x) = Cons (ijoin x)


-- joinCons :: Telescope j (f:j) (Telescope (f:j) k a) -> Telescope (f:j) k a
-- I cannot make this into an indexed monad, because you can't just compose monads like that!


-- This will have to be something else.

-- data Telescope' (m :: [ Type -> Type ]) a where
--   Pure :: a -> Telescope' '[] a
--   Cons :: Monad f => Telescope' j (f a) -> Telescope' (f ': j) a


-- class TeleMonad m where
--   ipure :: a -> m j a
--   ibind :: (a -> f b) -> m (f':j) a -> m (f':j) b

-- x >>= (\y -> y >>= g)

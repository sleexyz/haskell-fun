{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# LANGUAGE KindSignatures #-}

module Archer where

import GHC.Types
import Data.Proxy

type Exists (c :: * -> Constraint) = forall b. (forall a. c a => a -> b) -> b

type Exists2 (c :: * -> Constraint) = forall b. (forall a1 a2. c a => (a1, a2) -> b) -> b

-- Exists c -> Exists c -> Exists2 c


data App = App

exists2capture :: Exists Show -> (forall a. Show a => a -> App) -> App
exists2capture thing handle = thing handle





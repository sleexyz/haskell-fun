{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeApplications #-}

module Archer where

import GHC.Types
import Data.Proxy

-- DeMorgan encoding of existential types
newtype Exists p = MkExists {unExists :: forall b. (forall a. p a -> b) -> b }

-- introduction
pack :: forall p a. p a -> Exists p
pack x = MkExists (\f -> f x)

-- elimination
apply :: forall p b. (forall a. p a -> b) -> Exists p -> b
apply f (MkExists k) = k f


packUnpack :: Exists p -> Exists p
packUnpack = apply pack

-- *

data Exists2 p  = MkExists2 {unExists2 :: forall b. (forall a1 a2. p a1 -> p a2 -> b) -> b }

-- introduction
pack2 :: forall p a1 a2. p a1 -> p a2 -> Exists2 p
pack2 x y = MkExists2 (\f -> f x y)

-- elimination
apply2 :: forall p b. (forall a1 a2. p a1 -> p a2 -> b) -> Exists2 p -> b
apply2 f (MkExists2 k) = k f

-- combine :: Exists c -> Exists c -> Exists2 c
-- combine (MkExists x) (MkExists y) = MkExists2 (\f -> f (x id) (y id))

-- data App = App

-- exists2capture :: Exists Show -> (forall a. Show a => a -> App) -> App
-- exists2capture = unExists

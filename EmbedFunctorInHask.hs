{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- contravariant functors...

import GHC.Types
import Prelude hiding (Functor, fmap)
import Data.Proxy

-- This we carry around 
data Embed f = Embed {unEmbed :: forall a. f a -> f a }


type family App k (f :: k) a where
  App (Type) (Embed f) a = f a
  App (Type -> Type) f a = f a


class Functor k (f :: k)  where
  _fmap :: (Proxy k) -> (Proxy f) -> (a -> b) -> App k f a -> App k f b

instance Functor (Type -> Type) [] where
  _fmap _ _ = map

instance Functor (Type) (Embed Maybe) where
  _fmap _ _ f Nothing = Nothing
  _fmap _ _ f (Just x) = Just (f x)


fmap :: forall (f :: Type -> Type) a b. (Functor (Type -> Type) f) => (a -> b) -> f a -> f b
fmap = _fmap (Proxy @(Type -> Type)) (Proxy @f)

fmap' = _fmap (Proxy @Type)

foo = fmap' (Proxy :: Proxy (Embed Maybe)) (+1) Nothing

--class Contravariant f where
--  cfmap :: (a -> b) -> f b -> f a


--instance Functor ((->) t) where
----fmap :: (a -> b) -> (t -> a) -> (t -> b)
--  fmap = (.)

--instance Contravariant ((->) t) where
----contramap :: (a -> b) -> (t -> a) -> (t -> b)
--  cfmap = flip (.)


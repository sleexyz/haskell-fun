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


import GHC.Types
import Prelude hiding (Functor, fmap)
import Data.Proxy


-- Hask as Set?


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

instance Functor (Type) (Embed ((->) a)) where
  _fmap _ _ = (.)


fmap :: forall (f :: Type -> Type) a b.
        (Functor (Type -> Type) f) =>
        (a -> b) -> f a -> f b

-- | Your favorite fmap
fmap = _fmap (Proxy @(Type -> Type)) (Proxy @f)

fmapMaybe  = _fmap (Proxy @Type) (Proxy @(Embed Maybe))

fmapReader :: (a -> b) -> (t -> a) -> t -> b
fmapReader = _fmap (Proxy @Type) (Proxy @(Embed ((->) _))) 


-- class Contravariant k f where
--   contramap :: (a -> b) -> f b -> f a


--instance Functor ((->) t) where
----fmap :: (a -> b) -> (t -> a) -> (t -> b)
--  fmap = (.)

--instance Contravariant ((->) t) where
----contramap :: (a -> b) -> (t -> a) -> (t -> b)
--  cfmap = flip (.)


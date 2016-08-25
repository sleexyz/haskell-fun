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
{-# LANGUAGE ScopedTypeVariables #-}

-- It works!
-- I even have contravariant functors without newtyping!
-- The issue is that one needs to provide a proof of a kind


-- But there is a pattern here! Provide different proxies and typeclasses can behave differently!

import GHC.TypeLits
import GHC.Types
import Prelude hiding (Functor, fmap)
import Data.Proxy

data Indirection = Indirection_ (Type -> Type)
data Curried a = Curried_ (Type -> Type -> Type) a
data FlipCurried b = FlipCurried_ (Type -> Type -> Type) b

type family App k (f :: k) a = r | r -> k f where
  App Type (Proxy f) a = (Proxy Type, f a)
  App (Type -> Type) f a = (Proxy (Type -> Type), f a)
  App (Indirection) (Indirection_ f) a = (Proxy Indirection, f a)
  App (Curried Type) (Curried_ f a) b = (Proxy (Curried Type), f a b)
  App (FlipCurried Type) (FlipCurried_ f b) a = (Proxy (FlipCurried Type), f a b)


class Functor k (f :: k)  where
  _fmap :: (a -> b) -> App k f a -> App k f b

instance Functor (Type -> Type) [] where
  _fmap f (p, x) = (p, map f x)

instance Functor (Type) (Proxy []) where
  _fmap f (p, x) = (p, map f x)

instance Functor (Indirection) (Indirection_ []) where
  _fmap f (p, x) = (p, map f x)

instance Functor (Type -> Type) ((->) a) where
  _fmap f (p, x)  =  (p, f . x)

instance Functor (Curried Type) (Curried_ (->) a) where
  _fmap f (p, x)  =  (p, f . x)



fmap f x  = snd $ _fmap f (Proxy :: Proxy (Type -> Type), x)
fmap' f x = snd $ _fmap f (Proxy :: Proxy Type, x)
fmapIn f x = snd $ _fmap f (Proxy :: Proxy Indirection, x)
fmapCurried f x = snd $ _fmap f (Proxy :: Proxy (Curried Type), x)


foo = fmap  (+1) [1..100]
bar = fmap' (+1) [1..100]
baz = fmapIn (+1) [1..100]

foo2 = fmap (+1) (+2) 0
foo3 = fmapCurried (+1) (+2) 0



class Contravariant k (f :: k)  where
  _contramap :: (a -> b) -> App k f b -> App k f a

instance Contravariant (FlipCurried Type) (FlipCurried_ (->) b) where
  _contramap f (p, x)  =  (p, x . f)

contramapFC f x = snd $ _contramap f (Proxy :: Proxy (FlipCurried Type), x)


covariantFoo     = fmap        (*0) (+2) 10 -- 0
contravariantFoo = contramapFC (*0) (+2) 10 -- 2

{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}


data NatTrans f g = NatTrans {unNatTrans :: forall b. f b -> g b }


rep :: (Functor f) => f a -> NatTrans ((->) a) f
rep x = NatTrans (<$>x)


unrep :: (Functor f) => NatTrans ((->) a) f -> f a
unrep y_a = unNatTrans y_a $ id


-- class Isomorphic k where
--   isomorphic :: (a -> b) -> (b -> a) -> k a b

-- instance Isomorphic (->) where
--   isomorphic f _ = f

-- data Isomorphism a b = Isomorphism (a -> b) (b -> a)

-- instance Isomorphic Isomorphism where
--   isomorphic = Isomorphism

-- from :: Isomorphic k => Isomorphism a b -> k b a
-- from (Isomorphism f g) = isomorphic g f


-- yoneda :: Functor f => Isomorphism (f a) (NatTrans ((->) a) f)
-- yoneda = Isomorphism rep unrep

class Profunctor p where
  dimap :: (a -> b) -> (s -> t) -> p b s -> p a t


data Const a b = Const b
instance Profunctor Const where
  dimap _ f (Const b) = Const (f b)

instance Profunctor (->) where
  dimap ab st = \f -> st . f  . ab

type Simple f s a = f s s a a

type Optic p s t a b = p a b -> p s t
type OpticP p s a = Optic p s s a a



type Iso s t a b = forall p. (Profunctor p) => Optic p s t a b
type IsoP s a = Iso s s a a

iso :: (s -> a) -> (b -> t) -> Iso s t a b
iso = dimap


isoP :: (s -> a) -> (a -> s) -> IsoP s a
isoP = iso



yonedaIso :: (Functor f) => IsoP (NatTrans ((->) a) f) (f a)
yonedaIso = isoP unrep rep

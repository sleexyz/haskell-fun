{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE PatternSynonyms#-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeInType #-}

import Prelude hiding (Monoid, (+), (*), Functor, map)
import qualified Prelude as P
import GHC.Types

-- life without typeclasses :)


infixl 8 &
x & f = f x

class Default a where
  def :: a

data Semigroup s =
  Semigroup { (+) :: s -> s -> s }

data Monoid m =
  Monoid { (+) :: m -> m -> m
         , zero :: m
         }

data Semiring m =
  Semiring { (+) :: m -> m -> m
           , zero :: m
           , (*) :: m -> m -> m
           , one :: m
           }


-- pattern SR a b c d =  Semiring a b c d


semigroupToMonoid :: Semigroup s -> s -> Monoid s
semigroupToMonoid Semigroup{..} s =
  Monoid { (+)
         , zero = s
         }

semigroupToSemiring  :: Monoid m -> Monoid m -> Semiring m
semigroupToSemiring add mul =
  Semiring { (+) = let Monoid{..} = add in (+)
           , zero = let Monoid{..} = add in (zero)
           , (*) = let Monoid{..} = mul in (+)
           , one = let Monoid{..} = mul in zero
           }

cayleyRep :: Monoid m -> Monoid (m -> m)
cayleyRep Monoid{..} =
  Monoid { (+) = (.)
         , zero = (zero+)
         }



additive = Monoid (P.+) 0
multiplicative = Monoid (P.*) 1
num = semigroupToSemiring additive multiplicative

freem = Monoid { (+) = (++), zero = [] }
dlist = cayleyRep freem


data Functor f = Functor { map :: forall a b. (a -> b) -> f a -> f b }

rep :: Functor ((->) r)
rep = Functor { map = \f x -> f . x }



data Hom k (f:: k -> k -> Type) (a :: k) (b :: k) =
  Hom { hom :: f a b }

data Eta f g = Eta {eta :: forall a. f a -> g a}

type Hom1 a b = Hom Type (->) a b
type Hom2 f g = Hom (Type -> Type) Eta f g

data NatTrans f g  =
  NatTrans { source :: Functor f
           , target :: Functor g
           , eta :: Eta f g
           }

data Iso k (f :: k -> k -> Type) (a :: k) (b :: k) =
  Iso { to :: f a b
      , from :: f b a
      }
type Iso1 a b = Iso Type (->) a b
type Iso2 f g = Iso(Type -> Type) Eta f g

flipIso :: Iso k f a b -> Iso k f b a
flipIso Iso{..} = Iso { to = from, from = to }


-- covariant yoneda lemma
yoneda :: Functor f -> Iso1 (f a) (NatTrans ((->) a) f)
yoneda functor =
  Iso { to = \fa ->
          NatTrans { source = rep
                    , target = functor
                    , eta =
                        let
                          Functor{map} = functor
                        in
                          Eta $ \h ->  map h fa
                    }
       , from = \NatTrans {eta} ->
                  let (Eta f) = eta
                  in f id
       }




-- here's a corollary :)
yoneda' :: Iso1 (b -> a) (Eta ((->)a) ((->)b))
yoneda' =
  let
    Iso{..} = yoneda rep
  in
    Iso { to = \ra ->
            let NatTrans{eta} = to ra in eta
        , from = \eta ->
            from NatTrans { source = rep
                          , target = rep
                          , eta
                          }
        }


-- Something to do with enriched categories?
data Category k (rep :: k -> k -> Type) =
  Category { identity :: forall (a :: k). rep a a
           , (<<<) :: forall (a :: k) (b :: k) (c :: k). rep b c -> rep a b -> rep a c
           }

-- Hask
-- object : Type
-- morphism : terms
category1 :: Category Type (->)
category1 = Category { identity = id, (<<<) = (.) }

-- functor category [Hask, Hask]
-- objects : endofunctors
-- morphisms : natural transformations
category2 :: Category (Type -> Type) (Eta)
category2 = Category { identity = Eta id, (<<<) = \(Eta g) (Eta f) -> Eta (g . f) }




-- this one contains the proof of functorality as well
-- strictly weaker than having the proof of g and f
-- is having a dependency of g on f

-- useful if your functors are generative.

data NatTrans' f g  =
  NatTrans' { transF :: Functor f -> Functor g
            , eta :: Eta f g
            }

category2' :: Category (Type -> Type) (NatTrans')
category2' =
  Category { identity = NatTrans' {transF = id, eta = Eta id }
           , (<<<) = \alpha beta ->
               let
                 _g2h = let NatTrans'{..} = alpha in transF
                 _f2g = let NatTrans'{..} = beta in transF

                 Eta g2h = let NatTrans' {..} = alpha in eta
                 Eta f2g = let NatTrans'{..} = beta in eta
               in
               NatTrans' { transF = _g2h . _f2g
                         , eta = Eta (g2h . f2g)
                         }
           }

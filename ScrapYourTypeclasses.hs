{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ScrapYourTypeclasses where

import Prelude hiding (Monoid)


data Person = Person { address :: String
                     , age :: Int 
                     }

me = Person "foo" 20

greeting = "I live in " ++ address
  where  Person(..) = me





data Monoid a = Monoid { zero :: a
                       , (<>) :: a -> a -> a 
                       }









data Semiring a = Semiring { zero :: a
                           , one  :: a
                           , (+.) :: a -> a -> a
                           , (*.) :: a -> a -> a
                           }

data SemiringFromMonoids a = SemiringFromMonoids { additive :: Monoid a
                                                 , multiplicative :: Monoid a
                                                 }
data Iso foo bar = Iso { from :: foo -> bar
                       , to :: bar -> foo
                       }

isoSemiring = Iso { from = from
                  , to = to
                  }
  where 
    from x = Semiring { zero = z
                      , one = o
                      , (+.) = add
                      , (*.) = mul
                      }
      where
        SemiringFromMonoids {..} = x
        Monoid {zero=z, (<>)=add} = additive
        Monoid {zero=o, (<>)=mul} = multiplicative 
    to y = SemiringFromMonoids { additive = additive
                               , multiplicative = multiplicative
                               }
      where
        Semiring {..} = y
        additive = Monoid { zero = zero
                          , (<>) = (+.)
                          }
        multiplicative = Monoid { zero = one
                                , (<>) = (*.)
                                }
      

data Prelude a = Prelude { _Person :: Person
                         , _Monoid :: Monoid a
                         }



me = Person {address = "315 Seigel", age = 20}

addMon :: Num a => Monoid a
addMon = Monoid {zero = 0, (<>) = (+)}

mulMon :: Num a => Monoid a
mulMon = Monoid {zero = 1, (<>) = (*)}

_Prelude = Prelude { _Person = me
                   , _Monoid = addMon
                   }

-- | Chaining!
foo = zero <> zero
  where
    Prelude {..} = _Prelude
    Monoid {..} = _Monoid
    Person {..} = _Person

-- | Functors!
bar :: Int
bar = zero +. one +. 100
  where
    Semiring {..} = from isoSemiring $ SemiringFromMonoids {additive=addMon, multiplicative=mulMon}

{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE KindSignatures #-}

import Data.Proxy
import GHC.TypeLits
import GHC.Types

data Hole (a :: Nat)

data Answer = Nat :== Type
data a :& b

data Run (o :: [Answer]) = Run
  deriving Show

-- | The typeclass forces the unification
class Unified o
instance Unified '[]
instance Unified xs => Unified (x:xs)

run :: (Unified o) => Run o
run = Run

type family x / gamma where
  (x :== v) / '[] = '[]
  (x :== v) / ((x :== v):xs) = (x :== v) xs
  -- Substitute (x :- v) (S a:xs) = Substitute (x :- v) a:xs

type family as <> bs where
  '[ ] <> bs = bs
  (a : as) <> bs = a : (as <> bs)


data Dog
type family IsDog a where
  IsDog Dog = '[]
  IsDog (Hole x) = '[x :== Dog]


data Nil
data a :> b

type family List a where
  List Nil = '[]
  List (Hole x)= '[x :== Nil]

  List (a :> b) = List b

data Z
data S a
type family N a where
  N Nil = '[]
  N (S a) = N a

infixr :>
type MyList = () :> () :> Nil

type Apply s a = s a

type family Length l n where
  Length Nil Z = '[]
  Length Nil (Hole x) = '[x :== Z]

  Length (a :> b) (S a) = Length b a
  Length (a :> b) (Hole x) =  Length (b) (Hole (x+1)) <> (Hole x := S (Hole (x+1)))


type family a := b where
  a := a = '[]
  a := (Hole x) = '[x :== a]

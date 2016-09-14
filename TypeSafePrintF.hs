{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeApplications #-}

-- | typesafe printf in haskell
--
-- who needs idris lol
--
-- we didn't even need -XTypeInType :p

import GHC.TypeLits
import GHC.Types
import Data.Proxy


-- | Type level token
data Token = Var Type
           | Const Symbol

type family Tokenize (s :: [Symbol]) :: ([Token]) where
  Tokenize ("%i" ': xs) = Var Int ': Tokenize xs
  Tokenize ("%s" ': xs) = Var String ': Tokenize xs
  Tokenize (x ': xs) = Const x ': Tokenize xs
  Tokenize ('[]) = '[]



-- | Arbitrarily sized products encoded in a GADT.
--
-- aka an HList.
--
data HList (k :: [Type]) where
  Nil :: HList '[]
  (:+) :: x -> HList xs -> HList (x ':xs)


-- | Now we need a way to generate a function
-- at both the term and type level
-- that grows as we recurse down
-- our typelevel list of tokens
class Parseable (s :: [Token]) where
  type Needs s :: [Type]
  printf'' :: Proxy s -> HList (Needs s) -> String


instance Parseable ('[]) where
  type Needs '[] = '[]
  printf'' _ x = ""

instance (KnownSymbol str, Parseable as) => Parseable (Const str ': as) where
  type Needs (Const str ': as) = Needs as
  printf'' _ x = symbolVal (Proxy :: Proxy str)
               ++ printf'' (Proxy :: Proxy as) x

instance {-# OVERLAPS #-} (Parseable as) => Parseable (Var String ': as) where
  type Needs (Var String ': as) = (String ': (Needs as))
  printf'' _ (x :+ xs) = x
                       ++ printf'' (Proxy :: Proxy as) xs

instance (Show a, Parseable as) => Parseable (Var a ': as) where
  type Needs (Var a ': as) = (a ': (Needs as))
  printf'' _ (x :+ xs) = show x
                       ++ printf'' (Proxy :: Proxy as) xs




-- | Uncurried version.
printf' :: forall l. (Parseable (Tokenize l)) => Proxy l -> HList (Needs (Tokenize l)) -> String
printf' _ = printf'' (Proxy :: Proxy (Tokenize l))


-- | Can we generalize currying for arbitrarily sized products?
--
-- Here's plain old currying:
curry' :: ((a, b) -> c) -> (a -> b -> c)
curry' f head = g
  where
    g tail = f (head, tail)


class Curryable k o where
  type Curried k o
  genCurry :: (HList k -> o) -> Curried k o


-- | base case
instance Curryable '[] o where
  type Curried '[] o = o
  genCurry f = f Nil

-- | inductive case
instance (Curryable as o) => Curryable (a ': as) o where
  type Curried (a ': as) o = (a -> Curried as o)
  genCurry f head = genCurry g
    where
      g tail = f (head :+ tail)




-- | Type-safe printf in Haskell!
printf :: forall l. (Parseable (Tokenize l), Curryable (Needs (Tokenize l)) String) => Curried (Needs (Tokenize l)) String
printf = genCurry (printf' (Proxy @l))



cool :: String
cool  = printf @'["cool mayn"]

greet :: String -> String
greet = printf @'["hello ", "%s", "!"]

tellAge :: String -> Int -> String
tellAge = printf @'["%s", " is ", "%i", " years old."]

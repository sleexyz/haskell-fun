{-# LANGUAGE TypeInType #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE LambdaCase #-}


import GHC.Types
import GHC.TypeLits
import Data.Proxy


data Sum (l :: [k]) where
  Pass :: Sum l -> Sum (b:l)
  Any :: Sum l
  Put :: a -> Sum l -> Sum (a:l)



class CanPut l a where
  put :: a -> Sum l

instance CanPut (a:l) a where
  put x = Put x Any
instance {-# INCOHERENT #-} (CanPut l a) => CanPut (b:l) a where
  put x = Pass (put x)


class Has l a where
  get :: Proxy a -> Sum l -> Maybe a

instance Has (a:xs) a where
  get _ (Put x _) = Just x
  get _ _  = Nothing

instance {-# INCOHERENT #-} (Has xs a) => Has (b:xs) a where
  get p (Pass x)  = get p x
  get _ _ = Nothing

foo1 :: Sum '[String]
foo1 = put "hello"

foo2 :: Sum '[String, Int]
foo2 = put "hello"

foo3 :: Sum '[Int, String]
foo3 = put "hello"

foo4 :: Sum '[Int, String]
foo4 = put (1 :: Int)


type family (xs :: [k]) :< (ys :: [k]) :: Constraint where
  '[] :< ys = ()
  (x:xs) :< ys = (Has ys x, xs :< ys)


pattern Is x <- (get (Proxy :: Proxy a) -> Just x)


-- | Example:

getString = \case
  Is (x :: Int) -> show x
  Is (x :: String) -> x
  _ -> "error"


-- | fields

data (s :: Symbol) ~> t = Field t
instance (KnownSymbol s, Show t) => Show (s ~> t) where
  show (Field t) = symbolVal (Proxy @s) ++ ": " ++ show t

-- colors :: Sum [ "red" ~> String
--               , "blue" ~> String
--               , "blue" ~> String
--               ]

colors :: Sum (("red" ~> String):k)
colors = put (Field @"red" "hello")

-- colorstoString = case colors of
--   Is (Field x :: "red" ~> String) -> x
  -- Is (Field x :: "blue" ~> String) -> x

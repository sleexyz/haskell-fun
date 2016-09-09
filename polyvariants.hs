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
{-# LANGUAGE PartialTypeSignatures #-}


import GHC.Types
import GHC.TypeLits
import Data.Proxy


data Sum (l :: [Type]) where
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


type family (xs :: [Type]) :< (ys :: [Type]) :: Constraint where
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


type RGBField = "rgb" ~> (Int, Int, Int)
type RGBAField = "rgba" ~> (Int, Int, Int, Int)
type Colors = Sum [RGBField, RGBAField]


-- Input type cannot be inferred.
-- I wish I could have defaulting rules with type parameters

red :: Colors
red = put (Field @ "rgb" (1 :: Int, 0 :: Int, 0 :: Int))


-- Output types can be inferred!

colorsToString = case red of
  Is (Field x :: RGBField) -> show x
  Is (Field x :: RGBAField) -> show x

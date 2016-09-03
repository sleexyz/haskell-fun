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
import Data.Proxy

data V (l :: [Type]) where
  Pass :: V l -> V (b:l)
  Any :: V l
  Put :: a -> V l -> V (a:l)


class CanPut l a where
  put :: a -> V l
instance CanPut (a:l) a where
  put x = Put x Any
instance {-# INCOHERENT #-} (CanPut l a) => CanPut (b:l) a where
  put x = Pass (put x)

class Has l a where
  get :: Proxy a -> V l -> Maybe a

instance Has (a:xs) a where
  get _ (Put x _) = Just x
  get _ _  = Nothing

instance {-# INCOHERENT #-} (Has xs a) => Has (b:xs) a where
  get p (Pass x)  = get p x
  get _ _ = Nothing

foo1 :: V '[String]
foo1 = put "hello"

foo2 :: V '[String, Int]
foo2 = put "hello"

foo3 :: V '[Int, String]
foo3 = put "hello"

foo4 :: V '[Int, String]
foo4 = put (1 :: Int)


type family (xs :: [Type]) :< (ys :: [Type]) :: Constraint where
  '[] :< ys = ()
  (x:xs) :< ys = (Has ys x, xs :< ys)


pattern Is x <- (get (Proxy :: Proxy a) -> Just x)





getString :: '[String, Int] :< l => V l -> String
getString = \case
  Is (x :: Int) -> show x
  Is (x :: String) -> x
  _ -> "error"


-- data (s :: Symbol) ~> 
-- pattern Is' x <- (get (Proxy :: Proxy a) -> Just (x))

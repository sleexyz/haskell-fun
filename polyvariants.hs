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

import GHC.Types
import Data.Proxy

data V (l :: [Type]) where
  ConsH :: V l -> V (b:l)
  Tail :: V l
  Cons :: a -> V l -> V (a:l)


class Puttable l a where
  put :: a -> V l
instance Puttable (a:l) a where
  put x = Cons x Tail
instance {-# INCOHERENT #-} (Puttable l a) => Puttable (b:l) a where
  put x = ConsH (put x)

class Gettable l a where
  get :: Proxy a -> V l -> Maybe a

instance Gettable (a:xs) a where
  get _ (Cons x _) = Just x
  get _ _  = Nothing

instance {-# INCOHERENT #-} (Gettable xs a) => Gettable (b:xs) a where
  get p (ConsH x)  = get p x
  get _ _ = Nothing

foo1 :: V '[String]
foo1 = put "hello"

foo2 :: V '[String, Int]
foo2 = put "hello"

foo3 :: V '[Int, String]
foo3 = put "hello"


getString :: (Gettable l String) => V l -> String
getString (get (Proxy @String) -> Just x) = x

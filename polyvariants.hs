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

import GHC.Types
import Data.Proxy


-- | Non-empty list

infixr 5 :+
data Nel a = Head a | a :+ (Nel a)


data V (l :: Nel Type) where
  ConsH :: V l -> V (b :+ l)
  Tail :: V l

  In :: a -> V (Head a)
  Cons :: a -> V l -> V (a :+ l)


class Puttable l a where
  put :: a -> V l

instance Puttable (Head a) a where
  put x = In x
instance Puttable (a :+ l) a where
  put x = Cons x Tail
instance {-# INCOHERENT #-} (Puttable l a) => Puttable (b :+ l) a where
  put x = ConsH (put x)

class Gettable l a where
  get :: Proxy a -> V l -> a

instance Gettable (Head a) a where
  get _ (In x) = x
instance Gettable (a :+ xs) a where
  get _ (Cons x _) = x
instance {-# INCOHERENT #-} (Gettable xs a) => Gettable (b :+ xs) a where
  get p (ConsH x)  = get p x

foo1 :: V (Head String)
foo1 = put "hello"

foo2 :: V (String :+ Head Int)
foo2 = put "hello"

foo3 :: V (Int :+ Int :+ Head String)
foo3 = put "hello"

getString :: (Gettable l String) => V l -> String
getString = get Proxy

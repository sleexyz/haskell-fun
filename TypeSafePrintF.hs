{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

import GHC.OverloadedLabels (IsLabel)
import GHC.TypeLits
import Data.Type.Set
import GHC.Types


data Token = Var Type
           | Const Symbol

type family Parse (s :: [Symbol]) :: ([Token]) where
  Parse ("%" ': "i" ': xs) = Var Int ': Parse xs
  Parse ("%" ': "s" ': xs) = Var String ': Parse xs
  Parse (x ': xs) = Const x ': Parse xs
  Parse ('[]) = '[]

-- type family ToString (ts :: [Token]) where
--   ToString (Const x : xs)

data HList (k :: [Type]) where
  Nil :: HList '[]
  (:+) :: forall x xs. x -> HList xs -> HList (x ':xs)



class Printable (s :: [Token]) where
  type Needs s :: [Type]
  _printf :: Proxy s -> HList (Needs s) -> String


instance Printable ('[]) where
  type Needs '[] = '[]
  _printf _ x = ""

instance (KnownSymbol a, Printable as) => Printable (Const a ': as) where
  type Needs (Const a ': as) = Needs as
  _printf _ x = symbolVal (Proxy :: Proxy a)
               ++ _printf (Proxy :: Proxy as) x

instance (Show a, Printable as) => Printable (Var a ': as) where
  type Needs (Var a ': as) = (a ': (Needs as))
  _printf _ (x :+ xs) = show x
                       ++ _printf (Proxy :: Proxy as) xs


poo :: Proxy ["hello ", "%", "i", " world"]
poo = poo


printf :: forall l. (Printable (Parse l)) => Proxy l -> HList (Needs (Parse l)) -> String
printf _ = _printf (Proxy :: Proxy (Parse l))

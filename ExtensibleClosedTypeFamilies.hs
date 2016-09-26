{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeOperators #-}

module ExtensibleClosedTypeFamilies where

import GHC.Types
import GHC.TypeLits
import Data.Proxy
import Test.Hspec

type family DemoteFail (k :: Type) where
  DemoteFail k = TypeError (
                             Text ""
                             :$$: Text ""
                             :$$: Text "hey mayn cant demote "
                             :<>: ShowType k
                             :$$: Text "yet, srry..."
                             :$$: Text ""
                             :$$: Text ""
                           )

-- | We use a fallback
type family Demote' (fb :: Type) (k :: Type) where
  Demote' fb Nat = Int
  Demote' fb Symbol = String
  Demote' fb (Either a b) = Either (Demote' fb a) (Demote' fb b)
  Demote' fb a = fb


data Foo a = F a

type family MyDemote (fb :: Type) (k :: Type) where
  MyDemote fb (Foo a) = Foo (Demote a)
  MyDemote fb a = fb

type Demote k = Demote' (MyDemote (DemoteFail k) k) k


canTypeCheck = Proxy @(Demote (Foo Nat))

failsToTypeCheck = Proxy @(Demote (Maybe Int))

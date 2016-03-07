{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}

module RowPoly where

import GHC.TypeLits
import Data.Proxy


-- | Normal record:
-- data NormalPerson = NormalPerson { firstName :: String
--                                  , lastName :: String
--                                  }

-- | That desugars to:
data NormalPerson' where
  NormalPerson' :: String -> String -> NormalPerson'

firstName' :: NormalPerson' -> String
firstName' (NormalPerson' str _) = str

lastName' :: NormalPerson' -> String
lastName' (NormalPerson' _ str) = str



-- | now for the row polymorphism!
-- | but first, datakinds

data MonoKindedProxy (r :: [Symbol])  :: *
  where
    Singleton :: MonoKindedProxy r

example1 = Singleton :: MonoKindedProxy '["hello", "world"]
example2 = Singleton :: MonoKindedProxy '["goodbye", "world"]



-- | Data.Proxy is equivalent to Proxy, except Data.Proxy is polykinded
-- | Aka generic on Kinds
-- | So we'll use Data.Proxy from now on

-- Existential types!
-- We hide key via a Proxy, and then an existential type
-- data Key :: *
--   where
--     Key :: forall (key :: Symbol). Proxy key -> Key


-- lol dependent hack
data Key (sym:: Symbol) where
  MkKey :: String -> Key sym


instance Show (Key sym) where
  show (MkKey str) = str

firstName = MkKey "firstName" :: Key "firstName"
lastName = MkKey "lastName" :: Key "lastName"




data Entry (key :: Symbol) (value :: *) :: *
  where
    MkEntry :: Key key -> value -> Entry key value

instance (Show value) => Show (Entry key value) where
  show (MkEntry key val) = show key ++ ": "++  show val

getVal :: forall (key :: Symbol) (value :: *). Entry key value -> value
getVal (MkEntry _ val) = val


testEntry1 = MkEntry (MkKey "firstName" :: Key "firstName") "Sean"
testEntry2 = MkEntry (MkKey "lastName" :: Key "lastName") "Lee"

-- getVal testEntry1 == "Sean"
-- getVal testEntry2 == "Lee"


-- | What the fuck, entries is kind list of types!
-- | value-level list type are polytypic type constructor (* -> *)
-- | type-level list kind are polykinded kind constructor (k -> k)


data Record (entries :: [*] ) where

  MkSingleRowRecord :: forall
              (k :: Symbol) (v :: *)
              . Record '[Entry k v ]

  MkDoubleRowRecord :: forall
               (k1 :: Symbol) (v1 :: *)
               (k2 :: Symbol) (v2 :: *)
               . Record '[Entry k1 v1, Entry k2 v2]




singlerow = MkSingleRowRecord :: Record '[  Entry "firstName" String ]

doublerow = MkDoubleRowRecord :: Record '[  Entry "firstName" String
                                         ,  Entry "lastName" String
                                         ]


-- | How do I make this extensible?


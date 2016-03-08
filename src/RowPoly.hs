{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}

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


-- | How do I make it so that I don't have to manually connect between universes?
class    Reifiable (n :: Symbol) where reify :: Key n
instance Reifiable ("firstName" :: Symbol)   where reify = MkKey ("firstName" :: String)
instance Reifiable ("lastName" :: Symbol)    where reify = MkKey ("lastName" :: String)



data Entry (key :: Symbol) (value :: *) :: *
  where
    MkEntry :: Key key -> value -> Entry key value

instance (Show value) => Show (Entry key value) where
  show (MkEntry key val) = show key ++ ": "++  show val

getVal :: forall (key :: Symbol) (value :: *). Entry key value -> value
getVal (MkEntry _ val) = val


testEntry1 = MkEntry (reify :: Key "firstName") "Sean"
testEntry2 = MkEntry (reify :: Key "lastName")  "Lee"

-- getVal testEntry1 == "Sean"
-- getVal testEntry2 == "Lee"


-- | What the fuck, entries is kind list of types!
-- | value-level list type are polytypic type constructor (* -> *)
-- | type-level list kind are polykinded kind constructor (k -> k)


data ClosedRecord (entries :: [*] ) where

  MkSingleRowRecord :: forall
              (k :: Symbol) (v :: *)
              . ClosedRecord '[Entry k v ]

  MkDoubleRowRecord :: forall
               (k1 :: Symbol) (v1 :: *)
               (k2 :: Symbol) (v2 :: *)
               . ClosedRecord '[Entry k1 v1, Entry k2 v2]




singlerow = MkSingleRowRecord :: ClosedRecord '[  Entry "firstName" String ]

doublerow = MkDoubleRowRecord :: ClosedRecord '[  Entry "firstName" String
                                               ,  Entry "lastName" String
                                               ]


-- | How do I make this extensible? Recursive definition?

data Record (r :: [*] ) where
  Record :: forall (k :: Symbol) (v :: *) (r :: [*])
              . Record ((Entry k v) ': r)

deriving instance Show (Record r)

openRec :: forall (r :: [*]). Record (Entry "firstName" String ': r)
openRec = Record

closedRec :: forall (r :: [*]). Record (Entry "firstName " String ': '[])
closedRec = Record


-- F-Algebra this shit?

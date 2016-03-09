{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UnicodeSyntax #-}

module RowPoly where

import GHC.TypeLits
import Data.Proxy
import Data.Type.Equality



type family Find (a :: *) (row :: [*]) where
  Find a '[] = 'False
  Find a (a ': row) = 'True
  Find a (b ': row) = Find a row



data Record (row :: [*])  where
  Nil    :: Record '[]

  Add    :: ∀ (row :: [*]) (a :: *). ( Show a
                                     , Find a row ~ 'False
                                     )
            => a -> Record row -> Record (a ': row)

deriving instance Show (Record row)



-- | Returns (head entry, rest of record)
remove :: ∀ (row :: [*]) (a :: *). ( Show a
                                   , Find a row ~ 'False
                                   )
          => Record (a ': row) -> (a, Record row)

remove (Add x record) = (x, record)


class (Find a row ~ 'True) => Has a row where
  get :: Record row -> a


instance {-# OVERLAPPING #-} ( Show a
                             , Find a row ~ 'False
                             )
                             => Has a (a ': row) where
  get = fst . remove

instance {-# OVERLAPPING #-} ( Show b
                             , Find b row ~ 'False
                             , Find a (b ': row) ~ 'True
                             , (a == b) ~ 'False
                             , Has a row
                             )
                             => Has a (b ': row) where
  get = get . snd . remove





test :: Record '[String, Bool]
test = Add "hello" $ Add True Nil

-- > get test :: String
-- "hello"

-- > get test :: Bool
-- True

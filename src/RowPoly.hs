{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module RowPoly where

import GHC.TypeLits
import Data.Proxy
import Data.Type.Equality

-- | Arrgh how do I project the elements?

-- data List a = Nil | Cons a (List a)
-- type Row = List (*)


type family Find (a :: *) (r :: [*]) where
  Find a '[] = 'False
  Find a (a ': r) = 'True
  Find a (b ': r1) = Find a r1

data Record (r :: [*])  where
  Nil    :: Record '[]

  Add    :: forall (r :: [*]) (a :: *).
            (Find a r ~ 'False, Show a) =>
            a -> Record r -> Record (a ': r)

  -- Remove :: forall (r :: [*]) (a :: *).
  --           (Find a r ~ 'False, Show a) =>
  --           Record (a ': r) -> Record r

deriving instance Show (Record r)

remove :: forall (r :: [*]) (a :: *).
          (Find a r ~ 'False, Show a) =>
          Record (a ': r) -> (a, Record r)

remove (Add x record) = (x, record)

select ::  forall (r :: [*]) (a :: *).
           (Find a r ~ 'False, Show a) =>
           Record (a ': r) -> a

select = fst . remove


-- foo :: forall (r :: [*]) (a :: *) (b :: *). 
--        (Find a r ~ 'False, Show a, Show b) =>
--        Record (b ': r) -> a :~: b -> a


class (Find a r ~ 'True) => Has a r where
  get :: Record r -> a

-- How do I avoid OVERLAPPING?

instance {-# OVERLAPPING #-} (Show a, Find a r0 ~ 'False) => Has a (a ': r0) where
  get = select

instance {-# OVERLAPPING #-} (Show b, Find b r0 ~ 'False, Find a (b ': r0) ~ 'True, (a == b) ~ 'False, Has a r0) => Has a (b ': r0) where
  get = get . snd . remove



test :: Record '[String, Bool]
test = Add "hello" $ Add True Nil

-- > get test :: String
-- "hello"

-- > get test :: Bool
-- True

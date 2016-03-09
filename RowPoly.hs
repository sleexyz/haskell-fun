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
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE InstanceSigs #-}

module RowPoly where

import GHC.TypeLits
import GHC.Prim
import Data.Proxy
import Data.Type.Equality



type family Find (a :: *) (row :: [*]) where
  Find a '[] = 'False
  Find a (a ': row) = 'True
  Find a (b ': row) = Find a row


-- | Idempotent remove
type family Remove (a :: *) (row :: [*]) where
  Remove a '[] = '[]
  Remove a (a ': row) = row
  Remove a (b ': row) = b ': Remove a row




data Record (row :: [*])  where
  Nil    :: Record '[]

  Add    :: ∀ (row :: [*]) (a :: *). ( Show a
                                     , Find a row ~ 'False
                                     )
            => a -> Record row -> Record (a ': row)

  -- Cover   :: ∀ (row :: [*]) (a :: *) (row_ :: [*]). ( row ~ (a ': row_)
  --                                                   )
  --           => Record (a ': row_) -> Record row

  -- Uncover :: ∀ (row :: [*]) (a :: *) (row_ :: [*]). ( row ~ (a ': row_)
  --                                                   )
  --           => Record row -> Record (a ': row)

-- I need a hide and show!

deriving instance Show (Record row)



-- | Returns (head entry, rest of record)
remove :: ∀ (row :: [*]) (a :: *). ( Show a
                                   , Find a row ~ 'False
                                   )
          => Record (a ': row) -> (a, Record row)

remove (Add x record) = (x, record)


class (Find a row ~ 'True) => Has a row where
  get    :: Record row -> a

  delete :: ( Find a row_ ~ 'False
            , Remove a row ~ row_
            )
            => Proxy a -> Record row -> Record row_


instance {-# OVERLAPPING #-} ( Show a
                             , Find a row ~ 'False
                             )
                             => Has a (a ': row) where

  get :: Record (a ': row) -> a
  get = fst . remove

  delete :: Proxy a -> Record (a ': row) -> Record row
  delete _ = snd . remove

  -- I need a proof that my types are equal, and then safe cast!

instance {-# OVERLAPPING #-} ( Show b
                             , Find b row ~ 'False
                             , Find a (b ': row) ~ 'True
                             , (a == b) ~ 'False
                             , Has a row
                             )
                             => Has a (b ': row) where
  get = get . snd . remove
  delete = undefined




test :: Record '[Bool, String]
test = Add True $ Add "hello" Nil


deleteBool :: ( Find Bool row_ ~ 'False
              , Remove Bool row ~ row_
              , Has Bool row
              )
              => Record row -> Record row_
deleteBool = delete (Proxy :: Proxy Bool)

-- > get test :: String
-- "hello"

-- > get test :: Bool
-- True

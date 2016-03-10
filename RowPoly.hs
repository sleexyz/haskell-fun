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
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module RowPoly where

import GHC.TypeLits
import GHC.Prim
import Data.Proxy
import Data.Type.Equality
import Data.Type.Bool



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


deriving instance Show (Record row)



-- | Returns (head entry, rest of record)
remove :: ∀ (row :: [*]) (a :: *). ( Show a
                                   , Find a row ~ 'False
                                   )
          => Record (a ': row) -> (a, Record row)

remove (Add x record) = (x, record)


class (Find a row ~ 'True) => Has a row where
  get    :: Record row -> a

  -- I need to prove that row is *minimally* destructive.
  -- i.e. that I get back everything.

  delete :: ( Find a newrow ~ 'False
            , Remove a row ~ newrow
            )
            => Proxy a -> Record row -> Record newrow


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
                             -- , (a == b) ~ 'False
                             , Find a (b ': row) ~ 'True
                             , Find b row ~ 'False
                             , Find a row ~ 'True
                             , Find a (Remove a row) ~ 'False
                             , Find b (Remove a row) ~ 'False
                             , (b ': Remove a row) ~ newrow
                             , (b ': Remove a row) ~ Remove a (b ': row)
                             , Has a row
                             )
                             => Has a (b ': row) where

  get :: Record (b ': row) -> a
  get = get . snd . remove

  delete :: Proxy a -> Record (b ': row) -> Record (Remove a (b ':row))

  delete p r = Add a. delete p $ rest
    where
      (a, rest) = remove r




testA :: Record '[Bool, String]
testA = Add True $ Add "hello" Nil

testB :: Record '[String, Bool]
testB = Add "hello" $ Add True Nil

-- > get test :: String
-- "hello"

-- > get test :: Bool
-- True

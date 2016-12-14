{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE TypeFamilies #-}

module GeneralizedIndexedVectors where

import GHC.Types

type family Pi :: k -> Type

data Sigma (k :: Type) (f :: k -> Type) where
  Pair :: Pi (a :: k) -> (f a) -> Sigma k f

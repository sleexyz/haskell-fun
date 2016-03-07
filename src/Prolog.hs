{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}

module Prolog where

data a :~: b where
  Refl :: a :~: a

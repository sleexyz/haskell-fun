{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTSyntax #-}

module Prolog where

data a :~: b where
  Refl :: a :~: a

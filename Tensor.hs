-- {-# LANGUAGE TypeFamilies #-}
-- {-# LANGUAGE DataKinds #-}

module Tensor
       where

-- import Control.Monad
-- import Peano


-- newtype Vect Nat b = V Nat [ b ]
--                  deriving (Eq, Ord, Show)

-- zero :: Vect Nat b
-- zero = V Nat []

-- add :: (Num b) => Vect b -> Vect b -> Vect b
-- add (V ts) (V us) = V $ addmerge ts us
--   where
--     addmerge ts us = zipWith (\t -> \u -> t + u) ts us

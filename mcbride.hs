{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}

module McBride where

data a ~> b = forall x. a x -> b x

{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}

-- | Idk what Ends are yet...
-- but here I attempt to derive the naturality condition
module End where

import Test.Hspec
import Prelude hiding (Functor)

type Functor f = (forall a b. (a -> b) -> f a -> f b)
type Nat f g = Functor f -> Functor g

type f ~> g = forall a. f a -> g a


makeNat :: forall f g.  Nat f g -> f ~> g
makeNat nat = _


spec :: Spec
spec = it "works" $ do
  pending

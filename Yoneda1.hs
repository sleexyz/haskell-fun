{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

-- Hypothesis: a type is isomorphic to its yoneda embedding

data Iso a b = Iso { a2b :: a -> b
                   , b2a :: b -> a
                   }


-- We call it negation, because if we have a proof of bottom, we can generate any value.
negate0 :: forall r. Int -> r
negate0 = undefined

doubleNegate0 :: forall r. (Int -> r) -> r
doubleNegate0 f = f 0

newtype Yoneda a = Yoneda (forall r. (a -> r) -> r)



yoneda :: forall a. a -> Iso a (Yoneda a)
yoneda x = Iso to from
  where
    to :: a -> Yoneda a 
    to x = Yoneda (\cb -> cb x)

    from :: Yoneda a -> a
    from (Yoneda withCB) = withCB id


id' :: forall a. a -> a
id' x = b2a . a2b $ x
  where
    Iso {..} = yoneda x

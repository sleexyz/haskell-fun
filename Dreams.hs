-- {-# LANGUAGE GADTs #-}
-- {-# LANGUAGE StandaloneDeriving #-}

module Dreams where

data Pig = Piggy
         deriving (Show)

data Horse = Horsey
         deriving (Show)

piggy :: Pig
piggy = Piggy

horsey :: Horse
horsey = Horsey


class Animal a where
  sound :: a -> String

instance Animal Pig where
  sound Piggy = "Oink"

instance Animal Horse where
  sound Horsey = "Neigh"




data Dream a = Flying {height :: Int, thing :: a}
               deriving (Show)

instance Functor Dream where
  fmap f (Flying h a) = Flying h (f a)


instance Applicative Dream where
  pure = Flying 0
  (<*>) (Flying _ f) (Flying h x) = Flying h (f x)

instance Monad Dream where
  (>>=) (Flying _ x) f = f x


-- Dream functions!
higher :: Dream a -> Dream a
higher (Flying h t)  = Flying (h + 10) t

land :: Dream a -> Dream a
land (Flying _ t)  = Flying 0 t


-- Wacky functions!

giveWings :: (Animal a) => a -> Dream a
giveWings = return

superpiggy :: Dream Pig
superpiggy = giveWings piggy

superhorsey:: Dream Horse
superhorsey = giveWings horsey

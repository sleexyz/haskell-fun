{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DeriveFunctor #-}

-- Transliteration of
-- https://www.cs.cmu.edu/~rjsimmon/15312-s14/hws/hw1update2-handout.pdf

module ABT where

-- UTIL

infixl 8 &
(&) :: a -> (a -> b) -> b
x & f = f x

infixl 5 <&>
(<&>) :: (Functor f) => f a -> (a -> b) -> f b
x <&> f = f <$> x

newtype State s a = State { unState :: s -> (s, a) }
  deriving Functor

instance Applicative (State s) where
  pure x = State $ \s -> (s, x)
  (State rf) <*> (State rx)
    = State $ \s -> let (s', f) = rf s
                        (s'', x) = rx s'
                    in
                      (s'', f x)
instance Monad (State s) where
  (State rx) >>= kf
    = State $ \s -> let (s', x) = rx s
                        State (ry) = kf x
                        (s'', y) = ry s'
                    in
                      (s'', y)

get :: State s s
get = State $ \s -> (s, s)

put :: s -> State s ()
put s'= State $ \s -> (s', ())

modify :: (s -> s) -> State s ()
modify f = get >>= (put . f)

runState :: State s a -> s -> (s, a)
runState (State rs) s = rs s


-- signatures:

class (Eq t, Show t) =>
      Operator t where

  arity :: t -> [Int]


class (Eq t, Ord t, Show t) =>
      Variable t where
  type S t

  new :: String -> State (S t) t
  toUserString :: t -> String


class (Operator o, Variable v, Functor (View t), Eq t) =>
      Abt o v t where

  into :: View t t -> t
  out :: t -> View t t


-- implementation:

data Var = Var String Int

instance Eq Var where
  (Var _ i) == (Var _ j) = i == j

instance Ord Var where
  (Var _ i) <= (Var _ j) = i <= j

instance Show Var where
  show (Var s i) = s ++ "@" ++ show i

instance Variable (String, Int) where
  type S (String, Int) = Int

  new str = modify (+1) >> (get <&> (\s -> (str, s)))
  toUserString (t, _) = t




data View t a where
  Tick :: (Abt o v t) => v -> View t a
  (:\) :: (Abt o v t) => v -> a ->  View t a
  (:$) :: (Abt o v t) => o -> [a] ->  View t a


data ABT o v = FV v
             | BV Int
             | ABS (ABT o v)
             | OPER (o, [ABT o v])

instance ( Operator o
         , Variable v
         )
         => Eq (ABT o v) where
  (==) = undefined

instance ( Operator o
         , Variable v
         )
         => Functor (View (ABT o v)) where
  fmap = undefined


instance ( Operator o
         , Variable v
         )
         => Abt o v (ABT o v) where
  into = undefined
  out = undefined

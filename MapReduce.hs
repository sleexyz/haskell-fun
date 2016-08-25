{-# LANGUAGE DeriveFunctor #-}

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


sequential :: [State s a] -> State s [a]
sequential [] = pure []
sequential (s:ss) = do
  x <- s
  xs <- sequential (ss)
  pure (x:xs)


-- parallel :: (Monoid s) => [State s a] -> State s a
-- parallel = foldM ()




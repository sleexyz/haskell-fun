{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE StandaloneDeriving#-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

-- 2016-05-15
module LSystem where


data LSysF a = A a
             | B a
             | X
             deriving (Show, Functor)

newtype Fix f = In (f (Fix f))

instance Show (f (Fix f)) => Show (Fix f) where
  show (In x) = show x

out :: Fix f -> f (Fix f)
out (In x) = x



testVal1 :: Fix LSysF
testVal1 = In $ A $ In $ B $ In X

cata :: (Functor f) => (f a -> a) -> Fix f -> a
cata alg = alg . fmap (cata alg) . out


ana :: (Functor f) => (a -> f a) -> a -> Fix f
ana coalg = In . fmap (ana coalg) . coalg


fooLSys :: a -> LSysF a
fooLSys x = A x

barLSys :: LSysF (Fix (LSysF)) -> Fix (LSysF)
barLSys (A x) = In $ A $ In $ B x
barLSys (B x) = In $ A x
barLSys X = In $ X




-- plain old
data Alphabet = C
              | D
              deriving (Show)

rewrite :: [Alphabet] -> [Alphabet]
rewrite [] = []
rewrite (C:xs) = C:D:rewrite xs
rewrite (D:xs) = C:rewrite xs

fooV :: [Alphabet]
fooV = [C]


data RewriteF a = Rewrite [Alphabet] a
                | Y
             deriving (Show, Functor)
fooRewriteF :: Fix RewriteF -> RewriteF (Fix RewriteF)
fooRewriteF (In  ( Rewrite [] x)) = Rewrite [] x
fooRewriteF (In  ( Rewrite (C:xs) x)) = Rewrite (C:D:xs) x
fooRewriteF (In  ( Rewrite (D:xs) x)) = Rewrite (C:xs) x

testRewriteF :: Fix (RewriteF)
testRewriteF = In $ Rewrite [C] (In Y)

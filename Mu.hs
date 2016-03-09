{-# LANGUAGE TypeSynonymInstances#-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTSyntax #-}
{-# LANGUAGE StandaloneDeriving #-}

module Mu where

-- https://www.schoolofhaskell.com/user/bartosz/understanding-algebras

-- newtype Mu f = In (f (Mu f))
newtype Mu f where
  In :: f (Mu f) -> Mu f

-- out :: Mu f -> f (Mu f)
-- out (In x) = x

-- cata :: (Functor f) => (f a -> a) -> Mu f -> a
-- cata phi = phi . fmap (cata phi) . out

-- ana :: Functor f => (a -> f a) -> a -> Mu f
-- ana psi = In . fmap (ana psi) . psi






-- data NatF a = Succ a | Zero
--             deriving (Show)

-- instance Functor NatF where
--   fmap _ Zero = Zero
--   fmap f (Succ x) = Succ (f x)

-- type Nat = Mu NatF


-- zero :: Nat
-- zero = In Zero


-- elim :: a -> (b -> a) -> NatF b -> a
-- elim z _ Zero = z
-- elim _ s (Succ n) = s n

-- instance Show Nat where
--   show = cata (elim "Zero" ("Succ "++))


data ExprF a where
  Const :: Int -> ExprF a
  Add :: a -> a -> ExprF a
  Mul :: a -> a -> ExprF a


-- An F-algebra is built on top of an endofunctor (i.e. a functor in Hask)
instance Functor ExprF where
  fmap _ (Const i) = Const i
  fmap eval (Add left right) = Add (eval left) (eval right)
  fmap eval (Mul left right) = Mul (eval left) (eval right)

type Expr = Mu ExprF


testVal :: Expr
testVal = In $ Mul (In $ Const 2) (In $ Add (In $ Const 3) (In $ Const 5))

-- Int is my carrier type
alg_ :: ExprF Int -> Int
alg_ (Add x y) = x + y
alg_ (Mul x y) = x * y


-- An F-algebra consists of:
-- 1. an endofunctor in a category C,
-- 2. an object A in that category, and
-- 3. a morphism from F(A) to A.


-- So an F-algebra in Haskell is defined by a functor f, a carrier type a, and a function :: f a -> a

type Algebra f a = f a -> a

type SimpleA = Algebra ExprF Int

simpleAlg :: SimpleA
simpleAlg (Const i) = i
simpleAlg (Add x y) = x + y
simpleAlg (Mul x y) = x * y

-- There are many algebras based on a given functor
-- But there is one algebra to bind them all -- the initial algebra!

-- We use ExprF as our functor, Mu ExprF as the carrier type.

type ExprInitAlg = Algebra ExprF (Mu ExprF)

-- initAlg :: ExprF (Mu ExprF) -> Mu ExprF
initAlg :: ExprInitAlg
initAlg = In -- Non-lossy evaluator


-- What's so initial about the initial algebra?
-- There exists a unique homomorphism from it to any algebra based on the same endofunctor
-- In a sense, it is the fullest possible homomorphism (given the fullest possible carrier type, Mu f)
-- What is required of a homomorphism in the category of algebras of an endofunctor?

-- Just a map from carrier type to carrier type (that satisfies some certain properties!)

-- Because In, the initial algebra, is a lossless transformation, it can easily be inverted
unMu :: Mu f -> f (Mu f)
unMu (In x) = x


-- a simple algebra homomorphism, ie. evaluation based on simpleAlg
phiInt :: Mu ExprF -> Int
phiInt = simpleAlg . fmap phiInt . unMu
-- We losslessly tranform our initial algebra back to its functor with unMu
-- and then recursively fmap our algebra homomorphism (ie. evaluator) into our inner datatypes
-- coming out with simpleAlg, our simple homomorphism.

-- Given an algebra, return an algebra homomorphism from the initial algebra to it
cata :: Functor f => (f a -> a) -> (Mu f -> a)

-- cata alg = alg . fmap (cata alg) . unMu -- cata alg is g
cata alg = phi
  where
    phi = alg . fmap phi . unMu
-- a catamorphism lets us evaluate arbitrarily nested expressions!

-- a catamorphism is an algebra homomorphism from the initial algebra to some other algebra
-- cata (really makeCata) lets us create that catamorphism, given any algebra


-- algebra morphism from Initial algebra, aka evaluation
phiInt' :: Mu ExprF -> Int
phiInt' = cata simpleAlg

-- algebra morphism from Initial algebra, aka evaluation
eval :: Mu ExprF -> Int
eval = phiInt'


-- Here we define another endofunctor to define an algebra out of: ListF a
data ListF a b = Nil | Cons a b
instance Functor (ListF a) where
  fmap f Nil = Nil
  fmap f (Cons e x) = Cons e (f x)


-- algSum is an algebra over endofunctor ListF with carrier type Int
algSum :: ListF Int Int -> Int
algSum Nil = 0
algSum (Cons e acc) = e + acc

algList :: ListF Int [Int] -> [Int]
algList Nil = []
algList (Cons e acc) = e:acc

-- Here we construct a morphism from the initial algebra (algebra with carrier type Mu (ListF Int))
testlst :: Mu (ListF Int)
testlst = In $ Cons 2 (In $ Cons 3 (In $ Cons 4 (In Nil)))





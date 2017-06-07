{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PatternSynonyms #-}

-- Taken almost directly from S11
-- https://bitbucket.org/S11001001/pliste
--
-- Syntactically, this looks best in lisp

module PList where

import GHC.TypeLits
import Test.Hspec

type family Flip n where
  Flip 0 = 1
  Flip 1 = 0

type family Pick n a b where
  Pick 0 a b = a
  Pick 1 a b = b

data PList' (polarity :: Nat) a b where
  Cons :: Pick polarity a b -> PList' (Flip polarity) a b -> PList' polarity a b
  Nil :: PList' 0 a b

type PList = PList' 0

infixr 6 :+
pattern a :+ b = Cons a b

plist :: PList a b -> PList a b
plist = id

-- | No inference
example1 :: PList String Int
example1 = plist $ "hello" :+ 1 :+ "world" :+ 2 :+ "asdf" :+ 3 :+ Nil 

-- | Inference!
example2 = plist $ "hello" :+ 1 :+ "world" :+ 2 :+ "asdf" :+ 3 :+ Nil 

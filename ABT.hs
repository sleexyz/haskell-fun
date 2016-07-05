{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}

module ABT where

-- We need a pure way to represent variables!
-- class (Ord t, Eq t, Show t) => Variable t where
--   newvar :: String -> t
--   hash :: t -> Int
--   toUserString :: t -> String

-- newtype Var = Var String Int
-- instance Eq t where
--   Var _ id1 == Var _ id2 = (id1 == id2)
-- instance Ord t where
--   Var _ id1 =< Var _ id2 = id1 =< id2
-- instance Show t where
--   show (Var s id) = s ++ "@" ++ show id
-- instance




-- | length (arity(x)) will be arity, int values are valences, i.e. number of variable bound by that argument
class (Eq t, Show t) => (Operator t) where
  arity :: t -> [Int] 



class (Eq (T f), Functor f) => View f where
  type T f :: *
  -- type T f :: *

  into :: f (T f) -> (T f)
  out :: (T f) -> f (T f)

  aequiv :: (T f) -> (T f) -> Bool
  aequiv = (==)





-- class (Functor f) => Sig f where
--   j :: (Monoid a) => a ->
  


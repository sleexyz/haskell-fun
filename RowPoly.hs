{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- {-# LANGUAGE ImpredicativeTypes #-}

module RowPoly where

import GHC.TypeLits
import GHC.Prim
import Data.Proxy
import Data.Type.Equality
import Data.Type.Bool



type family Find (a :: *) (row :: [*]) where
  Find a '[] = 'False
  Find a (a ': row) = 'True
  Find a (b ': row) = Find a row


-- | Idempotent remove
type family Remove (a :: *) (row :: [*]) where
  Remove a '[] = '[]
  Remove a (a ': row) = row
  Remove a (b ': row) = b ': Remove a row



infixr 5 :+

data Set (row :: [*])  where
  Nil    :: Set '[]

  (:+)   :: ∀ (row :: [*]) (a :: *). ( Show a
                                     , Find a row ~ 'False
                                     )
            => a -> Set row -> Set (a ': row)
deriving instance Show (Set row)



-- | Returns (head entry, rest of set)
remove :: ∀ (row :: [*]) (a :: *). ( Show a
                                   , Find a row ~ 'False
                                   )
          => Set (a ': row) -> (a, Set row)

remove (x :+ set ) = (x, set)


class (Find a row ~ 'True) => Has a row where
  get    :: Set row -> a

  -- I need to prove that row is *minimally* destructive.
  -- i.e. that I get back everything.

  delete :: ( Find a newrow ~ 'False
            , Remove a row ~ newrow
            )
            => Proxy a -> Set row -> Set newrow

  set :: a -> Set row -> Set row


-- | When a is equal to the top of our row:
instance {-# OVERLAPPING #-} ( Show a
                             , Find a row ~ 'False
                             )
                             => Has a (a ': row) where

  get :: Set (a ': row) -> a
  get = fst . remove

  delete :: Proxy a -> Set (a ': row) -> Set row
  delete _ = snd . remove

  set :: a -> Set (a ':row) -> Set (a ': row)
  set x (_ :+ rest)= x :+ rest


-- | When a is not equal to the top of our row:
instance {-# OVERLAPPING #-} ( Show b
                             , Find a (b ': row)     ~ 'True
                             , Find b row            ~ 'False
                             , Find a (Remove a row) ~ 'False
                             , Find b (Remove a row) ~ 'False
                             , (b ': Remove a row)   ~ Remove a (b ': row)
                             , Has a row
                             )
                             => Has a (b ': row) where

  get :: Set (b ': row) -> a
  get = get . snd . remove

  delete :: Proxy a -> Set (b ': row) -> Set (Remove a (b ':row))

  delete p r = (:+) a. delete p $ rest
    where
      (a, rest) = remove r

  set :: a -> Set (b ': row) -> Set (b ': row)
  set x (y :+ rest)= y :+ set x rest


data (sym :: Symbol) --> a = Proxy sym :-> a
                           deriving (Show)

extract :: (sym --> a) -> a
extract (_ :-> x) = x


-- Test

testA :: Set '[ "firstName" --> String
              , "lastName" --> String
              , "age" --> Int
              ]

testA = ((Proxy :: Proxy "firstName") :-> "Sean")
  :+    ((Proxy :: Proxy "lastName") :-> "Lee")
  :+    ((Proxy :: Proxy "age") :-> 19)
  :+    Nil


testB :: Set '[ "lastName" --> String
              , "firstName" --> String
              , "age" --> Int
              ]
testB = ((Proxy :: Proxy "lastName") :-> "Wadler")
  :+    ((Proxy :: Proxy "firstName") :-> "Philip")
  :+    ((Proxy :: Proxy "age") :-> 59)
  :+    Nil



changeName :: (Has ("firstName" --> String) row) => Set row -> Set row
changeName = set ((Proxy :: Proxy "firstName") :-> "Snoop")

greet :: ( Has ("firstName" --> String) row
         , Has ("lastName" --> String) row
         ) => Set row -> String
greet s = "Hello "
  ++ extract (get s :: "firstName" --> String)
  ++ " "
  ++ extract (get s :: "lastName" --> String)

bounce :: (Has ("age" --> Int) row) => Set row -> String
bounce s = f $ extract (get s :: "age" --> Int)
  where
    f x
      | x < 21 = "Get out of here!"
      | otherwise = "Come on in."

main :: IO ()
main = do
  print testA
  print (get testA :: "firstName" --> String)
  print (get testA :: "lastName" --> String)
  print (delete (Proxy :: Proxy ("lastName" --> String)) testA)
  print (delete (Proxy :: Proxy ("lastName" --> String)) testA)

-- TODO: record nesting? How do we do that?

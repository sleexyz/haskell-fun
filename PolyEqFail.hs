{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilyDependencies #-}

-- | This didn't work because instance resolution needs to be at compile time, and at compile time we don't know whether our type-level equality is True or False in general.
module PolyEqFail where

import Data.Proxy
import Data.Typeable

class Reify a where
  type T a
  reify :: Proxy a -> T a

instance Reify 'True where
  type T 'True = Bool
  reify _ =  True

instance Reify 'False where
  type T 'False = Bool
  reify _ =  False


-- | Star-kinded type equality
type family (a :: *) == (b :: *) where
  a == a = True
  a == b = False



tyeq :: forall a b. a -> b -> Bool
tyeq x y  = case (cast (Proxy :: Proxy (a == b)) :: Maybe (Proxy 'True)) of
  Just _ -> True
  Nothing -> False

{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}

import Data.Proxy
import GHC.TypeLits

data B = T
       | B :& B
       | B :| B
       | B :-> B


-- data N = Z | S N

-- type family FromNat n where
--   FromNat 0 = Z
--   FromNat a = S (FromNat (a - 1))

type family Length (a :: [b]) :: Nat where
  Length '[] = 0
  Length (x:xs) = 1 + Length xs





type family a := b where
  a := a = T


type family Prove a where
  Prove T = T

  Prove (T :| b) = T
  Prove (a :| b) = Prove a :| b

  Prove (T :& b) = Prove b
  Prove (a :& b) = Prove a :& b

  Prove (T :-> b) = Prove b
  Prove (a :-> b) = Prove a :-> b




true :: Proxy T
true = Proxy

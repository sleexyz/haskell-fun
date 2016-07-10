{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- I want to use my original types
-- I also want to avoid implicit parameters

module MLModularImplicits where

import GHC.Exts
import Data.Kind
import Prelude hiding (Monoid)

type Module = * -> Constraint

class HasDefault (m :: Module ) ty where
  type Default m ty  = r | r -> m ty
  def :: (m (Default m ty)) => Default m ty


class Monoid m where
  type Monoid' m
  zero :: (?monoid :: m) => Monoid' m
  


data Add = Add

instance HasDefault Monoid Int where
  type Default Monoid Int = Add
  def = Add

instance {-# INCOHERENT #-} (a ~ Default Monoid Int) => Monoid a where
  type Monoid' a = Int
  zero = 0

foo :: Int
foo = zero
  where ?monoid = def

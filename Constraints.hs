{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Constraints where

import Test.Hspec
import GHC.Types
import GHC.TypeLits

data Crap

type family Void a :: Constraint where

infixr :<>

data Delayed :: Constraint -> * where
  Delay :: Delayed p
  (:<>) :: Delayed p -> Delayed q -> Delayed ()

step :: p => Delayed p -> ()
step Delay = ()
step (x :<> y) = ()

run :: Delayed () -> ()
run x@Delay = const () (step x)



foo''''' = run $ Delay @(1 ~ 2)



spec = it "" pending

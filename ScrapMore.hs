{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes #-}

import qualified Prelude as Prelude hiding (Functor, Monoid)

class Default a where
  d :: a

data Category m = Category { id :: forall a. m a a
                           , (.) :: forall a b. m a b
                           }

func :: Category (->)
func = Category { id = Prelude.id, (.) = (Prelude..)}

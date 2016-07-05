{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}

module ZeroParam where

-- | zero parameter typeclasses can serve as interfaces with only one parameters...
-- Why you'd want it, maybe for software development purposes.
-- You'd write the zero parameter typeclass first, and then you'd write the implementation.
class Test where
  type Foo :: *

  def :: Foo

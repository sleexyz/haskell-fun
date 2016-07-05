{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}

module ZeroParamInst where

import ZeroParam

data Bar = Bar
  deriving (Show)

instance Test where
  type Foo = Bar
  def = Bar

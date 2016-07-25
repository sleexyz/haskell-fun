module Coerce where

import Data.Coerce

newtype Foo = Foo Int

foo :: Foo
foo = Foo 1

test :: Int
test =  coerce foo + 2

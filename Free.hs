
-- | I'm interested in iterated endofunctors
-- but what exactly am I missing with free monads?

module Free where

import Control.Monad.Free
import Data.Profunctor

type IterList = Free []


foo :: Free [] String
foo = Pure "hello"

-- | This is too expressive
--
-- Here we get heterogenous lists
bar :: Free [] String
bar = Free [Free [Pure "hello", Pure "world"], Free [Pure "what's your name"], Pure "hello"]

pprint :: Free [] String ->  String
pprint = iter (rmap (\x -> "(" ++ x ++")") mconcat)

-- | The issue with Free monads is that they aren't typed!
--
-- There is no way to statically know the levels; they just "eat up" all the
-- nesting information

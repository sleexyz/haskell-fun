{-# LANGUAGE ViewPatterns #-}


import Data.Profunctor

foo :: Int -> Int
foo@(lmap foo -> bar) = (+1)

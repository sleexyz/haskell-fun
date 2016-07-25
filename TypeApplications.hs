{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}

module TypeApplications where

idInt :: Int -> Int
idInt = (id @ (Int -> Int)) (id @ Int)

data V k = V
foo = V @"hello"

{-# LANGUAGE DataKinds #-}

import GHC.TypeLits

data Package 
  (imports :: [Symbol]) 
  (exports :: [Symbol]) 
  = Package

make :: Package '[] '["make"]
make = Package

foo :: Package '["make"] '["foo"]
foo = Package

data Built = Built

build :: Package '[] '[


{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}

-- | I'm interested in iterated endofunctors
-- Free monads lose iteration level information

-- Might the indexed free monad help?

module IndexedFree where

import Control.Monad.Indexed.Free

-- Let's defined a language called P

data PInt
data PString

data AST i j a where
  Str :: String -> AST
  Length :: a -> AST PString PInt a
  Incr :: a -> AST PInt PInt a


foo :: IxFree AST i i String
foo = Pure "hello"


-- | Indexed free monad of a GADT? 

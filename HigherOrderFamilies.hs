{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ImplicitParams #-}

import Data.Kind
import Prelude hiding (Monoid)
import Data.Proxy



-- lol not possible, type families can only work with data types and type synonym familes are not datatypes.
-- That means the Kind system is stratified into datatypes and function types
-- And we can only work with datatypes.
--
--
-- Perhaps this has something to do with the fact that datatypes always create new types

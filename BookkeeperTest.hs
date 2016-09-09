{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE PatternSynonyms #-}

import Bookkeeper
import Bookkeeper.Internal
import GHC.OverloadedLabels (IsLabel)
import Data.Type.Map

type PersonBook = Book '[ "firstName" :=> String
                        , "lastName"  :=> String
                        , "location"  :=> String
                        ]

me :: PersonBook
me = emptyBook
  & set #firstName "Sean"
  & set #lastName "Lee"
  & set #location "Brooklyn"


getFirstName :: PersonBook -> String
getFirstName (getBook -> (Ext _ x _))
  = x

infixr :-
pattern x :- y <- Ext _ x y

getAll :: PersonBook -> (String, String, String)
getAll (getBook -> x :- y :- z :- _)
  = (x, y, z)

{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}

{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

import GHC.TypeLits
import Data.Type.Bool

infixl 8 &
x & f = f x


-- | Type for person construction

data PersonC (i :: [Symbol]) =
  PersonC { firstNameC :: String
          , lastNameC :: String
          , locationC :: String
          }


-- | Unparametrized type after construction

data Person =
  Person { firstName :: !String
         , lastName :: !String
         , location :: !String
         }


-- | Setters

setFirstName :: String -> PersonC i -> PersonC ("firstName" ': i)
setFirstName x pc = pc {firstNameC=x}

setLastName :: String -> PersonC i -> PersonC ("lastName" ': i)
setLastName x pc = pc {lastNameC=x}

setLocation :: String -> PersonC i -> PersonC ("location" ': i)
setLocation x pc = pc {locationC=x}


-- | Type of least defined Person, ⊥

type PersonCBottom = PersonC '[]

personCBottom :: PersonC '[]
personCBottom = PersonC undefined undefined undefined


-- | Equivalence class of greatest-defined Persons

class PersonCTop a where
  makePerson :: a -> Person

instance (["firstName", "lastName", "location"] :< i ~ True) => PersonCTop (PersonC i) where
  makePerson PersonC{..} = Person firstNameC lastNameC locationC


-- | Ordering relation of subset inclusion

type family (ls :: [Symbol]) :< (rs :: [Symbol]) where
  (l ': ls) :< rs = Elem l rs && (ls :< rs)
  ('[]) :< rs = True
  
type family Elem (ls :: Symbol) (rs :: [Symbol]) where
  Elem l '[] = False
  Elem l (l ':rs) = True
  Elem l (r ':rs) = Elem l rs


-- Now, our typesafe constructors in action!:

-- | The following typechecks:

me1 :: Person
me1 = makePerson $ personCBottom
  & setFirstName "Sean"
  & setLastName "Lee"
  & setLocation "Brooklyn"


-- | As does this: (different order)

me2 :: Person
me2 = makePerson $ personCBottom
  & setLocation "Brooklyn"
  & setLastName "Lee"
  & setFirstName "Sean"


-- | And this: (non-increasing updates)

me3 :: Person
me3 = makePerson $ personCBottom
  & setFirstName "Sean"
  & setLastName "Lee"
  & setLocation "Queens"
  & setLocation "just kidding Brooklyn"


-- | But not this, as expected, because we forgot to define locationC!

-- me :: Person
-- me = makePerson $ personCBottom
--   & setFirstName "Sean"
--   & setLastName "Lee"

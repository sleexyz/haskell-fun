infixl 8 &
x & f = f x

data Person = Person { firstName :: String
                     , lastName :: String
                     , location :: String
                     }


-- | personBottom is the least defined Person

personBottom :: Person
personBottom = Person undefined undefined undefined


-- | A completely defined person

me :: Person
me = personBottom
  & (\x -> x {firstName="Sean"})
  & (\x -> x {lastName="Lee"})
  & (\x -> x {location="Brooklyn"})


-- | A partially defined person

me' :: Person
me' = personBottom
  & (\x -> x {firstName="Sean"})


-- | The following typechecks but will blow up at runtime!

myLastName :: String
myLastName = lastName me'

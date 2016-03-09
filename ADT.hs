{-# LANGUAGE GADTSyntax #-}
{-# LANGUAGE StandaloneDeriving #-}
module ADT where



-- -- Most langs
-- (+) :: (Int, Int) -> Int

-- -- Haskell/Elm
-- (+) :: Int -> (Int -> Int)
-- (+) :: Int -> Int -> Int












-- Colors is "set" with three objects
-- ..or a type with three value possibilities

-- data Color = Red 
--            | Blue 
--            | Green
--            | Lighten Color
--            | Mix Color Color

data Color where
  Red     ::                   Color
  Blue    ::                   Color
  Green   ::                   Color

  Lighten ::          Color -> Color
  Mix     :: Color -> Color -> Color



  

data User = Anonymous | LoggedIn String

-- data User where
--   Anonymous :: User
--   LoggedIn  :: String -> User


data LList where
  Nil       :: LList
  Cons      :: Int -> LList -> LList
deriving instance (Show LList) 


exampleList :: LList
exampleList = Cons 1 $ Cons 2 $ Cons 3 $ Nil



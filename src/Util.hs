module Util
       ( module Util
       , module AN
       , color
       ) where

import           System.Console.ANSI as AN

color :: AN.Color -> String -> String
color c str =
  AN.setSGRCode [AN.SetColor AN.Foreground AN.Vivid c]
  ++  str
  ++ AN.setSGRCode [AN.Reset]

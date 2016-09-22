module ScrapYourImports where


data Exports = Exports 
  { greet :: IO ()
  }

makeExports :: String -> Exports
makeExports str = Exports
  { greet = putStrLn str
  }

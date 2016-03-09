module Middleware where


-- | Middleware in Haskell!
-- | It's all revealed in the types!

data Message = Msg Int Int
              deriving (Show, Eq, Ord)
data Result = Res String
             deriving (Show, Eq, Ord)

handleKnob :: Message -> Result -> Result
handleKnob (Msg knob _) (Res result) = Res $ "Knob: " ++ show knob ++ result

handlePad :: Message -> Result -> Result
handlePad (Msg _ pad) (Res result) = Res $ "Pad: " ++ show pad ++ result



makeHandler :: [Message -> Result -> Result] -> (Message -> Result)
makeHandler = foldl f (const (Res ""))
  where
    f :: (Message -> Result)               -- accHandler
      -> (Message -> Result -> Result)     -- midHandler
      -> (Message -> Result)               -- accHandler
    f handler middlewareHandle = passHandler
      where
        passHandler m = middlewareHandle m (handler m)



-- TODO: Reader monad?

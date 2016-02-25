{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}

module ElmFrp where



type Signal a = [a]

type Element = String


state :: Signal Int
state = [1, 2, 3, 4]


-- TODO: Ask Evan why foldp over scanp, over integrate

-- behavior, event


-- foldp is just discrete integration over a signal!
--     a      is to dx
-- as  Signal is to x

-- foldp kinda is just scanl!
-- foldp :: (a -> b ->  b) -> b -> Signal a -> Signal b
integrate :: forall a state.
         (a -> state ->  state)
         -> state
         -> Signal a
         -> Signal state
integrate = scanr
-- integrate f initialState sigIn = scanl (flip ($)) initialState (fmap f sigIn)


data Message = Msg Int
             deriving (Show, Eq, Ord)
data Result = Res Int
            deriving (Show, Eq, Ord)

testMsg :: Message
testMsg = Msg 20

testHandler :: Message -> Result -> Result
testHandler (Msg x) (Res y) = Res $ y + x


testInput :: Signal Message
testInput = [Msg x|  x <- reverse [1..10]]

testOutput :: Signal Result
testOutput = integrate testHandler (Res 0) testInput



testElmMain :: Signal Element
testElmMain = integrate (\x -> \str -> "> " ++ show x) "" testOutput

run :: Signal Element -> IO ()
run = mapM_ putStrLn

main :: IO ()
main = run testElmMain

-- TODO: get inputs from IO

-- Explore limits of integration, lambda calculus


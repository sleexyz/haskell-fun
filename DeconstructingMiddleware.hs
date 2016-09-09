{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE RankNTypes #-}


import Control.Monad
import Control.Monad.Reader
import Control.Arrow
import Data.Monoid
import Data.Function
import Control.Lens.Wrapped

-- One approach of composable web app is with premonoid of endomorphisms.

-- i.e. constructing middleware


type Request = String
type Response = String


-- A side-effecting computation of a response
-- which depends on a request.

type App = Request → IO Response



-- We want a composable way to construct our app.
-- We can construct an app

type Middleware = App -> App

handleGreeting ∷ Middleware
handleGreeting _ "hello" = pure "How are you?"
handleGreeting _ "hi" = pure "Hello!"
handleGreeting f x = f x

handleQuestion ∷ Middleware
handleQuestion _ "How are you?" = pure "swell."
handleQuestion _ "What is your name?" = pure "Asdf"
handleQuestion f x = f x

makeApp :: [Middleware] -> App -> App
-- makeApp = appEndo . mconcat . (fmap Endo)
makeApp = ala Endo foldMap


myApp :: App
myApp = makeApp [handleGreeting, handleQuestion] _404
  where
    _404 = const (pure "404 Not Found")



-- IO Response → Codensity IO Response

-- type App = Request → IO Response

-- IO Response = IO Response

-- IO Response = ∀b. (Response -> IO b) -> IO b

-- yoneda:
-- f a = ∀b. (a → b) → f b
-- IO Response = ∀b. (Response → b) → IO Response

-- type App' = Request → (∀b. (Response → b) → IO b)



-- -- This is what WAI should be:
-- type Application = ∀a. Request -> (∀b. Response → IO b) → a

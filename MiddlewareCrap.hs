{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE RankNTypes #-}


import Control.Monad
import Control.Monad.Reader
import Control.Arrow
import Data.Monoid
import Data.Function


-- i.e. constructing middleware


type Request = String
type Response = String


-- A side-effecting computation of a response
-- which depends on a request

type App = Request → IO Response

-- Middleware takes a fallback computation of a response
-- and returns a computation of a response dependent on a a request

type Middleware = IO Response -> Request → IO Response

handleGreeting ∷ Middleware
handleGreeting _ "hello" = pure "How are you?"
handleGreeting _ "hi" = pure "Hello!"
handleGreeting x _ = x

handleQuestion ∷ Middleware
handleQuestion _ "How are you?" = pure "swell."
handleQuestion _ "What is your name?" = pure "Asdf"
handleQuestion x _ = x


-- | Simple, elegant:
-- Layers of middleware get squished by a fold.

makeApp ∷ [Middleware] → App
makeApp mids req = foldr (\mid res → mid res req) _404 mids
  where
    _404 = pure ""

app ∷ App
app = makeApp [handleGreeting, handleQuestion]




-- Middleware stuff

-- -- | But this middleware looks like something else!
-- -- Almost a monoid of endomorphisms on IO Response

-- type AppM = ReaderT Request IO
-- type Middleware' = AppM (Response → Response)

-- makeApp' ∷ [AppM (Response → Response)] → App
-- makeApp' mids = mids
--   & sequence
--   & (fmap mconcat')
--   & (fmap applyWithDefault)
--   & runReaderT
--   where
--     mconcat' = appEndo . mconcat . (fmap Endo)
--     applyWithDefault f = f ""


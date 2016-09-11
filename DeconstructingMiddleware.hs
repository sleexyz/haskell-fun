{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor#-}


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
handleGreeting fallback  = \case
  "hello" -> pure "How are you?"
  "hi" -> pure "Hello!"
  x -> fallback x

handleQuestion ∷ Middleware
handleQuestion fallback = \case
  "How are you?" -> pure "swell."
  "What is your name?" -> pure "Asdf"
  x -> fallback x

makeApp :: [Middleware] -> App -> App
makeApp = ala Endo foldMap


myApp :: App
myApp = makeApp [handleGreeting, handleQuestion] _404
  where
    _404 = const (pure "404 Not Found")


newtype Codensity m a = Codensity { runCodensity :: forall b. (a -> m b) -> m b }

instance Functor (Codensity k) where
  fmap f (Codensity m) = Codensity (\k -> m (k . f))

instance Applicative (Codensity f) where
  pure x = Codensity (\k -> k x)
  Codensity f <*> Codensity g = Codensity (\bfr -> f (\ab -> g (bfr . ab)))

instance Monad (Codensity f) where
  return = pure
  m >>= k = Codensity (\c -> runCodensity m (\a -> runCodensity (k a) c))

type Response' = Codensity IO Response

type App' = Request → Response'
type Middleware' = App' -> App'


liftCodensity :: Monad m => m a -> Codensity m a
liftCodensity x = Codensity (x >>=)

lowerCodensity :: Applicative m => Codensity m a -> m a
lowerCodensity x = runCodensity x pure

app2app' :: App ->  App'
app2app' f r = liftCodensity (f r)

app'2app :: App' ->  App
app'2app f r = lowerCodensity (f r)

mid2mid' :: Middleware -> Middleware'
mid2mid' m a = app2app' (m (app'2app a))

makeApp' :: [Middleware'] -> App' -> App'
makeApp' = ala Endo foldMap


handleGreeting' ∷ Middleware'
handleGreeting' f = \case
  "hello" -> Codensity $ \respond -> do
    respond "Hi!"
    -- putStrLn "cleaning up..."
  "hi" -> pure "How are you?"
  x -> f x

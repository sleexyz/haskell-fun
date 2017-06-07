{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- * Here we demonstrate a method to generically
-- derive composite interpreters of Free Monads of coproducts of functors,
-- for a given target monad.
--
-- This hinges on the fact that left adjoint functors preserve colimits.
--
-- TODO: encode exceptions
module Free2 where

import Control.Monad.Free
import Test.Hspec
import Control.Monad.Identity
import Control.Monad.Writer
import GHC.Generics

type MockM = Writer String

class Interpreter f m where
  interpreter :: f a -> m a

-- * User input

data UserInput next = Readline (String -> next)
  deriving (Functor)

instance Interpreter UserInput IO where
  interpreter :: UserInput a -> IO a
  interpreter (Readline f) = f <$> readLn

instance Interpreter UserInput MockM where
  interpreter :: UserInput a -> MockM a
  interpreter (Readline f) = f <$> return "hello"

userInputSpec :: Spec
userInputSpec = do
  describe "UserInput" $ do
    it "can interpret user input" $ do
      let 
        test :: Free UserInput String
        test = do
          a <- Free $ Readline pure
          b <- Free $ Readline pure
          return $ a ++ " " ++ b
          
        (returnVal, (_ :: String)) = runWriter $ (foldFree interpreter) test
      returnVal `shouldBe` "hello hello"

-- * Console Printing

data ConsolePrinting next = PrintLine String next
  deriving (Functor)

instance Interpreter ConsolePrinting IO where
  interpreter :: ConsolePrinting a -> IO a
  interpreter (PrintLine str next) = const next <$> putStrLn str

instance Interpreter ConsolePrinting MockM where
  interpreter :: ConsolePrinting a -> MockM a
  interpreter (PrintLine str next) = const next <$> tell (str ++ "\n")

consolePrintingSpec :: Spec
consolePrintingSpec = do
  describe "ConsolePrinting" $ do
    it "can print strings" $ do
      let 
        test :: Free ConsolePrinting ()
        test = do
          liftF $ PrintLine "hello" ()
          liftF $ PrintLine "world" ()
          
        (_, log) = runWriter $ (foldFree interpreter) test
      log `shouldBe` "hello\nworld\n"

-- * Composite Effects

type Composite = UserInput :+: ConsolePrinting 

instance (Interpreter f m, Interpreter g m) => Interpreter (f :+: g) m where
  interpreter :: (f :+: g) a -> m a
  interpreter (L1 x) = interpreter x
  interpreter (R1 x) = interpreter x

compositeSpec :: Spec
compositeSpec = do
  describe "UserInput :+: ConsolePrinting" $ do
    it "can use composite operations" $ do
      let 
        test :: Free (UserInput :+: ConsolePrinting) ()
        test = do
          userInput <- Free . L1 $ Readline pure
          liftF . R1 $ PrintLine userInput ()
          liftF . R1 $ PrintLine "foo" ()
          userInput' <- Free . L1 $ Readline pure
          liftF . R1 $ PrintLine userInput' ()
          
        (_, log) = runWriter $ (foldFree interpreter) test
      log `shouldBe` "hello\nfoo\nhello\n"

-- * Smart Constructors

class (Functor f, Functor g) => Inject f g where
  inj :: f a -> g a

instance (Functor f) => Inject f f where
  inj :: f a -> f a
  inj = id

instance (Functor f, Functor g) => Inject f (f :+: g) where
  inj :: f a -> (f :+: g) a
  inj = L1

instance (Functor f, Functor g) => Inject g (f :+: g) where
  inj :: g a -> (f :+: g) a
  inj = R1

readLine :: (Inject UserInput g) => Free g String
readLine = Free (inj (Readline pure))

printLine :: (Inject ConsolePrinting g) => String -> Free g ()
printLine str = Free (inj (PrintLine str (Pure ())))

smartConstructorSpec :: Spec
smartConstructorSpec = do
  describe "Smart Constructors" $ do
    describe "UserInput" $ do
      it "can use smart constructors" $ do
        let 
          test :: Free UserInput String
          test = do
            a <- readLine
            b <- readLine
            return $ a ++ " " ++ b
            
          (returnVal, (_ :: String)) = runWriter $ (foldFree interpreter) test
        returnVal `shouldBe` "hello hello"

    describe "ConsolePrinting" $ do
      it "can use smart constructors" $ do
        let 
          test :: Free ConsolePrinting ()
          test = do
            printLine "hello"
            printLine "world"
            
          (_, log) = runWriter $ (foldFree interpreter) test
        log `shouldBe` "hello\nworld\n"

    describe "UserInput :+: ConsolePrinting" $ do
      it "can use smart constructors" $ do
        let 
          test :: Free (UserInput :+: ConsolePrinting) ()
          test = do
            readLine >>= printLine
            printLine "foo"
            readLine >>= printLine
            
          (_, log) = runWriter $ (foldFree interpreter) test
        log `shouldBe` "hello\nfoo\nhello\n"

-- * (run tests)

spec :: Spec
spec = do
  userInputSpec
  consolePrintingSpec
  compositeSpec
  smartConstructorSpec 

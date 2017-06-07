{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- * raw ass haskell synth + live code system based on lazy lists

module Oscillator where

import Control.Concurrent (forkIO)
import Control.Monad (void, ap)
import Data.Char (chr)
import Data.Function ((&))
import Numeric.Natural (Natural)
import System.IO
import System.Process (
  callCommand,
  createProcess,
  CreateProcess(..),
  proc,
  StdStream(..),
  )
import Test.Hspec
import Test.QuickCheck

sineWavetable :: Int -> [Double]
sineWavetable (fromIntegral -> tableSize) = sin <$> period
  where
    period = do
      x <- [0..(tableSize - 1)]
      return $ x * 2 * pi / tableSize

squareWavetable :: Int -> [Double]
squareWavetable tableSize = replicate half 0.5 ++ replicate (tableSize - half) (-0.5)
  where
    half = tableSize `div` 2

sawWavetable :: Int -> [Double]
sawWavetable (fromIntegral -> tableSize) = (/tableSize) <$> [0..(tableSize -1)]

-- decimator
-- good when n, the decimation ratio, is a positive integer
everyNth :: Int -> [a] -> [a]
everyNth 0 xs = error "n must be > 0"
everyNth n xs = everyNth' n 1 xs
  where
    everyNth' :: Int -> Int -> [a] -> [a]
    everyNth' _ _ [] = []
    everyNth' n 1 (x:xs) = x : everyNth' n n xs
    everyNth' n m (_:xs) = everyNth' n (m - 1) xs

-- resampler, now using linear interpolation
-- linearResample :: Int -> [a] -> [a]
-- linearResample = _

printSignal :: [Double] -> IO ()
printSignal = mapM_ $ \x ->
  let
    n = floor (x * size + size)
    size = 31 / 2
  in
    putStrLn $ replicate n ' ' ++ "."

type SampleRate = Int

tones :: SampleRate -> [Double] -> [(Double, Double)] -> [Double]
tones sr wavetable notes = mconcat (toSample <$> notes)
  where
    toSample (freq, dur) = signal sr freq wavetable
      & take (floor (dur * fromIntegral sr))

delaySecs :: SampleRate -> Double -> Double -> [Double] -> [Double]
delaySecs sr delay m sig = zipWith mix sig (drop numFrames sig)
  where
    numFrames = floor $ fromIntegral sr * delay
    mix a b = (1 - m) * a + m * b

midi2cps :: Double -> Double
midi2cps x = 2 ** ((x - 69)/12) * 440

signal :: SampleRate -> Double -> [Double] -> [Double]
signal sr freq wavetable = wavetable
  & cycle
  & everyNth flooredResampleRatio
  where
    flooredResampleRatio = floor freq * (length wavetable) `div` sr

quantize :: Double -> Int
quantize x = floor (x * size + size)
  where
    size = 255 / 2

-- aplay is by default unsigned 8 bit, 8000Hz
play :: [Double] -> IO ()
play list = do
  (Just h, _, _, _) <- createProcess (proc "aplay" []) { std_in = CreatePipe }
  hSetBuffering h NoBuffering
  hSetEncoding h char8
  mapM_ (hPutChar h . chr . quantize) list

-- A cheap livecode command
-- FIXME: don't use pkill, reduce scope of side effects :)
livecode :: [Double] -> IO ()
livecode list = do
  callCommand "pkill aplay"
  void $ forkIO (play list)

-- ap transposed
apT :: (Monad m) => m (a -> b) -> m a -> m b
apT f x = fmap (&) x `ap` f

main :: IO ()
main = livecode $ stream
  where
    stream = tones 8000 (squareWavetable 1024) notePairs
      & zipWith (*) (signal 8000 1 envelope)
      & delaySecs 8000 (1/4) 0.25
      & fmap tanh
      where
        envelope = reverse $ sawWavetable 8000

    notePairs = cycle (midi2cps <$> notes) `zip` cycle durations

    -- notes = [0, 1, 3, 5, 10, 12, 24]
    --   & ap [(+48), (+72)]
    --   & apT [(+0), (+12)]


    notes = [0]
      & ap [(+0), (+3), (+10)]
      & ap [(+0), (+5)]
      & ap [(+60)]
      & apT [(+0), (+12)]

    durations = [1]
      & ap [(/24)]


spec :: Spec
spec = do
  describe "Oscillator" $ do
    describe "Wavetables" $ do
      let
        testWaveTableGen :: String -> (Int -> [Double]) -> Spec
        testWaveTableGen name wavetableGen = do
          describe name $ do
            it "preserves length" $ property $ \(fromIntegral @ Natural -> n) ->
              length (wavetableGen n) == n

            it "is between -1 and 1" $ property $ \(fromIntegral @ Natural -> n) ->
              and $ (\x -> (x <= 1) && (x >= -1)) <$> wavetableGen n

      testWaveTableGen "sineWavetable" sineWavetable
      testWaveTableGen "squareWavetable" squareWavetable
      testWaveTableGen "sawWavetable" sawWavetable

    describe "midi2cps" $ do
      it "sends 69 to 440" $ do
        midi2cps 69 `shouldBe` 440

    describe "linearResampler" $ it "" pending

    describe "signal" $ do
      it "can modulate at any positive real frequency" $ pending

    describe "quantize" $ do
      it "maps -1 to 0" $ do
        quantize (-1) `shouldBe` 0

      it "maps 1 to 255" $ do
        quantize 1 `shouldBe` 255

    describe "convolution based reverb" $ it "" pending

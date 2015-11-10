{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Text.Lazy            as L
import qualified Data.Text.Lazy.IO         as L
import qualified System.Random             as R

import           Control.Monad
import qualified Control.Monad.Trans.State as S
import           Data.Char                 (ord)
import           System.IO

-- Total number of unicode points
maxCodePoints :: Integer
maxCodePoints = 1114112

alphabet :: L.Text
alphabet = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"

passwordCharactersExtended :: L.Text
passwordCharactersExtended =
  L.concat [L.toLower alphabet, L.toUpper alphabet, "!@#$%^&*+-_=~"]

getPasswordChar :: L.Text -> S.State R.StdGen Char
getPasswordChar chars = do
  generator <- S.get
  let textLength = L.length chars
      (randomValue, nextGenerator) = R.randomR (0, textLength - 1) generator
      char = L.index chars randomValue
  S.put nextGenerator
  return char

randomTextSequence :: Int -> R.StdGen -> (L.Text, R.StdGen)
randomTextSequence n generator = (L.pack password, nextGenerator)
  where (password, nextGenerator) =
          S.runState (replicateM n (getPasswordChar passwordCharactersExtended))
                     generator

textToWord :: L.Text -> Integer
textToWord = L.foldl folder 0
  where folder n ch = fromIntegral (ord ch) + (n * maxCodePoints)

bindToInt :: (Integral a) => a -> Int
bindToInt n = fromIntegral (n `mod` fromIntegral (maxBound :: Int))

blockSize :: (Integral a) => a
blockSize = fromIntegral $ L.length alphabet

main :: IO ()
main = do
  L.putStr "Generation key: "
  hFlush stdout
  key <- L.getLine

  let passwordIntegerValue = textToWord key
      boundPasswordIntegerValue = bindToInt passwordIntegerValue
      generator = R.mkStdGen (fromIntegral boundPasswordIntegerValue)
      randomText = fst (randomTextSequence (blockSize*blockSize) generator)
      outputLines = zipWith (\ch line -> L.concat [L.pack [ch], "|", line])
                            (L.unpack alphabet)
                            (L.chunksOf blockSize randomText)

  L.putStrLn ""
  L.putStrLn $ L.concat ["  ", alphabet]
  L.putStrLn $ L.concat ["  ", L.take blockSize (L.repeat '-')]
  mapM_ L.putStrLn outputLines

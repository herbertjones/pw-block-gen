{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Text.Lazy            as T
import qualified Data.Text.Lazy.IO         as T
import qualified System.Random             as R

import           Control.Monad
import qualified Control.Monad.Trans.State as S
import           Data.Char                 (ord)
import           Data.Int                  (Int64)
import           System.IO

import           Options.Applicative

data Spacing = Narrow | Wide

data PasswordOptions = PasswordOptions
  { spacing :: Spacing
  }

parseSpacing :: Parser Spacing
parseSpacing = flag Narrow Wide
  ( long "widespacing"
 <> short 'w'
 <> help "Add space between output"
  )

parseOptions :: Parser PasswordOptions
parseOptions = PasswordOptions
  <$> parseSpacing

-- Total number of unicode points
maxCodePoints :: Integer
maxCodePoints = 1114112

alphabet :: T.Text
alphabet = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"

passwordCharactersExtended :: T.Text
passwordCharactersExtended =
  T.concat [T.toLower alphabet, T.toUpper alphabet, "1234567890", "!@#$%^&*+-_=~"]

getPasswordChar :: T.Text -> S.State R.StdGen Char
getPasswordChar chars = do
  generator <- S.get
  let textLength = T.length chars
      (randomValue, nextGenerator) = R.randomR (0, textLength - 1) generator
      char = T.index chars randomValue
  S.put nextGenerator
  return char

randomTextSequence :: T.Text -> Int -> R.StdGen -> (T.Text, R.StdGen)
randomTextSequence chars n generator = (T.pack password, nextGenerator)
  where (password, nextGenerator) =
          S.runState (replicateM n (getPasswordChar chars))
                     generator

textToWord :: T.Text -> Integer
textToWord = T.foldl folder 0
  where folder n ch = fromIntegral (ord ch) + (n * maxCodePoints)

bindToInt :: (Integral a) => a -> Int
bindToInt n = fromIntegral (n `mod` fromIntegral (maxBound :: Int))

blockSize :: (Integral a) => a
blockSize = fromIntegral $ T.length alphabet

genPasswordBlock :: PasswordOptions -> IO ()
genPasswordBlock opts = do
  T.putStr "Generation key: "
  hFlush stdout
  key <- T.getLine

  let passwordIntegerValue = textToWord key
      boundPasswordIntegerValue = bindToInt passwordIntegerValue
      generator = R.mkStdGen (fromIntegral boundPasswordIntegerValue)
      randomText = fst (randomTextSequence passwordCharactersExtended (blockSize*blockSize) generator)
      randomTextLines' = T.chunksOf blockSize randomText
      randomTextLines = case spacing opts of
                          Narrow -> randomTextLines'
                          Wide -> fmap (T.intersperse ' ') randomTextLines'
      headerText = case spacing opts of
                        Narrow -> alphabet
                        Wide -> T.intersperse ' ' alphabet
      dashWidth = T.length headerText
      outputLines = zipWith (\ch line -> T.concat [T.pack [ch], "|", line])
                            (T.unpack alphabet)
                            randomTextLines

  T.putStrLn ""
  T.putStrLn $ T.concat ["  ", headerText]
  T.putStrLn $ T.concat ["  ", T.take dashWidth (T.repeat '-')]
  mapM_ T.putStrLn outputLines

main :: IO ()
main = execParser opts >>= genPasswordBlock
  where
    opts = info (helper <*> parseOptions)
      ( fullDesc
     <> progDesc "Generate a password block."
     <> header "pw-block-gen - Generate password block"
      )

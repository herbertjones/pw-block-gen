{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified System.Random as R
import qualified Data.Text.Lazy as L
import qualified Data.Text.Lazy.IO as L

import qualified Control.Monad.Trans.State as S
import Control.Monad

passwordCharactersExtended :: L.Text
passwordCharactersExtended =
  L.concat
   [ "abcdefghijklmnopqrstuvwxyz"  -- lowercase
   , "ABCDEFGHIJKLMNOPQRSTUVWXYZ"  -- uppercase
   , "!@#$%^&*+-_=~"               -- other
   ]

getPasswordChar :: L.Text -> S.State R.StdGen Char
getPasswordChar chars = do
  generator <- S.get
  let textLength = L.length chars
      (randomValue, nextGenerator) = R.randomR (0, textLength - 1) generator
      char = L.index chars randomValue
  S.put nextGenerator
  return char

randomPassword :: Int -> R.StdGen -> (L.Text, R.StdGen)
randomPassword n generator = (L.pack password, nextGenerator)
  where (password, nextGenerator) =
          S.runState (replicateM n (getPasswordChar passwordCharactersExtended))
                     generator


main :: IO ()
main = do
  generator <- R.getStdGen
  let passwords = S.evalState (replicateM 10 (S.state (randomPassword 10)))
                              generator
  forM_ passwords L.putStrLn

{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Monoid (mconcat)
import Network.CubeCotillion

main :: IO ()
main = do
  key <- loadKey "server-keys"
  cubeCotillion 8080 key $ do
    cmd "greet" $ do
      bs "Hello, world!\n"
    cmd "greet :name" $ do
      name <- param "name"
      bs $ mconcat ["Hello, ", name, "!\n"]

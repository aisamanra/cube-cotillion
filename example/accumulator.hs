{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad.IO.Class (liftIO)
import Data.IORef
import Network.CubeCotillion

main :: IO ()
main = do
  ref <- newIORef (0 :: Int)
  pk <- loadKey "server-keys"
  let action f = liftIO (modifyIORef ref f) >> bs "ok.\n"
  cubeCotillion 9999 pk $ do
    cmd "incr" $ action (+1)
    cmd "decr" $ action (\ x -> x-1)
    cmd "double" $ action (*2)
    cmd "add :n" $ do
      n <- readParam "n"
      action (+n)
    cmd "get" $ do
      n <- liftIO $ readIORef ref
      string (show n ++ "\n")

{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Monad      (forever, (>=>))
import qualified Data.Text          as T
import qualified Network.WebSockets as WS

meow :: WS.Connection -> IO ()
meow conn = forever $ do
  msg <- WS.receiveData conn
  WS.sendTextData conn $ msg `T.append` ", meow"

main :: IO ()
main = WS.runServer "127.0.0.1" 1337 (WS.acceptRequest >=> meow)

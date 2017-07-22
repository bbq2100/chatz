{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Concurrent (forkIO)
import           Control.Monad      (forever, unless)
import           Data.Text          (Text)
import qualified Data.Text          as T
import qualified Data.Text.IO       as T
import           Network.Socket     (withSocketsDo)
import qualified Network.WebSockets as WS

app :: WS.ClientApp ()
app conn = do
    putStrLn "Connected!"

    _ <- forkIO $ forever $ do
        msg <- WS.receiveData conn
        T.putStrLn msg

    let loop = do
            line <- T.getLine
            unless (T.null line) $ WS.sendTextData conn line >> loop

    loop
    WS.sendClose conn ("Bye!" :: Text)

main :: IO ()
main = withSocketsDo $ WS.runClient "127.0.0.1" 1337 "/" app

{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Concurrent (MVar, modifyMVar, modifyMVar_, newMVar,
                                     readMVar)
import           Control.Exception  (finally)
import           Control.Monad      (forM_, forever)
import           Data.Char          (isPunctuation, isSpace)
import           Data.Monoid        (mappend)
import           Data.Text          (Text)
import qualified Data.Text          as T
import qualified Network.WebSockets as WS

type Client = (Text, WS.Connection)

type ServerState = [Client]

data AlreadyExists = AlreadyExists

newServerState :: ServerState
newServerState = []

numClients :: ServerState -> Int
numClients = length

clientExists :: Client -> ServerState -> Bool
clientExists c = any ((== fst c) . fst)

addClient :: Client -> ServerState -> Either AlreadyExists ServerState
addClient client clients
        | clientExists client clients = Left AlreadyExists
        | otherwise = Right $ client : clients

removeClient :: Client -> ServerState -> ServerState
removeClient c = filter ((== fst c) . fst)

broadcast :: Text -> ServerState -> IO ()
broadcast m s = forM_ s $ \(_, conn) -> WS.sendTextData conn m

main :: IO ()
main = do
  state <- newMVar newServerState
  WS.runServer "127.0.0.1" 1337 $ application state

application :: MVar ServerState -> WS.ServerApp
application state pending = do
  conn <- WS.acceptRequest pending
  WS.forkPingThread conn 30
  msg <- WS.receiveData conn
  clients <- readMVar state
  case msg of
        _ | not (prefix `T.isPrefixOf` msg) -> WS.sendTextData conn ("Wrong announcement" :: Text)
          | any ($ userName msg) [T.null, T.any isPunctuation, T.any isSpace] ->
                             WS.sendTextData conn ("Name cannot " `mappend`
                         "contain punctuation or whitespace, and " `mappend`
                         "cannot be empty" :: Text)
          | clientExists (userName msg, conn) clients -> WS.sendTextData conn ("User already exists" :: Text)
          | otherwise -> flip finally (do
                                          s <- modifyMVar state $ \s ->
                                           let s' = removeClient (userName msg, conn) s in return (s', s')
                                          broadcast (userName msg `mappend` " disconnected") s) $
              modifyMVar_ state $ \s -> do
                let s' = addClient (userName msg, conn) s
                case s' of
                  Left _ -> undefined
                  Right ss -> do
                    WS.sendTextData conn $ "Welcome! Users: " `mappend` T.intercalate ", " (map fst s)
                    broadcast (userName msg `mappend` " joined") ss
                    return ss
  talk conn state (userName msg, conn)
  where
    prefix = "Hi! I am "
    userName = T.drop (T.length prefix)

talk :: WS.Connection -> MVar ServerState -> Client -> IO ()
talk conn state (userName, _) = forever $ do
  msg <- WS.receiveData conn
  readMVar state >>= broadcast (userName `mappend` ": " `mappend` msg)

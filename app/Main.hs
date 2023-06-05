{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Wuss

import Control.Concurrent (forkIO)
import Control.Monad (forever, void)
import Data.Text (Text, pack, unpack)
import Network.WebSockets (ClientApp, receiveData, sendClose, sendTextData)
import Data.Aeson
import GHC.Generics

main :: IO ()
main = print . encode @GatewayEvent $ Event 1 "null" 41250 "null"
-- main :: IO ()
-- main = runSecureClient "gateway.discord.gg" 443 "/" ws

ws :: ClientApp ()
ws connection = do
    putStrLn "Connected!"
    file <- readFile "./testdata.json"

    void . forkIO . forever $ do
        message <- receiveData connection
        let result = (decode @GatewayEvent message) 
        print result
        print message 

    let loop = do
            _ <- getLine
            let line1 = file
            sendTextData connection (pack line1)
            loop
    _ <- loop

    sendClose connection (pack "Bye!")

instance FromJSON GatewayEvent where
    parseJSON = withObject "GatewayEvent" $ \gv -> pure Event
       <*> gv .: "t" 
       <*> gv .: "s"
       <*> gv .: "op"
       <*> gv .: "d"

instance ToJSON GatewayEvent where
  toJSON (Event op evdata seqn evname) = object ["op" .= op, "d" .= evdata, "s" .= seqn, "t" .= evname]

data GatewayEvent
    = Event { gatewayOpcode :: !Int
            , gatewayEventdata :: !Text -- Should be JSON, can be null
            , gatewaySequenceNumber :: !Int -- can be null
            , gatewayEventname :: !Text } -- can be null
    deriving (Show, Generic)


data Intent = MESSAGE_REACTION_ADD
            | MESSAGE_REACTIVE_REMOVE
            | READY
            | MESSAGE_CREATE

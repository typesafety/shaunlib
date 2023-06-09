module Shaunlib where

import Wuss

import Control.Concurrent (forkIO)
import Control.Monad (forever, void)
import Data.Aeson
import Data.Text (Text, pack, unpack)
import GHC.Generics
import Network.WebSockets (ClientApp, receiveData, sendClose, sendTextData)

import Data.ByteString.Lazy qualified as LBS

-- testapp :: IO ()
-- testapp = print . encode @GatewayEvent $ Event 1 "null" 41250 "null"
testapp :: IO ()
testapp = runSecureClient "gateway.discord.gg" 443 "/" ws

ws :: ClientApp ()
ws connection = do
    putStrLn "Connected!"
    file <- readFile "./local/testdata.json"

    void . forkIO . forever $ do
        message <- receiveData @LBS.ByteString connection

        LBS.putStr "message:\n"
        LBS.putStr (message <> "\n")

        LBS.writeFile "message.txt" message

        let result = decode @GatewayEvent message
        print result

    let loop = do
            _ <- getLine
            let line1 = file
            sendTextData connection (pack line1)
            loop
    _ <- loop

    sendClose connection (pack "Bye!")

instance FromJSON GatewayEvent where
    parseJSON = withObject "GatewayEvent" $ \event ->
        Event
            <$> event
            .: "op"
            <*> event
            .: "d"
            <*> event
            .: "s"
            <*> event
            .: "t"

instance ToJSON GatewayEvent where
    toJSON (Event op evdata seqn evname) =
        object
            [ "op" .= op
            , "d" .= evdata
            , "s" .= seqn
            , "t" .= evname
            ]

data GatewayEvent = Event
    { gatewayOpcode :: !Int  -- op,
    , gatewayEventdata :: !Object  -- d, Should be JSON, can be null
    , gatewaySequenceNumber :: !(Maybe Text)  -- s, can be null
    , gatewayEventname :: !(Maybe Text)  -- t, can be null
    }
    deriving (Show, Generic)

data Intent
    = MESSAGE_REACTION_ADD
    | MESSAGE_REACTIVE_REMOVE
    | READY
    | MESSAGE_CREATE
{-# LANGUAGE DeriveGeneric #-}

module Shaunlib where

import Wuss

import Control.Concurrent (forkIO)
import Control.Monad (forever, void)
import Data.Aeson
import Data.Aeson.Types
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

        let result = decode @EventPayload message
        case result of
            Just payload -> do
                putStrLn "Successfully event parsed payload. payload:\n"
                print payload

                case payloadEventdata payload of
                    Nothing -> pure ()
                    -- TODO: try to extract EvHello
                    -- Just eventData -> do
                    --     print (decode @EvHello eventData)

            Nothing -> do
                LBS.putStr "Failed to parse event payload. message:\n"
                LBS.putStr (message <> "\n")

    let loop = do
            _ <- getLine
            let line1 = file
            sendTextData connection (pack line1)
            loop
    _ <- loop

    sendClose connection (pack "Bye!")

-- * Gateway events

-- https://discord.com/developers/docs/topics/gateway-events#

-- | https://discord.com/developers/docs/topics/gateway-events#payload-structure
data EventPayload = EventPayload
    { payloadOpcode :: !Int -- op,
    , payloadEventdata :: !(Maybe Object) -- d, Should be JSON, can be null
    , payloadSequenceNumber :: !(Maybe Text) -- s, can be null
    , payloadEventname :: !(Maybe Text) -- t, can be null
    }
    deriving (Show, Generic)

instance FromJSON EventPayload where
    parseJSON :: Value -> Parser EventPayload
    parseJSON = withObject "GatewayEvent" $ \event ->
        EventPayload
            <$> event
            .: "op"
            <*> event
            .: "d"
            <*> event
            .: "s"
            <*> event
            .: "t"

instance ToJSON EventPayload where
    toJSON :: EventPayload -> Value
    toJSON (EventPayload op evdata seqn evname) =
        object
            [ "op" .= op
            , "d" .= evdata
            , "s" .= seqn
            , "t" .= evname
            ]

-- * Send/Receive event types

{- | Send event: https://discord.com/developers/docs/topics/gateway-events#identify-identify-structure

Opcode 2
-}
data EvIdentify = EvIdentify
    { evIdentifyToken :: !Token
    , evIdentifyProperties :: !ConnectionProperties
    , evCompress :: !(Maybe Bool)
    , evLargeThreshold :: !(Maybe Int)
    , evShard :: !(Maybe (ShardId, Int))
    , evPresence :: !EvUpdatePresence
    , evIntents :: !Int -- TODO: not a raw int
    }
    deriving (Eq, Show, Generic)

{- | Send event: https://discord.com/developers/docs/topics/gateway-events#update-presence-gateway-presence-update-structure

Opcode 3
-}
data EvUpdatePresence = EvUpdatePresence
    { evUpdatePresenceSince :: !(Maybe Int)
    , evUpdatePresenceActivities :: ![Activity]
    , evUpdatePresenceStatus :: !Status
    , evUpdatePresenceAfk :: !Bool
    }
    deriving (Eq, Show, Generic)

{- | Receive event: https://discord.com/developers/docs/topics/gateway-events#hello

Opcode 10
-}
data EvHello = EvHello
    { evHelloHeartbeatInterval :: !Int
    }
    deriving (Eq, Show, Generic)

instance ToJSON EvHello
instance FromJSON EvHello

-- * Other object types

-- TODO (typesafety): Implement this
-- https://discord.com/developers/docs/topics/gateway-events#activity-object
data Activity
    deriving (Eq, Show)

-- | https://discord.com/developers/docs/topics/gateway-events#update-presence-status-types
data Status
    = Online
    | Dnd
    | Idle
    | Invisible
    | Offline
    deriving (Eq, Show)

-- | https://discord.com/developers/docs/topics/gateway-events#identify-identify-connection-properties
data ConnectionProperties = ConnectionProperties
    { connectionPropertiesOs :: Text
    , connectionPropertiesBrowser :: Text
    , connectionPropertiesDevice :: Text
    }
    deriving (Eq, Show)

-- * Other types

-- | https://discord.com/developers/docs/topics/gateway#sharding
newtype ShardId = ShardId {unShardId :: Int}
    deriving (Eq, Show)

-- | Opaque token newtype
newtype Token = Token {unToken :: Text}
    deriving (Eq)

instance Show Token where
    show = const "<TOKEN>"

-- | https://discord.com/developers/docs/topics/gateway#gateway-intents
data Intent
    = MESSAGE_REACTION_ADD
    | MESSAGE_REACTIVE_REMOVE
    | READY
    | MESSAGE_CREATE
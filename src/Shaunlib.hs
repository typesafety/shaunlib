-- TODO next:
-- * figure out why we get a CloseEvent 1000 after 30 seconds or so

{-# LANGUAGE DeriveGeneric #-}

module Shaunlib where

import Control.Concurrent (forkIO)
import Control.Monad (forever, void)
import Data.Aeson (
    (.:),
    (.=),
    FromJSON,
    ToJSON,
    Value,
    decode,
    object,
    withObject,
    )
import Data.Text (Text, pack, strip)
import Data.Text.Lazy (toStrict)
import GHC.Generics (Generic)
import Network.WebSockets (
    ClientApp,
    Connection,
    receiveData,
    sendClose,
    sendTextData,
    )
import Text.Pretty.Simple (pShow)
import Wuss (runSecureClient)

import Data.Aeson qualified as Aeson
import Data.Text.IO qualified

import Control.Exception (Exception, throwIO)
import Data.ByteString.Lazy qualified as LBS
import Data.Int (Int64)
import Data.List (List)
import Data.Tuple.Experimental (Tuple2, Unit)

runTestApp :: IO Unit
runTestApp = runSecureClient "gateway.discord.gg" 443 "/" testApp

-- test
makeIdentifyEvent :: Token -> EvIdentify
makeIdentifyEvent token = EvIdentify {
    evIdentifyToken = token,
    evIdentifyProperties = ConnectionProperties {
        connectionPropertiesOs = "somedistro",
        connectionPropertiesBrowser = "shaunlib",
        connectionPropertiesDevice = "shaunlib"
        },
    evCompress = Nothing,
    evLargeThreshold = Nothing,
    evShard = Nothing,
    evPresence = Nothing,
    evIntents = 46850
    }

-- test
makeIdentifyPayload :: Token -> Payload
makeIdentifyPayload token = Payload {
    payloadOpcode = 2,
    payloadEventData = Just (Aeson.toJSON (makeIdentifyEvent token)),
    payloadSequenceNumber = Nothing,
    payloadEventName = Nothing
    }

todo :: a
todo = undefined

putTxtLn :: Text -> IO Unit
putTxtLn = Data.Text.IO.putStrLn

pShowTxt :: Show a => a -> Text
pShowTxt = toStrict . pShow

handleDispatchEvent :: DispatchEvent -> IO Unit
handleDispatchEvent _dispatchEvent = putTxtLn "TODO"

-- | Runs in a loop, listening for gateway events on the given connection.
listenForGatewayEvents :: Connection -> IO Unit
listenForGatewayEvents connection = forever $ do
    message <- receiveData @LBS.ByteString connection

    let result = decode @Payload message
    case result of
        Just payload -> do
            putTxtLn $ "Received gateway event with opcode: " <> pack (show (payloadOpcode payload))
            putTxtLn $ "Payload content: \n" <> (pShowTxt payload)

            -- https://discord.com/developers/docs/topics/opcodes-and-status-codes#gateway-gateway-opcodes
            case payloadOpcode payload of
                -- Event dispatched
                0 -> case dispatchEventFromPayload payload of
                    Left errMsg -> throwIO
                        $ GenericDiscordError
                            ("Malformed dispatch event payload; \n" <> errMsg <> "\n" <> pShowTxt payload)
                    Right dispatchEvent -> handleDispatchEvent dispatchEvent

                -- Hello
                10 -> case payloadEventData payload of
                    Nothing -> throwIO
                        $ GenericDiscordError ("Malformed Hello payload; missing data: \n" <> pShowTxt payload)
                    Just eventData -> do
                        putTxtLn $ pShowTxt (Aeson.fromJSON @EvHello eventData)

                n -> putTxtLn $ "Opcode not yet supported: " <> pack (show n)

        Nothing -> do
            LBS.putStr "Failed to parse event payload. message:\n"
            LBS.putStr (message <> "\n")

-- | Test application entry point.
testApp :: ClientApp Unit
testApp connection = do
    putTxtLn "Connected!"
    token <- strip . pack <$> readFile "token"

    let identifyPayload = makeIdentifyPayload (Token token)

    -- Listen to payloads in a separate thread
    void . forkIO $ (listenForGatewayEvents connection)

    -- Send Identify paylod
    let encodedIdentify = Aeson.encode identifyPayload
    putTxtLn $ "Sending Identify payload:\n" <> (pShowTxt identifyPayload)
    sendTextData connection encodedIdentify

    let loop = do
            _ <- getLine
            loop
    _ <- loop

    sendClose connection (pack "Bye!")

-- * Exceptions

class Exception e => DiscordError e

newtype GenericDiscordError = GenericDiscordError Text
    deriving (Eq, Show)

instance Exception GenericDiscordError
instance DiscordError GenericDiscordError

-- * Lower-level

-- | Payload, top-level data structure. `payloadEventData` contains the specific
-- data for the particular event that is sent.
-- https://discord.com/developers/docs/topics/gateway-events#payload-structure
data Payload = Payload
    { payloadOpcode :: !Int -- op,

    -- TODO(typesafety): This shouldn't be Aeson.Object (or Value), it should be
    -- some kind of event type.  How do we define a type for all events?  Normal
    -- ADT?  Or do we need something fancier?
    , payloadEventData :: !(Maybe Value) -- d, Should be a JSON object, can be null

    , payloadSequenceNumber :: !(Maybe Int) -- s, can be null
    , payloadEventName :: !(Maybe Text) -- t, can be null
    }
    deriving (Show, Generic)

instance FromJSON Payload where
    parseJSON = withObject "Payload" $ \event ->
        Payload
            <$> event
            .: "op"
            <*> event
            .: "d"
            <*> event
            .: "s"
            <*> event
            .: "t"

instance ToJSON Payload where
    toJSON (Payload op evdata seqn evname) =
        object
            [ "op" .= op
            , "d" .= evdata
            , "s" .= seqn
            , "t" .= evname
            ]

data DispatchEvent = DispatchEvent {
    dispatchEventData :: !Value,
    dispatchEventSequenceNumber :: !Int,
    dispatchEventName :: !Text
    }
    deriving (Eq, Show)

dispatchEventFromPayload :: Payload -> Either Text DispatchEvent
dispatchEventFromPayload = \case
    Payload _ (Just eventData) (Just sequenceNumber) (Just eventName) ->
        Right (DispatchEvent eventData sequenceNumber eventName)
    _ -> Left "Missing sequence number ('s'), event name ('t'), or event data ('d')"

{-| Receive Events

Official documentation: https://discord.com/developers/docs/topics/gateway-events#receive-events

Receive events are Gateway events encapsulated in an event payload, and are sent
by Discord to an app through a Gateway connection.
-}
data ReceiveEvent
    -- | Defines the heartbeat interval.
    --
    -- Sent on connection to the websocket.  Defines the heartbeat interval
    -- that an app should heartbeat to.
    --
    -- https://discord.com/developers/docs/topics/gateway-events#hello
    = Hello
        -- | Heartbeat interval
        Int

    -- | Contains the initial state information.
    --
    -- The ready event is dispatched when a client has completed the initial
    -- handshake with the gateway (for new sessions).  The ready event can be
    -- the largest and most complex event the gateway will send, as it
    -- contains all the state required for a client to begin interacting with
    -- the rest of the platform.
    --
    -- https://discord.com/developers/docs/topics/gateway-events#ready
    | Ready
        -- | [API version](https://discord.com/developers/docs/reference#api-versioning-api-versions).
        Int
        -- | Information about the user.
        User
        -- | [List of Unavailable Guilds](https://discord.com/developers/docs/resources/guild#unavailable-guild-object).
        (List UnavailableGuild)
        -- | Session ID.  Used for resuming connections.
        Text
        -- | Gateway URL for resuming connections.
        Text
        -- | Id and flags

    -- | Response to the Resume send event.
    --
    -- The resumed event is dispatched when a client has sent a resume payload
    -- to the gateway (for resuming existing sessions).
    --
    -- https://discord.com/developers/docs/topics/gateway-events#resumed
    | Resumed

    -- | Server is going away, client should reconnect to gateway and resume.
    --
    -- The reconnect event is dispatched when a client should reconnect to the
    -- gateway (and resume their existing session, if they have one). This event
    -- usually occurs during deploys to migrate sessions gracefully off old
    -- hosts.
    --
    -- https://discord.com/developers/docs/topics/gateway-events#reconnect
    | Reconnect

    -- | Failure response to Identify or Resume or invalid active session.
    --
    -- Sent to indicate one of at least three different situations:
    --
    -- * the gateway could not initialize a session after receiving an Opcode 2
    --   Identify
    --
    -- * the gateway could not resume a previous session after receiving an
    --   Opcode 6 Resume
    --
    -- * the gateway has invalidated an active session and is requesting client
    --   action
    --
    -- https://discord.com/developers/docs/topics/gateway-events#invalid-session
    | InvalidSession
        -- | Whether the session may be resumable or not.  Instructions on
        -- resuming: https://discord.com/developers/docs/topics/gateway#resuming
        Bool

    -- ApplicationCommandPermissionsUpdate
    -- https://discord.com/developers/docs/topics/gateway-events#application-command-permissions-update


-- | Unavailable [Guild](https://discord.com/developers/docs/resources/guild#guild-object-guild-features)
data UnavailableGuild = UnavailableGuild
    { unavailableGuildId :: Snowflake
    , unavailableGuildIsUnavailable :: Bool
    }

-- | [User object](https://discord.com/developers/docs/resources/user#user-object)
data User = User
    { userId :: Snowflake
    , userUsername :: Text
    , userDiscriminator :: Text
    , userAvatar :: Maybe Text
    }
    deriving (Eq, Show)

newtype Snowflake = Snowflake Int64
    deriving (Eq, Show)

-- Application Command Permissions Update	Application command permission was updated
-- Auto Moderation Rule Create	Auto Moderation rule was created
-- Auto Moderation Rule Update	Auto Moderation rule was updated
-- Auto Moderation Rule Delete	Auto Moderation rule was deleted
-- Auto Moderation Action Execution	Auto Moderation rule was triggered and an action was executed (e.g. a message was blocked)
-- Channel Create	New guild channel created
-- Channel Update	Channel was updated
-- Channel Delete	Channel was deleted
-- Channel Pins Update	Message was pinned or unpinned
-- Thread Create	Thread created, also sent when being added to a private thread
-- Thread Update	Thread was updated
-- Thread Delete	Thread was deleted
-- Thread List Sync	Sent when gaining access to a channel, contains all active threads in that channel
-- Thread Member Update	Thread member for the current user was updated
-- Thread Members Update	Some user(s) were added to or removed from a thread
-- Guild Create	Lazy-load for unavailable guild, guild became available, or user joined a new guild
-- Guild Update	Guild was updated
-- Guild Delete	Guild became unavailable, or user left/was removed from a guild
-- Guild Audit Log Entry Create	A guild audit log entry was created
-- Guild Ban Add	User was banned from a guild
-- Guild Ban Remove	User was unbanned from a guild
-- Guild Emojis Update	Guild emojis were updated
-- Guild Stickers Update	Guild stickers were updated
-- Guild Integrations Update	Guild integration was updated
-- Guild Member Add	New user joined a guild
-- Guild Member Remove	User was removed from a guild
-- Guild Member Update	Guild member was updated
-- Guild Members Chunk	Response to Request Guild Members
-- Guild Role Create	Guild role was created
-- Guild Role Update	Guild role was updated
-- Guild Role Delete	Guild role was deleted
-- Guild Scheduled Event Create	Guild scheduled event was created
-- Guild Scheduled Event Update	Guild scheduled event was updated
-- Guild Scheduled Event Delete	Guild scheduled event was deleted
-- Guild Scheduled Event User Add	User subscribed to a guild scheduled event
-- Guild Scheduled Event User Remove	User unsubscribed from a guild scheduled event
-- Integration Create	Guild integration was created
-- Integration Update	Guild integration was updated
-- Integration Delete	Guild integration was deleted
-- Interaction Create	User used an interaction, such as an Application Command
-- Invite Create	Invite to a channel was created
-- Invite Delete	Invite to a channel was deleted
-- Message Create	Message was created
-- Message Update	Message was edited
-- Message Delete	Message was deleted
-- Message Delete Bulk	Multiple messages were deleted at once
-- Message Reaction Add	User reacted to a message
-- Message Reaction Remove	User removed a reaction from a message
-- Message Reaction Remove All	All reactions were explicitly removed from a message
-- Message Reaction Remove Emoji	All reactions for a given emoji were explicitly removed from a message
-- Presence Update	User was updated
-- Stage Instance Create	Stage instance was created
-- Stage Instance Update	Stage instance was updated
-- Stage Instance Delete	Stage instance was deleted or closed
-- Typing Start	User started typing in a channel
-- User Update	Properties about the user changed
-- Voice State Update	Someone joined, left, or moved a voice channel
-- Voice Server Update	Guild's voice server was updated
-- Webhooks Update	Guild channel webhook was created, update, or deleted

{- | Identify

* Send event
* Opcode 2

https://discord.com/developers/docs/topics/gateway-events#identify
-}
data EvIdentify = EvIdentify
    { evIdentifyToken :: !Token
    , evIdentifyProperties :: !ConnectionProperties
    , evCompress :: !(Maybe Bool)
    , evLargeThreshold :: !(Maybe Int)
    , evShard :: !(Maybe (Tuple2 ShardId Int))
    , evPresence :: !(Maybe EvUpdatePresence)
    , evIntents :: !Int  -- TODO: not a raw int
    }
    deriving (Eq, Show, Generic)

instance ToJSON EvIdentify where
    toJSON (EvIdentify token properties compress largeThreshold shard presence intents) =
        object [
            "token" .= token,
            "properties" .= properties,
            "intents" .= intents

            -- TODO(typesafety): Get these working when needed
            -- "compress" .= compress,
            -- "large_threshold" .= largeThreshold,
            -- "shard" .= shard,
            -- "presence" .= presence,
            ]

{- | Send event: https://discord.com/developers/docs/topics/gateway-events#update-presence-gateway-presence-update-structure

Opcode 3
-}
data EvUpdatePresence = EvUpdatePresence
    { evUpdatePresenceSince :: !(Maybe Int)
    , evUpdatePresenceActivities :: !(List Activity)
    , evUpdatePresenceStatus :: !Status
    , evUpdatePresenceAfk :: !Bool
    }
    deriving (Eq, Show, Generic)

instance ToJSON EvUpdatePresence where
    toJSON ev = object [
        "since" .= evUpdatePresenceSince ev,
        "activities" .= evUpdatePresenceActivities ev,
        "status" .= evUpdatePresenceStatus ev,
        "afk" .= evUpdatePresenceAfk ev
        ]

{- | Receive event: https://discord.com/developers/docs/topics/gateway-events#hello

Opcode 10
-}
data EvHello = EvHello
    { evHelloHeartbeatInterval :: !Int
    }
    deriving (Eq, Show, Generic)

instance FromJSON EvHello where
    parseJSON = withObject "EvHello" $ \event ->
        EvHello <$> event .: "heartbeat_interval"

-- * Other object types

-- | https://discord.com/developers/docs/topics/gateway-events#activity-object
data Activity = Activity {
    activityName :: Text,
    activityType :: ActivityType,
    activityCreatedAt :: Int  -- TODO(typesafety): Add Unix timestamp type
    -- TODO(typesafety): Finish implementing missing fields
    }
    deriving (Eq, Show, Generic)

instance ToJSON Activity where
    toJSON activity = Aeson.object [
        "name" .= activityName activity,
        "type" .= activityType activity,
        "created_at" .= activityCreatedAt activity
        ]

-- | https://discord.com/developers/docs/topics/gateway-events#activity-object-activity-types
data ActivityType
    = Playing
    | Streaming
    | Listening
    | Watching
    | Custom
    | Competing
    deriving (Eq, Show)

instance ToJSON ActivityType where
    toJSON = Aeson.Number . \case
        Playing -> 0
        Streaming -> 1
        Listening -> 2
        Watching -> 3
        Custom -> 4
        Competing -> 5

-- | https://discord.com/developers/docs/topics/gateway-events#update-presence-status-types
data Status
    = Online
    | Dnd
    | Idle
    | Invisible
    | Offline
    deriving (Eq, Show)

instance ToJSON Status where
    toJSON = Aeson.String . \case
        Online -> "online"
        Dnd -> "dnd"
        Idle -> "idle"
        Invisible -> "invisible"
        Offline -> "offline"

-- | https://discord.com/developers/docs/topics/gateway-events#identify-identify-connection-properties
data ConnectionProperties = ConnectionProperties
    { connectionPropertiesOs :: Text
    , connectionPropertiesBrowser :: Text
    , connectionPropertiesDevice :: Text
    }
    deriving (Eq, Show, Generic)

instance ToJSON ConnectionProperties where
    toJSON cp = Aeson.object [
        "os" .= connectionPropertiesOs cp,
        "browser" .= connectionPropertiesBrowser cp,
        "device" .= connectionPropertiesDevice cp
        ]

-- * Other types

-- | https://discord.com/developers/docs/topics/gateway#sharding
newtype ShardId = ShardId {unShardId :: Int}
    deriving (Eq, Show, Generic)

instance ToJSON ShardId where
    toJSON = Aeson.Number . fromIntegral . unShardId

-- | Opaque token newtype
newtype Token = Token {unToken :: Text}
    deriving (Eq, Generic)

instance ToJSON Token where
    toJSON = Aeson.String . unToken

instance Show Token where
    show = const "<TOKEN>"

-- | https://discord.com/developers/docs/topics/gateway#gateway-intents
data Intent
    = MESSAGE_REACTION_ADD
    | MESSAGE_REACTIVE_REMOVE
    | READY
    | MESSAGE_CREATE

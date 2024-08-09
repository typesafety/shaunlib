module Shaunlib.Internal.Gateway where

import Data.Aeson qualified as Aeson
import Data.ByteString qualified as BS
import Data.Text (pack, strip)
import Network.WebSockets (
    Connection,
    receiveData,
    sendTextData,
    toLazyByteString,
    )
import Wuss (runSecureClient)

-- import Control.Concurrent (forkIO)
import Control.Exception (throwIO)
import Control.Monad (forever)
import Data.Text.Encoding (decodeUtf8)
import Data.Tuple.Experimental (Unit)

import Shaunlib qualified as Shaunlib
import Shaunlib.AppEnv (BotEnv(..), Token(..))
import Shaunlib.Internal.Utils (putTxtLn, pShowTxt)

-- | Run the gateway.
-- See: https://discord.com/developers/docs/topics/gateway#connection-lifecycle
-- TODO(typesafety): should probably move this out
startGateway :: IO Unit
startGateway = runSecureClient "gateway.discord.gg" 443 "/" $ \connection -> do
    token <- Token . strip . pack <$> readFile "token"
    let botEnv = BotEnv {
            botEnvToken = token,
            botEnvConnection = connection
            }

    gateway botEnv

gateway :: BotEnv -> IO Unit
gateway botEnv = do
    -- Initialise the handshake
    evHello <- handshake botEnv
    putTxtLn $ "TODO: successfully shook hands, what now? message:\n" <> pShowTxt evHello

    -- Start listening for events
    listenForGatewayEvents botEnv

handshake :: BotEnv -> IO Shaunlib.EvHello
handshake env = do
    -- Send the Identify event
    let identifyPayload = makeIdentifyPayload (botEnvToken env)
    let encodedIdentify = Aeson.encode identifyPayload
    putTxtLn $ "Sending Identify payload:\n" <> (pShowTxt identifyPayload)
    sendTextData (botEnvConnection env) encodedIdentify

    -- Wait for the Hello response
    let waitForHello = do
            payload <- receiveAndDecodeGatewayMessage (botEnvConnection env)
            case Shaunlib.payloadOpcode payload of
                10 -> pure payload
                _ -> waitForHello

    waitForHello >>= \payload -> case Shaunlib.payloadEventData payload of
        Just eventData -> case Aeson.fromJSON eventData of
            Aeson.Error errMsg -> throwIO $ Shaunlib.GenericDiscordError (pack errMsg)
            Aeson.Success evHello -> pure evHello

        Nothing -> throwIO
            $ Shaunlib.GenericDiscordError ("Malformed Hello payload; missing data: \n" <> pShowTxt payload)
  where
    makeIdentifyPayload :: Token -> Shaunlib.Payload
    makeIdentifyPayload token = Shaunlib.Payload {
        payloadOpcode = 2,
        payloadEventData = Just (Aeson.toJSON (makeIdentifyEvent token)),
        payloadSequenceNumber = Nothing,
        payloadEventName = Nothing
        }

    makeIdentifyEvent :: Token -> Shaunlib.EvIdentify
    makeIdentifyEvent token = Shaunlib.EvIdentify {
        evIdentifyToken = token,
        evIdentifyProperties = Shaunlib.ConnectionProperties {
            connectionPropertiesOs = "os",
            connectionPropertiesBrowser = "shaunlib",
            connectionPropertiesDevice = "shaunlib"
            },
        evCompress = Nothing,
        evLargeThreshold = Nothing,
        evShard = Nothing,
        evPresence = Nothing,
        evIntents = 46850
        }

-- | Runs in a loop listening for gateway events, and performs actions depending
-- on the events it receives.
listenForGatewayEvents :: BotEnv -> IO Unit
listenForGatewayEvents env = forever do
    putTxtLn "Waiting for gateway events..."

    payload <- receiveAndDecodeGatewayMessage (botEnvConnection env)
    case Shaunlib.payloadOpcode payload of
        -- Event dispatched
        0 -> handleDispatchEvent payload

        n -> putTxtLn $ "Opcode not yet supported: " <> pack (show n)

  where
    handleDispatchEvent :: Shaunlib.Payload -> IO Unit
    handleDispatchEvent payload = do
        putTxtLn "handleDispatchEvent: TODO"
        putTxtLn $ "Message:\n" <> pShowTxt payload

-- | Receive and decode to JSON a payload from the gateway.
--
-- Throw an exception on decoding failure.
receiveAndDecodeGatewayMessage :: Connection -> IO Shaunlib.Payload
receiveAndDecodeGatewayMessage connection = do
    message <- receiveData @BS.ByteString connection
    let decodedMessage = Aeson.decode (toLazyByteString message)

    case decodedMessage of
        Just payload -> do
            putTxtLn $ "Received gateway event with opcode: " <> pack (show (Shaunlib.payloadOpcode payload))
            putTxtLn $ "Payload content: \n" <> (pShowTxt payload)
            pure payload
        Nothing -> throwIO exception
          where
            exception = Shaunlib.GenericDiscordError
                $ "Failed to decode gateway event payload. Message:\n" <> (decodeUtf8 message)

module Shaunlib.AppEnv (
    BotEnv(..),
    Token(..),
) where

import Data.Aeson qualified as Aeson
import Data.Aeson (ToJSON)
import Data.Text (Text)
import Network.WebSockets (Connection)


-- * Application environment

-- | Application environment.  Holds connection handles, configuration settings,
-- the bot token etc.
data BotEnv = BotEnv {
    botEnvToken :: Token,
    botEnvConnection :: Connection
    }

-- | Opaque token newtype
newtype Token = Token {unToken :: Text}
    deriving (Eq)

instance ToJSON Token where
    toJSON = Aeson.String . unToken

instance Show Token where
    show = const "<TOKEN>"

module Shaunlib.Internal.Gateway where

import Data.Tuple.Experimental (Unit)

import Network.WebSockets (Connection)

import Shaunlib qualified as Shaunlib
import Shaunlib.Internal.Utils


-- | Perform initial handshake and start heartbeat loop.
setup :: Connection -> IO Unit
setup = todo

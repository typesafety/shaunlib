import Wuss

import Control.Concurrent (forkIO)
import Control.Monad (forever, void)
import Data.Text (Text, pack, unpack)
import Network.WebSockets (ClientApp, receiveData, sendClose, sendTextData)

main :: IO ()
main = runSecureClient "gateway.discord.gg" 443 "/" ws

ws :: ClientApp ()
ws connection = do
    putStrLn "Connected!"
    file <- readFile "./testdata.json"

    void . forkIO . forever $ do
        message <- receiveData connection
        putStrLn $ unpack (message :: Text)

    let loop = do
            _ <- getLine
            let line1 = file

            sendTextData connection (pack line1)

            loop
    _ <- loop

    sendClose connection (pack "Bye!")

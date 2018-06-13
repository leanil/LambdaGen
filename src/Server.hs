module Server where

import qualified Network.WebSockets as WS
import qualified Data.Text as T

main :: IO ()
main = do
    WS.runServer "localhost" 2103 $ compiler

compiler :: WS.ServerApp
compiler pending = do
    conn <- WS.acceptRequest pending
    WS.forkPingThread conn 30
    msg <- WS.receiveData conn
    putStrLn $ T.unpack msg
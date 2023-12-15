module ClientNetwork where

import Network.Socket
import System.IO
import Control.Concurrent (forkIO, ThreadId)
import Control.Monad (forever, unless)

connectToServer :: String -> String -> IO (Socket, Handle)
connectToServer host port = do
    addrinfos <- getAddrInfo Nothing (Just host) (Just port)
    let serveraddr = head addrinfos
    sock <- socket (addrFamily serveraddr) Stream defaultProtocol
    connect sock (addrAddress serveraddr)
    handle <- socketToHandle sock ReadWriteMode
    hSetBuffering handle LineBuffering
    return (sock, handle)

sendMessage :: Handle -> String -> IO ()
sendMessage handle msg = hPutStrLn handle msg

receiveMessages :: Handle -> (String -> IO ()) -> IO ThreadId
receiveMessages handle callback = forkIO $ forever $ do
    msg <- hGetLine handle
    callback msg

module Main where

import qualified Graphics.UI.Threepenny as UI
import qualified Data.ByteString.Char8 as C
import qualified Network.Socket.ByteString as NSB
import Graphics.UI.Threepenny.Core
import Network.Socket
import Control.Concurrent (forkIO)
import Control.Concurrent
import Control.Concurrent.STM
import System.IO
import Control.Monad (forever, unless, forM_)
import Control.Exception (finally, bracket, catch, handle, IOException)

type Client = (Socket, TChan String)
type ClientList = TVar [Client]

startServer :: IO ()
startServer = withSocketsDo $ do
    addrinfos <- getAddrInfo (Just (defaultHints {addrFlags = [AI_PASSIVE]})) Nothing (Just "3000")
    let serveraddr = head addrinfos
    sock <- socket (addrFamily serveraddr) Stream defaultProtocol
    bind sock (addrAddress serveraddr)
    listen sock 1
    putStrLn "Listening on port 3000"

    clients <- atomically $ newTVar []  -- TVar for managing clients
    forever $ do
        (conn, _) <- accept sock
        forkIO $ runConn conn clients

runConn :: Socket -> ClientList -> IO ()
runConn conn clients = do
    chan <- atomically newTChan
    let client = (conn, chan)
    atomically $ modifyTVar' clients (client :)
    clientHandler conn clients

clientHandler :: Socket -> ClientList -> IO ()
clientHandler conn clients = handleClient conn `catch` clientDisconnected
  where
    handleClient sock = do
        msg <- NSB.recv sock 1024
        unless (C.null msg) $ do
            let receivedMsg = C.unpack msg
            putStrLn $ "Received message: " ++ receivedMsg
            broadcast receivedMsg clients
            handleClient sock

    clientDisconnected :: IOException -> IO ()
    clientDisconnected e = do
        putStrLn $ "Client disconnected abruptly: " ++ show e
        removeClient conn clients

removeClient :: Socket -> ClientList -> IO ()
removeClient conn clients = do
    putStrLn "Removing client"  -- Log client removal
    atomically $ modifyTVar' clients $ filter ((/= conn) . fst)

broadcast :: String -> ClientList -> IO ()
broadcast msg clients = do
    clist <- atomically $ readTVar clients
    forM_ clist $ \(sock, _) -> NSB.send sock (C.pack msg) `catch` handleSendException sock
  where
    handleSendException :: Socket -> IOException -> IO Int
    handleSendException sock e = do
        putStrLn $ "Broadcast exception for client: " ++ show sock ++ ", error: " ++ show e
        return 0  -- Return a default value indicating error

sendMsg :: String -> IO ()
sendMsg msg = withSocketsDo $ do
    putStrLn $ "Sending message: " ++ msg  -- Debug print
    addrinfos <- getAddrInfo Nothing (Just "localhost") (Just "3000")
    let serveraddr = head addrinfos
    sock <- socket (addrFamily serveraddr) Stream defaultProtocol
    connect sock (addrAddress serveraddr)
    NSB.send sock (C.pack msg)
    close sock

setup :: Window -> UI ()
setup window = do
    return window # set title "Haskell Chat"

    -- UI elements
    input <- UI.input
    sendButton <- UI.button #+ [string "Send"]
    output <- UI.div

    -- Layout
    getBody window #+ [element input, element sendButton, element output]

    -- Event handling
    on UI.click sendButton $ \_ -> do
        message <- get value input
        liftIO $ sendMsg message  -- Send message to server
        newMsg <- UI.div #+ [string ("You: " ++ message)]
        element output #+ [element newMsg]
        element input # set value ""

main :: IO ()
main = do
    forkIO startServer  -- Start the server in a separate thread
    startGUI defaultConfig setup  -- Start the GUI

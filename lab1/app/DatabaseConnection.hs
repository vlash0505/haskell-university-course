module DatabaseConnection where

import Database.MySQL.Base
import qualified System.IO.Streams as Streams
import qualified Data.ByteString.Char8 as BS

-- Database connection info
connectInfo :: ConnectInfo
connectInfo = defaultConnectInfo {
    ciUser = BS.pack "root",
    ciPassword = BS.pack "rootroot",
    ciDatabase = BS.pack "faculty_network"
}

-- Function to establish a connection
getConnection :: IO MySQLConn
getConnection = connect connectInfo

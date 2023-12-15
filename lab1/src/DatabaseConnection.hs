module DatabaseConnection where

import Database.MySQL.Base
import qualified System.IO.Streams as Streams

-- Database connection info
connectInfo :: ConnectInfo
connectInfo = defaultConnectInfo {
    ciUser = "root",
    ciPassword = "rootroot",
    ciDatabase = "faculty_network"
}

-- Function to establish a connection
getConnection :: IO MySQLConn
getConnection = connect connectInfo

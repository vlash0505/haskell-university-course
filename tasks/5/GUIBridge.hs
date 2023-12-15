module GUIBridge where

import Qtc.Classes.Qccs
import Qtc.ClassTypes.Gui
import Qtc.Gui.Base
import Qtc.Gui.QApplication
import Qtc.Gui.QWidget
import Qtc.Gui.QPushButton
import Qtc.Gui.QTextEdit
import Qtc.Gui.QVBoxLayout
import qualified Data.Text as T

import ClientNetwork

main :: IO ()
main = do
    (sock, handle) <- connectToServer "127.0.0.1" "3000"
    qApplication ()

    -- Setup GUI
    mainWindow <- qWidget ()
    layout <- qVBoxLayout mainWindow
    messageArea <- qTextEdit mainWindow
    setReadOnly messageArea True
    addWidget layout messageArea
    inputField <- qTextEdit mainWindow
    addWidget layout inputField
    sendButton <- qPushButton "Send" mainWindow
    addWidget layout sendButton
    setLayout mainWindow layout
    setWindowTitle mainWindow "Chat Application"
    resize mainWindow (300, 200)

    -- Update GUI with received messages
    _ <- receiveMessages handle $ \msg ->
        append messageArea $ T.pack msg

    -- Send message when button is clicked
    connectSlot sendButton "clicked()" mainWindow "sendMessage()" $ do
        text <- toPlainText inputField
        unless (T.null text) $ do
            let msg = T.unpack text
            sendMessage handle msg
            setPlainText inputField ""

    qshow mainWindow ()
    qApplicationExec ()
    hClose handle
    close sock

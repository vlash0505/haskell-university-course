module CRUDOperations where

import Entities
import DatabaseConnection (getConnection)
import Database.MySQL.Base
import qualified Data.Text as T
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as BSL
import qualified System.IO.Streams as Streams

-- Helper functions for type conversion
intToMySQL :: Int -> MySQLValue
intToMySQL = MySQLInt32 . fromIntegral

textToMySQL :: String -> MySQLValue
textToMySQL = MySQLText . T.pack

queryToByteString :: String -> Query
queryToByteString = Query . BSL.fromStrict . BSC.pack

-- Article Operations
createArticle :: Article -> IO ()
createArticle (Article id title authorId annotation) = do
    conn <- getConnection
    let q = queryToByteString "INSERT INTO Articles (ArticleID, Title, AuthorID, AnnotationText) VALUES (?, ?, ?, ?)"
    execute conn q [intToMySQL id, textToMySQL title, intToMySQL authorId, textToMySQL annotation]
    close conn

fetchArticle :: Int -> IO (Maybe Article)
fetchArticle articleId = do
    conn <- getConnection
    let q = queryToByteString "SELECT ArticleID, Title, AuthorID, AnnotationText FROM Articles WHERE ArticleID = ?"
    stmt <- prepareStmt conn q
    (defs, is) <- queryStmt conn stmt [intToMySQL articleId]
    rows <- Streams.toList is
    close conn
    return $ if null rows then Nothing else Just (convertToArticle (head rows))

convertToArticle :: [MySQLValue] -> Article
convertToArticle [MySQLInt32 id, MySQLText title, MySQLInt32 authorId, MySQLText annotation] =
    Article (fromIntegral id) (T.unpack title) (fromIntegral authorId) (T.unpack annotation)
convertToArticle _ = error "Invalid row format"

updateArticle :: Article -> IO ()
updateArticle (Article id title authorId annotation) = do
    conn <- getConnection
    let q = queryToByteString "UPDATE Articles SET Title = ?, AuthorID = ?, AnnotationText = ? WHERE ArticleID = ?"
    execute conn q [textToMySQL title, intToMySQL authorId, textToMySQL annotation, intToMySQL id]
    close conn

deleteArticle :: Int -> IO ()
deleteArticle articleId = do
    conn <- getConnection
    let q = queryToByteString "DELETE FROM Articles WHERE ArticleID = ?"
    execute conn q [intToMySQL articleId]
    close conn

-- Reader Operations
createReader :: Reader -> IO ()
createReader (Reader id name contactInfo) = do
    conn <- getConnection
    let q = queryToByteString "INSERT INTO Readers (ReaderID, Name, ContactInfo) VALUES (?, ?, ?)"
    execute conn q [intToMySQL id, textToMySQL name, textToMySQL contactInfo]
    close conn

fetchReader :: Int -> IO (Maybe Reader)
fetchReader readerId = do
    conn <- getConnection
    let q = queryToByteString "SELECT ReaderID, Name, ContactInfo FROM Readers WHERE ReaderID = ?"
    stmt <- prepareStmt conn q
    (defs, is) <- queryStmt conn stmt [intToMySQL readerId]
    rows <- Streams.toList is
    close conn
    return $ if null rows then Nothing else Just (convertToReader (head rows))

convertToReader :: [MySQLValue] -> Reader
convertToReader [MySQLInt32 id, MySQLText name, MySQLText contactInfo] =
    Reader (fromIntegral id) (T.unpack name) (T.unpack contactInfo)
convertToReader _ = error "Invalid row format"

updateReader :: Reader -> IO ()
updateReader (Reader id name contactInfo) = do
    conn <- getConnection
    let q = queryToByteString "UPDATE Readers SET Name = ?, ContactInfo = ? WHERE ReaderID = ?"
    execute conn q [textToMySQL name, textToMySQL contactInfo, intToMySQL id]
    close conn

deleteReader :: Int -> IO ()
deleteReader readerId = do
    conn <- getConnection
    let q = queryToByteString "DELETE FROM Readers WHERE ReaderID = ?"
    execute conn q [intToMySQL readerId]
    close conn

-- Feedback Operations
createFeedback :: Feedback -> IO ()
createFeedback (Feedback id articleId readerId text) = do
    conn <- getConnection
    let q = queryToByteString "INSERT INTO Feedbacks (FeedbackID, ArticleID, ReaderID, Text) VALUES (?, ?, ?, ?)"
    execute conn q [intToMySQL id, intToMySQL articleId, intToMySQL readerId, textToMySQL text]
    close conn

fetchFeedback :: Int -> IO (Maybe Feedback)
fetchFeedback feedbackId = do
    conn <- getConnection
    let q = queryToByteString "SELECT FeedbackID, ArticleID, ReaderID, Text FROM Feedbacks WHERE FeedbackID = ?"
    stmt <- prepareStmt conn q
    (defs, is) <- queryStmt conn stmt [intToMySQL feedbackId]
    rows <- Streams.toList is
    close conn
    return $ if null rows then Nothing else Just (convertToFeedback (head rows))

convertToFeedback :: [MySQLValue] -> Feedback
convertToFeedback [MySQLInt32 id, MySQLInt32 articleId, MySQLInt32 readerId, MySQLText text] =
    Feedback (fromIntegral id) (fromIntegral articleId) (fromIntegral readerId) (T.unpack text)
convertToFeedback _ = error "Invalid row format"

updateFeedback :: Feedback -> IO ()
updateFeedback (Feedback id articleId readerId text) = do
    conn <- getConnection
    let q = queryToByteString "UPDATE Feedbacks SET ArticleID = ?, ReaderID = ?, Text = ? WHERE FeedbackID = ?"
    execute conn q [intToMySQL articleId, intToMySQL readerId, textToMySQL text, intToMySQL id]
    close conn

deleteFeedback :: Int -> IO ()
deleteFeedback feedbackId = do
    conn <- getConnection
    let q = queryToByteString "DELETE FROM Feedbacks WHERE FeedbackID = ?"
    execute conn q [intToMySQL feedbackId]
    close conn

-- Statistic Operations
createStatistic :: Statistic -> IO ()
createStatistic (Statistic articleId views likes shares) = do
    conn <- getConnection
    let q = queryToByteString "INSERT INTO Statistics (ArticleID, Views, Likes, Shares) VALUES (?, ?, ?, ?)"
    execute conn q [intToMySQL articleId, intToMySQL views, intToMySQL likes, intToMySQL shares]
    close conn

fetchStatistic :: Int -> IO (Maybe Statistic)
fetchStatistic articleId = do
    conn <- getConnection
    let q = queryToByteString "SELECT ArticleID, Views, Likes, Shares FROM Statistics WHERE ArticleID = ?"
    stmt <- prepareStmt conn q
    (defs, is) <- queryStmt conn stmt [intToMySQL articleId]
    rows <- Streams.toList is
    close conn
    return $ if null rows then Nothing else Just (convertToStatistic (head rows))

convertToStatistic :: [MySQLValue] -> Statistic
convertToStatistic [MySQLInt32 articleId, MySQLInt32 views, MySQLInt32 likes, MySQLInt32 shares] =
    Statistic (fromIntegral articleId) (fromIntegral views) (fromIntegral likes) (fromIntegral shares)
convertToStatistic _ = error "Invalid row format"

updateStatistic :: Statistic -> IO ()
updateStatistic (Statistic articleId views likes shares) = do
    conn <- getConnection
    let q = queryToByteString "UPDATE Statistics SET Views = ?, Likes = ?, Shares = ? WHERE ArticleID = ?"
    execute conn q [intToMySQL views, intToMySQL likes, intToMySQL shares, intToMySQL articleId]
    close conn

deleteStatistic :: Int -> IO ()
deleteStatistic articleId = do
    conn <- getConnection
    let q = queryToByteString "DELETE FROM Statistics WHERE ArticleID = ?"
    execute conn q [intToMySQL articleId]
    close conn

module CRUDOperations where

import Entities
import DatabaseConnection
import Database.MySQL.Base
import DatabaseConnection (getConnection)

--article entity operations ---------------------------------------------------------------------------
createArticle :: Article -> IO ()
createArticle (Article id title authorId annotation) = do
    conn <- getConnection
    let q = "INSERT INTO Articles (ArticleID, Title, AuthorID, AnnotationText) VALUES (?, ?, ?, ?)"
    execute conn q [MySQLInt32 id, MySQLText title, MySQLInt32 authorId, MySQLText annotation]
    close conn

fetchArticle :: Int -> IO (Maybe Article)
fetchArticle articleId = do
    conn <- getConnection
    let q = "SELECT ArticleID, Title, AuthorID, AnnotationText FROM Articles WHERE ArticleID = ?"
    stmt <- prepareStmt conn q
    (defs, is) <- queryStmt conn stmt [MySQLInt32 articleId]
    rows <- Streams.toList is
    close conn
    return $ if null rows then Nothing else Just (convertToArticle (head rows))

convertToArticle :: [MySQLValue] -> Article
convertToArticle [MySQLInt32 id, MySQLText title, MySQLInt32 authorId, MySQLText annotation] =
    Article id title authorId annotation
convertToArticle _ = error "Invalid row format"

updateArticle :: Article -> IO ()
updateArticle (Article id title authorId annotation) = do
    conn <- getConnection
    let q = "UPDATE Articles SET Title = ?, AuthorID = ?, AnnotationText = ? WHERE ArticleID = ?"
    execute conn q [MySQLText title, MySQLInt32 authorId, MySQLText annotation, MySQLInt32 id]
    close conn

deleteArticle :: Int -> IO ()
deleteArticle articleId = do
    conn <- getConnection
    let q = "DELETE FROM Articles WHERE ArticleID = ?"
    execute conn q [MySQLInt32 articleId]
    close conn

--reader entity operations ------------------------------------------------------------------------------
createReader :: Reader -> IO ()
createReader (Reader id name contact) = do
    conn <- getConnection
    let q = "INSERT INTO Readers (ReaderID, Name, ContactInfo) VALUES (?, ?, ?)"
    execute conn q [MySQLInt32 id, MySQLText name, MySQLText contact]
    close conn

fetchReader :: Int -> IO (Maybe Reader)
fetchReader readerId = do
    conn <- getConnection
    let q = "SELECT ReaderID, Name, ContactInfo FROM Readers WHERE ReaderID = ?"
    stmt <- prepareStmt conn q
    (defs, is) <- queryStmt conn stmt [MySQLInt32 readerId]
    rows <- Streams.toList is
    close conn
    return $ if null rows then Nothing else Just (convertToReader (head rows))

convertToReader :: [MySQLValue] -> Reader
convertToReader [MySQLInt32 id, MySQLText name, MySQLText contactInfo] =
    Reader id name contactInfo
convertToReader _ = error "Invalid row format"

updateReader :: Reader -> IO ()
updateReader (Reader id name contactInfo) = do
    conn <- getConnection
    let q = "UPDATE Readers SET Name = ?, ContactInfo = ? WHERE ReaderID = ?"
    execute conn q [MySQLText name, MySQLText contactInfo, MySQLInt32 id]
    close conn

deleteReader :: Int -> IO ()
deleteReader readerId = do
    conn <- getConnection
    let q = "DELETE FROM Readers WHERE ReaderID = ?"
    execute conn q [MySQLInt32 readerId]
    close conn

--feedback entity operations ------------------------------------------------------------------------------
createFeedback :: Feedback -> IO ()
createFeedback (Feedback id articleId readerId text) = do
    conn <- getConnection
    let q = "INSERT INTO Feedbacks (FeedbackID, ArticleID, ReaderID, Text) VALUES (?, ?, ?, ?)"
    execute conn q [MySQLInt32 id, MySQLInt32 articleId, MySQLInt32 readerId, MySQLText text]
    close conn

fetchFeedback :: Int -> IO (Maybe Feedback)
fetchFeedback feedbackId = do
    conn <- getConnection
    let q = "SELECT FeedbackID, ArticleID, ReaderID, Text FROM Feedbacks WHERE FeedbackID = ?"
    stmt <- prepareStmt conn q
    (defs, is) <- queryStmt conn stmt [MySQLInt32 feedbackId]
    rows <- Streams.toList is
    close conn
    return $ if null rows then Nothing else Just (convertToFeedback (head rows))

convertToFeedback :: [MySQLValue] -> Feedback
convertToFeedback [MySQLInt32 id, MySQLInt32 articleId, MySQLInt32 readerId, MySQLText text] =
    Feedback id articleId readerId text
convertToFeedback _ = error "Invalid row format"

updateFeedback :: Feedback -> IO ()
updateFeedback (Feedback id articleId readerId text) = do
    conn <- getConnection
    let q = "UPDATE Feedbacks SET ArticleID = ?, ReaderID = ?, Text = ? WHERE FeedbackID = ?"
    execute conn q [MySQLInt32 articleId, MySQLInt32 readerId, MySQLText text, MySQLInt32 id]
    close conn

deleteFeedback :: Int -> IO ()
deleteFeedback feedbackId = do
    conn <- getConnection
    let q = "DELETE FROM Feedbacks WHERE FeedbackID = ?"
    execute conn q [MySQLInt32 feedbackId]
    close conn


--statistics entity operations ------------------------------------------------------------------------------
createStatistic :: Statistic -> IO ()
createStatistic (Statistic articleId views likes shares) = do
    conn <- getConnection
    let q = "INSERT INTO Statistics (ArticleID, Views, Likes, Shares) VALUES (?, ?, ?, ?)"
    execute conn q [MySQLInt32 articleId, MySQLInt32 views, MySQLInt32 likes, MySQLInt32 shares]
    close conn

fetchStatistic :: Int -> IO (Maybe Statistic)
fetchStatistic articleId = do
    conn <- getConnection
    let q = "SELECT ArticleID, Views, Likes, Shares FROM Statistics WHERE ArticleID = ?"
    stmt <- prepareStmt conn q
    (defs, is) <- queryStmt conn stmt [MySQLInt32 articleId]
    rows <- Streams.toList is
    close conn
    return $ if null rows then Nothing else Just (convertToStatistic (head rows))

convertToStatistic :: [MySQLValue] -> Statistic
convertToStatistic [MySQLInt32 articleId, MySQLInt32 views, MySQLInt32 likes, MySQLInt32 shares] =
    Statistic articleId views likes shares
convertToStatistic _ = error "Invalid row format"

updateStatistic :: Statistic -> IO ()
updateStatistic (Statistic articleId views likes shares) = do
    conn <- getConnection
    let q = "UPDATE Statistics SET Views = ?, Likes = ?, Shares = ? WHERE ArticleID = ?"
    execute conn q [MySQLInt32 views, MySQLInt32 likes, MySQLInt32 shares, MySQLInt32 articleId]
    close conn

deleteStatistic :: Int -> IO ()
deleteStatistic articleId = do
    conn <- getConnection
    let q = "DELETE FROM Statistics WHERE ArticleID = ?"
    execute conn q [MySQLInt32 articleId]
    close conn

module Main where

import CRUDOperations
import Entities
import Text.Read (readMaybe)

main :: IO ()
main = do
    putStrLn "Welcome to the Faculty Network Newspaper System =========================================="
    mainMenu

mainMenu :: IO ()
mainMenu = do
    putStrLn "Select an option: ==========================================================="
    putStrLn "1. Manage Articles"
    putStrLn "2. Manage Readers"
    putStrLn "3. Manage Feedback"
    putStrLn "4. Manage Statistics"
    putStrLn "5. Exit"
    option <- getLine
    case option of
        "1" -> manageArticles
        "2" -> manageReaders
        "3" -> manageFeedbacks
        "4" -> manageStatistics
        "5" -> putStrLn "Exiting the system."
        _   -> putStrLn "Invalid option" >> mainMenu

--manage articles input--------------------------------------------------------------------------
manageArticles :: IO ()
manageArticles = do
    putStrLn "Article Management: ==========================================================="
    putStrLn "1. Add an Article"
    putStrLn "2. View an Article"
    putStrLn "3. Update an Article"
    putStrLn "4. Delete an Article"
    putStrLn "5. Return to Main Menu"
    option <- getLine
    case option of
        "1" -> addArticleOperation >> manageArticles
        "2" -> viewArticleOperation >> manageArticles
        "3" -> updateArticleOperation >> manageArticles
        "4" -> deleteArticleOperation >> manageArticles
        "5" -> mainMenu
        _   -> putStrLn "Invalid option" >> manageArticles

addArticleOperation :: IO ()
addArticleOperation = do
    putStrLn "Enter article details: ==========================================================="
    putStrLn "Title:"
    title <- getLine
    putStrLn "Author ID:"
    authorIdStr <- getLine
    putStrLn "Annotation Text:"
    annotation <- getLine
    case readMaybe authorIdStr of
        Just authorId -> do
            -- Assuming ArticleID is auto-incremented
            let article = Article 0 title authorId annotation
            createArticle article
            putStrLn "Article added successfully!"
        Nothing -> putStrLn "Invalid Author ID" >> addArticleOperation

viewArticleOperation :: IO ()
viewArticleOperation = do
    putStrLn "Enter the ID of the article to view:"
    articleIdStr <- getLine
    case readMaybe articleIdStr :: Maybe Int of
        Just articleId -> do
            maybeArticle <- fetchArticle articleId
            case maybeArticle of
                Just article -> print article
                Nothing -> putStrLn "Article not found."
        Nothing -> putStrLn "Invalid Article ID" >> viewArticleOperation

updateArticleOperation :: IO ()
updateArticleOperation = do
    putStrLn "Enter the ID of the article to update:"
    articleIdStr <- getLine
    putStrLn "Enter new title:"
    newTitle <- getLine
    putStrLn "Enter new author ID:"
    newAuthorIdStr <- getLine
    putStrLn "Enter new annotation text:"
    newAnnotation <- getLine
    case (readMaybe articleIdStr, readMaybe newAuthorIdStr) of
        (Just articleId, Just newAuthorId) -> do
            let article = Article articleId newTitle newAuthorId newAnnotation
            updateArticle article
            putStrLn "Article updated successfully!"
        _ -> putStrLn "Invalid input" >> updateArticleOperation

deleteArticleOperation :: IO ()
deleteArticleOperation = do
    putStrLn "Enter the ID of the article to delete:"
    articleIdStr <- getLine
    case readMaybe articleIdStr :: Maybe Int of
        Just articleId -> do
            deleteArticle articleId
            putStrLn "Article deleted successfully!"
        Nothing -> putStrLn "Invalid Article ID" >> deleteArticleOperation

--manage readers input--------------------------------------------------------------------------
manageReaders :: IO ()
manageReaders = do
    putStrLn "Reader Management: ==========================================================="
    putStrLn "1. Add a Reader"
    putStrLn "2. View a Reader"
    putStrLn "3. Update a Reader"
    putStrLn "4. Delete a Reader"
    putStrLn "5. Return to Main Menu"
    option <- getLine
    case option of
        "1" -> addReaderOperation >> manageReaders
        "2" -> viewReaderOperation >> manageReaders
        "3" -> updateReaderOperation >> manageReaders
        "4" -> deleteReaderOperation >> manageReaders
        "5" -> mainMenu
        _   -> putStrLn "Invalid option" >> manageReaders

addReaderOperation :: IO ()
addReaderOperation = do
    putStrLn "Enter reader's name:"
    name <- getLine
    putStrLn "Enter reader's contact info:"
    contactInfo <- getLine
    -- Assuming ReaderID is auto-incremented
    let reader = Reader 0 name contactInfo
    createReader reader
    putStrLn "Reader added successfully!"

viewReaderOperation :: IO ()
viewReaderOperation = do
    putStrLn "Enter the ID of the reader to view:"
    readerIdStr <- getLine
    case readMaybe readerIdStr :: Maybe Int of
        Just readerId -> do
            maybeReader <- fetchReader readerId
            case maybeReader of
                Just reader -> print reader
                Nothing -> putStrLn "Reader not found."
        Nothing -> putStrLn "Invalid Reader ID" >> viewReaderOperation

updateReaderOperation :: IO ()
updateReaderOperation = do
    putStrLn "Enter the ID of the reader to update:"
    readerIdStr <- getLine
    putStrLn "Enter new name:"
    newName <- getLine
    putStrLn "Enter new contact info:"
    newContactInfo <- getLine
    case readMaybe readerIdStr :: Maybe Int of
        Just readerId -> do
            let reader = Reader readerId newName newContactInfo
            updateReader reader
            putStrLn "Reader updated successfully!"
        Nothing -> putStrLn "Invalid input" >> updateReaderOperation

deleteReaderOperation :: IO ()
deleteReaderOperation = do
    putStrLn "Enter the ID of the reader to delete:"
    readerIdStr <- getLine
    case readMaybe readerIdStr :: Maybe Int of
        Just readerId -> do
            deleteReader readerId
            putStrLn "Reader deleted successfully!"
        Nothing -> putStrLn "Invalid Reader ID" >> deleteReaderOperation

--manage feedbacks input--------------------------------------------------------------------------
manageFeedbacks :: IO ()
manageFeedbacks = do
    putStrLn "Feedback Management: ==========================================================="
    putStrLn "1. Add a Feedback"
    putStrLn "2. View a Feedback"
    putStrLn "3. Update a Feedback"
    putStrLn "4. Delete a Feedback"
    putStrLn "5. Return to Main Menu"
    option <- getLine
    case option of
        "1" -> addFeedbackOperation >> manageFeedbacks
        "2" -> viewFeedbackOperation >> manageFeedbacks
        "3" -> updateFeedbackOperation >> manageFeedbacks
        "4" -> deleteFeedbackOperation >> manageFeedbacks
        "5" -> mainMenu
        _   -> putStrLn "Invalid option" >> manageFeedbacks

addFeedbackOperation :: IO ()
addFeedbackOperation = do
    putStrLn "Enter article ID for the feedback:"
    articleIdStr <- getLine
    putStrLn "Enter reader ID for the feedback:"
    readerIdStr <- getLine
    putStrLn "Enter feedback text:"
    text <- getLine
    case (readMaybe articleIdStr, readMaybe readerIdStr) :: (Maybe Int, Maybe Int) of
        (Just articleId, Just readerId) -> do
            -- Assuming FeedbackID is auto-incremented
            let feedback = Feedback 0 articleId readerId text
            createFeedback feedback
            putStrLn "Feedback added successfully!"
        _ -> putStrLn "Invalid input" >> addFeedbackOperation

viewFeedbackOperation :: IO ()
viewFeedbackOperation = do
    putStrLn "Enter the ID of the feedback to view:"
    feedbackIdStr <- getLine
    case readMaybe feedbackIdStr :: Maybe Int of
        Just feedbackId -> do
            maybeFeedback <- fetchFeedback feedbackId
            case maybeFeedback of
                Just feedback -> print feedback
                Nothing -> putStrLn "Feedback not found."
        Nothing -> putStrLn "Invalid Feedback ID" >> viewFeedbackOperation

updateFeedbackOperation :: IO ()
updateFeedbackOperation = do
    putStrLn "Enter the ID of the feedback to update:"
    feedbackIdStr <- getLine
    putStrLn "Enter new article ID:"
    newArticleIdStr <- getLine
    putStrLn "Enter new reader ID:"
    newReaderIdStr <- getLine
    putStrLn "Enter new feedback text:"
    newText <- getLine
    case (readMaybe feedbackIdStr, readMaybe newArticleIdStr, readMaybe newReaderIdStr) :: (Maybe Int, Maybe Int, Maybe Int) of
        (Just feedbackId, Just newArticleId, Just newReaderId) -> do
            let feedback = Feedback feedbackId newArticleId newReaderId newText
            updateFeedback feedback
            putStrLn "Feedback updated successfully!"
        _ -> putStrLn "Invalid input" >> updateFeedbackOperation

deleteFeedbackOperation :: IO ()
deleteFeedbackOperation = do
    putStrLn "Enter the ID of the feedback to delete:"
    feedbackIdStr <- getLine
    case readMaybe feedbackIdStr :: Maybe Int of
        Just feedbackId -> do
            deleteFeedback feedbackId
            putStrLn "Feedback deleted successfully!"
        Nothing -> putStrLn "Invalid Feedback ID" >> deleteFeedbackOperation


--manage statistic input--------------------------------------------------------------------------
manageStatistics :: IO ()
manageStatistics = do
    putStrLn "Feedback Management: ==========================================================="
    putStrLn "1. Add a Statistic"
    putStrLn "2. View a Statistic"
    putStrLn "3. Update a Statistic"
    putStrLn "4. Delete a Statistic"
    putStrLn "5. Return to Main Menu"
    option <- getLine
    case option of
        "1" -> addStatisticOperation >> manageStatistics
        "2" -> viewStatisticOperation >> manageStatistics
        "3" -> updateStatisticOperation >> manageStatistics
        "4" -> deleteStatisticOperation >> manageStatistics
        "5" -> mainMenu
        _   -> putStrLn "Invalid option" >> manageStatistics

addStatisticOperation :: IO ()
addStatisticOperation = do
    putStrLn "Enter article ID for the statistic:"
    articleIdStr <- getLine
    putStrLn "Enter number of views:"
    viewsStr <- getLine
    putStrLn "Enter number of likes:"
    likesStr <- getLine
    putStrLn "Enter number of shares:"
    sharesStr <- getLine
    case (readMaybe articleIdStr, readMaybe viewsStr, readMaybe likesStr, readMaybe sharesStr) :: (Maybe Int, Maybe Int, Maybe Int, Maybe Int) of
        (Just articleId, Just views, Just likes, Just shares) -> do
            let statistic = Statistic articleId views likes shares
            createStatistic statistic
            putStrLn "Statistic added successfully!"
        _ -> putStrLn "Invalid input" >> addStatisticOperation

viewStatisticOperation :: IO ()
viewStatisticOperation = do
    putStrLn "Enter the Article ID of the statistic to view:"
    articleIdStr <- getLine
    case readMaybe articleIdStr :: Maybe Int of
        Just articleId -> do
            maybeStatistic <- fetchStatistic articleId
            case maybeStatistic of
                Just statistic -> print statistic
                Nothing -> putStrLn "Statistic not found."
        Nothing -> putStrLn "Invalid Article ID" >> viewStatisticOperation

updateStatisticOperation :: IO ()
updateStatisticOperation = do
    putStrLn "Enter the Article ID of the statistic to update:"
    articleIdStr <- getLine
    putStrLn "Enter new number of views:"
    newViewsStr <- getLine
    putStrLn "Enter new number of likes:"
    newLikesStr <- getLine
    putStrLn "Enter new number of shares:"
    newSharesStr <- getLine
    case (readMaybe articleIdStr, readMaybe newViewsStr, readMaybe newLikesStr, readMaybe newSharesStr) :: (Maybe Int, Maybe Int, Maybe Int, Maybe Int) of
        (Just articleId, Just newViews, Just newLikes, Just newShares) -> do
            let statistic = Statistic articleId newViews newLikes newShares
            updateStatistic statistic
            putStrLn "Statistic updated successfully!"
        _ -> putStrLn "Invalid input" >> updateStatisticOperation

deleteStatisticOperation :: IO ()
deleteStatisticOperation = do
    putStrLn "Enter the Article ID of the statistic to delete:"
    articleIdStr <- getLine
    case readMaybe articleIdStr :: Maybe Int of
        Just articleId -> do
            deleteStatistic articleId
            putStrLn "Statistic deleted successfully!"
        Nothing -> putStrLn "Invalid Article ID" >> deleteStatisticOperation

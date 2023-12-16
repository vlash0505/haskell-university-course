module Entities where

-- Author Entity
data Author = Author {
    authorId :: Int,
    authorName :: String,
    authorContactInfo :: String
} deriving (Show)

-- Article Entity
data Article = Article {
    articleId :: Int,
    articleTitle :: String,
    articleAuthorId :: Int,
    articleAnnotationText :: String
} deriving (Show)

-- Reader Entity
data Reader = Reader {
    readerId :: Int,
    readerName :: String,
    readerContactInfo :: String
} deriving (Show)

-- Feedback Entity
data Feedback = Feedback {
    feedbackId :: Int,
    feedbackArticleId :: Int,
    feedbackReaderId :: Int,
    feedbackText :: String
} deriving (Show)

-- Statistic Entity
data Statistic = Statistic {
    statisticArticleId :: Int,
    statisticViews :: Int,
    statisticLikes :: Int,
    statisticShares :: Int
} deriving (Show)

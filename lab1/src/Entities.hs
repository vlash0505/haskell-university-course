module Entities where

data Author = Author {
    authorId :: Int,
    name :: String,
    contactInfo :: String
} deriving (Show)

data Article = Article {
    articleId :: Int,
    title :: String,
    authorId :: Int,
    annotationText :: String
} deriving (Show)

data Reader = Reader {
    readerId :: Int,
    name :: String,
    contactInfo :: String
} deriving (Show)

data Feedback = Feedback {
    feedbackId :: Int,
    articleId :: Int,
    readerId :: Int,
    text :: String
} deriving (Show)

data Statistic = Statistic {
    articleId :: Int,
    views :: Int,
    likes :: Int,
    shares :: Int
} deriving (Show)

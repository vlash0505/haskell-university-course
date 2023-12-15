CREATE TABLE Authors (
                         AuthorID INT PRIMARY KEY,
                         Name VARCHAR(100),
                         ContactInfo VARCHAR(100)
);

CREATE TABLE Articles (
                          ArticleID INT PRIMARY KEY,
                          Title VARCHAR(200),
                          AuthorID INT,
                          AnnotationText TEXT,
                          FOREIGN KEY (AuthorID) REFERENCES Authors(AuthorID)
);

CREATE TABLE Readers (
                         ReaderID INT PRIMARY KEY,
                         Name VARCHAR(100),
                         ContactInfo VARCHAR(100)
);

CREATE TABLE Feedbacks (
                           FeedbackID INT PRIMARY KEY,
                           ArticleID INT,
                           ReaderID INT,
                           Text TEXT,
                           FOREIGN KEY (ArticleID) REFERENCES Articles(ArticleID),
                           FOREIGN KEY (ReaderID) REFERENCES Readers(ReaderID)
);

CREATE TABLE Statistics (
                            ArticleID INT PRIMARY KEY,
                            Views INT,
                            Likes INT,
                            Shares INT,
                            FOREIGN KEY (ArticleID) REFERENCES Articles(ArticleID)
);

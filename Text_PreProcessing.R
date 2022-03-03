
library(tidyverse)
library(dtplyr) # Translation to data.table
library(data.table)
library(ggplot2)
library(lubridate)
library(stringr)
library(e1071)
library(caret)
library(quanteda)
library(irlba)
library(randomForest)

# NOTES: Should probably create sample data that has both parties, and the same people
# So that I can validity check - statements from the same party should be more similar
# Statements from the same people should maybe be more simliar

# Load sample of data
# 100 rows
sample_data.raw <- readRDS("~/OneDrive - The Ohio State University/Party_messaging/Data/Press_releases/Sample100forNLP.RDS")

# Use caret to create 70/30 stratified split and maintain gender balance
# Seed seed for reproducibility 
set.seed(6793)
indexes <- createDataPartition(sample_data.raw$gender, times = 1,
                              p = 0.7, list = FALSE)

train <- sample_data.raw[indexes,]
test <- sample_data.raw[-indexes,]

# Pull out a weird one for reference
train$body_pr[56]

# Remove non-English documents (primarily Spanish)
library(cld3)
train <- subset(train, detect_language(train$body_pr) == "en")

# Tokenize statements with quanteda
train.tokens <- tokens(train$body_pr,
                       what = "word",
                       remove_punct = TRUE,
                       remove_symbols = TRUE,
                       remove_numbers = TRUE,
                       remove_url = TRUE,
                       remove_separators = TRUE,
                       split_hyphens = TRUE,
                       include_docvars = TRUE)
train.tokens[[56]]

# Lower case the tokens
train.tokens <- tokens_tolower(train.tokens)

# Remove stopwords with quanteda built-in list
train.tokens <- tokens_select(train.tokens, stopwords(),
                              selection = "remove")

# Remove single character tokens (the only one-letter words in English are a and I)
train.tokens <- tokens_select(train.tokens,
                              min_nchar = 2,
                              selection = "keep")

# Perform token stemming
train.tokens <- tokens_wordstem(train.tokens, language = "english")

# Create a bag of words DFM
train.tokens.dfm <- dfm(train.tokens, tolower = FALSE, )
# Transform to a matrix
train.tokens.matrix <- as.matrix(train.tokens.dfm)
View(train.tokens.matrix[1:20, 1:100])
dim(train.tokens.matrix) # 65 (texts), 3549 (tokens)

# Experiment with predicting the speaker's gender
# Set up a feature df with labels
train.tokens.df <- cbind(gender = train$gender, convert(train.tokens.dfm, to = "data.frame"))

# Set gender as a factor
train.tokens.df$gender <- as.factor(train.tokens.df$gender)

# Clean up column names
names(train.tokens.df) <- make.names(names(train.tokens.df))

##################################################
# TF-IDF

# Function for calculating relative term frequency (TF)
term.frequency <- function(row) {
  row / sum(row)
}

# Function for calculating inverse document frequency (IDF)
inverse.doc.freq <- function(col) {
  corpus.size <- length(col)
  doc.count <- length(which(col > 0))
  
  log10(corpus.size / doc.count)
}

# Function for calculating TF-IDF.
tf.idf <- function(x, idf) {
  x * idf
}

# First step - normalize all documents (via TF)
train.tokens.df <- apply(train.tokens.matrix, 1, term.frequency)
dim(train.tokens.df)
View(train.tokens.df[1:20, 1:20])

# Second step - calculate the IDF vector that will be used - both
# for training data and for test data
train.tokens.idf <- apply(train.tokens.matrix, 2, inverse.doc.freq)
str(train.tokens.idf)

# Last step - calculate TF-IDF for the training corpus
train.tokens.tfidf <- apply(train.tokens.df, 2, tf.idf, idf = train.tokens.idf)
dim(train.tokens.tfidf)
View(train.tokens.tfidf[1:25, 1:25])

# Transpose the matrix
train.tokens.tfidf <- t(train.tokens.tfidf)
dim(train.tokens.tfidf)
View(train.tokens.tfidf[1:25, 1:25])

# Check for incomplete cases.
incomplete.cases <- which(!complete.cases(train.tokens.tfidf))
train$body_pr[incomplete.cases]


# Fix incomplete cases
train.tokens.tfidf[incomplete.cases,] <- rep(0.0, ncol(train.tokens.tfidf))
dim(train.tokens.tfidf)
sum(which(!complete.cases(train.tokens.tfidf)))


# Make a clean data frame using the same process as before.
train.tokens.tfidf.df <- cbind(gender = train$gender, data.frame(train.tokens.tfidf))
names(train.tokens.tfidf.df) <- make.names(names(train.tokens.tfidf.df))

#########################
# Predict gender
#########################

# Use caret to create stratified folds for 10-fold cross validation repeated 
# 3 times (i.e., create 30 random stratified samples)
set.seed(48743)
cv.folds <- createMultiFolds(train$gender, k = 10, times = 1)

cv.cntrl <- trainControl(method = "repeatedcv", number = 10,
                         repeats = 1, index = cv.folds)

start.time <- Sys.time()
rpart.cv <- train(gender ~., data = train.tokens.tfidf.df,
                  method = "rpart",
                  trControl = cv.cntrl,
                  tuneLength = 7)

total.time <- Sys.time() - start.time
total.time

rpart.cv$terms


# To do: Maybe explore tidytext package







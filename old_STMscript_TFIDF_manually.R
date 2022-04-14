###############################

# STM_unityScript.R
# Author: Mackenzie Weiler
# Date: 2022-03
# Description: Load data, preprocess, and run STM topic models

##############################

# Packages
packages <- c("tidyverse", "dtplyr", "data.table", "ggplot2", "lubridate",
              "stringr", "e1071", "caret", "quanteda", "irlba", "randomForest", 
              "cld3", "tm", "foreach", "parallel", "doParallel", "purrr", "dplyr")


# # Install new packages
# new.packages <- packages[!(packages %in% installed.packages()[,"Package"])]
# if(length(new.packages)) install.packages(new.packages, dependencies = TRUE)

# Load packages
lapply(packages, require, character.only = TRUE)

#####################################################################
# Full Data - Load
# All press releases and membership information

####
# Load data set
####
# # Local 
# fullData <- readRDS("~/OneDrive - The Ohio State University/Party_messaging/Data/Press_releases/2022-02_CompleteData.RDS")
# Unity
fullData <- readRDS("~/party_messaging/2022-02_CompleteData.RDS")

# Give all press releases a unique id
fullData <- fullData %>% 
  mutate(pr_id = seq(1:nrow(fullData)))

####################################################################
# Segmented data - segment
# Segment data by type (house/senate) and party

# House Republicans
houseR <- fullData %>% 
  filter(type == "rep" & party_atPR == "R")

# House Democrats
houseD <- fullData %>% 
  filter(type == "rep" & party_atPR == "R")

# Senate Republicans
senateR <- fullData %>% 
  filter(type == "sen" & party_atPR == "R")

# Senate Democrats
senateD <- fullData %>% 
  filter(type == "sen" & party_atPR == "D")


##################################################################
# Pre-process text
##################################################################

(start.time <- Sys.time())

# Remove non-English documents (primarily Spanish)
fullData_en <- subset(fullData, detect_language(fullData$body_pr) == "en")

print(paste("Time to run english detection:", (Sys.time() - start.time)))

# Tokenize statements with quanteda
dat.tokens <- tokens(fullData_en$body_pr,
                     what = "word",
                     remove_punct = TRUE,
                     remove_symbols = TRUE,
                     remove_numbers = TRUE,
                     remove_url = TRUE,
                     remove_separators = TRUE,
                     split_hyphens = TRUE,
                     include_docvars = TRUE)

# Lower case the tokens
dat.tokens <- tokens_tolower(dat.tokens)

# Remove stopwords with quanteda built-in list
dat.tokens <- tokens_select(dat.tokens, stopwords(),
                            selection = "remove")

# Remove single character tokens (the only one-letter words in English are a and I)
dat.tokens <- tokens_select(dat.tokens,
                            min_nchar = 2,
                            selection = "keep")

# Perform token stemming
dat.tokens <- lapply(dat.tokens, tokens_wordstem, languge = 'english')
# dat.tokens <- tokens_wordstem(dat.tokens, language = "english")

# Create a bag of words DFM
dat.tokens.dfm <- dfm(dat.tokens, tolower = FALSE)
# Transform to a matrix
dat.tokens.matrix <- as.matrix(dat.tokens.dfm)


##################################################
# TF-IDF - Functions

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

#########################################################
# TF-IDF - Compute

# Normalize all documents (via TF)
dat.tokens.df <- apply(dat.tokens.matrix, 1, term.frequency)
# Clean up column names
names(dat.tokens.df) <- make.names(names(dat.tokens.df))

# Second step - calculate the IDF vector that will be used - both
# for training data and for test data
dat.tokens.idf <- apply(dat.tokens.matrix, 2, inverse.doc.freq)

# Last step - calculate TF-IDF for the training corpus
dat.tokens.tfidf <- apply(dat.tokens.df, 2, tf.idf, idf = dat.tokens.idf)

# Transpose the matrix
dat.tokens.tfidf <- t(dat.tokens.tfidf)

# Check for incomplete cases.
incomplete.cases <- which(!complete.cases(dat.tokens.tfidf))

# Fix incomplete cases
dat.tokens.tfidf[incomplete.cases,] <- rep(0.0, ncol(dat.tokens.tfidf))


# Make a clean data frame 
# Add features
dat.tokens.tfidf.df <- cbind(gender = fullData_en$gender
                             , data.frame(dat.tokens.tfidf))



saveRDS(dat.tokens.tfidf.df, "dat.tokens.tfidf.df.rds")









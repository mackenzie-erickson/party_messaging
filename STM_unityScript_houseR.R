###############################

# STM_unityScript_houseR.R
# Author: Mackenzie Weiler
# Date: 2022-04
# Description: Load trimmed DFM, prep docs for stm, estimate STM
# Details: house Republicans
# Following tutorial here: https://burtmonroe.github.io/TextAsDataCourse/Tutorials/IntroSTM.nb.html

##############################

# Packages
packages <- c("ggplot2", "lubridate",
              "stringr", "quanteda", "irlba", 
              "cld2", "tm", "foreach", "parallel", "doParallel", "purrr", "dplyr",
              "lda", "slam", "stm")

# Load packages
lapply(packages, require, character.only = TRUE)

####
# Load data set
####
# # Local 
# fullData <- readRDS("~/OneDrive - The Ohio State University/Party_messaging/Data/Press_releases/2022-02_CompleteData.RDS")
# Unity
fullData <- readRDS("~/party_messaging/2022-02_CompleteData.RDS")

print("data loaded.")


# Give all press releases a unique id
fullData <- fullData %>% 
  mutate(indx = seq(1:nrow(fullData)))

print("indices added.")


# Create numeric date var with min(date) = 0, and # of days since
# 01 Jan 2013 = day 0
fullData <- fullData %>% 
  mutate(days_since_origin= as.numeric(
    date_pr - min(date_pr)
  ))

print("Converted dates.")



###############################################################
# Function - fitSTM.fns

# Clean and pre-process corpus
# Tokenize, pre-process tokens
# Convert dfm to stm format
# Fit an STM with num_Topics number of topics
###############################################################

# Filter to Republicans
dat <- fullData %>%
  filter(type == "rep" & party_atPR == "R")


# Remove non-English documents (primarily Spanish)
fullData_en <- 
  dat[(cld2::detect_language(dat$body_pr) == "en") == TRUE, ]

print("Filtered to English docs.")
  
  
  
#######################################
# Preprocessing with quanteda
#######################################

# Create corpus
fullData.corpus <- quanteda::corpus(fullData_en$body_pr, 
                                    docvars = select(fullData_en, -body_pr))

print("corpus created.")

# Tokens
system.time(
  fullData.tokens <- tokens(fullData.corpus,
                            remove_punct = TRUE,
                            remove_symbols = TRUE,
                            remove_numbers = TRUE,
                            remove_url = TRUE,
                            remove_separators = TRUE,
                            split_hyphens = TRUE,
                            split_tags = TRUE,
                            include_docvars = TRUE,
                            padding = TRUE,
                            verbose = TRUE)
)


print("Preprocessed and tokenized words.")


# #########################################
# # Create DFM


system.time(
  fullData.dfm <- quanteda::dfm(
    x = fullData.tokens
    , tolower = TRUE
    , remove_padding = TRUE
    , verbose = TRUE
  )
)


print("created DFM.")


# Remove stop words
system.time(
  fullData.dfm <- dfm_select(fullData.dfm
                             , pattern = stopwords("en")
                             , selection = "remove")
)


print("removed stop words.")

# Stem words
fullData.dfm.stems <- dfm_wordstem(fullData.dfm)

print("stemmed tokens.")

# Trim - filter out words that appear only once (makes STM fitting difficult)
# and terms that appear in more than 90% of docs

fullData.dfm.trim <- dfm_trim(
  fullData.dfm.stems
  , min_termfreq = 1
  , termfreq_type = "count"
  , max_docfreq = 0.95
  , docfreq_type = "prop", 
)

print("trimmed infrequent words.")

saveRDS(fullData.dfm.trim, "2022-04-12_fullDataDMFtrim_houseR.rds")


# Convert to stm format
dat.stm <- convert(fullData.dfm.trim
                   , to = "stm"
                   , omit_empty = TRUE)

# Save stm objects as variables
docs <- dat.stm$documents
vocab <- dat.stm$vocab
meta <- dat.stm$meta


print("Prepared docs and data.")

# Estimate STM
# With topic prevalence parameter
# Can ask how the prevalence of topics varies across doc's metadata

system.time(
  fit <- stm(docs,
             vocab,
             K = 20,
             # prevalence = ~ s(days_since_origin),
             max.em.its = 150,
             data = meta,
             init.type = "Spectral",
             # gamma.prior = "L1",
             seed = 8590392)
)

saveRDS(fit, "2022-04-13_Fit_20topics_houseR.rds")







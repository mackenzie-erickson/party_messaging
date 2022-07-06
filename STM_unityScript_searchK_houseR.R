###############################

# STM_unityScript_searchK_houseR.R
# Author: Mackenzie Weiler
# Date: 2022-07
# Description: Try various numbers of topic, start with pre-processed data
# Details: house Republicans

# Revision history:
  # 05-07-2022:  Estimated 20, 50 and 75 clusters
  # 06-07-2022:  Estimated 40, 42, 44, 46 clusters (Grimmer 2009 found 44 
                # to be optimal for press releases)

##############################

# Packages
packages <- c("ggplot2", "lubridate",
              "stringr", "quanteda", "irlba", 
              "cld2", "tm", "foreach", "parallel", "doParallel", "purrr", "dplyr",
              "lda", "slam", "stm")

# Load packages
lapply(packages, require, character.only = TRUE)


####
# Load pre-processed STM data (aka "out" data)
####
# # Local 
# setwd("/Users/mackenzieweiler/Library/CloudStorage/OneDrive-TheOhioStateUniversity/Party_messaging")
# dat.stm <- readRDS(paste0(getwd(), "/Data/output/noStateNames/attempt3/2022-05-02_stmPrepped_houseR.rds"))
# Unity
dat.stm <- readRDS("~/2022-05-02_stmPrepped_houseR.rds")

print("stm-prepped data loaded.")


# Save stm objects as variables
documents <- dat.stm$documents
vocab <- dat.stm$vocab
meta <- dat.stm$meta


print("Prepared docs and data.")


# Fit models with a range of topic numbers
system.time(
  kresult <- 
  searchK(
    documents
    , vocab
    , K = c(40, 42, 44, 46)
    , prevalence = ~congress
    , data = meta
    , init.type = "Spectral"
    # num docs to be partially held out
    , N = floor(0.1 * length(documents)) 
    # proprotion of docs to be held out
    , proportion = 0.5
    , heldout.seed = 55647
    # M value for exclusivity computation
    , M = 10
    )
)



saveRDS(kresult, "2022-07-06_searchKresults_40-46_houseR.rds")






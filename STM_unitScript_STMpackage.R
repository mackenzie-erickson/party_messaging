###############################

# STM_unityScript_STMpackage.R
# Author: Mackenzie Weiler
# Date: 2022-04
# Description: Load data, preprocess, and run STM topic models
# Details: Updated script using STM package
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

# fullData <- sample_n(fullData, 100)


# Give all press releases a unique id
fullData <- fullData %>% 
  mutate(pr_id = seq(1:nrow(fullData)))

print("indices added.")


# Remove non-English documents (primarily Spanish)
fullData_en <- subset(fullData, cld2::detect_language(fullData$body_pr) == "en")

print("Filtered to English docs.")

# Create numeric date var with min(date) = 0, and # of days since
# 01 Jan 2013 = day 0
fullData_en <- fullData_en %>% 
  mutate(days_since_origin= as.numeric(
    date_pr - min(date_pr)
  ))

print("Converted dates.")

dat_subset <- sample_n(fullData_en, 100)


# Preprocessing with stm package
fullData.proc <- textProcessor(documents = dat_subset$body_pr,
                               metadata = dat_subset,
                               lowercase = TRUE,
                               removestopwords = TRUE,
                               removenumbers = TRUE,
                               removepunctuation = TRUE,
                               stem = TRUE,
                               wordLengths = c(3, Inf),
                               sparselevel = 1,
                               language = "en",
                               verbose = TRUE,
                               onlycharacter = TRUE,
                               # striphtml = TRUE,
                               customstopwords = NULL,
                               v1 = FALSE)

print("Preprocessed docs.")

# # Prep documents
# # Structure and index the data for stm
# # Object should have no missing values
# # Low freq words can be removed using lower.thresh option
# 
# out <- prepDocuments(fullData.proc$documents, 
#                      fullData.proc$vocab,
#                      fullData.proc$meta)
# 
# # Save output objects as variables
# docs <- out$documents
# vocab <- out$vocab
# meta <- out$meta
# 
# print("Prepared docs and data.")
# 
# # Estimate STM
# # With topic prevalence parameter
# # Can ask how the prevalence of topics varies across doc's metadata
# 
# 
# fit20 <- stm(out$documents,
#              out$vocab,
#              K = 20,
#              prevalence = ~party_atPR + s(days_since_origin),
#              max.em.its = 75,
#              data = out$meta,
#              init.type = "Spectral",
#              seed = 8590392)
# 
# print("STM fit.")
# 
# saveRDS(fit20, "~/party_messaging/STM_fit_20.rds")
# 
# print("STM saved.")
# 
# # # Summary of model topics
# # plot(fit20, type = "summary", xlim = c(0, 0.4))
# # 
# # # Most frequent words in the model, for specific topics
# # plot(fit20, type = "labels", topics = c(14, 19, 20))
# # 
# # # Histogram of topics
# # plot(fit20, type = "hist")
# # 
# # # Comparison of two topics, for example, 3 and 20
# # plot(fit20, type = "perspective", topics = c(3, 20))
# 
# 
# ###################################################################
# # Evaluate
# # Search and select model for a fixed number of topics
# # selectModel() function - helps user in finding and selecting a model
# # with desirable properties (in both semantic coherence and exclustivity dimensions)
# # exclusivity (e.g. models with avg scores towards the upper right of the plot)
# # STM will compare a number of models side by side and will keep
# # mods that don't converge quickly
# 
# fit20Select <- selectModel(out$documents,
#                            out$vocab,
#                            K = 20,
#                            prevalence = ~ party_atPR + s(days_since_origin),
#                            max.em.its = 75,
#                            data = meta,
#                            runs = 20,
#                            seed = 8590392)
# 
# print("Select models fit done.")
# 
# saveRDS(fit20Select, "~/party_messaging/fit20SelectModels.rds")
# 
# print("Select models fit saved.")
# 
# # Each STM has semantic coherence and exclusivity values associated with each topic
# # Plotting the diff models that have made the cut along 
# # exclusivity and semantic coherence of their topics:
# 
# # plotModels(fit20Select)
# 
# 
# # # Plots comparison models with these values and labels with the topic number
# # topicQuality(model = fit20, documents = docs)
# 
# 
# # # Select model from plotModels() that is in the further upper right corner
# # # (best semantic coherence and exclusivity)
# # selectedModel1 <- fit20Select$runout[[1]] # choose model 1
# 
# 
# 
# 
# 
# 
# 
# 














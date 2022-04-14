###############################

# STM_unityScript_STMpackage.R
# Author: Mackenzie Weiler
# Date: 2022-04
# Description: Load data, preprocess, and run STM topic models
# Details: Updated script using STM package
# Following tutorial here: https://burtmonroe.github.io/TextAsDataCourse/Tutorials/IntroSTM.nb.html

##############################

local_dir <- "/Users/mackenzieweiler/Library/CloudStorage/OneDrive-TheOhioStateUniversity/Party_messaging"

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

fitSTM.fns <- function(dat, num_Topics){
  

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
  # , min_docfreq = 0.005
  , max_docfreq = 0.95
  , docfreq_type = "prop", 
)

print("trimmed infrequent words.")

saveRDS(fullData.dfm.trim, "/Users/mackenzieweiler/Library/CloudStorage/OneDrive-TheOhioStateUniversity/Party_messaging/Data/output/2022-04-12_fullDataDMFtrim_houseR.rds")

# fullData.dfm.trim <- readRDS("/Users/mackenzieweiler/Library/CloudStorage/OneDrive-TheOhioStateUniversity/Party_messaging/Data/output/2022-04-12_fullDataDMFtrim_houseR.rds")

# Prep documents
# Structure and index the data for stm
# Object should have no missing values
# Low freq words can be removed using lower.thresh option

# Convert to stm format
fullData.stm <- convert(fullData.dfm.trim, to = "stm")

system.time(
out <- prepDocuments(fullData.stm$documents,
                     fullData.stm$vocab,
                     fullData.stm$meta)
)

saveRDS(out, paste0(local_dir, "/Data/output/outPreppedDocs_houseR.rds"))




# Save output objects as variables
docs <- out$documents
vocab <- out$vocab
meta <- out$meta


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

# return(fit)
# 
# } # end fitSTM.fns function

saveRDS(fit, paste0(local_dir, "/Data/output/2022-04-11_Fit_20topics_houseR.rds"))

# saveRDS(fit20_days, "2022-04-09_Fit20")



#############################################
# Split data
############################################
# House
dat.houseR <- fullData %>% 
  filter(type == "rep" & party_atPR == "R")

dat.houseD <- fullData %>% 
  filter(type == "rep" & party_atPR == "D")

# Senate
dat.senateR <- fullData %>% 
  filter(type == "sen" & party_atPR == "R")

dat.senateD <- fullData %>% 
  filter(type == "sen" & party_atPR == "D")


###########################
# Set up parallel processing
cores <- detectCores()
cl <- makeCluster(cores - 2)
registerDoParallel(cl)


system.time(
  fit_houseR_20topics <- fitSTM.fns(dat.houseR, num_Topics = 20)
    )


stopCluster(cl)

saveRDS(fit_houseR_20topics, "fit_HouseR_20topics")



# system.time(
#   fit20_party_and_days <- stm(docs,
#              vocab,
#              K = 20,
#              prevalence = ~ as.factor(party_atPR) + s(days_since_origin),
#              max.em.its = 75,
#              data = meta,
#              init.type = "Spectral",
#              seed = 8590392)
# )
# 
# print("STM fit.")
# 
# saveRDS(fit20, "/Users/mackenzieweiler/Library/CloudStorage/OneDrive-TheOhioStateUniversity/Party_messaging/Data/output/2022-04-08_Fit_20_daysAndParty.rds")
# 
# print("STM saved.")

stopCluster(cl)

# # Summary of model topics
# plot(fit20, type = "summary", xlim = c(0, 0.4))
# 
# # Most frequent words in the model, for specific topics
# plot(fit20, type = "labels", topics = c(14, 19, 20))
# 
# # Histogram of topics
# plot(fit20, type = "hist")

# # Comparison of two topics, for example, 3 and 20
# plot(fit20, type = "perspective", topics = c(3, 20))


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



# Read in fitted houseD model
fit <- readRDS("/Users/mackenzieweiler/Library/CloudStorage/OneDrive-TheOhioStateUniversity/Party_messaging/Data/output/2022-04-11_Fit_20topics_noPrev.rds")
# Read in output from prepping docs
out <- readRDS("/Users/mackenzieweiler/Library/CloudStorage/OneDrive-TheOhioStateUniversity/Party_messaging/Data/output/2022-04-09_outHouseD.rds")


as.data.frame(t(labelTopics(fit, n = 20)$prob))

pdf("/Users/mackenzieweiler/Library/CloudStorage/OneDrive-TheOhioStateUniversity/Party_messaging/Data/output/topicPlots/topicPrev_houseD.pdf")
plot(fit,
     type = "summary",
     text.cex = 0.5,
     main = "Democratic House topic shares",
     xlab = "Share estimation")
dev.off()

stm::cloud(fit, topic = 1, scale = c(2.25, .5))

plot(fit, type = "perspectives", topics = c(19, 11), 
     main = "Comparing topics 11 and 19")



# Effect estimation of topic prevalence over time

fit.labels <- labelTopics(fit, 1:20)
out$meta$datum <- as.numeric(out$meta$date_pr)
# Smoothed date - prevalence of topic
fit.ee.date <- estimateEffect(1:20 ~ s(datum),
                         fit,
                         metadata = out$meta)
saveRDS(fit.ee.date, "/Users/mackenzieweiler/Library/CloudStorage/OneDrive-TheOhioStateUniversity/Party_messaging/Data/output/2022-04-12_fitEE_houseD.rds")


topic12plot <- 
  plot(fit.ee.date, "datum",
     method = "continuous", topics = 12,
     main = paste0(fit.labels$prob[12, 1:3], collapse = ", ")
     ) 


  
# Plot each topic and it's prevalence by time/date using ggplot

# Pull out effects
library(stminsights)
gg.effects <- get_effects(estimates = fit.ee.date,
                          variable = "datum",
                          type = "continuous")

# Plot - Dem House, Topic 12 ("President Trump")
gg.effects %>% 
  filter(topic == 2) %>% 
  ggplot(aes(x = value, y = proportion)) +
  geom_line(color = "blue") +
  geom_line(aes(x = value, y = lower), lty = "dashed", color = "blue") +
  geom_line(aes(x = value, y = upper), lty = "dashed", color = "blue") +
  theme_light() +
  ggtitle("Effect of date on Topic 2 prevalence", 
          subtitle = paste0(fit.labels$prob[2, 1:3], collapse = ", ")) +
  scale_x_continuous(breaks = c(
    as.numeric(as.Date("2014-01-01")),
    as.numeric(as.Date("2016-01-01")),
    as.numeric(as.Date("2018-01-01")),
    as.numeric(as.Date("2020-01-01"))
  ),
  labels = c("2014", "2016", "2018", "2020"),
  name = "Year") +
  ylab("Topic proportion")


# Make list to store plots
plot_list <- list()

i <- 1
for (i in 1:20) {
  
  p <-
    gg.effects %>% 
    filter(topic == i) %>% 
    ggplot(aes(x = value, y = proportion)) +
    geom_line(color = "blue") +
    geom_line(aes(x = value, y = lower), lty = "dashed", color = "blue") +
    geom_line(aes(x = value, y = upper), lty = "dashed", color = "blue") +
    theme_light() +
    ggtitle(paste("Effect of date on Topic", i, "prevalence"), 
            subtitle = paste0(fit.labels$prob[i, 1:3], collapse = ", ")) +
    scale_x_continuous(breaks = c(
      as.numeric(as.Date("2014-01-01")),
      as.numeric(as.Date("2016-01-01")),
      as.numeric(as.Date("2018-01-01")),
      as.numeric(as.Date("2020-01-01"))
    ),
    labels = c("2014", "2016", "2018", "2020"),
    name = "Year") +
    ylab("Topic proportion")
  
  plot_list[[i]] <- p
}

# Save plots

i <- 1
pdf("/Users/mackenzieweiler/Library/CloudStorage/OneDrive-TheOhioStateUniversity/Party_messaging/Data/output/topicPlots/plotTopicsbyYear.pdf")
for (i in 1:20) {
  print(plot_list[[i]])
}
dev.off()





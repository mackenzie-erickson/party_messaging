
###################
# TopicModelsEval.Rmd
# DESCRIPTION:
# Evaluate topic models of various k clusters
# Exclusivity and Semantic Coherence

# AUTHOR: Mackenzie Weiler
# DATE: 07-2022

# REVISION HISTORY
# 06-07-2022:   # Compared 20, 50 and 75 topics
# 22-07-2022:  Comparing 20-35 topics
##################



rm(list = ls())

# Packages
packages <- c("NetworkInference", "tidyverse", "igraph", "ggplot2", "lubridate",
              "stringr", "stm", "quanteda")

# Load packages
lapply(packages, require, character.only = TRUE)





####
# Load output
####

# Set temp working dir
work_dir <- "/Users/mackenzieweiler/Library/CloudStorage/OneDrive-TheOhioStateUniversity/Party_messaging"

# 20-35 (R)
kresult.R_20.30 <- readRDS(paste0(work_dir, "/Data/output/kTopics/2022-07-19_searchKresults_20-35_houseR.rds"))

# 20-35 (D)
kresult.D_20.30 <- readRDS(paste0(work_dir, "/Data/output/kTopics/2022-07-19_searchKresults_20-35_houseD.rds"))

# 40-46 (R)
kresult.R_40.46 <- readRDS(paste0(work_dir, "/Data/output/kTopics/2022-07-06_searchKresults_40-46_houseR.rds"))

# 40-46 (D)
kresult.D_40.46 <- readRDS(paste0(work_dir, "/Data/output/kTopics/2022-07-06_searchKresults_40-46_houseD.rds"))




# Append lists together - Republican

# Convert each results element to a df
dfR_20.30 <- as.data.frame(kresult.R_20.30$results)
dfR_40.46 <- as.data.frame(kresult.R_40.46$results)
# Combine
dfR.merge <- rbind(dfR_20.30, dfR_40.46)

# Copy of a list
R.list <- kresult.R_20.30

# Replace results section of the searchK object 
R.list$results <- dfR.merge


# Append lists together - Democrats

# Convert each results element to a df
dfD_20.30 <- as.data.frame(kresult.D_20.30$results)
dfD_40.46 <- as.data.frame(kresult.D_40.46$results)
# Combine
dfD.merge <- rbind(dfD_20.30, dfD_40.46)

# Copy of a list
D.list <- kresult.D_20.30

# Replace results section of the searchK object 
D.list$results <- dfD.merge



#####################
# Plot - searchK (which best)
######################
# Following this blog: https://juliasilge.com/blog/evaluating-stm/

plot(R.list) # 25 topics is best

plot(D.list)



###############
# plot_exclusivityCoherenceTradeoff

R.list$results %>%
  select(K, exclus, semcoh) %>%
  # filter(K %in% c(20, 60, 100)) %>%
  unnest(cols = c(K, exclus, semcoh)) %>%
  mutate(K = as.factor(K)) %>%
  ggplot(aes(semcoh, exclus, color = K, shape = K)) +
  geom_point(size = 2, alpha = 0.7) +
  labs(x = "Semantic coherence",
       y = "Exclusivity",
       title = "Comparing exclusivity and semantic coherence",
       subtitle = "House Republicans") +
  theme_minimal() +
  scale_x_continuous(limits = c(-90, -60))



#############
# plot_exclusivityCoherenceTradeoff}

D.list$results %>%
  select(K, exclus, semcoh) %>%
  # filter(K %in% c(20, 60, 100)) %>%
  unnest(cols = c(K, exclus, semcoh)) %>%
  mutate(K = as.factor(K)) %>%
  ggplot(aes(semcoh, exclus, color = K, shape = K)) +
  geom_point(size = 2, alpha = 0.7) +
  labs(x = "Semantic coherence",
       y = "Exclusivity",
       title = "Comparing exclusivity and semantic coherence",
       subtitle = "House Democrats") +
  theme_minimal() +
  scale_x_continuous(limits = c(-90, -60))


###################
# exploreTopics

##############################################
# 20 topics + removed state names and "Rep/Representative" from tokens
# Fit summaries

###
# Democrats
###

# # Read in fitted model
# fit_D_20_noState <- readRDS(paste0(local_dir, "/Data/output/noStateNames/2022-05-02_Fit_20topics_houseD.rds"))


###
# Republicans
###

# Read in fitted model
fit.R <- readRDS(paste0(work_dir, "/Data/output/noNames/2022-08-01_Fit_23topics_houseR.rds"))
out.R <- readRDS(paste0(work_dir, "/Data/output/noNames/2022-07-14_stmPrepped_houseR.rds"))

# Make topics DT
topics_DT.R <- make.dt(fit.R, meta = out.R$meta)



# Summary of model topics
plot(fit.R, type = "summary", xlim = c(0, 0.4))





# Most frequent words in the model, for specific topics
plot(fit20, type = "labels", topics = c(14, 19, 20))

# Histogram of topics
plot(fit20, type = "hist")

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



# The following section is to be run in tandem with 
# TopicsDF_create_sessionSlices.R. It uses objects produced from that script 
# to create tables and figures for the article.

###############
# createTopicLabelTable


# Build topics_df.R and topics_df.D

# Load in manual topic names
topicNames.R <- readxl::read_xlsx(paste0(work_dir, "/Data/Other/topicNames_30k_R.xlsx")) %>% mutate(topic = as.character(topic))
topicNames.D <- readxl::read_xlsx(paste0(work_dir, "/Data/Other/topicNames_30k_D.xlsx")) %>% mutate(topic = as.character(topic))

# Merge with topics_df
topicsWithNames.R <- 
  left_join(topics_df.R, topicNames.R,
            by = c("topic", "topic_label"))

topicsWithNames.D <- 
  left_join(topics_df.D, topicNames.D,
            by = c("topic", "topic_label"))

topicNames_tab.R <-
  topicsWithNames.R %>% 
  select(topic_label_short, topicName, salient) %>% 
  unique() %>% 
  arrange(desc(salient))

topicNames_tab.D <-
  topicsWithNames.D %>% 
  select(topic_label_short, topicName, salient) %>% 
  unique() %>% 
  arrange(desc(salient))



###############
# printTopicLabelTable
library(knitr)
library(kableExtra)

# Specify non-politically relevant rows
gray_rows.R <- which(topicNames_tab.R$salient == 0)
gray_rows.D <- which(topicNames_tab.D$salient == 0)

kable(select(topicNames_tab.R, -salient)
      , booktabs = TRUE
      , format = 'latex'
      # , row.names = NULL
      , col.names = c('FREX stems', 'Topic')
      , caption = "Republican press release topics"
      , label = "tab:topicNames.R"
      , ) %>% 
  kable_styling(latex_options = 'scale_down') %>%
  row_spec(gray_rows.R, color = "gray") %>% 
  footnote("Press releases clustered into 30 topics. FREX stems are the top 4 words in 
  each topic cluster that are both frequent and exclusive and are best able to 
  distinguish the topic.  Topics in gray represent those that may not be 
  considered politically relevant.")

kable(select(topicNames_tab.D, -salient)
      , booktabs = TRUE
      , format = 'latex'
      # , row.names = NULL
      , col.names = c('FREX stems', 'Topic')
      , caption = "Democratic press release topics"
      , label = "tab:topicNames.D"
      , ) %>% 
  kable_styling(latex_options = 'scale_down') %>%
  row_spec(gray_rows.D, color = "gray") %>% 
  footnote("Press releases clustered into 30 topics. FREX stems are the top 4 words in 
  each topic cluster that are both frequent and exclusive and are best able to 
  distinguish the topic.  Topics in gray represent those that may not be 
  considered politically relevant.")


########################
# Press release frequency by topic
########################

# % of press releases by topic - R
freq.R <- topicsWithNames.R %>%
  group_by(topicName) %>%
  summarise(cnt = n()) %>%
  mutate(freq = round(cnt / sum(cnt), 3)) %>% 
  arrange(desc(freq)) %>%
  left_join(., topicNames.R
            , by = c("topicName")) %>% 
  filter(salient == 1) %>% 
  select(topicName, freq)

# % of press releases by topic - D
freq.D <- topicsWithNames.D %>%
  group_by(topicName) %>%
  summarise(cnt = n()) %>%
  mutate(freq = round(cnt / sum(cnt), 3)) %>% 
  arrange(desc(freq)) %>%
  left_join(., topicNames.D
            , by = c("topicName")) %>% 
  filter(salient == 1) %>% 
  select(topicName, freq)

# Full join
freq.both <- 
  full_join(freq.R, freq.D
            , by = "topicName") %>% 
  rename(freq.R = freq.x
         , freq.D = freq.y)

# Edit for figure
freq.both.edit <-
  freq.both %>% 
  mutate(freq.D = 100 * freq.D
         , freq.R = 100 * freq.R) %>% 
  mutate(freq.D = as.numeric(paste0("-", freq.D)))

# Pivot longer
freq.both.long <-
  pivot_longer(data = freq.both.edit
               , cols = c("freq.R", "freq.D")
               , names_to = "party"
               , names_prefix = "freq."
               , values_to = "freq")

# Make party-topic figure
ggplot(freq.both.long, aes(y = factor(topicName), x = freq)) +
  geom_bar(stat = "identity", width = 0.8) +
  geom_vline(xintercept = 0) +
  theme_bw() +
  xlab("Dem                                                           Rep") +
  ylab("") +
  scale_x_continuous(breaks = c(-6, -3, 0, 3, 6), 
                     labels = paste0(c(6, 3, 0, 3, 6),"%")) +
  ggtitle("Topic Frequency by Party")


####################
# Comparison of topics by leadership/seniority
#####################

# Note: This relies on the leg_cov object being created in Topics_Df_create_sessionSlices.R 
# (around line 350)

# Merge leg_cov with topicsWithNames
topicsWithNames.R <-
  left_join(
    topicsWithNames.R
    , leg_covs
    , by = c("member_id" = "bioguide_id"
             , "congress" = "congress")
  )
  
topicsWithNames.D <-
  left_join(
    topicsWithNames.D
    , leg_covs
    , by = c("member_id" = "bioguide_id"
             , "congress" = "congress")
  )

###
# Party leadership vs rank-and-file
###

# Create separate leadership/non freqs - R

leader.rankfile.freq.fns <- function(topicsWithNames) {
  
  leadership <-
    topicsWithNames %>%
    filter(party_leadership == 1) %>%
    filter(salient == 1) %>%
    group_by(topicName) %>%
    summarise(cnt = n()) %>%
    mutate(freq = round(cnt / sum(cnt), 3)
           , party_leadership = 1) %>%
    arrange(desc(freq)) %>%
    select(topicName, freq, party_leadership)
  
  rankfile <-
    topicsWithNames %>%
    filter(party_leadership == 0) %>%
    filter(salient == 1) %>%
    group_by(topicName) %>%
    summarise(cnt = n()) %>%
    mutate(freq = round(cnt / sum(cnt), 3)
           , party_leadership = 0) %>%
    arrange(desc(freq)) %>%
    select(topicName, freq, party_leadership)
  
  # Join
  leaderandrank <-
    full_join(leadership, rankfile
              , by = "topicName") %>% 
    rename(freq.leader = freq.x
           , freq.rank = freq.y)
  
  
  # Pivot longer
  freq.leaderrank.long <-
    pivot_longer(data = leaderandrank
                 , cols = c("freq.leader", "freq.rank")
                 , names_to = "leadership"
                 , names_prefix = "freq."
                 , values_to = "freq") %>% 
    select(-c(party_leadership.x, party_leadership.y))
  
  # Turn into pcts 
  freq.leaderrank.long <-
    freq.leaderrank.long %>% 
    mutate(freq = 100 * freq)
  
  
} # End leader.rankfile.freq.fns

# Apply function
freq.leaderrank.R <- leader.rankfile.freq.fns(topicsWithNames.R) %>% mutate(party = "R")
freq.leaderrank.D <- leader.rankfile.freq.fns(topicsWithNames.D) %>% mutate(party = "D")

# Merge together
freq.leaderrank.all <- 
  full_join(freq.leaderrank.R
            , freq.leaderrank.D
            , by = c("topicName", "leadership")) %>% 
  rename(freq.R = freq.x
         , freq.D = freq.y)

# Turn Dem's into negative for now
freq.leaderrank.all <-
  freq.leaderrank.all %>% 
  mutate(freq.D = as.numeric(paste0("-", freq.D)))

# Pivot longer
freq.leaderrank.long <-
  freq.leaderrank.all %>% 
  pivot_longer(cols = c("freq.R", "freq.D")
               , names_to = "party"
               , names_prefix = "freq."
               , values_to = "freq") %>% 
  select(-c(party.x, party.y))

#############
# Leadership vs rank-and-file Figure (both parties) - Make
############
ggplot(freq.leaderrank.long
       , aes(y = factor(topicName)
             , x = freq
             , fill = factor(leadership))) +
  geom_bar(stat = "identity", width = 0.6, position = "dodge", color = "dark gray") +
  theme_bw() +
  scale_fill_manual(values = c("dark gray", "white")
                    , name = ""
                    , labels = c("Leadership", "Rank-and-file")) +
  ylab("") +
  xlab("Dem                                                           Rep") +
  scale_x_continuous(breaks = c(-10, 0, 10), 
                     labels = paste0(c(10, 0, 10),"%")) +
  geom_vline(xintercept = 0, color = "dark gray") +
  ggtitle("Topic allocation varies between leadership and rank-and-file")



############################
# Frequency by seniority

seniority.freq.fns <- function(topicsWithNames) {
  # Pull seniority quantiles
  quantiles <- quantile(topicsWithNames$seniority, na.rm = TRUE)
  
  # Split into quartiles
  seniority <-
    topicsWithNames %>%
    mutate(quartile =
             ifelse(
               seniority < quantiles[2],
               1
               ,
               ifelse(
                 seniority >= quantiles[2] & seniority < quantiles[3],
                 2
                 ,
                 ifelse(
                   seniority >= quantiles[3] & seniority < quantiles[4],
                   3
                   ,
                   ifelse(seniority >= quantiles[4], 4
                          ,
                          NA
                   )))))
  
  # Frequency for each
  seniority.freq <-
    seniority %>%
    filter(salient == 1) %>%
    group_by(quartile, topicName) %>% 
    summarise(cnt = n()
              , quartile = unique(quartile)) %>%
    mutate(freq = round(cnt / sum(cnt), 3)) %>%
    arrange(desc(freq)) %>%
    select(topicName, freq, quartile) %>% 
    ungroup()
  
  # Turn into pcts 
  seniority.freq <-
    seniority.freq %>% 
    mutate(freq = 100 * freq)
  
} # End seniority.freq.fns
  

# Apply function
seniority.freq.R <- seniority.freq.fns(topicsWithNames.R) %>% mutate(party = "R")
seniority.freq.D <- seniority.freq.fns(topicsWithNames.D) %>% mutate(party = "D")

# Merge together
seniority.freq.all <- 
  full_join(seniority.freq.R
            , seniority.freq.D
            , by = c("topicName", "quartile")) %>% 
  rename(freq.R = freq.x
         , freq.D = freq.y)

# Turn Dem's into negative for now
seniority.freq.all <-
  seniority.freq.all %>% 
  mutate(freq.D = as.numeric(paste0("-", freq.D)))

# Pivot longer
seniority.freq.long <-
  seniority.freq.all %>% 
  pivot_longer(cols = c("freq.R", "freq.D")
               , names_to = "party"
               , names_prefix = "freq."
               , values_to = "freq") %>% 
  select(-c(party.x, party.y))

# Subset to !is.na(seniority quartile)
seniority.freq.long <-
  seniority.freq.long %>% 
  filter(is.na(quartile) == FALSE)

#############
# Leadership vs rank-and-file Figure (both parties) - Make
############
ggplot(filter(seniority.freq.long, quartile %in% c(1, 4))
       , aes(y = factor(topicName)
             , x = freq
             , fill = factor(quartile))) +
  geom_bar(stat = "identity", width = 0.6, position = "dodge", color = "dark gray") +
  theme_bw() +
  scale_fill_manual(values = c("dark gray", "white")
                    , name = ""
                    , labels = c("Freshmen", "Top-quartile \nseniors")) +
  ylab("") +
  xlab("Dem                                                           Rep") +
  scale_x_continuous(limits = c(-12, 12)
                     , breaks = c(-10, 0, 10), 
                     labels = paste0(c(10, 0, 10),"%")) +
  geom_vline(xintercept = 0, color = "dark gray") +
  ggtitle("Topic allocation varies by seniority")

# Note: Republican members in the top quartile of seniority have between 6 and 24
# years of experience; Democratic members in the top quarilte have between
# 12 and 27 years. 


################################################################
# This section assumes first.obs.fun has been created (around line 190)

# Get first obs from topic Name info
first.obsNames.R <- first.obs.fun(topicsWithNames.R)
first.obsNames.D <- first.obs.fun(topicsWithNames.D)



###############
# Plot the CDF of first observations by session

# TO DO: Fix the naming of these variables, get a function going
# Ignore the x-axis for now - can always fix if actually using
# Make a few (2 dems and 2 reps, one for each session maybe)

# Filter to one session
# (insert here)

# Add a length variable for each topic for the session (need for some reason)
df <- plyr::ddply(session.tmp
                  , .variables = c("topicName")
                  , transform
                  , len = length(date_pr))


# Plot
ggplot(df, aes(x = date_pr, group = topicName)) + 
  geom_step(aes(len = len
                , y = ..y.. * len)
            , stat="ecdf") +
  facet_wrap(~ topicName) +
  theme_bw()













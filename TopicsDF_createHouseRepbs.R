###############################

# SNetInf.R
# Author: Mackenzie Weiler
# Date: 2022-04
# Description: Load fitted STM model and original document data, 
  # get in cascade format, and fit diffusion network
# Details: 
# Following tutorial: NetworkInference Tutorial: Persistent Policy Diffusion Ties

##############################

# Packages
packages <- c("NetworkInference", "tidyverse", "igraph", "ggplot2", "lubridate",
              "stringr", "stm", "quanteda")

# Load packages
lapply(packages, require, character.only = TRUE)

# Set working dir
setwd("/Users/mackenzieweiler/Library/CloudStorage/OneDrive-TheOhioStateUniversity/Party_messaging")


#################
# Load data and models
#################

# STM-format processed data (trimmed/stemmed)
out <- readRDS(paste0(getwd(), "/Data/output/2022-04-13_stmPreppedCorpus_houseR.rds"))

# Fitted topics - 20 topics
fit <- readRDS(paste0(getwd(), "/Data/output/2022-04-13_Fit_20topics_houseR.rds"))


###################################################
# Label each document with its most-probable topic
###################################################

# Make a data.table of topic proportions
topics_DT <- make.dt(fit, meta = out$meta)
rm(out)


# Pivot DT longer
# So that each doc is repeated 20 times (20 topics)
# Format:
  # indx; topic; prob
  # 101; 1    ; 0.359
  # 101; 2    ; 0.0003
  # 101; 3    ; 0.0156
  # ....
topics_long <- 
  pivot_longer(
  data = topics_DT,
  cols = starts_with("Topic"),
  names_to = "topic",
  names_prefix = "Topic",
  values_to = "topic_prob"
) %>% 
  as.data.frame()

# Arrange by doc index, then desc(prob)
# So the highest probability observation is first
topics_long <- 
  topics_long %>% 
  group_by(indx) %>% 
  arrange(desc(topic_prob), .by_group = TRUE) %>% 
  ungroup()

# Keep only the observation with the highest prob
topics.df <-
  topics_long %>% 
  distinct(indx, .keep_all = TRUE)


###############
# To do later:
# Decide what to do about near-ties in topic probabilities
# For example, about both education and STEM
# If education is a very popular topic at the time, then we
# probably want to say it's about education.
# But if STEM is very popular, we probably want to say STEM
# Maybe want to fit an STM taking congress # into account
# Or year
###############


###########################
# Fix caucus info - something didn't merge properly
# Should probably go back and fix this is the first script
##########################

# Remove the incorrect caucus info
topics.df <-
  topics.df %>% 
  select(-c(contains("_leader"), contains("_titles"),
            contains("_taskforces"), contains("_member")))

# Read in caucus info
factions <- readxl::read_xlsx(paste0(getwd(), "/Data/Other/factionsSimple_113_116.xlsx"))

# Subset to relevant vars
faction_vars <- factions %>% 
  select(bioguide_id, congress, contains("_leader"), contains("_titles"),
         contains("_taskforces"), contains("_member"))

# Subset to one observation per bioguide_id-congress
# Prioritize caucus = 1
# (i.e. if they left a caucus halfway through a congress, just code them as 1 for the whole congress)
faction_vars <-
  faction_vars %>% 
  group_by(bioguide_id, congress) %>% 
  arrange(across(where(is.numeric), desc)) %>% # Put 1s first
  distinct(bioguide_id, congress, .keep_all = TRUE)
  


# Merge back onto topics.df
topics.df <-
  topics.df %>% 
  left_join(., faction_vars,
            by = c("member_id" = "bioguide_id",
                   "congress" = "congress"))


#####################################################################
# Add topic labels onto data.frame
# To provide information about topics

# Get STM-generated topic labels
fit.labels <- labelTopics(fit, 1:20)

# Pull out 7-word summary and topic #
labels.df <- data.frame(topic = fit.labels$topicnums,
                        topic_label = apply(fit.labels$prob, 1, paste0, collapse = "; "))
labels.df$topic <- as.character(labels.df$topic)

# Merge onto data
topics.df <- 
  topics.df %>% 
  dtplyr::lazy_dt() %>% 
  left_join(labels.df, by = "topic") %>% 
  as.data.frame()


# Save df with topics labeled
saveRDS(topics.df, paste0(getwd(), "/Data/output/2022-04-21_TopicsDF_HouseR.rds"))


#####################################################################
# Explore topics

######
# Find thoughts
#####

# PR body data (for finding thoughts)
body <- readRDS("~/OneDrive - The Ohio State University/Party_messaging/Data/Press_releases/2022-04_BodyData_HouseR.RDS")


# Body - Attach the PR body to the dataframe
topics.df_withBody <- 
  topics.df %>% 
  left_join(select(body, indx, body_pr)
            , by = "indx")


# Return the top n documents for a topic
top_thoughts <- findThoughts(fit, texts = topics.df$body_pr, topics = 4, n = 1)

plotQuote(top_thoughts, text.cex = 0.5, width = 100, main = labels.df$topic_label[4])





# Pull samples
topics.df %>% sample_n(1) %>% select(topic, topic_label, title_pr)





############################################################
# Prepare data for network


# Filter to one congress session - 1st session 116th
topics116_session1 <- 
  topics.df %>% 
  filter(congress == 116) %>% 
  filter(date_pr >= session_1_startDate & date_pr <= session_1_endDate)

# Each topic needs one adoption date
# Group by topic, then by member
# NOTE: This may need to change

# # Keep use with highest topic prob
# mostProb.obs_116.1 <- 
#   topics116_session1 %>% 
#   group_by(topic, member_id) %>% 
#   arrange(desc(topic_prob), .by_group = TRUE) %>% 
#   distinct(member_id, topic, .keep_all = TRUE)


# Keep first use
first.obs_116.1 <- 
  topics116_session1 %>% 
  group_by(topic, member_id) %>% 
  arrange(date_pr, .by_group = TRUE) %>% 
  distinct(member_id, topic, .keep_all = TRUE)



##############################
# Prep data for NetInf - firsttopic use
##############################

# Transform into a cascades object
cascades_firstObs_116.1 <- as_cascade_long(
  data = first.obs_116.1
  , cascade_node_name = 'member_id'
  , event_time = 'date_pr'
  , cascade_id = 'topic'
)


# Inspect
summary(cascades_firstObs_116.1)

# Plot
# Subset in time
time_contrained <- subset_cascade_time(cascade = cascades_firstObs_116.1
                                       , start_time = ymd("2019-01-01")
                                       , end_time = ymd("2019-03-25"))

plot(time_contrained
     , label_nodes = TRUE
     , selection = "9"
     , guide = "none"
     , max.overlaps = 10)


png(paste0(getwd(), "/Data/output/cascadePlots/houseR_116.1_firstObsTopic.png"))
plot(cascades_firstObs_116.1, label_nodes = FALSE) +
  ggtitle("Date of first-use of topic by member"
          , subtitle = "House Republicans - 1st session of the 116th congress") +
  labs(x = "Topic cascade", y = "Jan 3, 2019 - Jan 3, 2020")
dev.off()


###############################################
# Infer edges based on a diffusion model

# Select # edges by, after each iteration of algorithm, check if the edge
  # added sig. improvement to the network
# Select parameters automatically

auto.netinf.result <- netinf(
  cascades = cascades_firstObs_116.1
  , trans_mod = "exponential"
  , p_value_cutoff = 0.1
  # , params = 0.5 # lambda/rate
  )

# Number of edges (above p-value cutoff)
nrow(auto.netinf.result)

# See the directed edges and improvements
head(auto.netinf.result)
plot(auto.netinf.result, type = "improvement")
# p-value from the Vuong test associated with each edge addition
plot(auto.netinf.result, type = "p-value")

# Visualize diffusion network
plot(auto.netinf.result, type = "network")


##################################################################
# Calculate eigenvector centrality

# Convert graph to igraph format (directed ties)
netinf.graph <- 
  auto.netinf.result %>% 
  select(origin_node, destination_node) %>% 
  graph_from_data_frame()

# Calculate eigenvector centrality
eigens_116.1 <- eigen_centrality(
  graph = netinf.graph
  , directed = TRUE
  , scale = TRUE
  , weights = NULL
)

###############################################################
# Regression
# Who is most likely to be central?

# Pull out eigen values and member_id
eigens_116.1_df <- data.frame(
  member_id = names(eigens_116.1$vector),
  eigen_value = unname(eigens_116.1$vector)
)

# Pull out member info from congress session data
member_data_116.1 <- first.obs_116.1 %>% 
  ungroup() %>% 
  select(member_id,
         party_atPR,
         state,
         last_name, first_name, full_name,
         birthday,
         gender,
         leadership_role, leadership_dateElected,
         at_large,
         bills_sponsored,
         bills_cosponsored,
         dw_nominate,
         cook_pvi,
         votes_with_party_pct, votes_against_party_pct,
         next_election,
         contains("_leader"),
         contains("_titles"),
         contains("_taskforces"),
         contains("_member")
         )

member_data_116.1 <- 
  member_data_116.1 %>% 
  distinct(.keep_all = TRUE)


# Join eigen values onto df
member_data_116.1 <-
  member_data_116.1 %>% 
  dtplyr::lazy_dt() %>% 
  left_join(., eigens_116.1_df
            , by = "member_id") %>% 
  as.data.frame()


# Change leadership_role to be a dummy
member_data_116.1 <-
  member_data_116.1 %>% 
  mutate(leader_dummy = ifelse(is.na(leadership_role) == TRUE, 0, 1))


################
# Regression
lm.mod <- lm(eigen_value ~ gender + dw_nominate + leader_dummy
             , data = member_data_116.1
             )
summary(lm.mod)


topics_DT %>% filter(full_name == "Justin Amash") %>% select(hfc_member, congress) %>% distinct()
topics_DT %>% filter(full_name == "Justin Amash") %>% distinct(member_id)

# Read in caucus info

factions <- readxl::read_xlsx(paste0(getwd(), "/Data/Other/factionsSimple_113_116.xlsx"))
names(factions)
tmp <- factions %>% select(bioguide_id, first_name, last_name, starts_with("hfc_"), congress)
tmp %>% filter(hfc_member == 1) %>% distinct()

tmp %>% filter(first_name == "Justin" & last_name == "Amash") %>% distinct(bioguide_id)

#########################


new_topicsDT %>% 
  select(full_name, congress, hfc_member) %>% 
  filter(full_name == "Mark Meadows") %>% 
  distinct()






















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
# Load fitted models
#################

# STM-format processed data (trimmed/stemmed)
out <- readRDS(paste0(getwd(), "/Data/output/outPreppedDocs_houseR.rds"))
# # This is just the DFM (probably named wrongly)
# out <- readRDS(paste0(getwd(), "/Data/output/2022-04-12_stmPrepped_houseR.rds"))

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
  arrange(desc(topic_prob), .by_group = TRUE)

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
# Prep data for NetInf - most Probable topic use
##############################

# Transform into a cascades object
cascades_mostProb_116.1 <- as_cascade_long(
  data = mostProb.obs_116.1
  , cascade_node_name = 'member_id'
  , event_time = 'date_pr'
  , cascade_id = 'topic'
)


# Inspect
summary(cascades_mostProb_116.1)

# Plot
# Subset in time
time_contrained <- subset_cascade_time(cascade = cascades_mostProb_116.1
                                       , start_time = ymd("2019-01-01")
                                       , end_time = ymd("2019-03-25"))

plot(time_contrained
     , label_nodes = TRUE
     , selection = "9"
     , guide = "none"
     , max.overlaps = 10)

# png(paste0(getwd(), "/Data/output/cascadePlots/houseR_116.1_mostProbTopic.png"))
# plot(cascades_mostProb_116.1, label_nodes = FALSE) +
#   ggtitle("Date of most-probable topic use by member"
#           , subtitle = "House Republicans - 1st session of the 116th congress") +
#   labs(x = "Topic cascade", y = "Jan 3, 2019 - Jan 3, 2020")
# dev.off()




##############################
# Prep data for NetInf - first  topic use
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

# Visualize diffusion network with igraph
netinf.graph <- 
  graph_from_data_frame(
    select(auto.netinf.result
    , origin_node, destination_node)
    )
plot(netinf.graph, edge.arrow.size = 0.2)


##################################################################
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
         district,
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

member_data_116.1.2 <- 
  member_data_116.1 %>% 
  distinct(.keep_all = TRUE)

vocabulary[duplicated(vocabulary$id),]

member_data_11

# Join eigen values onto df
first.obs_116.1 <-
  first.obs_116.1 %>% 
  dtplyr::lazy_dt() %>% 
  left_join(eigens_116.1_df
            ,  )









































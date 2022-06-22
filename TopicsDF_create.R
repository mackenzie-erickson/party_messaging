###############################

# SNetInf.R
# Author: Mackenzie Weiler
# Date: 2022-04
# Description: Load fitted STM model and original document data, 
  # get in cascade format, and fit diffusion network
# Details: 
# Following tutorial: NetworkInference Tutorial: Persistent Policy Diffusion Ties

##############################

rm(list = ls())

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

###
# Republican House
##############################

# Read in 20 topic models that were run on May 2 after removing state names

# STM-format processed data (trimmed/stemmed)
out.R <- readRDS(paste0(getwd(), "/Data/output/noStateNames/attempt3/2022-05-02_stmPrepped_houseR.rds"))

# Fitted topics - 20 topics
fit.R <- readRDS(paste0(getwd(), "/Data/output/noStateNames/attempt3/2022-05-02_Fit_20topics_houseR.rds"))

###
# Democratic House
#############################

# # STM-format processed data (trimmed/stemmed)
# out <- readRDS(paste0(getwd(), "/Data/output/2022-04-18_stmPreppedCorpus_houseD.rds"))
# 
# # Fitted topics - 20 topics
# fit <- readRDS(paste0(getwd(), "/Data/output/2022-04-18_Fit_20topics_houseD.rds"))


###################################################
# Label each document with its most-probable topic
###################################################

# Function to label each document with most-propable topic

label_statement_topics.fns <- function(fit, out){

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

###########
# Caucus info temporarily not added back in
# Data provided by William may include this
# Or, it can be added at a later date (i.e. for 3rd chapter)
##########

# # Read in caucus info
# factions <- readxl::read_xlsx(paste0(getwd(), "/Data/Other/factionsSimple_113_116.xlsx"))
# 
# # Subset to relevant vars
# faction_vars <- factions %>% 
#   select(bioguide_id, congress, contains("_leader"), contains("_titles"),
#          contains("_taskforces"), contains("_member"))
# 
# # Subset to one observation per bioguide_id-congress
# # Prioritize caucus = 1
# # (i.e. if they left a caucus halfway through a congress, just code them as 1 for the whole congress)
# faction_vars <-
#   faction_vars %>% 
#   group_by(bioguide_id, congress) %>% 
#   arrange(across(where(is.numeric), desc)) %>% # Put 1s first
#   distinct(bioguide_id, congress, .keep_all = TRUE)
#   
# 
# 
# # Merge back onto topics.df
# topics.df <-
#   topics.df %>% 
#   left_join(., faction_vars,
#             by = c("member_id" = "bioguide_id",
#                    "congress" = "congress"))


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

} # End label_statement_topics.fns function


###############
# Apply topic label function
##############
topics_df.R <- 
  label_statement_topics.fns(
    fit = fit.R, 
    out = out.R)

# # Save df with topics labeled
# saveRDS(topics_df.R, paste0(getwd(), "/Data/output/2022-06-20_TopicLabeled_DF_HouseR.rds"))
# topics_df.R <- readRDS(paste0(getwd(), "/Data/output/2022-06-20_TopicLabeled_DF_HouseR.rds"))



############################################################
# Calculate member eigens for each congress - Function

# Summary:
  # 1. Filter to one congress (this can be changed to sessions)
  # 2. Keep only the first use of each topic for each member
  # 3. Turn data into cascade format for NetInf
  # 4. Estimate the network structure using NetInf
  # 5. Calculate each member's eigencentrality for the congress
  # 6. Simply the dataframe to output 3 columns (bioguide_id, eigen score, congress)

# Start Function
first.obs.fun <- function(topics.df, congressNum){

# Filter to one congress session
topics.congress <- 
  topics.df %>% 
  filter(congress == congressNum)

# Select the first use of each topic by member
# Group by topic, then by member
# Keep first use
first.obs <- 
  topics.congress %>% 
  group_by(topic, member_id) %>% 
  arrange(date_pr, .by_group = TRUE) %>% 
  distinct(member_id, topic, .keep_all = TRUE)

} # end first.obs.fun


##############################
# Prep data for NetInf - Function
##############################

estimateNetwork_outputEigens.fns <- function(first.obs, congressNum){

# Transform into a cascades object
congress_Cascade <- as_cascade_long(
  data = first.obs
  , cascade_node_name = 'member_id'
  , event_time = 'date_pr'
  , cascade_id = 'topic'
)



###############################################
# Infer edges based on a diffusion model

# Select # edges by, after each iteration of algorithm, check if the edge
  # added sig. improvement to the network
# Select parameters automatically

auto.netinf.result <- netinf(
  cascades = congress_Cascade
  , trans_mod = "exponential"
  , p_value_cutoff = 0.1
  # , params = 0.5 # lambda/rate
  )



##################################################################
# Calculate eigenvector centrality

# Convert graph to igraph format (directed ties)
netinf.graph <- 
  auto.netinf.result %>% 
  select(origin_node, destination_node) %>% 
  graph_from_data_frame()

# Calculate eigenvector centrality
eigens_result <- eigen_centrality(
  graph = netinf.graph
  , directed = TRUE
  , scale = TRUE
  , weights = NULL
)


# Pull out eigen values, member_id, congress, and index
eigens_df <- data.frame(
  member_id = names(eigens_result$vector),
  eigen_value = unname(eigens_result$vector),
  congress = congressNum
)


} # End estimateNetwork_outputEigens.fns

##########################################################

# Create datasets of first.obs by congress
firstObs_113 <- first.obs.fun(topics.df = topics_df.R
                              , congressNum = 113)

firstObs_114 <- first.obs.fun(topics.df = topics_df.R
                              , congressNum = 114)

firstObs_115 <- first.obs.fun(topics.df = topics_df.R
                              , congressNum = 115)

firstObs_116 <- first.obs.fun(topics.df = topics_df.R
                              , congressNum = 116)

set.seed(4444)
# Apply functions to congresses
eigens_113 <- estimateNetwork_outputEigens.fns(first.obs = firstObs_113
                                               , congressNum = 113)

eigens_114 <- estimateNetwork_outputEigens.fns(first.obs = firstObs_114
                                               , congressNum = 114)

eigens_115 <- estimateNetwork_outputEigens.fns(first.obs = firstObs_115
                                               , congressNum = 115)

eigens_116 <- estimateNetwork_outputEigens.fns(first.obs = firstObs_116
                                               , congressNum = 116)

# Merge data together
eigens_all <- bind_rows(eigens_113, eigens_114, eigens_115, eigens_116)



##########################################################

# Merge in legislator data


# Get one distict bio info for each member-congress
meta_data <-
  out.R$meta %>% 
  # Select just vars that are consistent across member-congress
  select(member_id, congress, state, gender, leadership_role, bills_cosponsored
         , bills_sponsored, dw_nominate, cook_pvi, votes_with_party_pct
         , next_election, switched_party, contains("_member")) %>% 
  distinct(member_id, congress)
  

eigens_legData <- 
  left_join(x = eigens_all
            , y = meta_data
            , by = c("member_id", "congress"))



###########################################################
# How early in a congress is the majority of members' first-use of topics?

#### This would have to be done earlier
# Like split out the function
# Use the first.obs df, and return that
# Then use that to plot


###########################################################



#######################################################
# Clean up variables for regression
#######################################################



# Filter to numeric, and features that make sense
corr_mat_features <-
  eigens_legData %>% 
  select(eigen_value, congress, state, gender, leadership_role, bills_cosponsored
         , bills_sponsored, dw_nominate, cook_pvi, votes_with_party_pct
         , next_election, switched_party, contains("_member"))

###
# Mutate variables to be more suitable for regression (factors, simplifying)
###

# Leadership role - split into Speaker and Leadership_other factors
corr_mat_features_mutated1 <-
  corr_mat_features %>% 
  mutate(speaker = ifelse(leadership_role == "Speaker of the House", 1, 0)
         , speaker = ifelse(is.na(speaker) == TRUE, 0, speaker)
         , leadership_other = ifelse(leadership_role %in% c(
           "Republican Caucus Chariman"
           , "House Minority Leader"
           , "Republican Whip"
           , "House Majority Leader"
         ), 1, 0)
         ) %>% 
  select(-leadership_role) %>% 
  mutate(speaker = as.factor(speaker)
         , leadership_other = as.factor(leadership_other))

# gender - change to factor
corr_mat_features_mutated2 <-
  corr_mat_features_mutated1 %>% 
  mutate(female = ifelse(gender == "F", 1, 0)) %>% 
  mutate(female = as.factor(female)) %>% 
  select(-gender)

# Cooks pvi
# Turn into numeric R is positive and D is negative

####### NOTE: Currently, cook_pvi is only for congress 116
  # Therefore, the below code doens't work b/c it filters the data
  # to only the 116th congress

# cookpvi_D <-
#   corr_mat_features_mutated2 %>%
#   filter(str_detect(cook_pvi, "D")) %>% 
#   mutate(cook_pvi = gsub("D\\+", "", cook_pvi)) %>% 
#   mutate(cook_pvi = paste0("-", cook_pvi)) %>% 
#   mutate(cook_pvi = as.numeric(cook_pvi))
# 
# cookpvi_R <-
#   corr_mat_features_mutated2 %>%
#   filter(str_detect(cook_pvi, "R")) %>% 
#   mutate(cook_pvi = gsub("R\\+", "", cook_pvi)) %>% 
#   mutate(cook_pvi = as.numeric(cook_pvi))
# 
# # Merge back together
# corr_mat_features_mutated3 <-
#   bind_rows(cookpvi_D
#             , cookpvi_R)


# Also didn't work:::
#
# # next_election = years til next election
# corr_mat_features_mutated4 <-
#   corr_mat_features_mutated2 %>% 
#   mutate(congress_year = ifelse(congress == 113, 2014
#                                 , ifelse(congress == 114, 2016
#                                          , ifelse(congress == 115, 2018
#                                                   , ifelse(congress == 116, 2020
#                                                            , NA))))) %>% 
#   mutate(years_til_nextElection = next_election - congress_year) %>% 
#   mutate(election_year = as.factor(ifelse(years_til_nextElection == 0, 1, 0))) %>% 
#   select(-c(next_election, years_til_nextElection, congress_year))

# remove unnecessarily variables/ones that didn't work
corr_mat_features_mutated3 <-
  corr_mat_features_mutated2 %>% 
  select(-c(state, switched_party, cook_pvi, congress, next_election))



# Remove faction memberships that are all zeros (no Republicans)
no_repub_factions <- c("jd_member", "cpc_member", "pop_member", 
                       "ndc_member", "bdc_member")

corr_mat_features_mutated4 <-
  corr_mat_features_mutated3 %>% 
  select(-any_of(no_repub_factions))


# convert memberships to factors
corr_mat_features_mutated5 <-
  corr_mat_features_mutated4 %>% 
  mutate(across(contains("_member"), as.factor))

# Scale IVs
dat <-
  corr_mat_features_mutated5 %>% 
  mutate(across(where(is.numeric), scale))


mod1 <- lm(eigen_value ~ 
             bills_cosponsored +
             bills_sponsored +
             dw_nominate +
             votes_with_party_pct +
             female +
             speaker +
             leadership_other
           , data = corr_mat_features_mutated5)

summary(mod1)









###############################################################
# Figures, Tests
# 1. Are eigens correlated for individuals across congresses? - Not really


# Are eigens correlated for individuals across congresses?
# Spread data into columns of members with rows being years (values being their different eigens)
eigens_wide <- 
  eigens_all %>%
  spread(member_id, eigen_value) %>%
  select(-congress) 

# And then calculate the correlation
eigenCors_acrossCongresses <- 
  apply(eigens_wide, 2, function(x) {
    cor(x, seq.int(nrow(eigens_wide)))
  })

# Turn into df
eigenCors <- 
  eigenCors_acrossCongresses %>% 
  as.data.frame()
names(eigenCors) <- "cors"


# Plot histogram of absolute value of corrs
ggplot(data = eigenCors) +
  geom_histogram(aes(abs(cors)), bins= 50) +
  ggtitle("Correlation between Repub member's centrality across congresses"
          , subtitle = "Centrality does not appear very correlated across congresses") +
  labs(x = "abs(Correlation)", y = "")


# End Tests/Experiments Section
#################################################################






str(corr_mat_features)




# Pull out member info from congress session data




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






















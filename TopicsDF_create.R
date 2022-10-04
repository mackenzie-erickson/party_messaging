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
# Legislative covariates
##############################

leg_covariates.raw <- readRDS(paste0(getwd(), "/Data/Legislator_info/from_williams/legislative_covariates.rds"))


###
# Press releases - Republican House
##############################

# Read in 20 topic models that were run on May 2 after removing state names

# STM-format processed data (trimmed/stemmed)
out.R <- readRDS(paste0(getwd(), "/Data/output/noStateNames/attempt3/2022-05-02_stmPrepped_houseR.rds"))

# Fitted topics - 20 topics
fit.R <- readRDS(paste0(getwd(), "/Data/output/noStateNames/attempt3/2022-05-02_Fit_20topics_houseR.rds"))

###
# Press releases - Democratic House
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



#####################################################################
# Add topic labels onto data.frame
# To provide information about topics

# Get STM-generated topic labels
fit.labels <- labelTopics(fit, 1:20, n = 7)
fit.labels_short <- labelTopics(fit, 1:20, n = 4) 

# Pull out 7-word summary, 3-word summary,  and topic #
labels.df <- data.frame(topic = fit.labels$topicnums,
                        topic_label = apply(fit.labels$prob, 1, paste0, collapse = "; "),
                        topic_label_short = apply(fit.labels_short$prob, 1, paste0, collapse = "; "))
labels.df$topic <- as.character(labels.df$topic)

# Merge onto data
topics.df <- 
  topics.df %>% 
  dtplyr::lazy_dt() %>% 
  left_join(labels.df, by = "topic") %>% 
  as.data.frame()

# Remove 2021 press releases (there are only 8, compared to 20k for the others)
topics.df <-
  topics.df %>% 
  filter(date_pr < ymd("2021-01-01"))

} # End label_statement_topics.fns function


###############
# Apply topic label function
##############
topics_df.R <-
  label_statement_topics.fns(
    fit = fit.R,
    out = out.R)



############################################################
# Get the first use of each topic by each member

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
  distinct(member_id, topic, .keep_all = TRUE) %>% 
  ungroup()

} # end first.obs.fun

###############
# Apply Function: First topic observations by member
##############


firstObs_113 <- first.obs.fun(topics.df = topics_df.R
                              , congressNum = 113)

firstObs_114 <- first.obs.fun(topics.df = topics_df.R
                              , congressNum = 114)

firstObs_115 <- first.obs.fun(topics.df = topics_df.R
                              , congressNum = 115)

firstObs_116 <- first.obs.fun(topics.df = topics_df.R
                              , congressNum = 116)

# Pull all firstObs together
firstObs_all <- bind_rows(firstObs_113, firstObs_114, firstObs_115, firstObs_116)






##############################
# NetInf Function

  # Create Cascades
  # Infer edges/ estimate network
  # Calculate eigencentrality for each member
##############################

estimateNetwork_outputEigens.fns <- function(first.obs, congressNum){
  

#######
# Transform into a cascades object
#######
  
congress_Cascade <- as_cascade_long(
  data = first.obs
  , cascade_node_name = 'member_id'
  , event_time = 'date_pr'
  , cascade_id = 'topic'
)
  
  # # Investigate cascades - just playing around
  # summary(congress_Cascade)
  # 
  # # Convert event times to dates
  # cascades_dates <-
  #   congress_Cascade %>% 
  #   modify_depth(2) %>% 
  #   mutate(event_time = as.Date(event_time, origin = "1970-01-01"))
  # 
  # 
  # # Visualize cascades
  # cascade_ids <- unique(first.obs$topic)
  # selection <- cascade_ids[c(1)]
  # plot(congress_Cascade, label_nodes = TRUE, selection = selection)
  # 
  # p <- plot(congress_Cascade
  #      , label_nodes = TRUE
  #      , selection = 1)
  # 
  # p + 
  #   scale_x_date()

#######
# Infer edges based on a diffusion model
#######

# Select # edges by, after each iteration of algorithm, check if the edge
  # added sig. improvement to the network
# Select parameters automatically

auto.netinf.result <- netinf(
  cascades = congress_Cascade
  , trans_mod = "exponential"
  , p_value_cutoff = 0.1
  # , params = 0.5 # lambda/rate
  )


#######
# Calculate eigenvector centrality
#######

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

#######
# Create eigens DF
# Pull out eigen values, member_id, congress, and index
#######

eigens_df <- data.frame(
  member_id = names(eigens_result$vector),
  eigen_value = unname(eigens_result$vector),
  congress = congressNum
)


} # End estimateNetwork_outputEigens.fns

##########################################################




#######
# Apply Function: Estimate network/eigencentrality
#######

set.seed(4445)

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



###############################################################
# COVARIATES - Create

###
# William Minozzi covariates
###

# Filter covariates to just 113-116 congress (both Repubs and Dems)
leg_covs <-
  leg_covariates.raw %>% 
  filter(congress %in% c(113:116)) %>% 
  filter(chamber == "H") %>% 
  as_tibble()

# Create "white" variable
leg_covs <-
  leg_covs %>% 
  mutate(white = ifelse(race_ethnicity == "White", 1, 0))

# Scale unscaled variables
leg_covs <-
  leg_covs %>% 
  mutate(scale_speeches_daily = scale(n_speeches_daily))

###
# ProPublica covariates
###

# Pull in ProPublica data from STM
propubdat <- out.R$meta

# Pull out relevant vars
propubdat.relevant <-
  propubdat %>% 
  select(member_id
         , congress
         , bills_sponsored
         , bills_cosponsored
         , contains("_member")
         , votes_with_party_pct)

# Mutate relevant vars
propubdat.relevant <-
  propubdat.relevant %>% 
  mutate(scale_bills_sponsored = scale(bills_sponsored) # scale
         , scale_bills_cosponsored = scale(bills_cosponsored) # scale
         , scale_votes_with_party_pct = scale(votes_with_party_pct)
         , caucus_member = 
           # dummy for member of any caucus
           ifelse(rowSums(select(., contains("_member")), na.rm = TRUE) > 0, 1, 0)
  ) %>% 
  select(member_id
         , congress
         , bills_sponsored
         , bills_cosponsored
         , votes_with_party_pct
         , scale_bills_sponsored
         , scale_bills_cosponsored
         , scale_votes_with_party_pct
         , caucus_member)

# Some members have duplicate member-congress (caused by both their real cosponsored # and another row with 0)
# Just select the correct ones so there is one member-congress only
propubdat.relevantFixed <-
  propubdat.relevant %>% 
  group_by(member_id, congress) %>% 
  arrange(desc(bills_cosponsored)
          , desc(bills_sponsored)
          , desc(caucus_member)
          , desc(votes_with_party_pct)) %>% 
  distinct(member_id, congress, .keep_all = TRUE)

###
# Merge covariate DFs
###

# Join ProPub (just Repubs) with leg_covs (Repubs and Dems)
covariates.df <-
  left_join(
    propubdat.relevantFixed
    , leg_covs
    , by = c("member_id" = "bioguide_id"
             , "congress" = "congress")
  )


##########################################################
# Regressions: 
###########################################################


####
# Merge eigencentrality.df with covariates.df
####


# Merge covariates with eigens df
eigensTopics_legCovs <-
  left_join(
    eigens_all
    , covariates.df,
    by = c("member_id", "congress")
  )



# Transform eigen value to make more readable (multiple by 100)
eigensTopics_legCovs <-
  eigensTopics_legCovs %>% 
  mutate(eigen_value_100 = eigen_value * 100)


####
# Models
####

# Main variables (nominate, leadership, gender, race)
modT.mainCovs <- lm(
  eigen_value_100 ~
    
    as.factor(party_leadership)
  + as.factor(committee_leadership)
  + scale_seniority
  + scale_leslag
  + nominate_dim1
  + as.factor(woman)
  + as.factor(white)
  
  , data = eigensTopics_legCovs
)

summary(modT.mainCovs)

# Main vars + more identity variables
modT.addIdentity <- lm(
  eigen_value_100 ~
    
    as.factor(party_leadership)
  + as.factor(committee_leadership)
  + scale_seniority
  + scale_leslag
  + nominate_dim1
  + as.factor(poc_woman)
  + as.factor(poc_man)
  + as.factor(majority_party)
  + as.factor(unopposed)
  + proximity_to_floor_centroid
  + as.factor(caucus_member)

  , data = eigensTopics_legCovs
)

summary(modT.addIdentity)

# Main vars + more behavioral vars
modT.addBehavioral <- lm(
  eigen_value_100 ~
    
    as.factor(party_leadership)
  + as.factor(committee_leadership)
  + scale_seniority
  + scale_leslag
  + nominate_dim1
  + nominate_dim2
  + as.factor(woman)
  + as.factor(white)
  + scale_votepct
  + scale_speeches_daily
  + scale_benchratiolag
  + scale_bills_sponsored
  + scale_bills_cosponsored
  + scale_votes_with_party_pct
  
  , data = eigensTopics_legCovs
)

summary(modT.addBehavioral)

# Outlier removal
    # Behavioral mod again, but remove Brian Fitzpatrick (R-PA) ("F000466")
    # He is an outlier because he legitimately cosponsored 1267 bills in 116th congress
      # When the average is 207, and the next highest is 614
modT.addBehavioral.outlierRemoved <- lm(
  eigen_value_100 ~
    
    as.factor(party_leadership)
  + as.factor(committee_leadership)
  + scale_seniority
  + scale_leslag
  + nominate_dim1
  + nominate_dim2
  + as.factor(woman)
  + as.factor(white)
  + scale_votepct
  + scale_speeches_daily
  + scale_benchratiolag
  + scale_bills_sponsored
  + scale_bills_cosponsored
  + scale_votes_with_party_pct
  
  , data = filter(eigensTopics_legCovs, member_id != "F000466")
)


summary(modT.mainCovs)
summary(modT.addIdentity)
summary(modT.addBehavioral)
summary(modT.addBehavioral.outlierRemoved)



########################################################################


##################################
# Look at bar charts to see if anything stands out as highly correlated (factors)

# Topics
eigensTopics_legCovs %>%
  ggplot( aes(x=eigen_value, fill=as.factor(party_leadership))) +
  geom_histogram( color="#e9ecef", alpha=0.6, position = 'identity') +
  scale_fill_manual(values=c("#69b3a2", "#404080")) +
  labs(fill="")

# Frames
eigens_legCovs %>%
  ggplot( aes(x=eigen_value, fill=as.factor(party_leadership))) +
  geom_histogram( color="#e9ecef", alpha=0.6, position = 'identity') +
  scale_fill_manual(values=c("#69b3a2", "#404080")) +
  labs(fill="")

##################################
# Look at scatterplots to see if anything stands out as highly correlated (continuous)

# Topics
eigensTopics_legCovs %>% 
  ggplot(aes(x = eigen_value, y = nominate_dim2)) +
  geom_point(aes(fill = as.factor(republican)))






# Regression table
mod2 <- lm(
  eigen_value_100 ~
    proximity_to_floor_centroid_zero_code_missingness
  + scale_leslag_zero_code_missingness
  + scale_benchratiolag
  + scale_votepct
  + party_leadership
  + committee_leadership
  + unopposed
  + majority_party
  + nominate_dim1
  + nominate_dim2
  + scale_seniority
  + woman
  + white
  , data = eigens_legCovs
)

summary(mod2)




###########################################################
# Analysis:
###########################################################


#######
# How early in a congress is the majority of members' first-use of topics?
#######

# Time distribution - first use of topics ~ congress
ggplot(data = group_by(firstObs_all, congress)) +
  geom_histogram(aes(x = date_pr, fill = topic), bins = 50) +
  facet_wrap(~ congress, scales = "free_x") +
  ggtitle("Time distribution - first use of topics by congress")


# Distribution of first use within XXth congress ~ topic
ggplot(data = filter(firstObs_116)) +
  geom_histogram(aes(x = date_pr), bins = 30) +
  scale_x_date(limits = c(
    as.Date(firstObs_116$session_1_startDate[1])
    , as.Date(firstObs_116$session_2_endDate[1]))
  ) +
  facet_wrap(~ topic_label_short) +
  theme_minimal() +
  ggtitle("Distribution of first use by topic", subtitle = "116th Congress")


#######
# Are any topics increasing or decreasing over time?
#######

# Facet wrap plot of all topics and density over time
ggplot(data = topics_df.R) +
  geom_density(aes(x = date_pr)) +
  facet_wrap(~ topic_label_short
             , ncol = 3
             , scales = "free_y"
             ) +
  geom_vline(xintercept = c(
    ymd("2015-01-03"
        , ymd("2017-01-03")
        , ymd("2019-01-03"))
  ), linetype = "dotted") +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  theme_minimal()




###########################################################






#######################################################
# Clean up variables for regression
#######################################################



# # Filter to numeric, and features that make sense
# # Remove variables that were problematic for regression
# # Details on removed variables:
#   # 1. Cook_PVI: Tried to turn it into numerical (D below 0; R above), but I only
#     # have data for 116 congress, so it's pointless
#   # 2. Next elections: Tried to turn it into a years-til election number, but the
#     # only numbers were 0 and 2, so I turned it into an election_year dummy
#     # however, it did not work in the regression
#   # 3. Switched party: also did not work; potentially because there are very few
# simple_lm_features <-
#   eigens_legData %>% 
#   select(-c(cook_pvi, next_election, switched_party))
# 
# ###
# # Mutate variables to be more suitable for regression (factors, simplifying)
# ###
# 
# # Leadership role - split into Speaker and Leadership_other factors
# simple_lm_features2 <-
#   simple_lm_features %>% 
#   mutate(speaker = ifelse(leadership_role == "Speaker of the House", 1, 0)
#          , speaker = ifelse(is.na(speaker) == TRUE, 0, speaker)
#          , leadership_other = ifelse(leadership_role %in% c(
#            "Republican Caucus Chariman"
#            , "House Minority Leader"
#            , "Republican Whip"
#            , "House Majority Leader"
#          ), 1, 0)
#          ) %>% 
#   select(-leadership_role) %>% 
#   mutate(speaker = as.factor(speaker)
#          , leadership_other = as.factor(leadership_other))
# 
# # gender - change to factor
# simple_lm_features3 <-
#   simple_lm_features2 %>% 
#   mutate(female = ifelse(gender == "F", 1, 0)) %>% 
#   mutate(female = as.factor(female)) %>% 
#   select(-gender)
# 
# 
# # Remove faction memberships that are all zeros (no Republicans)
# no_repub_factions <- c("jd_member", "cpc_member", "pop_member", 
#                        "ndc_member", "bdc_member")
# 
# simple_lm_features4 <-
#   simple_lm_features3 %>% 
#   select(-any_of(no_repub_factions))
# 
# 
# # convert memberships to factors
# simple_lm_features5 <-
#   simple_lm_features4 %>% 
#   mutate(across(contains("_member"), as.factor))
# 
# # Scale IVs
# dat_simple_lm <-
#   simple_lm_features5 %>% 
#   mutate(across(where(is.numeric), scale))
# 
# 
# mod1 <- lm(eigen_value ~ 
#              bills_cosponsored +
#              bills_sponsored +
#              dw_nominate +
#              votes_with_party_pct +
#              female +
#              speaker +
#              leadership_other
#            , data = dat_simple_lm)
# 
# summary(mod1)



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
  geom_histogram(aes(abs(cors)), bins= 21, fill = "white", color = "black") +
  ggtitle("Correlation between Repub member's centrality across congresses"
          , subtitle = "Centrality does not appear very correlated across congresses") +
  labs(x = "abs(Correlation)", y = "") +
  theme_minimal()


# End Tests/Experiments Section
#################################################################










#################################################################
#################################################################
# SECTION 2: FRAMES
#################################################################
#################################################################

# Description:

# Within each topic, what is the second highest topic? 
# Who is most central in a diffusion network of that frame?




###################################################

# Function: - Label each doc with topic and frame
  # Repeat section 1 (label docs with most-prob topic)
  # Add frame label (most-prob second topic per doc)
  # Add topic/frame descriptions

###################################################



label_statement_topics.fns <- function(fit, out){

  ########
  # Add TOPIC label to each doc
  ########
  
  # Make a data.table of topic proportions
  topics_DT <- make.dt(fit, meta = out$meta)
  
  # Pivot longer so each doc is repeated 20 times
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
  
  
  # Create mainTopic.df
  # (keep only the observation with the highest prob)
  mainTopic.df <- 
    topics_long %>% 
    distinct(indx, .keep_all = TRUE)
  
  ########
  # Add FRAME label to each doc
  ########

  # Get the remaining frames
  remaining_frames <-
    setdiff(topics_long, mainTopic.df) %>% 
    group_by(indx) %>% 
    arrange(desc(topic_prob), .by_group = TRUE) %>% 
    ungroup()
  
  # Keep only the highest prob frame
  mainFrame <-
    remaining_frames %>% 
    distinct(indx, .keep_all = TRUE)
  
  # Rename "frame" and "frame_prob" and only keep them and indx in simple df
  mainFrame_simple <-
    mainFrame %>% 
    rename(frame = topic,
           frame_prob = topic_prob) %>% 
    select(indx, frame, frame_prob)
  
  
  ########
  # Merge TOPIC and FRAME dfs
  ########
  
  # Merge frames onto main topic df
  topicsFrames.df <- 
    left_join(
      mainTopic.df
      , mainFrame_simple
      , by = c("indx")
    )

  
  ########
  # Remove the incorrect caucus info
  ########
  
  topicsFrames.df <-
    topicsFrames.df %>% 
    select(-c(contains("_leader"), contains("_titles"),
              contains("_taskforces"), contains("_member")))
  
  
  
  ########
  # Add topic labels onto data.frame
  # To provide information about topics/frames
  ########
  

  # Pull 7 and 4 word summaries (estimated by STM)
  set.seed(5555)
  # 7-word summary
  fit.labels <- labelTopics(fit, 1:20, n = 7)
  # 4-word summary
  fit.labels_short <- labelTopics(fit, 1:20, n = 4) 
  
  
  # Labels for TOPICS
  # Topic # and short/long descriptions
  topic_labels.df <- data.frame(topic = as.character(fit.labels$topicnums),
                          topic_label = apply(fit.labels$prob, 1, paste0, collapse = "; "),
                          topic_label_short = apply(fit.labels_short$prob, 1, paste0, collapse = "; "))
  
  # Labels for FRAMES
  frame_labels.df <- data.frame(frame = as.character(fit.labels$topicnums),
                                frame_label = apply(fit.labels$prob, 1, paste0, collapse = "; "),
                                frame_label_short = apply(fit.labels_short$prob, 1, paste0, collapse = "; "))
  
  
  ########
  # Merge topic labels onto data
  ########
  topicsFrames.df <- 
    topicsFrames.df %>% 
    dtplyr::lazy_dt() %>% 
    left_join(topic_labels.df, by = "topic") %>% 
    as.data.frame()
  
  ########
  # Merge frame labels onto data
  ########
  topicsFrames.df <- 
    topicsFrames.df %>% 
    dtplyr::lazy_dt() %>% 
    left_join(frame_labels.df, by = "frame") %>% 
    as.data.frame()
  
  
} ##################################  End label_statement_topics.fns function





###############
# Apply topic label function
##############
set.seed(7777)
topicsFrames.R <-
  label_statement_topics.fns(
    fit = fit.R,
    out = out.R)

# Remove 2021 press releases (there are only 8, compared to 20k for the others)
topicsFrames.R <-
  topicsFrames.R %>% 
  filter(date_pr < ymd("2021-01-01"))


# Table: Examples of Statement title, Topic, Frame
# Pull random examples:
sample_n(topicsFrames.R, 1) %>% select(title_pr, topic_label, frame_label)







##########################################################
# Most Common Frames
# Make DF for diffusion network
##########################################################


# For each topic, what is the most common frame?
mostCommonFrames.R <-
  topicsFrames.R %>% 
  group_by(topic) %>% 
  count(frame) %>% 
  summarize(frame = as.character(which.max(n))) 


####
# Pull the unique descriptions of topics/frames
####

# Topic descriptions
topicDescs.df <-
  topicsFrames.R %>% 
  select(topic, topic_label, topic_label_short) %>% 
  distinct()

# Frame descriptions
frameDescs.df <-
  topicsFrames.R %>% 
  select(frame, frame_label, frame_label_short) %>% 
  distinct()
  
# Merge TOPIC descriptions onto table
mostCommonFrames.R <-
  mostCommonFrames.R %>% 
  left_join(., topicDescs.df
            , by = "topic"
            )

# Merge FRAME descriptions onto table
mostCommonFrames.R <-
  mostCommonFrames.R %>% 
  left_join(., frameDescs.df,
            by = "frame")




################################################################
# Analysis:
# Descriptive Figures/Tables: Frames
################################################################


# Clean/organize table for publish
library(huxtable)

mostCommonFrames.hux <-
  mostCommonFrames.R %>% 
  select(topic, frame, topic_label_short, frame_label_short) %>% 
  arrange(frame) %>% 
  rename("Topic" = topic
         , "Most-common frame" = frame
         , "Topic desc" = topic_label_short
         , "Frame desc" = frame_label_short) %>% 
  hux() %>% 
  set_bold(row = 1, col = everywhere, value = TRUE) %>% 
  set_all_borders(TRUE)
  
print_screen(mostCommonFrames.hux)


# Table: Most common frame across the board?

topicsFrames.R %>% 
  group_by(frame_label_short) %>% 
  summarize(cnt = n()) %>% 
  mutate(freq = round(cnt / sum(cnt), 3)) %>% 
  arrange(desc(freq)) %>% 
  select(frame_label_short, freq, cnt) %>% 
  hux() %>% 
  set_bold(row = 1, col = everywhere, value = TRUE) %>% 
  set_all_borders(TRUE)
  



# Body data
# Pull in full text of press releases
  # Look at examples of less clear topics (e.g. 14)

bodyDat <- readRDS(paste0(getwd(), "/Data/Press_releases/2022-04_BodyData_HouseR.RDS"))

bodyDat <-
  left_join(
    x = topicsFrames.R
    , y = bodyDat
    , by = "indx"
  )

# Topic 14
topicsFrames.R %>% filter(topic == "14") %>% distinct(topic_label)

bodyDat %>% 
  filter(topic == "14") %>% 
  sample_n(1) %>% 
  select(body_pr) %>% 
  print()


# Topic 8 - Appears to be complaints against govern't agencies (IRS, EPA)
  # Does it increase in popularity when Obama is president

topicsFrames.R %>% 
  # Filter to topic 8
  filter(topic == "8") %>% 
  ggplot(aes(x = date_pr)) +

  # Add vertical lines at congress changes
    geom_vline(xintercept = c(
      ymd("2015-01-03"
          , ymd("2017-01-03")
          , ymd("2019-01-03"))
    ), linetype = "dotted") +

  # Fill blue background behind Obama presidency
  annotate(geom = "rect"
           , xmin = ymd("2013-01-03")
           , xmax = ymd("2017-01-03")
           , ymin = 0
           , ymax = Inf
           , fill = "blue"
           , alpha = 0.5) +
  
  # Fill red background behind Trump presidency
  annotate(geom = "rect"
           , xmin = ymd("2017-01-03")
           , xmax = ymd("2021-01-03")
           , ymin = 0
           , ymax = Inf
           , fill = "red"
           , alpha = 0.5) +
  
  # Add density line for frequency of topic
  geom_density() +
  
  # Correct x-axis scale
  scale_x_date(limits = c(ymd("2013-01-03"), ymd("2021-01-03"))) +
  
  # Add title
  ggtitle("Frequency of Gov't Agency Blaming (Topic 8)")
  


# END: Analysis
################################################################




############################################################################
############################################################################
# FRAMES - Calculate Eigens
############################################################################
############################################################################



# There are only 556 observations of people using the main frame within each topic
# As op
# This means that the main frame is probably not that "main"/popular

data %>%
  group_by(month) %>%
  mutate(per =  100 *count/sum(count)) %>% 
  ungroup



############################################################
# Function: First Observation of TOPIC-FRAME usage by memeber
############################################################


# Start Function
first.obs.TopicFrame.fun <- function(topicsFrames.df, mostCommonFrames.df, congressNum){
  
  # Filter to one congress session
  topics.congress <- 
    topicsFrames.df %>% 
    filter(congress == congressNum)
  
  # Create topic_frame variable for all press releases
  topic.frame.df <-
    topics.congress %>% 
    mutate(topic_frame = paste(topic, frame, sep = "_"))
  
  # Create topic_frame variable within mostCommonFrames.df
  mostCommonFrames.df <-
    mostCommonFrames.df %>% 
    mutate(topic_frame = paste(topic, frame, sep = "_"))
  
  # Filter press releases to only those using the most common frame for each topic
  topic.frame.filtered <-
    topic.frame.df %>% 
    filter(topic_frame %in% unique(mostCommonFrames.df$topic_frame))
  
  
  # Select first use of each topic_frame by member
  first.obs.topic_frame <- 
    topic.frame.filtered %>% 
    group_by(topic_frame, member_id) %>% 
    select(topic_frame, member_id, date_pr, everything()) %>% 
    arrange(date_pr, .by_group = TRUE) %>% 
    distinct(member_id, topic_frame, .keep_all = TRUE) %>% 
    ungroup()
  

  
} ##################################### end first.obs.TopicFrame.fun


########################
# Apply function
# Create datesets of first TOPIC_FRAME observation by member-congress
########################


firstFrame.obs_113 <- first.obs.TopicFrame.fun(
  topicsFrames.df = topicsFrames.R
  , mostCommonFrames.df = mostCommonFrames.R
  , congressNum = 113)

firstFrame.obs_114 <- first.obs.TopicFrame.fun(
  topicsFrames.df = topicsFrames.R
  , mostCommonFrames.df = mostCommonFrames.R
  , congressNum = 114)

firstFrame.obs_115 <- first.obs.TopicFrame.fun(
  topicsFrames.df = topicsFrames.R
  , mostCommonFrames.df = mostCommonFrames.R
  , congressNum = 115)

firstFrame.obs_116 <- first.obs.TopicFrame.fun(
  topicsFrames.df = topicsFrames.R
  , mostCommonFrames.df = mostCommonFrames.R
  , congressNum = 116)

# Pull all firstObs together
firstFrame.obs_all <- bind_rows(firstFrame.obs_113
                                , firstFrame.obs_114
                                , firstFrame.obs_115
                                , firstFrame.obs_116)



##############################
# NetInf Function - FRAMES

# Create Cascades
# Infer edges/ estimate network
# Calculate eigencentrality for each member
##############################

estimateNetwork_outputEigens_frames.fns <- function(firstFrame.obs, congressNum){
  
  #######
  # Transform into a cascades object
  #######
  
  congress_Cascade <- as_cascade_long(
    data = firstFrame.obs
    , cascade_node_name = 'member_id'
    , event_time = 'date_pr'
    , cascade_id = 'topic_frame'
  )
  
  
  #######
  # Infer edges based on a diffusion model
  #######
  
  # Select # edges by, after each iteration of algorithm, check if the edge
  # added sig. improvement to the network
  # Select parameters automatically
  
  auto.netinf.result <- netinf(
    cascades = congress_Cascade
    , trans_mod = "exponential"
    , p_value_cutoff = 0.1
    # , params = 0.5 # lambda/rate
  )
  
  
  #######
  # Calculate eigenvector centrality
  #######
  
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
  
  #######
  # Create eigens DF
  # Pull out eigen values, member_id, congress, and index
  #######
  
  eigens_df <- data.frame(
    member_id = names(eigens_result$vector),
    eigen_value = unname(eigens_result$vector),
    congress = congressNum
  )
  
  
} # End estimateNetwork_outputEigens_frames.fns

##########################################################



#######
# Apply function: Estimate network/eigencentrality - frames
#######

set.seed(4444)

frame.eigens_113 <- estimateNetwork_outputEigens_frames.fns(
  firstFrame.obs = firstFrame.obs_113
  , congressNum = 113)
  
frame.eigens_114 <- estimateNetwork_outputEigens_frames.fns(
  firstFrame.obs = firstFrame.obs_114
  , congressNum = 114)

frame.eigens_115 <- estimateNetwork_outputEigens_frames.fns(
  firstFrame.obs = firstFrame.obs_115
  , congressNum = 115)

frame.eigens_116 <- estimateNetwork_outputEigens_frames.fns(
  firstFrame.obs = firstFrame.obs_116
  , congressNum = 116)

# Merge data together
frame.eigens_all <- bind_rows(frame.eigens_113
                        , frame.eigens_114
                        , frame.eigens_115
                        , frame.eigens_116)




# LEFT OFF HERE
##########################################################
# Merge eigencentrality.df with leg_data
###########################################################


###### TEMP #############
# Examine legislative covariates
# TO DO: Code to be cleaned up and moved to the beginning of the script most likely
#######################

# Examine legislative_covariates

# GO QUICK - DO it DIRTY
# Merge all the data
# Hand write a list 
  # - what definitely doesn't apply (e.g. Republican, Dem)
  # - what is probably repeats
  # - what is probably perfectly collinear
# Then from the rest, select the most interesting ones for a regression

# Then, google multilevel models - how do related to FE and how do they help with having a huge N or multiple people
  


#############
# Merge with legislative covariates

eigensFrames_legcovs <-
  left_join(
    frame.eigens_all
    , covariates.df,
    by = c("member_id", "congress")
  )
  




topic1 <- topicsFrames.R %>% 
  filter(topic == "1")

topic1 %>% 
  summarize()

###########################################################
# Analysis - Frames
##########################################################

###
# Frame Frequency Tables
###

ff_top1 <- as.data.frame(prop.table(table(filter(
  topicsFrames.R, topic == "1")$frame))) %>% 
  arrange(desc(Freq))
ff_top2 <- as.data.frame(prop.table(table(filter(
  topicsFrames.R, topic == "2")$frame))) %>% 
  arrange(desc(Freq))
ff_top3 <- as.data.frame(prop.table(table(filter(
  topicsFrames.R, topic == "3")$frame))) %>% 
  arrange(desc(Freq))
ff_top4 <- as.data.frame(prop.table(table(filter(
  topicsFrames.R, topic == "4")$frame))) %>% 
  arrange(desc(Freq))
ff_top5 <- as.data.frame(prop.table(table(filter(
  topicsFrames.R, topic == "5")$frame))) %>% 
  arrange(desc(Freq))
ff_top6 <- as.data.frame(prop.table(table(filter(
  topicsFrames.R, topic == "6")$frame))) %>% 
  arrange(desc(Freq))
ff_top7 <- as.data.frame(prop.table(table(filter(
  topicsFrames.R, topic == "7")$frame))) %>% 
  arrange(desc(Freq))
ff_top8 <- as.data.frame(prop.table(table(filter(
  topicsFrames.R, topic == "8")$frame))) %>% 
  arrange(desc(Freq))
ff_top9 <- as.data.frame(prop.table(table(filter(
  topicsFrames.R, topic == "9")$frame))) %>% 
  arrange(desc(Freq))
ff_top10 <- as.data.frame(prop.table(table(filter(
  topicsFrames.R, topic == "10")$frame))) %>% 
  arrange(desc(Freq))
ff_top11 <- as.data.frame(prop.table(table(filter(
  topicsFrames.R, topic == "11")$frame))) %>% 
  arrange(desc(Freq))
ff_top12 <- as.data.frame(prop.table(table(filter(
  topicsFrames.R, topic == "12")$frame))) %>% 
  arrange(desc(Freq))
ff_top13 <- as.data.frame(prop.table(table(filter(
  topicsFrames.R, topic == "13")$frame))) %>% 
  arrange(desc(Freq))
ff_top14 <- as.data.frame(prop.table(table(filter(
  topicsFrames.R, topic == "14")$frame))) %>% 
  arrange(desc(Freq))
ff_top15 <- as.data.frame(prop.table(table(filter(
  topicsFrames.R, topic == "15")$frame))) %>% 
  arrange(desc(Freq))
ff_top16 <- as.data.frame(prop.table(table(filter(
  topicsFrames.R, topic == "16")$frame))) %>% 
  arrange(desc(Freq))
ff_top17 <- as.data.frame(prop.table(table(filter(
  topicsFrames.R, topic == "17")$frame))) %>% 
  arrange(desc(Freq))
ff_top18 <- as.data.frame(prop.table(table(filter(
  topicsFrames.R, topic == "18")$frame))) %>% 
  arrange(desc(Freq))
ff_top19 <- as.data.frame(prop.table(table(filter(
  topicsFrames.R, topic == "19")$frame))) %>% 
  arrange(desc(Freq))
ff_top20 <- as.data.frame(prop.table(table(filter(
  topicsFrames.R, topic == "20")$frame))) %>% 
  arrange(desc(Freq))


frameFreqList <-
  list(
    ff_top1 = as.data.frame(prop.table(table(filter(
      topicsFrames.R, topic == "1")$frame))) %>% 
      arrange(desc(Freq)),
    ff_top2 = as.data.frame(prop.table(table(filter(
      topicsFrames.R, topic == "2")$frame))) %>% 
      arrange(desc(Freq)),
    ff_top3 = as.data.frame(prop.table(table(filter(
      topicsFrames.R, topic == "3")$frame))) %>% 
      arrange(desc(Freq)),
    ff_top4 = as.data.frame(prop.table(table(filter(
      topicsFrames.R, topic == "4")$frame))) %>% 
      arrange(desc(Freq)),
    ff_top5 = as.data.frame(prop.table(table(filter(
      topicsFrames.R, topic == "5")$frame))) %>% 
      arrange(desc(Freq)),
    ff_top6 = as.data.frame(prop.table(table(filter(
      topicsFrames.R, topic == "6")$frame))) %>% 
      arrange(desc(Freq)),
    ff_top7 = as.data.frame(prop.table(table(filter(
      topicsFrames.R, topic == "7")$frame))) %>% 
      arrange(desc(Freq)),
    ff_top8 = as.data.frame(prop.table(table(filter(
      topicsFrames.R, topic == "8")$frame))) %>% 
      arrange(desc(Freq)),
    ff_top9 = as.data.frame(prop.table(table(filter(
      topicsFrames.R, topic == "9")$frame))) %>% 
      arrange(desc(Freq)),
    ff_top10 = as.data.frame(prop.table(table(filter(
      topicsFrames.R, topic == "10")$frame))) %>% 
      arrange(desc(Freq)),
    ff_top11 = as.data.frame(prop.table(table(filter(
      topicsFrames.R, topic == "11")$frame))) %>% 
      arrange(desc(Freq)),
    ff_top12 = as.data.frame(prop.table(table(filter(
      topicsFrames.R, topic == "12")$frame))) %>% 
      arrange(desc(Freq)),
    ff_top13 = as.data.frame(prop.table(table(filter(
      topicsFrames.R, topic == "13")$frame))) %>% 
      arrange(desc(Freq)),
    ff_top14 = as.data.frame(prop.table(table(filter(
      topicsFrames.R, topic == "14")$frame))) %>% 
      arrange(desc(Freq)),
    ff_top15 = as.data.frame(prop.table(table(filter(
      topicsFrames.R, topic == "15")$frame))) %>% 
      arrange(desc(Freq)),
    ff_top16 = as.data.frame(prop.table(table(filter(
      topicsFrames.R, topic == "16")$frame))) %>% 
      arrange(desc(Freq)),
    ff_top17 = as.data.frame(prop.table(table(filter(
      topicsFrames.R, topic == "17")$frame))) %>% 
      arrange(desc(Freq)),
    ff_top18 = as.data.frame(prop.table(table(filter(
      topicsFrames.R, topic == "18")$frame))) %>% 
      arrange(desc(Freq)),
    ff_top19 = as.data.frame(prop.table(table(filter(
      topicsFrames.R, topic == "19")$frame))) %>% 
      arrange(desc(Freq)),
    ff_top20 = as.data.frame(prop.table(table(filter(
      topicsFrames.R, topic == "20")$frame))) %>% 
      arrange(desc(Freq))
  )

# What are the max frame frequencies in each topic?
maxFrameFreqs <- plyr::ldply(.data = frameFreqList, .fun = slice_max, Freq) %>% arrange(desc(Freq))


# Average max?
summary(maxFrameFreqs$Freq)













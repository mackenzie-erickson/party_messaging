###############################

# TopicsDF_create_sessionSlices.R
# Author: Mackenzie Weiler
# Date: 2022-04
# Description: Load fitted STM model and original document data, 
  # get in cascade format, and fit diffusion network


################################

rm(list = ls())

# Packages
packages <- c("NetworkInference", "tidyverse", "igraph", "ggplot2", "lubridate"
              , "stringr", "stm", "quanteda", "stminsights"
              , "texreg", "knitr", "kableExtra", "huxtable", "data.table"
              , "cowplot", "grid", "gridExtra", "egg", "scales"
              , "plm", "lfactors", "gtsummary")

# Install packages, if necessary
new.packages <- packages[!(packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

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
committee.raw <- readRDS(paste0(getwd(), "/Data/Legislator_info/committee-data-for-mac-2022-05-27.rds"))

###
# Press releases - Republican House
##############################

# Read in 30 topic models that were run on July 14 after removing state and member names

# STM-format processed data (trimmed/stemmed)
out.R <- readRDS(paste0(getwd(), "/Data/output/noNames/2022-07-14_stmPrepped_houseR.rds"))

# Fitted topics
fit.R <- readRDS(paste0(getwd(), "/Data/output/noNames/2022-08-01_Fit_30topics_houseR.rds"))



###
# Press releases - Democratic House
#############################

# STM-format processed data (trimmed/stemmed)
out.D <- readRDS(paste0(getwd(), "/Data/output/noNames/2022-07-14_stmPrepped_houseD.rds"))

# Fitted topics
fit.D <- readRDS(paste0(getwd(), "/Data/output/noNames/2022-08-01_Fit_30topics_houseD.rds"))




###################################################
# Label each document with its most-probable topic
###################################################

###
# Make a data.tables of topic proportions
###
topics_DT.R <- make.dt(fit.R, meta = out.R$meta)
topics_DT.D <- make.dt(fit.D, meta = out.D$meta)


# Function to label each document with most-propable topic

label_statement_topics.fns <- function(topics_DT, fit, k){

# Pivot DT longer
# So that each doc is repeated k times (k topics)
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

###########################
# Fix caucus info - something didn't merge properly
##########################

# Remove the incorrect caucus info
topics.df <-
  topics.df %>% 
  select(-c(contains("_leader"), contains("_titles"),
            contains("_taskforces"), contains("_member")))


# Add topic labels onto data.frame
# To provide information about topics

# Get STM-generated topic labels
fit.labels <- labelTopics(fit, 1:k, n = 10)
fit.labels_short <- labelTopics(fit, 1:k, n = 4) 

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
topics_df.R <- label_statement_topics.fns(topics_DT.R, fit.R, k = 30)
topics_df.D <- label_statement_topics.fns(topics_DT.D, fit.D, k = 30)





############################################################
# Create session variable

###
# Create variable for session slices - Function
###
createSessionVar <- function(topics.df){
  topics.df <- 
    topics.df %>% 
    mutate(sessionStartDate = ifelse(
      test = (date_pr >= session_1_startDate & date_pr < session_1_endDate)
      , yes = as.character(session_1_startDate)
      , no = as.character(session_2_startDate))) %>% 
    mutate(sessionStartDate = as.Date(sessionStartDate))
  
  ##
  # Add 'session' variable
  topics.df2 <- topics.df %>% 
    mutate(session = 
             ifelse(
               sessionStartDate %in% 
                 as.Date(c("2013-01-03", "2015-01-06", "2017-01-03", "2019-01-03"))
               , 1, 2
             ))
  return(topics.df2)
}

###
# Apply function
###
topics_df.R <- createSessionVar(topics_df.R)
topics_df.D <- createSessionVar(topics_df.D)

#########
# Load in manual topic names and salience
########
topicNames.R <- readxl::read_xlsx(paste0(getwd(), "/Data/Other/topicNames_30k_R.xlsx")) %>% mutate(topic = as.character(topic))
topicNames.D <- readxl::read_xlsx(paste0(getwd(), "/Data/Other/topicNames_30k_D.xlsx")) %>% mutate(topic = as.character(topic))

# Merge with topics_df
topics_df.R <- 
  left_join(topics_df.R, topicNames.R,
            by = c("topic", "topic_label"))

topics_df.D <- 
  left_join(topics_df.D, topicNames.D,
            by = c("topic", "topic_label"))

###
# Filter to first obs per congress - Function
###
first.obs.fun <- function(topics.df){
  first.obs <-
    topics.df %>% 
    group_by(congress, topic, member_id) %>% 
    arrange(date_pr, .by_group = TRUE) %>% 
    distinct(member_id, topic, congress, .keep_all = TRUE) %>% 
    ungroup()
}

###
# Apply function
###
first.obs.R <- first.obs.fun(topics_df.R)
first.obs.D <- first.obs.fun(topics_df.D)


# Jeff Van Drew switched from Dem to Repub during 116th congress, so he was
# included in both party's networks. 
# Remove Dem observation
first.obs.D <- first.obs.D %>% filter(member_id != "V000133")


###
# Filter to politically-salient topics
###
politSalient.R <- first.obs.R %>% 
  filter(salient == 1)
politSalient.D <- first.obs.D %>% 
  filter(salient == 1)



##############################################################################
##############################################################################
# Network Version 1: One network/score inferred from multiple sessions
##############################################################################
##############################################################################



##########
# Prepare data for cascades that are topic-session (so that there is more info for the netinf)
#########

# Create congress_session var (e.g. 115_1)
first.obs.R <- first.obs.R %>% 
  mutate(congress_session = ifelse( congress == 113 & sessionStartDate == ymd("2013-01-03"), "113_1",
                                    ifelse(congress == 113 & sessionStartDate == ymd("2014-01-03"), "113_2",
                                           ifelse(congress == 114 & sessionStartDate == ymd("2015-01-06"), "114_1",
                                                  ifelse(congress == 114 & sessionStartDate == ymd("2016-01-04"), "114_2",
                                                         ifelse(congress == 115 & sessionStartDate == ymd("2017-01-03"), "115_1",
                                                                ifelse(congress == 115 & sessionStartDate == ymd("2018-01-03"), "115_2",
                                                                       ifelse(congress == 116 & sessionStartDate == ymd("2019-01-03"), "116_1",
                                                                              ifelse(congress == 116 & sessionStartDate == ymd("2020-01-03"), "116_2",
                                                                                     NA)))))))
  ))

first.obs.D <- first.obs.D %>% 
  mutate(congress_session = ifelse( congress == 113 & sessionStartDate == ymd("2013-01-03"), "113_1",
                                    ifelse(congress == 113 & sessionStartDate == ymd("2014-01-03"), "113_2",
                                           ifelse(congress == 114 & sessionStartDate == ymd("2015-01-06"), "114_1",
                                                  ifelse(congress == 114 & sessionStartDate == ymd("2016-01-04"), "114_2",
                                                         ifelse(congress == 115 & sessionStartDate == ymd("2017-01-03"), "115_1",
                                                                ifelse(congress == 115 & sessionStartDate == ymd("2018-01-03"), "115_2",
                                                                       ifelse(congress == 116 & sessionStartDate == ymd("2019-01-03"), "116_1",
                                                                              ifelse(congress == 116 & sessionStartDate == ymd("2020-01-03"), "116_2",
                                                                                     NA)))))))
  ))


###
# Create topic_session var (e.g. 28_115_1)
###
first.obs.R <- first.obs.R %>% 
  mutate(topic_session = paste(topic, congress_session, sep = "_"))

first.obs.D <- first.obs.D %>% 
  mutate(topic_session = paste(topic, congress_session, sep = "_"))





##############################################################################
##############################################################################
# Network Version 2: 4 networks/multiple scores inferred from 4 separate congresses
##############################################################################
##############################################################################


###
# Estimate networks - Function - Separate nets for each congress
###

  
estimateNetwork_outputScore.fns <- 
  function(first.obs, model = c("exponential", "rayleigh", "log-normal")){
  
  results_list <- vector(mode = "list")
  
  for (i in unique(first.obs$congress)) {
    
    # Filter dataset
     congress.df <- 
       first.obs %>% 
       filter(congress == i)
     
     
     # Arrange
     congress.df <-
       congress.df %>% 
       arrange(topic, member_id)
     
     # Create cascades
     congress.cascade <-
       as_cascade_long(
         data = congress.df
         , cascade_node_name = 'member_id'
         , event_time = 'date_pr'
         , cascade_id = 'topic'
       )
     
     # Estimate network
     congress.netinf.result <- netinf(
       cascades = congress.cascade
       , trans_mod = model
       , p_value_cutoff = 0.1
     )
     
     ######
     #  Calculate Eigenvector centrality
     ######
     
     # Convert graph to igraph format (directed ties)
     igraph <-
       congress.netinf.result %>%
       select(origin_node, destination_node) %>%
       graph_from_data_frame(directed = TRUE)

     # Eigenvector centrality
     eigens <- eigen_centrality(
       graph = igraph
       , directed = TRUE
       , scale = TRUE
       , weights = NULL
     )
    
     # Make eigen df
     eigens.df <- data.frame(member_id = names(eigens$vector)
                                  , eigen = unname(eigens$vector))
     
     ######
     #  Calculate Pagerank
     ######
     
     # Pull out nodes
     nodes <- sort(unique(c(congress.netinf.result$origin_node, congress.netinf.result$destination_node)))
     
     
     # Create reversed edgelist

        # Create indexed relationship between destination node and the origin node
        # (in a unique list of node names, if the first nodes is connected to the 20th node, then it would be [1, 20])
        # Edge list of ["origin", "destination"]
     edge_list_REVERSE <- as.matrix(cbind(
       match(congress.netinf.result$destination_node, nodes),
       match(congress.netinf.result$origin_node, nodes)))
     
     # Create "normal" edgelist
     edge_list_NORMAL <- as.matrix(cbind(
       match(congress.netinf.result$origin_node, nodes),
       match(congress.netinf.result$destination_node, nodes)))
     
     # Turn the edgelists back into graphs
     g_REVERSE <- graph_from_edgelist(edge_list_REVERSE)
     g_NORMAL <- graph_from_edgelist(edge_list_NORMAL)
     
     #######
     # Pagerank
     #######
     
     # Calculate pagerank ('normal')
     pagerank_NORMAL <- page_rank(g_NORMAL)$vector
     # Calculate pagerank ('reverse')
     pagerank_REVERSE <- page_rank(g_REVERSE)$vector
     
     
     # Create table

     # DF of nodes and their index
     nodes_withIndex <- data.frame(
       member_id = nodes
       , indx = 1:length(nodes)
     )
     
     # Add pagerank info
     score.df <- cbind(
       nodes_withIndex
       , pagerank_REVERSE
       , pagerank_NORMAL
     ) %>% 
       mutate(pagerank_DIFF = pagerank_REVERSE - pagerank_NORMAL)
     
     # Add eigenvector centrality info
     score.df <- score.df %>% 
       left_join(., eigens.df, by = "member_id")
     
     
     # Add congress
     score.df <-
       score.df %>% 
       mutate(congress = i)
     
     results_list[[length(results_list) + 1]] <- score.df
     
  }
  return(results_list)
}


###
# Apply networks and centrality function - Exponential model
###
set.seed(1776)
score_congressNet.list.R <- estimateNetwork_outputScore.fns(first.obs.R, model = "exponential")
score_congressNet.list.D <- estimateNetwork_outputScore.fns(first.obs.D, model = "exponential")



###
# Flatten to dfs 
###

# Dems
score_congressNet.df.D <- bind_rows(
  score_congressNet.list.D[[1]],
  score_congressNet.list.D[[2]],
  score_congressNet.list.D[[3]],
  score_congressNet.list.D[[4]],
)

# Repubs
score_congressNet.df.R <- bind_rows(
  score_congressNet.list.R[[1]],
  score_congressNet.list.R[[2]],
  score_congressNet.list.R[[3]],
  score_congressNet.list.R[[4]],
)


###################
# Politically Salient - Create scores using only politically-salient topics
# For comparison
###################

###
# Apply networks and centrality function - Politically Salient
###
set.seed(1776)
score_list_salient.R <- estimateNetwork_outputScore.fns(politSalient.R, model = "exponential")
score_list_salient.D <- estimateNetwork_outputScore.fns(politSalient.D, model = "exponential")


###
# Flatten to dfs (politically salient)
###

# Dems
score_salient.df.D <- bind_rows(
  score_list_salient.D[[1]],
  score_list_salient.D[[2]],
  score_list_salient.D[[3]],
  score_list_salient.D[[4]],
)

# Repubs
score_salient.df.R <- bind_rows(
  score_list_salient.R[[1]],
  score_list_salient.R[[2]],
  score_list_salient.R[[3]],
  score_list_salient.R[[4]],
)


######
# Politically salient - comparison
#####

# Pull out relevant columns, rename and bind D and R - salient
score_salient.D <- score_salient.df.D %>%
  select(member_id, pagerank_REVERSE, pagerank_DIFF, congress) %>%
  rename(pagerank_REVERSE_salient = pagerank_REVERSE
         , pagerank_DIFF_salient = pagerank_DIFF) %>%
  mutate(party = "D")

score_salient.R <- score_salient.df.R %>%
  select(member_id, pagerank_REVERSE, pagerank_DIFF, congress) %>%
  rename(pagerank_REVERSE_salient = pagerank_REVERSE
         , pagerank_DIFF_salient = pagerank_DIFF) %>%
  mutate(party = "R")

score_salient.all <- bind_rows(score_salient.D, score_salient.R)

# Pull out relevant columns, rename and bind D and R - salient and non-salient
score_normal.D <- score_congressNet.df.D %>%
  select(member_id, pagerank_REVERSE, pagerank_DIFF, congress) %>%
  rename(pagerank_REVERSE_normal = pagerank_REVERSE
         , pagerank_DIFF_normal = pagerank_DIFF) %>%
  mutate(party = "D")

score_normal.R <- score_congressNet.df.R %>%
  select(member_id, pagerank_REVERSE, pagerank_DIFF, congress) %>%
  rename(pagerank_REVERSE_normal = pagerank_REVERSE
         , pagerank_DIFF_normal = pagerank_DIFF) %>%
  mutate(party = "R")

score_normal.all <- bind_rows(score_normal.D, score_normal.R)

# Join together (salient is smaller b/c some people have no salient press releases)
combined_scores <- left_join(score_normal.all, score_salient.all
                             , by = c("member_id", "congress"))

# Plot correlation
ggplot(combined_scores, aes(x = pagerank_REVERSE_normal, y = pagerank_REVERSE_salient)) +
  geom_point(size = 1, alpha = 0.75) +
  geom_smooth(color = "dark red") +
  theme_minimal() +
  labs(x = "Influence score - All topics", y = "Influence score - Politically salient topics") 
  # ggtitle("Influence Scores are Robust to Changes in Included Topics")

# Caluclate correlation
cor(x = combined_scores$pagerank_REVERSE_normal, y = combined_scores$pagerank_REVERSE_salient, use = "pairwise.complete.obs")



###############################################################
# Adding covariates
###############################################################


###
# Number PRs - Function
# Add in congress covariates; add in number of press releases; join with centrality df
###

numPR.fns <- function(topics.df, first.obs, score_congressNet.df) {
  # Number the PRs (total)
  numPR_total <-
    topics.df %>% 
    group_by(member_id, congress) %>% 
    count() %>% 
    rename(numPR_total = n) 

  
  # Number of PRs (first.obs)
  numPR_firstObs <- 
    first.obs %>% 
    group_by(member_id, congress) %>% 
    count() %>% 
    rename(numPR_firstobs = n)
  
  
  # Merge together
  numPR.df <-
    full_join(numPR_total, numPR_firstObs
              , by = c("member_id", "congress"))
  
  
  # Merge onto score.df
  new.score.df <- 
    left_join(score_congressNet.df, numPR.df
              , by = c("member_id", "congress")) 
  
  return(new.score.df)
}

###
# Apply function - Add on number of PRs variables
###
score_congressNet.df.R <- numPR.fns(topics_df.R, first.obs.R, score_congressNet.df.R)
score_congressNet.df.D <- numPR.fns(topics_df.D, first.obs.D, score_congressNet.df.D)




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


###
# Add ProPublica covariates - Function
###

propubVars.fun <- function(out, leg_covs){
  
  # Pull in ProPublica data from STM
  propubdat <- out$meta
  
  # Pull out relevant vars
  propubdat.relevant <-
    propubdat %>% 
    select(member_id
           , congress
           , first_name
           , last_name
           , middle_name
           , full_name
           , bills_sponsored
           , bills_cosponsored
           , contains("_member")
           , contains("_leader")
           , contains("_taskforces")
           , votes_with_party_pct
           , votes_against_party_pct
           , dw_nominate
           , switched_party
           , party_atPR
           , current_party
           , leadership_role) %>% 
    # dummy for member of any caucus
    mutate(caucus_member = ifelse(rowSums(select(., contains("_member")), na.rm = TRUE) > 0, 1, 0)) %>% 
    rename(leadership_role_propublica = leadership_role)
  

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
   
  return(covariates.df)
}

###
# Apply function
###
covariates.df.R <- propubVars.fun(out.R, leg_covs)
covariates.df.D <- propubVars.fun(out.D, leg_covs)




####
# Merge score.df with covariates.df
####

scoreCongressNets_legCovs.R <-
  left_join(
    score_congressNet.df.R
    , covariates.df.R,
    by = c("member_id", "congress")
  )

scoreCongressNets_legCovs.D <-
  left_join(
    score_congressNet.df.D
    , covariates.df.D
    , by = c("member_id", "congress")
  )



###
# Merge Democrats and Republicans together
###

dat_congressNets <- rbind(scoreCongressNets_legCovs.D, scoreCongressNets_legCovs.R)

#### STOPPED HERE - Jan 31 - making Chapter 2 Appendix

# Save
# saveRDS(dat_congressNets, paste0(getwd(), "/Data/congressNets_immediatelyAfterMerge.RDS"))



################## Ran code through here !!!!################
# - Need to get rid of factors (or at least put them in a separate object)
# - Remove factors(hopefully this doesn't mess up 1/0)
# Double check with RE/FE that it has the same output 


###############################################################
# Clean up variables - scale, center, create dummies
###############################################################

# dat_congressNets <- readRDS(paste0(getwd(), "/Data/congressNets_immediatelyAfterMerge.RDS"))

###
# 1. Member names - make pretty
###
dat_congressNets$bioname <- str_to_title(dat_congressNets$bioname)


###
# 2. Party names - make pretty
###
# Remove party_code b/c it has missing values and no contradictions with party_atPR
# Make new column with long-form name of party

dat_congressNets <-
  dat_congressNets %>% 
  mutate(party_name = ifelse(party_atPR == "R", "Republican",
                             ifelse(party_atPR == "D", "Democrat",
                                    NA))) %>% 
  select(-party_code)

# 3. Create simplified race/ethnicity variable
# To prevent curse of dimensionality (some party-congress combinations don't have all options)
dat_congressNets <- dat_congressNets %>% 
  mutate(race_ethnicity_simplified = 
           ifelse((race_ethnicity == "Asian PI" | race_ethnicity == "Latino" | race_ethnicity == "Native Am")
                  , "Other", race_ethnicity))



###
# 4. Create folded dw-nominate  score (extremism)
###
dat_congressNets <- dat_congressNets %>% 
  mutate(fold_nominate = ifelse(dw_nominate < 0, dw_nominate * -1, dw_nominate))



# Remove already-scaled variables (going to scale myself)
dat_congressNets <-
  dat_congressNets %>% 
  select(!contains("scale"))


###
# 3. Log "count" variables - if relevant
###

# Code to viz and decide which are skewed:
# Get numeric columns
# numeric_cols <- dat_congressNets %>% select(where(is.numeric))

# Plot histograms of all numeric to identify skewed vars
# invisible(lapply(colnames(numeric_cols),function(x){
#   hist(numeric_cols[,x],breaks = 100, main=x,type="l")
# }))

# Names of count vars to be logged
skewed_count_vars <- dat_congressNets %>% 
  select("minority_leslag", "majority_leslag", "benchratiolag"
                       , "leslag", "benchratio", "les", "seniority", "n_speeches_daily"
                       , "votes_against_party_pct", "votes_with_party_pct", "bills_cosponsored"
                       , "bills_sponsored", "pagerank_REVERSE")


# Log skewed vars and add prefix "log_" to the them
dat_congressNets <- dat_congressNets %>%
  mutate(across(names(skewed_count_vars)
                , ~log(.x + 0.001)
                , .names ="{.col}_ln"))



###
# 4. Scale and center continuous variables
###

# Get names of columns with unique count of less than 4 (factors; so they don't get scaled)
factor_col_names <- sapply(dat_congressNets, function(col) length(unique(col)) < 4)

# Updated list of continuous vars (including new logged vars)
numeric_cols <- dat_congressNets %>% 
  select(where(is.numeric)) %>% 
  select(-c(indx, congress, district_code, govtrack_id, speakerid
            , born, died, proximity_to_floor_centroid
            , lag_proximity_to_floor_centroid
            , proximity_to_floor_centroid_zero_code_missingness
            , lag_proximity_to_floor_centroid_zero_code_missingness)) %>% 
  select(-any_of(names(factor_col_names[factor_col_names == TRUE])))



dat_congressNets <- dat_congressNets %>%
  mutate(across(names(numeric_cols)
                , ~scale(.x, center = TRUE, scale = TRUE)
                , .names ="{.col}_scaled"))

dat_congressNets <- 
  dat_congressNets %>%
  mutate(fold_nominate_scaled = scale(fold_nominate, center = TRUE, scale = TRUE))



###
# 7. PageRank - multiply both logged and raw by 10
###
# Transform score to make more readable (multiple by 10)
dat_congressNets <-
  dat_congressNets %>% 
  mutate(revPageRank_10 = pagerank_REVERSE * 10
         , revPageRank_10_ln_scaled = pagerank_REVERSE_ln_scaled * 10)


# Vote pct - NAs were coded as zeros
dat_congressNets <- dat_congressNets %>% 
  mutate(votepct = ifelse(votepct == 0, NA, votepct))

# Vote pct - scale with correct vote pct
dat_congressNets <- dat_congressNets %>% 
  mutate(votepct_scaled = scale(votepct, center = TRUE, scale = TRUE))

# Make caucus_leader variable
dat_congressNets <- dat_congressNets %>% 
  mutate(caucus_leader = ifelse(bdc_leader == 1 |
                                  cpc_leader == 1 |
                                  hfc_leader == 1 |
                                  hlc_leader == 1 |
                                  jd_leader == 1 |
                                  ndc_leader == 1 |
                                  pop_leader == 1 |
                                  psc_leader == 1 |
                                  rmsp_leader == 1 |
                                  rsc_leader == 1 |
                                  tea_leader == 1
                                , 1, 0))


###
# 8. Order column names alphabetically for simplicity
###
dat_congressNets <- dat_congressNets[ , order(colnames(dat_congressNets))]


#######################################################################
# No factors version - Save
dat_congressNets_noFactors <- dat_congressNets
# saveRDS(dat_congressNets_noFactors, paste0(getwd(), "/Data/dat_congressNets_noFactors_21Jan.RDS"))
#######################################################################


###
# 5. Change dummies/categories to factors for regression
###


# Get names of columns with unique count of less than 4
factor_col_names <- sapply(dat_congressNets, function(col) length(unique(col)) < 4)

# Change to factor
dat_congressNets[ ,factor_col_names] <- lapply(dat_congressNets[ , factor_col_names] , factor)


# Yes/No labels - caucus membership
# (lfactor creates factors that can be referenced by either the label or the level)
dat_congressNets <- dat_congressNets %>% 
  mutate(across(c(
    ends_with("_member")
    , ends_with("_taskforce")
    , contains("leader")
  )
  , ~ lfactors::lfactor(.x, levels = 0:1, labels = c("No", "Yes"))))


# Other labels
dat_congressNets <- dat_congressNets %>% 
  mutate(#caucus_member = lfactor(caucus_member, levels = 0:1, labels = c("No", "Yes"))
    party_name = factor(party_atPR, levels = c("D", "R"), labels = c("Democrat", "Republican"))
    , gender = factor(gender, levels = c("man", "woman"), labels = c("Male", "Female"))
    , race_ethnicity_simplified = factor(race_ethnicity_simplified
                                         , levels = c("5", "2", "Other")
                                         , labels = c("White", "Black", "Other"))
    , race_ethnicity = factor(race_ethnicity
                              , levels = c("White", "Black", "Latino", "Asian PI", "Native Am")
                              , labels = c("White", "Black", "Latino", "Asian PI", "Native Am"))
  )




# saveRDS(dat_congressNets, paste0(getwd(), "/Data/dat_congressNets_21Jan"))

############# END - Clean Variables #################################





##############################################################################
##############################################################################
# Hypothesis Tests
##############################################################################
##############################################################################

# Load cleaned data (with factors)
dat_congressNets <- readRDS(paste0(getwd(), "/Data/dat_congressNets_21Jan"))
# Load cleaned data without factors
dat_congressNets_noFactors <- readRDS(paste0(getwd(), "/Data/dat_congressNets_noFactors_21Jan.RDS"))



# Rename DV
dat_congressNets <-
  dat_congressNets %>% 
  mutate(pagerank10 = pagerank_REVERSE_ln_scaled * 10)
  


###
# Create data
###

# OLS data
dat.ols <- dat_congressNets

# Panel data
dat.plm <- pdata.frame(
  dat_congressNets
  , index = c("member_id", "congress")
  , drop.index = TRUE)


###
# Hypotheses
###
# 1. Party leader hypothesis
# 2. Extremists hypothesis
# 3. Expertise hypothesis
#     3a. Committee leader
#     3b. LES
#     3c. Caucus leader
# 4. Moderates hypothesis
# 5. Factions



###
# Details
###

# b. OLS models
  # i. No controls
  # ii. Controls group
  # iii. Controls group + PR count
# c. Fixed effects models
  # i. Congress fixed effects
    # i. No controls
   # ii. Controls group
    # iii. Controls group + PR count
  # ii. Member fixed effects
    # i. No controls
    # ii. Controls group
    # iii. Controls group + PR count
  # iii. Party fixed effects
    # i. No controls
    # ii. Controls group
    # iii. Controls group + PR count
# d. Random effects model
  # i. Member random effects
    # i. No controls
    # ii. Controls group
    # iii. Controls group + PR count
# e. Sub-group models
  # i. Democrats
    # i. TBD
  # ii. Republicans
    # TBD





###########################################################
# OLS Models 
###########################################################

###
# 1. OLS - No Controls
###
ols.none <- lm(pagerank10 ~
                 party_leadership
               + committee_leadership
               + caucus_leader
               + seniority_ln_scaled
               + les_ln_scaled
               + fold_nominate_scaled
               
               + majority_party
               + party_name
               , data = dat.ols)

###
# 2. OLS - Controls Group
###
ols.controls <- lm(pagerank10 ~
                     party_leadership
                   + committee_leadership
                   + caucus_leader
                   + seniority_ln_scaled
                   + les_ln_scaled
                   + fold_nominate_scaled
                   
                   + majority_party
                   + party_name
                   
                   # Controls group
                   + votepct_scaled
                   + race_ethnicity
                   + gender
                   + bills_cosponsored_ln_scaled
                   + votes_with_party_pct_ln_scaled
                   
                   , data = dat.ols)



###
# 3. OLS - Controls group + PR count
###
ols.controlsExtra <- lm(pagerank10 ~
                          party_leadership
                        + committee_leadership
                        + caucus_leader
                        + seniority_ln_scaled
                        + les_ln_scaled
                        + fold_nominate_scaled
                        
                        + majority_party
                        + party_name
                        
                        # Controls group
                        + votepct_scaled
                        + race_ethnicity
                        + gender
                        + bills_cosponsored_ln_scaled
                        + votes_with_party_pct_ln_scaled
                        
                        # Extra control
                        + numPR_total_scaled
                        
                        , data = dat.ols)


###########################################################
# Fixed Effects Models 
###########################################################

#####################
# Fixed Effects - Time
####################

###
# 1. FE Time - No Controls
###
timeFE.none <- plm(pagerank10 ~
                     party_leadership
                   + committee_leadership
                   + caucus_leader
                   + seniority_ln_scaled
                   + les_ln_scaled
                   + fold_nominate_scaled
                   
                   + majority_party
                   + party_name
                   
                   , data = dat.plm
                   , index = c("member_id", "congress")
                   , model = "within"  
                   , effect = "time")


###
# 2. FE Time - Controls Group
###
timeFE.controls <- plm(pagerank10 ~
                         party_leadership
                       + committee_leadership
                       + caucus_leader
                       + seniority_ln_scaled
                       + les_ln_scaled
                       + fold_nominate_scaled
                       
                       + majority_party
                       + party_name
                       
                       # Controls group
                       + votepct_scaled
                       + race_ethnicity
                       + gender
                       + bills_cosponsored_ln_scaled
                       + votes_with_party_pct_ln_scaled
                       
                       , data = dat.plm
                       , index = c("member_id", "congress")
                       , model = "within"  
                       , effect = "time")


###
# 3. FE Time - Controls group + PR count
###
timeFE.controlsExtra <- plm(pagerank10 ~
                              party_leadership
                            + committee_leadership
                            + caucus_leader
                            + seniority_ln_scaled
                            + les_ln_scaled
                            + fold_nominate_scaled
                            
                            + majority_party
                            + party_name
                            
                            # Controls group
                            + votepct_scaled
                            + race_ethnicity
                            + gender
                            + bills_cosponsored_ln_scaled
                            + votes_with_party_pct_ln_scaled
                            
                            # Extra control
                            + numPR_total_scaled
                            
                            , data = dat.plm
                            , index = c("member_id", "congress")
                            , model = "within"  
                            , effect = "time")

#####################
# Fixed Effects - Individual 
#####################

###
# 1. FE Indiv - No Controls
###
memberFE.none <- plm(pagerank10 ~
                      party_leadership
                    + committee_leadership
                    + caucus_leader
                    + seniority_ln_scaled
                    + les_ln_scaled
                    + fold_nominate_scaled
                    
                    + majority_party
                    + party_name
                    
                    , data = dat.plm
                    , index = c("member_id", "congress")
                    , model = "within"  
                    , effect = "individual")


###
# 2. FE Indiv - Controls Group
###
memberFE.controls <- plm(pagerank10 ~
                           party_leadership
                         + committee_leadership
                         + caucus_leader
                         + seniority_ln_scaled
                         + les_ln_scaled
                         + fold_nominate_scaled
                         
                         + majority_party
                         + party_name
                         
                         # Controls group
                         + votepct_scaled
                         + race_ethnicity
                         + gender
                         + bills_cosponsored_ln_scaled
                         + votes_with_party_pct_ln_scaled
                         
                         , data = dat.plm
                         , index = c("member_id", "congress")
                         , model = "within"  
                         , effect = "individual")


###
# 3. FE Indiv - Controls group + PR count
###
memberFE.controlsExtra <- plm(pagerank10 ~
                                party_leadership
                              + committee_leadership
                              + caucus_leader
                              + seniority_ln_scaled
                              + les_ln_scaled
                              + fold_nominate_scaled
                              
                              + majority_party
                              + party_name
                              
                              # Controls group
                              + votepct_scaled
                              + race_ethnicity
                              + gender
                              + bills_cosponsored_ln_scaled
                              + votes_with_party_pct_ln_scaled
                              
                              # Extra control
                              + numPR_total_scaled
                              
                              , data = dat.plm
                              , index = c("member_id", "congress")
                              , model = "within"  
                              , effect = "individual")

#####################
# Fixed Effects - Party
#####################

###
# 1. Fixed Party - No Controls
###
partyFE.none <- lm(pagerank10 ~
                      party_leadership
                    + committee_leadership
                    + caucus_leader
                    + seniority_ln_scaled
                    + les_ln_scaled
                    + fold_nominate_scaled
                    + majority_party
                      
                      # Intercept
                    + factor(party_name)
                    
                    , data = dat.ols)


###
# 2. Fixed Party - Controls Group
###
partyFE.controls <- lm(pagerank10 ~
                         party_leadership
                       + committee_leadership
                       + caucus_leader
                       + seniority_ln_scaled
                       + les_ln_scaled
                       + fold_nominate_scaled
                       + majority_party
                       
                       # Controls group
                       + votepct_scaled
                       + race_ethnicity
                       + gender
                       + bills_cosponsored_ln_scaled
                       + votes_with_party_pct_ln_scaled
                       
                       # Intercept
                       + factor(party_name)
                       
                       , data = dat.ols)


###
# 3. Fixed Party - Controls group + PR count
###
partyFE.controlsExtra <- lm(pagerank10 ~
                              party_leadership
                            + committee_leadership
                            + caucus_leader
                            + seniority_ln_scaled
                            + les_ln_scaled
                            + fold_nominate_scaled
                            + majority_party
                            
                            # Controls group
                            + votepct_scaled
                            + race_ethnicity
                            + gender
                            + bills_cosponsored_ln_scaled
                            + votes_with_party_pct_ln_scaled
                            
                            # Extra control
                            + numPR_total_scaled
                            
                            # Intercept
                            + factor(party_name)
                            
                            , data = dat.ols)




###########################################################
# Random Effects Models 
###########################################################

###
# 1. Random Indiv - No Controls
###
memberRE.none <- plm(pagerank10 ~
                       party_leadership
                     + committee_leadership
                     + caucus_leader
                     + seniority_ln_scaled
                     + les_ln_scaled
                     + fold_nominate_scaled
                     
                     + majority_party
                     + party_name
                     
                     , data = dat.plm
                     , index = c("member_id", "congress")
                     , model = "random"  
                     , effect = "individual")


###
# 2. Random Indiv - Controls Group
###
memberRE.controls <- plm(pagerank10 ~
                           party_leadership
                         + committee_leadership
                         + caucus_leader
                         + seniority_ln_scaled
                         + les_ln_scaled
                         + fold_nominate_scaled
                         
                         + majority_party
                         + party_name
                         
                         # Controls group
                         + votepct_scaled
                         + race_ethnicity
                         + gender
                         + bills_cosponsored_ln_scaled
                         + votes_with_party_pct_ln_scaled
                         
                         , data = dat.plm
                         , index = c("member_id", "congress")
                         , model = "random"  
                         , effect = "individual")

###
# 3. Random Indiv - Controls group + PR count
###
memberRE.controlsExtra <- plm(pagerank10 ~
                                party_leadership
                              + committee_leadership
                              + caucus_leader
                              + seniority_ln_scaled
                              + les_ln_scaled
                              + fold_nominate_scaled
                              
                              + majority_party
                              + party_name
                              
                              # Controls group
                              + votepct_scaled
                              + race_ethnicity
                              + gender
                              + bills_cosponsored_ln_scaled
                              + votes_with_party_pct_ln_scaled
                              
                              # Extra control
                              + numPR_total_scaled
                              
                              , data = dat.plm
                              , index = c("member_id", "congress")
                              , model = "random"  
                              , effect = "individual")


###
# List of models
###
modelList <- list(
  ols_models = list(ols.none = ols.none, ols.controls = ols.controls, ols.controlsExtra = ols.controlsExtra)
  , timeFE_models = list(timeFE.none = timeFE.none, timeFE.controls = timeFE.controls, timeFE.controlsExtra = timeFE.controlsExtra)
  , memberFE_models = list(memberFE.none = memberFE.none, memberFE.controls = memberFE.controls, memberFE.controlsExtra = memberFE.controlsExtra)
  , partyFE_models = list(partyFE.none = partyFE.none, partyFE.controls = partyFE.controls, partyFE.controlsExtra = partyFE.controlsExtra)
  , memberRE_models = list(memberRE.none = memberRE.none, memberRE.controls = memberRE.controls, memberRE.controlsExtra = memberRE.controlsExtra)
)


##########################################################################
##########################################################################
# Grand Latex Tables - Appendix Chapter 3 (all models)
##########################################################################
##########################################################################

#############################################################
# OLS Models - Latex Tables
#############################################################

texreg(l = list(modelList$ols_models$ols.none, modelList$ols_models$ols.controls, modelList$ols_models$ols.controlsExtra)
       
       , custom.model.names = c("Model 1", "Model 2", "Model 3"),
       custom.coef.map = list(
         "party_leadershipYes" = "Party leadership"
         , "committee_leadershipYes" = "Committee leadership"
         , "caucus_leaderYes" = "Caucus leader"
         , "seniority_ln_scaled" = "log(Seniority)"
         , "les_ln_scaled" = "log(LES)"                    
         , "fold_nominate_scaled" = "Extremism"    
         , "majority_party1" = "Majority party"
         , "party_nameRepublican" = "Republican" 
         
         , "votepct_scaled" = "Win vote pct."        
         , "race_ethnicityBlack" = "Black"      
         , "race_ethnicityLatino" = "Latino"
         , "race_ethnicityAsian PI" = "Asian/PI"
         , "race_ethnicityNative Am" = "Native Am."
         , "genderFemale" = "Female"        
         , "bills_cosponsored_ln_scaled"  = "log(Bills cosponsored)"   
         , "votes_with_party_pct_ln_scaled" = "log(Votes w/ party pct.)"
         
         , "numPR_total_scaled" = "Total press releases"   
         
       )
       , groups = list("\\textbf{Member characteristics}" = 1:8, "\\textbf{Controls}" = 9:16, "\\textbf{Extra Control}" = 17)
       , bold = 0.05
       , stars = numeric(0)
       , scalebox = 1.0
       , include.rsquared = TRUE
       , include.adjrs = FALSE
       , caption = "OLS Models Accounting for Member Influence"
       , label = "tab:appxOLS"
       , float.pos = "h"
)

#############################################################
# Fixed Effects - Latex Tables
#############################################################

##########
# Time FE - Latex
#########

texreg(l = list(modelList$timeFE_models$timeFE.none, modelList$timeFE_models$timeFE.controls, modelList$timeFE_models$timeFE.controlsExtra)
       
       , custom.model.names = c("Model 1", "Model 2", "Model 3"),
       custom.coef.map = list(
         "party_leadershipYes" = "Party leadership"
         , "committee_leadershipYes" = "Committee leadership"
         , "caucus_leaderYes" = "Caucus leader"
         , "seniority_ln_scaled" = "log(Seniority)"
         , "les_ln_scaled" = "log(LES)"                    
         , "fold_nominate_scaled" = "Extremism"    
         , "majority_party1" = "Majority party"
         , "party_nameRepublican" = "Republican" 
         
         , "votepct_scaled" = "Win vote pct."        
         , "race_ethnicityBlack" = "Black"      
         , "race_ethnicityLatino" = "Latino"
         , "race_ethnicityAsian PI" = "Asian/PI"
         , "race_ethnicityNative Am" = "Native Am."
         , "genderFemale" = "Female"        
         , "bills_cosponsored_ln_scaled"  = "log(Bills cosponsored)"   
         , "votes_with_party_pct_ln_scaled" = "log(Votes w/ party pct.)"
         
         , "numPR_total_scaled" = "Total press releases"   
         
       )
       , groups = list("\\textbf{Member characteristics}" = 1:8, "\\textbf{Controls}" = 9:16, "\\textbf{Extra Control}" = 17)
       , bold = 0.05
       , stars = numeric(0)
       , scalebox = 1.0
       , include.rsquared = TRUE
       , include.adjrs = FALSE
       , caption = "Time Fixed Effects Models Accounting for Member Influence"
       , label = "tab:appxTimeFE"
       , float.pos = "h"
)

##########
# Member FE - Latex
#########

texreg(l = list(modelList$memberFE_models$memberFE.none, modelList$memberFE_models$memberFE.controls, modelList$memberFE_models$memberFE.controlsExtra)
       
       , custom.model.names = c("Model 1", "Model 2", "Model 3"),
       custom.coef.map = list(
         "party_leadershipYes" = "Party leadership"
         , "committee_leadershipYes" = "Committee leadership"
         , "caucus_leaderYes" = "Caucus leader"
         , "seniority_ln_scaled" = "log(Seniority)"
         , "les_ln_scaled" = "log(LES)"                    
         , "fold_nominate_scaled" = "Extremism"
         , "majority_party1" = "Majority party"
         # , "party_nameRepublican" = "Republican" 
         
         , "votepct_scaled" = "Win vote pct."        
         # , "race_ethnicityBlack" = "Black"      
         # , "race_ethnicityLatino" = "Latino"
         # , "race_ethnicityAsian PI" = "Asian/PI"
         # , "race_ethnicityNative Am" = "Native Am."
         # , "genderFemale" = "Female"        
         , "bills_cosponsored_ln_scaled"  = "log(Bills cosponsored)"   
         , "votes_with_party_pct_ln_scaled" = "log(Votes w/ party pct.)"
         
         , "numPR_total_scaled" = "Total press releases"   
         
       )
       , groups = list("\\textbf{Member characteristics}" = 1:7, "\\textbf{Controls}" = 8:10, "\\textbf{Extra Control}" = 11)
       , bold = 0.05
       , stars = numeric(0)
       , scalebox = 1.0
       , include.rsquared = TRUE
       , include.adjrs = FALSE
       , caption = "Individual Fixed Effects Models Accounting for Member Influence"
       , label = "tab:appxMemberFE"
       , float.pos = "h"
)



##########
# Party FE - Latex
#########

texreg(l = list(modelList$partyFE_models$partyFE.none, modelList$partyFE_models$partyFE.controls, modelList$partyFE_models$partyFE.controlsExtra)
       
       , custom.model.names = c("Model 1", "Model 2", "Model 3"),
       custom.coef.map = list(
         "party_leadershipYes" = "Party leadership"
         , "committee_leadershipYes" = "Committee leadership"
         , "caucus_leaderYes" = "Caucus leader"
         , "seniority_ln_scaled" = "log(Seniority)"
         , "les_ln_scaled" = "log(LES)"                    
         , "fold_nominate_scaled" = "Extremism"    
         , "majority_party1" = "Majority party"
         # , "party_nameRepublican" = "Republican" 
         
         , "votepct_scaled" = "Win vote pct."        
         , "race_ethnicityBlack" = "Black"      
         , "race_ethnicityLatino" = "Latino"
         , "race_ethnicityAsian PI" = "Asian/PI"
         , "race_ethnicityNative Am" = "Native Am."
         , "genderFemale" = "Female"        
         , "bills_cosponsored_ln_scaled"  = "log(Bills cosponsored)"   
         , "votes_with_party_pct_ln_scaled" = "log(Votes w/ party pct.)"
         
         , "numPR_total_scaled" = "Total press releases"   
         
       )
       , groups = list("\\textbf{Member characteristics}" = 1:7, "\\textbf{Controls}" = 8:15, "\\textbf{Extra Control}" = 16)
       , bold = 0.05
       , stars = numeric(0)
       , scalebox = 1.0
       , include.rsquared = TRUE
       , include.adjrs = FALSE
       , caption = "Party Fixed Effects Models Accounting for Member Influence"
       , label = "tab:appxPartyFE"
       , float.pos = "h"
)



#############################################################
# Random Effects - Latex Table
#############################################################

texreg(l = list(modelList$memberRE_models$memberRE.none, modelList$memberRE_models$memberRE.controls, modelList$memberRE_models$memberRE.controlsExtra)
       
       , custom.model.names = c("Model 1", "Model 2", "Model 3"),
       custom.coef.map = list(
         "party_leadershipYes" = "Party leadership"
         , "committee_leadershipYes" = "Committee leadership"
         , "caucus_leaderYes" = "Caucus leader"
         , "seniority_ln_scaled" = "log(Seniority)"
         , "les_ln_scaled" = "log(LES)"                    
         , "fold_nominate_scaled" = "Extremism"    
         , "majority_party1" = "Majority party"
         , "party_nameRepublican" = "Republican" 
         
         , "votepct_scaled" = "Win vote pct."        
         , "race_ethnicityBlack" = "Black"      
         , "race_ethnicityLatino" = "Latino"
         , "race_ethnicityAsian PI" = "Asian/PI"
         , "race_ethnicityNative Am" = "Native Am."
         , "genderFemale" = "Female"        
         , "bills_cosponsored_ln_scaled"  = "log(Bills cosponsored)"   
         , "votes_with_party_pct_ln_scaled" = "log(Votes w/ party pct.)"
         
         , "numPR_total_scaled" = "Total press releases"   
         
       )
       , groups = list("\\textbf{Member characteristics}" = 1:8, "\\textbf{Controls}" = 9:16, "\\textbf{Extra Control}" = 17)
       , bold = 0.05
       , stars = numeric(0)
       , scalebox = 1.0
       , include.rsquared = TRUE
       , include.adjrs = FALSE
       , caption = "Individual Random Effects Models Accounting for Member Influence"
       , label = "tab:appxRE"
       , float.pos = "h"
)




#############################################################
#############################################################
# Main models - latex table
# Selection of illustrative models 
#############################################################
#############################################################


texreg(l = list(
  modelList$ols_models$ols.none
  , modelList$memberFE_models$memberFE.controls
  , modelList$partyFE_models$partyFE.controls
  , modelList$ols_models$ols.controlsExtra)
  , custom.model.names = c("Model 1", "Model 2", "Model 3", "Model 4")
  , custom.coef.map = list(
         "party_leadershipYes" = "Party leadership"
         , "committee_leadershipYes" = "Committee leadership"
         , "caucus_leaderYes" = "Caucus leader"
         , "seniority_ln_scaled" = "log(Seniority)"
         , "les_ln_scaled" = "log(LES)"                    
         , "fold_nominate_scaled" = "Extremism"    
         , "majority_party1" = "Majority party"
         , "party_nameRepublican" = "Republican" 
         
         , "votepct_scaled" = "Win vote pct."        
         , "race_ethnicityBlack" = "Black"      
         , "race_ethnicityLatino" = "Latino"
         , "race_ethnicityAsian PI" = "Asian/PI"
         , "race_ethnicityNative Am" = "Native Am."
         , "genderFemale" = "Female"        
         , "bills_cosponsored_ln_scaled"  = "log(Bills cosponsored)"   
         , "votes_with_party_pct_ln_scaled" = "log(Votes w/ party pct.)"
         
         , "numPR_total_scaled" = "Total press releases")
       , groups = list("\\textbf{Member characteristics}" = 1:8, "\\textbf{Controls}" = 9:16, "\\textbf{Extra Control}" = 17)
       , bold = 0.05
       , stars = numeric(0)
       , scalebox = 1.0
       , include.rsquared = TRUE
       , include.adjrs = FALSE
       , custom.gof.row = list(
         "Fixed effects" = c("\\textit{No}", "\\textit{Indiv.}", "\\textit{Party}", "\\textit{No}")
       )
       , caption = "Accounting for Member Influence"
       , custom.note = "\\texit{Note:} Coefficient estimates are bold at the 0.05 significance level."
       , label = "tab:mainmodels"
       , float.pos = "h"
)












#############################################################

#############################################################
# Main Models - by Majority/Minority status
#############################################################

###
# Make main models - Maj/Min
###

# OLS - Majority - No controls
ols.none.MAJ <- lm(pagerank10 ~
                 party_leadership
               + committee_leadership
               + caucus_leader
               + seniority_ln_scaled
               + les_ln_scaled
               + fold_nominate_scaled
               # + party_name
               
               , data = filter(dat.ols, majority_party == 1))

# OLS - Minority - No controls
ols.none.MIN <- lm(pagerank10 ~
                     party_leadership
                   + committee_leadership
                   + caucus_leader
                   + seniority_ln_scaled
                   + les_ln_scaled
                   + fold_nominate_scaled
                   # + party_name
                   
                   , data = filter(dat.ols, majority_party == 0))



# OLS - Majority - Controls Extra
ols.controlsExtra.MAJ <- lm(pagerank10 ~
                          party_leadership
                        + committee_leadership
                        + caucus_leader
                        + seniority_ln_scaled
                        + les_ln_scaled
                        + fold_nominate_scaled
                        # + party_name
                        
                        # Controls group
                        + votepct_scaled
                        + race_ethnicity
                        + gender
                        + bills_cosponsored_ln_scaled
                        + votes_with_party_pct_ln_scaled
                        
                        # Extra control
                        + numPR_total_scaled
                        
                        , data = filter(dat.ols, majority_party == 1))


# OLS - Minority - Controls Extra
ols.controlsExtra.MIN <- lm(pagerank10 ~
                              party_leadership
                            + committee_leadership
                            + caucus_leader
                            + seniority_ln_scaled
                            + les_ln_scaled
                            + fold_nominate_scaled
                            # + party_name
                            
                            # Controls group
                            + votepct_scaled
                            + race_ethnicity
                            + gender
                            + bills_cosponsored_ln_scaled
                            + votes_with_party_pct_ln_scaled
                            
                            # Extra control
                            + numPR_total_scaled
                            
                            , data = filter(dat.ols, majority_party == 0))





texreg(l = list(
  ols.none.MAJ, ols.controlsExtra.MAJ
  , ols.none.MIN, ols.controlsExtra.MIN
  )
  , custom.model.names = c("Model 1", "Model 2", "Model 1", "Model 2")
  , custom.header = list("Majority Party" = 1:2, "Minority Party" = 3:4)
  , custom.coef.map = list(
    "party_leadershipYes" = "Party leadership"
    , "committee_leadershipYes" = "Committee leadership"
    , "caucus_leaderYes" = "Caucus leader"
    , "seniority_ln_scaled" = "log(Seniority)"
    , "les_ln_scaled" = "log(LES)"                    
    , "fold_nominate_scaled" = "Extremism"    
    # , "majority_party1" = "Majority party"
    # , "party_nameRepublican" = "Republican" 
    
    # , "votepct_scaled" = "Win vote pct."        
    # , "race_ethnicityBlack" = "Black"      
    # , "race_ethnicityLatino" = "Latino"
    # , "race_ethnicityAsian PI" = "Asian/PI"
    # , "race_ethnicityNative Am" = "Native Am."
    # , "genderFemale" = "Female"        
    # , "bills_cosponsored_ln_scaled"  = "log(Bills cosponsored)"   
    # , "votes_with_party_pct_ln_scaled" = "log(Votes w/ party pct.)"
    # 
    # , "numPR_total_scaled" = "Total press releases"
    )
  # , groups = list("\\textbf{Member characteristics}" = 1:6, "\\textbf{Controls}" = 7:14, "\\textbf{Extra Control}" = 15)
  , bold = 0.05
  , stars = numeric(0)
  # , scalebox = 1.0
  , custom.gof.rows = list(
    "Controls" = c("No", "Yes", "No", "Yes")
    , "Num. Democrats" = c(169, 160, 493, 456)
    , "Num. Republicans" = c(602, 559, 158, 156)
  )
  , include.rsquared = TRUE
  , include.adjrs = FALSE
  , reorder.gof = c(1, 5, 2, 3, 4)
  , caption = "Accounting for Member Influence Conditional on Majority Party Status"
  , custom.note = "\\texit{Note:} Coefficient estimates are bold at the 0.05 significance level."
  , label = "tab:majoritypartymodels"
  , float.pos = "h"
)




#############################################################
# Coefficient Visualizations - Hypothesis tests
# Plotting main variable coefficients, from all models
#############################################################
#############################################################

###
# Hypotheses
###
# 1. Party leader hypothesis
# 2. Extremists hypothesis
# 3. Expertise hypothesis
#     3a. Committee leader
#     3b. LES
#     3c. Caucus leader
# 4. Moderates hypothesis
# 5. Factions




####
# Party Leader
####

# Name the models
models.all <- list(
    "OLS" = ols.none
  , "OLS + Controls" = ols.controls
  , "OLS + Extra controls" = ols.controlsExtra
  , "Time FE" = timeFE.none
  , "Time FE + Controls" = timeFE.controls
  , "Time FE + Extra controls" = timeFE.controlsExtra
  , "Indiv. FE" = memberFE.none
  , "Indiv. FE + Controls" = memberFE.controls
  , "Indiv. FE + Extra controls" = memberFE.controlsExtra
  , "Party FE" = partyFE.none
  , "Party FE + Controls" = partyFE.controls
  , "Party FE + Extra controls" = partyFE.controlsExtra
  , "Indiv. RE" = memberRE.none
  , "Indiv. RE + Controls" = memberRE.controls
  , "Indiv. RE + Extra controls" = memberRE.controlsExtra
)


###
# Party leadership
###
# Map to a figure
fig.partyLeadership <- modelsummary::modelplot(rev(models.all), coef_map = c("party_leadershipYes" = ""))

# Make pretty
pdf("/Users/mackenzieweiler/Library/CloudStorage/OneDrive-TheOhioStateUniversity/Dissertation/ch3/coefplot_partyLeadership.pdf")
fig.partyLeadership +
  ggtitle("Party Leadership Association with Influence") +
  geom_vline(xintercept = 0) +
  guides(color = guide_legend(reverse = TRUE)) +
  scale_color_manual(values = unname(pals::alphabet(n = 15)))
dev.off()


###
# Seniority
###
fig.seniority <- modelsummary::modelplot(rev(models.all), coef_map = c("seniority_ln_scaled" = ""))

# Make pretty
pdf("/Users/mackenzieweiler/Library/CloudStorage/OneDrive-TheOhioStateUniversity/Dissertation/ch3/coefplot_seniority.pdf")
fig.seniority +
  ggtitle("Association between Seniority and Influence") +
  geom_vline(xintercept = 0) +
  guides(color = guide_legend(reverse = TRUE)) +
  scale_color_manual(values = unname(pals::alphabet(n = 15)))
dev.off()


###
# Extremism
###

# Map to a figure
fig.extremism <- modelsummary::modelplot(rev(models.all), coef_map = c("fold_nominate_scaled" = ""))

# Make pretty
pdf("/Users/mackenzieweiler/Library/CloudStorage/OneDrive-TheOhioStateUniversity/Dissertation/ch3/coefplot_extremism.pdf")
fig.extremism +
  ggtitle("Association between Extremism and Influence") +
  geom_vline(xintercept = 0) +
  guides(color = guide_legend(reverse = TRUE)) +
  scale_color_manual(values = unname(pals::alphabet(n = 15)))
dev.off()


##########
# Expertise - multiple variables
#########


# Expertise 1: Committee Leadership

fig.committeeLeadership <- modelsummary::modelplot(rev(models.all), coef_map = c("committee_leadershipYes" = ""))

pdf("/Users/mackenzieweiler/Library/CloudStorage/OneDrive-TheOhioStateUniversity/Dissertation/ch3/coefplot_committeeLeadership.pdf")
fig.committeeLeadership +
  ggtitle("Committee Leadership Association with Influence") +
  geom_vline(xintercept = 0) +
  guides(color = guide_legend(reverse = TRUE)) +
  scale_color_manual(values = unname(pals::alphabet(n = 15)))
dev.off()

# Expertise 2: LES
fig.LES <- modelsummary::modelplot(rev(models.all), coef_map = c("les_ln_scaled" = ""))

pdf("/Users/mackenzieweiler/Library/CloudStorage/OneDrive-TheOhioStateUniversity/Dissertation/ch3/coefplot_LES.pdf")
fig.LES +
  ggtitle("Association between Legislative Effectiveness and Influence") +
  geom_vline(xintercept = 0) +
  guides(color = guide_legend(reverse = TRUE)) +
  scale_color_manual(values = unname(pals::alphabet(n = 15)))
dev.off()

# Expertise 3: Caucus leadership
fig.caucusLeader <- modelsummary::modelplot(rev(models.all), coef_map = c("caucus_leaderYes" = ""))

pdf("/Users/mackenzieweiler/Library/CloudStorage/OneDrive-TheOhioStateUniversity/Dissertation/ch3/coefplot_caucusLeader.pdf")
fig.caucusLeader +
  ggtitle("Caucus Leadership Association with Influence") +
  geom_vline(xintercept = 0) +
  guides(color = guide_legend(reverse = TRUE)) +
  scale_color_manual(values = unname(pals::alphabet(n = 15)))
dev.off()



fig.majority <- modelsummary::modelplot(rev(models.all), coef_map = c("majority_party1" = ""))
fig.majority +
  ggtitle("Majority Party Association with Influence") +
  geom_vline(xintercept = 0) +
  guides(color = guide_legend(reverse = TRUE)) +
  scale_color_manual(values = unname(pals::alphabet(n = 15)))








##############################################################
# Appendix Chapter 3: Summary Tables (maybe for body)
# Add column for # of people who changed status


########
# Detect number of people who changed status (e.g. did they go from committee -> not)
########
changeStatus.fns <- function(dat, x){
  out <- dat %>%
    group_by(member_id) %>% 
    mutate(change = ifelse(n_distinct(.$x, na.rm = TRUE) == 1, 0, 1)) %>% 
    select(change)
  return(out)
} 


leads <- dat_congressNets %>% 
  filter(party_leadership == "Yes") %>% 
  select(member_id) %>% 
  distinct() %>% 
  pull()

change.df <- 
  dat_congressNets %>% 
  filter(member_id %in% leads) %>% 
  group_by(member_id) %>% 
  mutate(change = ifelse(n_distinct(party_leadership, na.rm = TRUE) == 1, "noChange", "change")) %>% 
  ungroup() %>% 
  select(change)


ggplot(dat_congressNets) +
  geom_density(aes(fold_nominate_scaled, fill = party_name), alpha = 0.6)




###############################################################################
###############################################################################
# Chapter 2 - FREX stems tables
###############################################################################
###############################################################################


# # Create tables of topic names and labels
# 
# topicNames_tab.R <-
#   first.obs.R %>% 
#   select(topic_label, topicName, salient) %>% 
#   unique() %>% 
#   arrange(desc(salient))
# 
# topicNames_tab.D <-
#   first.obs.D %>% 
#   select(topic_label, topicName, salient) %>% 
#   unique() %>% 
#   arrange(desc(salient))
# 
# # Specify non-politically relevant rows
# gray_rows.R <- which(topicNames_tab.R$salient == 0)
# gray_rows.D <- which(topicNames_tab.D$salient == 0)
# 
# kable(select(topicNames_tab.R, -salient)
#       , booktabs = TRUE
#       , format = 'latex'
#       # , row.names = NULL
#       , col.names = c('FREX stems', 'Topic')
#       , caption = "Republican press release topics"
#       , label = "tab:topicNames.R"
#       , ) %>% 
#   kable_styling(latex_options = 'scale_down') %>%
#   row_spec(gray_rows.R, color = "gray") %>% 
#   footnote("Press releases clustered into 30 topics. FREX stems are the top words in 
#   each topic cluster that are both frequent and exclusive and are best able to 
#   distinguish the topic.  Topics in gray represent those that may not be 
#   considered politically relevant.")
# 
# kable(select(topicNames_tab.D, -salient)
#       , booktabs = TRUE
#       , format = 'latex'
#       # , row.names = NULL
#       , col.names = c('FREX stems', 'Topic')
#       , caption = "Democratic press release topics"
#       , label = "tab:topicNames.D"
#       , ) %>% 
#   kable_styling(latex_options = 'scale_down') %>%
#   row_spec(gray_rows.D, color = "gray") %>% 
#   footnote("Democratic press releases clustered into 30 topics. FREX stems are the top stems in 
#   each topic cluster that are both frequent and exclusive and are best able to 
#   distinguish the topic.  Topics in gray represent those that may not be 
#   considered politically relevant.")




####################################################
# DESCRIPTIVE STATS - Appendix
####################################################

# # Distribution of pagerank scores
# ggpubr::gghistogram(
#   dat_congressNets
#   , x = "pagerank_REVERSE"
#   , bins = 100
#   , color = "#0073C2FF"
#     , fill = "#0073C2FF"
#     , add = "mean"
#   , rug = TRUE
#   , xlab = "Influence Score"
#     , ylab = ""
#     , title = "Raw Distribution of PageRank Influence Scores"
#   )
# 
# # Distribution of logged, scaled, pagerank scores
# ggplot(dat_congressNets) +
#   geom_boxplot(aes(revPageRank_1000_ln_scaled)) +
#   theme_minimal() +
#   labs(x = "PageRank influence score") +
#   ggtitle("Distribution of Influence Scores")



##########################################################################
# Chapter 2: Table of influence scores - most and least
##########################################################################

# Create table data
dat_congressNets %>% 
  select(full_name, congress, party_atPR, pagerank_REVERSE) %>% 
  arrange(desc(pagerank_REVERSE)) %>% 
  write.csv("InfluenceScores.csv")




#######
# Psuedo-correlation test:
# Are the people who are most influential in the 113th congress, also influential in successive congresses?
######

# People who are in all 4 congresses
multicongressReps <- 
  dat_congressNets %>% 
  count(member_id) %>% 
  filter(n == 4) %>% 
  rename(n_congress = n)


# People who are in all 4 congresses who are above the median in 113th congress
majority_113 <-
  dat_congressNets %>% 
  filter(member_id %in% multicongressReps$member_id) %>% 
  filter(congress == 113) %>% 
  group_by(party_atPR) %>% 
  filter(pagerank_REVERSE > median(pagerank_REVERSE)) %>% 
  select(member_id, full_name, pagerank_REVERSE) %>% 
  ungroup()

# People in 4 congresses who are above the median in a given congress
majority_median <-
  dat_congressNets %>% 
  filter(member_id %in% multicongressReps$member_id) %>% 
  group_by(party_atPR, congress) %>% 
  filter(pagerank_REVERSE > median(pagerank_REVERSE)) %>% 
  select(member_id, full_name, congress, pagerank_REVERSE) %>% 
  ungroup() %>% 
  count(member_id) %>% 
  rename(n_median = n)

# Merge
majority_median_merge<- left_join(majority_113, majority_median, by = "member_id")

# How many other congresses were they above the majority in?
prop.table(table(majority_median_merge$n_median))




###########################################################################
###########################################################################
# Chapter 3: Topics by Predictor (seniority, leadership)
# We can see that leadership and seniority talks about different topics
# than rank-and-file and freshman
###########################################################################
###########################################################################

########################
# Press release frequency by party - ch2
########################

# # % of press releases by topic - R
# freq.R <- topics_df.R %>%
#   group_by(topicName) %>%
#   summarise(cnt = n()) %>%
#   mutate(freq = round(cnt / sum(cnt), 3)) %>%
#   arrange(desc(freq)) %>%
#   left_join(., topicNames.R
#             , by = c("topicName")) %>%
#   filter(salient == 1) %>%
#   select(topicName, freq)
# 
# # % of press releases by topic - D
# freq.D <- topics_df.D %>%
#   group_by(topicName) %>%
#   summarise(cnt = n()) %>%
#   mutate(freq = round(cnt / sum(cnt), 3)) %>%
#   arrange(desc(freq)) %>%
#   left_join(., topicNames.D
#             , by = c("topicName")) %>%
#   filter(salient == 1) %>%
#   select(topicName, freq)
# 
# # Full join
# freq.both <-
#   full_join(freq.R, freq.D
#             , by = "topicName") %>%
#   rename(freq.R = freq.x
#          , freq.D = freq.y)
# 
# 
# # Edit for figure
# freq.both.edit <-
#   freq.both %>%
#   mutate(freq.D = 100 * freq.D
#          , freq.R = 100 * freq.R) %>%
#   mutate(freq.D = as.numeric(paste0("-", freq.D)))
# 
# # Pivot longer
# freq.both.long <-
#   pivot_longer(data = freq.both.edit
#                , cols = c("freq.R", "freq.D")
#                , names_to = "party"
#                , names_prefix = "freq."
#                , values_to = "freq")
# 
# #####
# # Arranging for figure
# ####
# 
# # Descending order for Dems
# freq.both.long <-
#   freq.both.long %>%
#   arrange(desc(freq), .by_group = TRUE)
# 
# # Get names to set the limits - Democrats (ascending order b/c negative)
# dem.topic.limits <-
#   freq.both.long %>%
#   filter(party == "D") %>%
#   filter(!is.na(freq)) %>%
#   arrange(freq) %>%
#   select(topicName) %>%
#   pull()
# 
# # Additional republican topics that Dems don't talk about (also ascending order)
# party.topic.limits <-
#   c(dem.topic.limits
#     , "Law and order", "China", "Small business", "Public lands", "Human trafficking"
#     , "Executive power", "Higher edu.", "Veterans' affairs", "Energy")
# 
# 
# 
# # Make party-topic figure
# ggplot(freq.both.long, aes(
#   y = factor(topicName)
#   , x = freq
#   , fill = factor(freq < 0)) # color dem and rep differently
#   ) +
#   geom_bar(stat = "identity", width = 0.8) +
#   geom_vline(xintercept = 0) +
#   theme_bw() +
#   xlab("Dem                                                           Rep") +
#   ylab("") +
#   scale_x_continuous(breaks = c(-6, -3, 0, 3, 6),
#                      labels = paste0(c(6, 3, 0, 3, 6),"%")) +
#   scale_y_discrete(limits = rev(party.topic.limits)) +
#   scale_fill_manual(guide = "none", values = c("TRUE" = "dark blue", "FALSE" = "dark red"))
#   # ggtitle("Democrats and Republicans diverge in topic focus")
# 
# 
# 
# 
# ##################
# # Ch2: topic frequency by leadership role
# #################
# 
# # Merge extra data that is already appended to the score df for regression
# topics_dfExtra.D <-
#   left_join(topics_df.D, scoreCongressNets_legCovs.D
#             , by = c("member_id", "congress"))
# 
# topics_dfExtra.R <-
#   left_join(topics_df.R, scoreCongressNets_legCovs.R
#             , by = c("member_id", "congress"))
# 
# 
# # Create separate leadership/rank-and-file freqs
# 
# leader.rankfile.freq.fns <- function(topics_dfExtra) {
#   
#   leadership <-
#     topics_dfExtra %>%
#     filter(party_leadership == 1) %>%
#     filter(salient == 1) %>%
#     group_by(topicName) %>%
#     summarise(cnt = n()) %>%
#     mutate(freq = round(cnt / sum(cnt), 3)
#            , party_leadership = 1) %>%
#     arrange(desc(freq)) %>%
#     select(topicName, freq, party_leadership)
#   
#   rankfile <-
#     topics_dfExtra %>%
#     filter(party_leadership == 0) %>%
#     filter(salient == 1) %>%
#     group_by(topicName) %>%
#     summarise(cnt = n()) %>%
#     mutate(freq = round(cnt / sum(cnt), 3)
#            , party_leadership = 0) %>%
#     arrange(desc(freq)) %>%
#     select(topicName, freq, party_leadership)
#   
#   # Join
#   leaderandrank <-
#     full_join(leadership, rankfile
#               , by = "topicName") %>% 
#     rename(freq.leader = freq.x
#            , freq.rank = freq.y)
#   
#   
#   # Pivot longer
#   freq.leaderrank.long <-
#     pivot_longer(data = leaderandrank
#                  , cols = c("freq.leader", "freq.rank")
#                  , names_to = "leadership"
#                  , names_prefix = "freq."
#                  , values_to = "freq") %>% 
#     select(-c(party_leadership.x, party_leadership.y))
#   
#   # Turn into pcts 
#   freq.leaderrank.long <-
#     freq.leaderrank.long %>% 
#     mutate(freq = 100 * freq)
#   
#   
# } # End leader.rankfile.freq.fns
# 
# # Apply function
# freq.leaderrank.R <- leader.rankfile.freq.fns(topics_dfExtra.R) %>% mutate(party = "R")
# freq.leaderrank.D <- leader.rankfile.freq.fns(topics_dfExtra.D) %>% mutate(party = "D")
# 
# # Merge together
# freq.leaderrank.all <- 
#   full_join(freq.leaderrank.R
#             , freq.leaderrank.D
#             , by = c("topicName", "leadership")) %>% 
#   rename(freq.R = freq.x
#          , freq.D = freq.y)
# 
# # Turn Dem's into negative for now
# freq.leaderrank.all <-
#   freq.leaderrank.all %>% 
#   mutate(freq.D = as.numeric(paste0("-", freq.D)))
# 
# # Pivot longer
# freq.leaderrank.long <-
#   freq.leaderrank.all %>% 
#   pivot_longer(cols = c("freq.R", "freq.D")
#                , names_to = "party"
#                , names_prefix = "freq."
#                , values_to = "freq") %>% 
#   select(-c(party.x, party.y))
# 
# #############
# # Leadership vs rank-and-file Figure (both parties) - Make
# ############
# 
# # Unite leadership and party to create a new factor
# freq.leaderrank.long <-
#   tidyr::unite(freq.leaderrank.long, "role_party", leadership, party, remove = FALSE) %>% 
#   mutate(role_party = factor(role_party, levels = c("rank_D", "rank_R", "leader_D", "leader_R"), ordered = TRUE))
#          
# 
# ggplot(freq.leaderrank.long
#        , aes(y = factor(topicName)
#              , x = freq
#              , group = factor(leadership))) +
#   geom_bar(aes(fill = factor(role_party)
#                , color = factor(role_party))
#            , stat = "identity"
#            , width = 0.6
#            , position = "dodge") +
#   theme_bw() +
#   geom_vline(xintercept = 0) +
#   scale_fill_manual(values = c("leader_R" = "dark red", "rank_R" = "#ffdadb"
#                                , "leader_D" = "dark blue", "rank_D" = "light blue")
#                     , name = ""
#                     , labels = c("Rank-and-file (D)"
#                                  , "Rank-and-file (R)"
#                                  , "Leadership (D)"
#                                  , "Leadership (R)")) +
#   scale_color_manual(values = c("leader_R" = "dark red", "rank_R" = "dark red"
#                                 , "leader_D" = "dark blue", "rank_D" = "dark blue")
#                      , guide = "none") +
#   scale_y_discrete(limits = rev(party.topic.limits)) +
#   ylab("") +
#   xlab("Dem                                                           Rep") +
#   scale_x_continuous(breaks = c(-10, 0, 10), 
#                      labels = paste0(c(10, 0, 10),"%")) +
#   ggtitle("Leadership focus on different topics compared to rank-and-file members") +
#   theme(plot.title.position = "plot"
#         , plot.title = element_text(hjust = 0.5))
#   

















###############################################################################
###############################################################################
# Network Figures
###############################################################################
###############################################################################

# This section is using first.obs data
# Here's a pre-run copy of it from Nov 11
first.obs.D <- readRDS(paste0(getwd(), "/Data/output/firstobs_D_Nov11"))
first.obs.R <- readRDS(paste0(getwd(), "/Data/output/firstobs_R_Nov11"))

# Here's also a pre-run copy of the full influence-score data set
dat_congressNets <- readRDS(paste0(getwd(), "/Data/output/dat_congressNets_Nov11"))




##############
# Estimate party-congress networks using same seed as before
# Prior function returned centrality stats
# Now we need the actual network objects
##############

###
# Create cascade objects and network objects
###

# Create cascade mini function
createCascade.minifns <- function(first.obs, congressNum){
  
  # Filter dataset
  congress.df <- 
    first.obs %>% 
    filter(congress == congressNum)
  
  # Arrange
  congress.df <-
    congress.df %>% 
    arrange(topic, member_id)
  
  # Create cascades
  congress.cascade <-
    as_cascade_long(
      data = congress.df
      , cascade_node_name = 'member_id'
      , event_time = 'date_pr'
      , cascade_id = 'topic'
    )
  return(congress.cascade)
}

# Create networks mini function
createNetworks.minifns <- function(congress.cascade){
  
  congress.netinf.result <- netinf(
    cascades = congress.cascade
    , trans_mod = "log-normal"
    , p_value_cutoff = 0.1
    , trees = TRUE
  )
  return(congress.netinf.result)
}



###
# Apply functions - create cascade and network objects
###

# Cascades - Republicans
R113.cascades <- createCascade.minifns(first.obs.R, 113)
R114.cascades <- createCascade.minifns(first.obs.R, 114)
R115.cascades <- createCascade.minifns(first.obs.R, 115)
R116.cascades <- createCascade.minifns(first.obs.R, 116)

# Cascades - Democrats
D113.cascades <- createCascade.minifns(first.obs.D, 113)
D114.cascades <- createCascade.minifns(first.obs.D, 114)
D115.cascades <- createCascade.minifns(first.obs.D, 115)
D116.cascades <- createCascade.minifns(first.obs.D, 116)


set.seed(1776) # same seed as before

# Networks - Republicans
R113.network <- createNetworks.minifns(R113.cascades)
R114.network <- createNetworks.minifns(R114.cascades)
R115.network <- createNetworks.minifns(R115.cascades)
R116.network <- createNetworks.minifns(R116.cascades)

# Networks - Democrats
D113.network <- createNetworks.minifns(D113.cascades)
D114.network <- createNetworks.minifns(D114.cascades)
D115.network <- createNetworks.minifns(D115.cascades)
D116.network <- createNetworks.minifns(D116.cascades)



####################################

###
# Mini example of cascade (2 topics, 10 days)
###

# # Create 2-topic cascade
# # With topicName instead, so it labels correctly
# cascadePlotData <-
#   first.obs.D %>% 
#   filter(congress == 115) %>% 
#   filter(topicName %in% c("Taxes", "Climate")) %>% 
#   arrange(topicName, member_id) %>% 
#   as_cascade_long(
#    cascade_node_name = 'member_id'
#   , event_time = 'date_pr'
#   , cascade_id = 'topicName'
# )
# 
# # Pick first 10 days
# time_constrainedData <- subset_cascade_time(cascade = cascadePlotData
#                                             , start_time = as.Date("2017-01-03")
#                                             , end_time = as.Date("2017-01-13"))
# 
# # NetInf changed dates to numeric - change it back
# time_constrainedData$cascade_times$Climate <- as.Date(time_constrainedData$cascade_times$Climate, origin = "1970-01-01")
# time_constrainedData$cascade_times$Taxes <- as.Date(time_constrainedData$cascade_times$Taxes, origin = "1970-01-01")
# 
# # Plot
# plot(time_constrainedData, label_nodes = TRUE) +
#   labs(x = "", y = "")





####################################

###
# Plot: Improvements (gain in model fit) from each added edge
###

# # Create plot objects
# impD113.pl <- plot(D113.network, type = "improvement") + theme(axis.title = element_blank()) + labs(subtitle = "113th Congress")
# impD114.pl <- plot(D114.network, type = "improvement") + theme(axis.title = element_blank()) + labs(subtitle = "114th Congress")
# impD115.pl <- plot(D115.network, type = "improvement") + theme(axis.title = element_blank()) + labs(subtitle = "115th Congress")
# impD116.pl <- plot(D116.network, type = "improvement") + theme(axis.title = element_blank()) + labs(subtitle = "116th Congress")
# 
# impR113.pl <- plot(R113.network, type = "improvement") + theme(axis.title = element_blank()) + labs(subtitle = " ")
# impR114.pl <- plot(R114.network, type = "improvement") + theme(axis.title = element_blank()) + labs(subtitle = " ")
# impR115.pl <- plot(R115.network, type = "improvement") + theme(axis.title = element_blank()) + labs(subtitle = " ")
# impR116.pl <- plot(R116.network, type = "improvement") + theme(axis.title = element_blank()) + labs(subtitle = " ")
# 
# ###
# # Create "master" axis grobs
# ###
# 
# # Master y axis
# y.grobImprovement <- textGrob("Improvement", rot = 90)
# 
# # Master x axis
# x.grobEdgeNum <- textGrob("Edge Number")
# 
# # Super axis - party
# x.grobParty <- textGrob("Democrats                                  Republicans"
#                         , gp = gpar(fontsize = 14, fontface = "bold"))
# 
# # Arrange axis grobs and plot objects
# grid.arrange(arrangeGrob(impD113.pl, impR113.pl
#                          , impD114.pl, impR114.pl
#                          , impD115.pl, impR115.pl
#                          , impD116.pl, impR116.pl
#                          , ncol = 2
#                          , left = y.grobImprovement, top = x.grobParty, bottom = x.grobEdgeNum))
# 
# 
# 
# 
# ###################################
# # Plot: p-value from the Vuong test associated with each edge addition
# ###################################
# # Create plot objects
# vuongD113.pl <- plot(D113.network, type = "p-value") + theme(axis.title = element_blank()) + labs(subtitle = "113th Congress")
# vuongD114.pl <- plot(D114.network, type = "p-value") + theme(axis.title = element_blank()) + labs(subtitle = "114th Congress")
# vuongD115.pl <- plot(D115.network, type = "p-value") + theme(axis.title = element_blank()) + labs(subtitle = "115th Congress")
# vuongD116.pl <- plot(D116.network, type = "p-value") + theme(axis.title = element_blank()) + labs(subtitle = "116th Congress")
# 
# vuongR113.pl <- plot(R113.network, type = "p-value") + theme(axis.title = element_blank()) + labs(subtitle = " ")
# vuongR114.pl <- plot(R114.network, type = "p-value") + theme(axis.title = element_blank()) + labs(subtitle = " ")
# vuongR115.pl <- plot(R115.network, type = "p-value") + theme(axis.title = element_blank()) + labs(subtitle = " ")
# vuongR116.pl <- plot(R116.network, type = "p-value") + theme(axis.title = element_blank()) + labs(subtitle = " ")
# 
# ###
# # Create "master" axis grobs
# ###
# 
# # Master y axis
# y.grobPValue <- textGrob("P-Value", rot = 90)
# 
# # Master x axis
# x.grobEdgeNum <- textGrob("Edge Number")
# 
# # Super axis - party
# x.grobParty <- textGrob("Democrats                                  Republicans"
#                         , gp = gpar(fontsize = 14, fontface = "bold"))
# 
# # Arrange axis grobs and plot objects
# grid.arrange(arrangeGrob(vuongD113.pl, vuongR113.pl
#                          , vuongD114.pl, vuongR114.pl
#                          , vuongD115.pl, vuongR115.pl
#                          , vuongD116.pl, vuongR116.pl
#                          , ncol = 2
#                          , left = y.grobPValue, top = x.grobParty, bottom = x.grobEdgeNum))




###################################################
# Ego networks of most and least influential person
##################################################

###
# Most/Least influential legislator, congress, and party
###
most_influential <- 
  c(dat_congressNets[which.max(dat_congressNets$pagerank_REVERSE), ]$member_id
  , dat_congressNets[which.max(dat_congressNets$pagerank_REVERSE), ]$party_code
  , dat_congressNets[which.max(dat_congressNets$pagerank_REVERSE), ]$congress
  , dat_congressNets[which.max(dat_congressNets$pagerank_REVERSE), ]$bioname)
  

least_influential <- 
  c(dat_congressNets[which.min(dat_congressNets$pagerank_REVERSE), ]$member_id
    , dat_congressNets[which.min(dat_congressNets$pagerank_REVERSE), ]$party_code
    , dat_congressNets[which.min(dat_congressNets$pagerank_REVERSE), ]$congress
    , dat_congressNets[which.min(dat_congressNets$pagerank_REVERSE), ]$bioname)

###
# Convert diffnet object to igraph
###

# Most influential: Bruce Westerman (R) in 114th congress
R114.graph <- igraph::graph_from_data_frame(d = R114.network[, 1:2])

# Least influential: Colin Allred (D) 116th congress
D116.graph <- igraph::graph_from_data_frame(d = D116.network[, 1:2])



#############################
# Set additional vertex attributes
##############################

###
# Additional attributes - most influential
###

# List of member_ids 
R114.vertexInfo <- data.frame(member_id = V(R114.graph)$name)

# Add on pagerank_REVERSE and bioname
R114.vertexInfo <-
  R114.vertexInfo %>% 
  left_join(
    select(
      filter(dat_congressNets, congress == 114 & party_code == 200)
      , member_id, pagerank_REVERSE, bioname)
    , by = "member_id"
  )


###
# Additional attributes - least influential
###

# List of member_ids
D116.vertexInfo <- data.frame(member_id = V(D116.graph)$name)

# Add on pagerank_REVERSE and bioname
D116.vertexInfo <-
  D116.vertexInfo %>% 
  left_join(
    select(
      filter(dat_congressNets, congress == 116 & party_code == 100)
      , member_id, pagerank_REVERSE, bioname)
    , by = "member_id"
  )

# Set attributes
vertex_attr(R114.graph, "bioname") <- R114.vertexInfo$bioname
vertex_attr(R114.graph, "pagerank_REVERSE") <- (R114.vertexInfo$pagerank_REVERSE*1000)

vertex_attr(D116.graph, "bioname") <- D116.vertexInfo$bioname
vertex_attr(D116.graph, "pagerank_REVERSE") <- (D116.vertexInfo$pagerank_REVERSE*1000)


# Get list of all ego networks in each congress, and induce them each into subgraphs
R114.egoList <- lapply(ego(R114.graph), function(x) induced_subgraph(R114.graph, x))
D116.egoList <- lapply(ego(D116.graph), function(x) induced_subgraph(D116.graph, x))

# Index for min/max influential in their respective ego lists
most.index <- which(V(R114.graph)$name == most_influential[1])
least.index <- which(V(D116.graph)$name == least_influential[1])

# Pull out ego-graphs
most.graph <- R114.egoList[[most.index]]
least.graph <- D116.egoList[[least.index]]

####
# Additional attributes for plotting
###

# Set color of highlighted member to be different 
vertex_attr(most.graph, "color") <- 
  ifelse(V(most.graph)$name == most_influential[1], "slateblue", "gold")

vertex_attr(least.graph, "color") <- 
  ifelse(V(least.graph)$name == least_influential[1], "slateblue", "gold")


###
# Plot ego network for most influential member - use tkplot to stretch out and export
###
set.seed(1)
tkplot(
  most.graph
  , vertex.label = V(most.graph)$bioname
  , vertex.label.cex = 0.6
  , vertex.size = V(most.graph)$pagerank_REVERSE
  , vertex.color = V(most.graph)$color
  , vertex.label.dist = 0.9
  , edge.color = "black"
  , edge.arrow.size = 0.5
  , edge.curved = TRUE
  , main = "Most Influential: \nBruce Westerman (R-AR) in the 114th Congress"
  )


###
# Plot ego network for least influential member - use tkplot to stretch out and export
###
set.seed(5)
tkplot(
  least.graph
  , vertex.label = V(least.graph)$bioname
  , vertex.label.cex = 0.6
  , vertex.size = V(least.graph)$pagerank_REVERSE
  , vertex.color = V(least.graph)$color
  , vertex.label.dist = 0.9
  , edge.color = "black"
  , edge.arrow.size = 0.5
  , edge.curved = TRUE
  , main = "Least Influential: \nColin Allred (D-TX) in the 116th Congress"
)






########################################################################
########################################################################
# Topic Exploration - Plots
########################################################################
########################################################################
# Ch2 - S-curves (all topics and example topic)

######
# D & R s-curves of Immigration during 115th congress (first immigration ban signed Jan 2017)
#####
# # Both parties on one figure
# 
# # Merge topics
# topics_all <- rbind(topics_df.D, topics_df.R)
# 
# # Plot
# ggplot(data = filter(topics_all, congress == 115 & topicName == "Immigration")
#        , aes(x = date_pr)) +
#   geom_step(aes(y = after_stat(y), color = party_atPR)
#             , stat = "ecdf"
#             , alpha = 0.5
#   ) +
#   annotate(geom = "text"
#            , x = ymd("2017-05-01")
#            , y = 0.12
#            , label = "Early adopters"
#            , size = 3.1) +
#   annotate(geom = "text"
#            , x = ymd("2017-11-10")
#            , y = 0.39
#            , label = "Early majority"
#            , size = 3.1) +
#   annotate(geom = "text"
#            , x = ymd("2018-04-15")
#            , y = 0.73
#            , label = "Late majority"
#            , size = 3.1) +
#   annotate(geom = "text"
#            , x = ymd("2018-07-30")
#            , y = 0.94
#            , label = "Laggards"
#            , size = 3.1) +
#   scale_x_date(labels = scales::label_date_short()) +
#   theme_minimal() +
#   labs(x = "", y = "") +
#   scale_color_manual(values = c("blue", "dark red")
#                      , name = ""
#                      , labels = c("D" = "Democrats", "R" = "Republicans")
#   ) +
#   theme(legend.position = "bottom")
# 
# 
# #########################################################################
# # S-Curves - All for 113th congress
# #########################################################################
# 
# # Start with a single topic - Dems talking about climate in 116th congress
# dem.climate.116 <- first.obs.D %>% 
#   filter(congress == 113 & topicName == "Iran nuclear")
# 
# infections.by.time <- dem.climate.116 %>% 
#   # filter(date_pr < ymd("2014-03-01")) %>% 
#   group_by(lubridate::month(date_pr, label = TRUE, abbr = TRUE)) %>% 
#   summarize(n.infections = n()) %>% 
#   ungroup() %>% 
#   mutate(cum.infections = cumsum(n.infections))
# 
# 
# ggplot(infections.by.time, aes(x = `month(date_pr)`)) +
#   geom_smooth(aes(y = n.infections), se = FALSE) +
#   geom_smooth(aes(y = cum.infections), se = FALSE)
#   
# 
# 
# 
# 
# 
#   
# topics_df.D %>% 
#   filter(congress == 113 & salient == 1 & topicName != "COVID-19") %>% 
#   # ggplot(., aes(x = lubridate::month(date_pr, label = TRUE, abbr = TRUE))) +
#   ggplot(data = ., aes(x = date_pr)) +
#   geom_step(aes(y = ..y..), stat = "ecdf") +
#   scale_x_date(labels = scales::label_date_short()) +
#   facet_wrap(~topicName) +
#   labs(y = "", x = "") +
#   # ggtitle("Diffusion of Democratic topics in the 113th Congress") +
#   # theme_minimal() +
#   theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
#         panel.background = element_blank(), axis.line = element_line(colour = "black")) +
#   theme(panel.spacing.x = unit(6, "mm")) # prevent overlapping labels
# 
# 
# topics_df.R %>% 
#   filter(congress == 113 & salient == 1 & topicName != "COVID-19" & topicName != "Higher edu.") %>% 
#   # ggplot(., aes(x = lubridate::month(date_pr, label = TRUE, abbr = TRUE))) +
#   ggplot(data = ., aes(x = date_pr)) +
#   geom_step(aes(y = ..y..), stat = "ecdf") +
#   scale_x_date(labels = scales::label_date_short()) +
#   facet_wrap(~topicName) +
#   labs(y = "", x = "") +
#   theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
#         panel.background = element_blank(), axis.line = element_line(colour = "black")) +
#   theme(panel.spacing.x = unit(6, "mm")) 





#########################################################################
#########################################################################


















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
gg.effects <- stminsights::get_effects(estimates = fit.ee.date,
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




########################################################################
########################################################################














##################################
# Look at bar charts to see if anything stands out as highly correlated (factors)

# Party leadership
dat_congressNets %>%
  filter(!is.na(party_leadership)) %>% 
  ggplot( aes(x=pagerank_REVERSE, fill=as.factor(party_leadership))) +
  geom_histogram( color="#e9ecef", alpha=0.6, position = 'identity', bins = 30) +
  scale_fill_manual(values=c("#69b3a2", "#404080")) +
  labs(fill="Party leadership", x = "PageRank")

# Committee leadership
dat_congressNets %>%
  filter(!is.na(committee_leadership)) %>% 
  ggplot( aes(x=pagerank_REVERSE, fill=as.factor(committee_leadership))) +
  geom_histogram( color="#e9ecef", alpha=0.6, position = 'identity', bins = 30) +
  scale_fill_manual(values=c("#69b3a2", "#404080")) +
  labs(fill="Committee leadership", x = "PageRank")





###########################################################
# Analysis:
###########################################################


#######
# How early in a congress is the majority of members' first-use of topics?
#######

# Merge first.obs D & R together
firstObs_all <- rbind(first.obs.D, first.obs.R)

# Time distribution - first use of topics ~ congress
ggplot(data = group_by(firstObs_all, congress)) +
  geom_histogram(aes(x = date_pr, fill = topic), bins = 50) +
  facet_wrap(~ congress, scales = "free_x") +
  guides(fill = "none") +
  labs(x = "", y = "") +
  theme_bw() +
  theme(panel.spacing.x = unit(5, "mm")) +
  ggtitle("Distribution of First Topic Usage by Congress")


# Distribution of first use within XXth congress ~ topic
firstObs_all %>% 
  filter(congress == 116) %>% 
  {ggplot(data = .) +
  geom_histogram(aes(x = date_pr), bins = 30) +
      facet_wrap(~ topicName, scales = "free_y") +
      theme_minimal() +
    labs(x = "Date of first use, Jan 2019 - Jan 2021", y = "") +
      theme(axis.text.x = element_blank()
            , axis.text.y = element_blank()
            , strip.text.x = element_text(size = 8)
            ) +
      ggtitle("Example distribution of first use by topic", subtitle = "116th Congress")}


#######
# Are any topics increasing or decreasing over time?
#######

# Facet wrap plot of all topics and density over time - Republicans
ggplot(data = topics_df.R) +
  geom_density(aes(x = date_pr)) +
  facet_wrap(~ topicName
             , ncol = 3
             , scales = "free_y"
             ) +
  geom_vline(xintercept = c(ymd("2015-01-03"
                                , ymd("2017-01-03")
                                , ymd("2019-01-03")))
             , linetype = "dotted") +
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  scale_y_continuous(labels = NULL) +
  labs(y = "", x = "") +
  theme_minimal() +
    ggtitle("Relative density plots of topics in the 113-116th Congress"
            , subtitle = "Republicans")

# Facet wrap plot of all topics and density over time - Democrats
ggplot(data = topics_df.D) +
  geom_density(aes(x = date_pr)) +
  facet_wrap(~ topicName
             , ncol = 3
             , scales = "free_y"
  ) +
  geom_vline(xintercept = c(ymd("2015-01-03"
                                , ymd("2017-01-03")
                                , ymd("2019-01-03")))
             , linetype = "dotted") +
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  scale_y_continuous(labels = NULL) +
  labs(y = "", x = "") +
  theme_minimal() +
  ggtitle("Relative density plots of topics in the 113-116th Congress"
          , subtitle = "Democrats")




###########################################################











# End Tests/Experiments Section
#################################################################










# #################################################################
# #################################################################
# # SECTION 2: FRAMES
# #################################################################
# #################################################################
# 
# # Description:
# 
# # Within each topic, what is the second highest topic? 
# # Who is most central in a diffusion network of that frame?
# 
# 
# 
# 
# ###################################################
# 
# # Function: - Label each doc with topic and frame
#   # Repeat section 1 (label docs with most-prob topic)
#   # Add frame label (most-prob second topic per doc)
#   # Add topic/frame descriptions
# 
# ###################################################
# 
# 
# 
# label_statement_topics.fns <- function(fit, out){
# 
#   ########
#   # Add TOPIC label to each doc
#   ########
#   
#   # Make a data.table of topic proportions
#   topics_DT <- make.dt(fit, meta = out$meta)
#   
#   # Pivot longer so each doc is repeated 20 times
#   topics_long <- 
#     pivot_longer(
#       data = topics_DT,
#       cols = starts_with("Topic"),
#       names_to = "topic",
#       names_prefix = "Topic",
#       values_to = "topic_prob"
#     ) %>% 
#     as.data.frame()
#   
#   # Arrange by doc index, then desc(prob)
#   # So the highest probability observation is first
#   topics_long <- 
#     topics_long %>% 
#     group_by(indx) %>% 
#     arrange(desc(topic_prob), .by_group = TRUE) %>% 
#     ungroup()
#   
#   
#   # Create mainTopic.df
#   # (keep only the observation with the highest prob)
#   mainTopic.df <- 
#     topics_long %>% 
#     distinct(indx, .keep_all = TRUE)
#   
#   ########
#   # Add FRAME label to each doc
#   ########
# 
#   # Get the remaining frames
#   remaining_frames <-
#     setdiff(topics_long, mainTopic.df) %>% 
#     group_by(indx) %>% 
#     arrange(desc(topic_prob), .by_group = TRUE) %>% 
#     ungroup()
#   
#   # Keep only the highest prob frame
#   mainFrame <-
#     remaining_frames %>% 
#     distinct(indx, .keep_all = TRUE)
#   
#   # Rename "frame" and "frame_prob" and only keep them and indx in simple df
#   mainFrame_simple <-
#     mainFrame %>% 
#     rename(frame = topic,
#            frame_prob = topic_prob) %>% 
#     select(indx, frame, frame_prob)
#   
#   
#   ########
#   # Merge TOPIC and FRAME dfs
#   ########
#   
#   # Merge frames onto main topic df
#   topicsFrames.df <- 
#     left_join(
#       mainTopic.df
#       , mainFrame_simple
#       , by = c("indx")
#     )
# 
#   
#   ########
#   # Remove the incorrect caucus info
#   ########
#   
#   topicsFrames.df <-
#     topicsFrames.df %>% 
#     select(-c(contains("_leader"), contains("_titles"),
#               contains("_taskforces"), contains("_member")))
#   
#   
#   
#   ########
#   # Add topic labels onto data.frame
#   # To provide information about topics/frames
#   ########
#   
# 
#   # Pull 7 and 4 word summaries (estimated by STM)
#   set.seed(5555)
#   # 7-word summary
#   fit.labels <- labelTopics(fit, 1:20, n = 7)
#   # 4-word summary
#   fit.labels_short <- labelTopics(fit, 1:20, n = 4) 
#   
#   
#   # Labels for TOPICS
#   # Topic # and short/long descriptions
#   topic_labels.df <- data.frame(topic = as.character(fit.labels$topicnums),
#                           topic_label = apply(fit.labels$prob, 1, paste0, collapse = "; "),
#                           topic_label_short = apply(fit.labels_short$prob, 1, paste0, collapse = "; "))
#   
#   # Labels for FRAMES
#   frame_labels.df <- data.frame(frame = as.character(fit.labels$topicnums),
#                                 frame_label = apply(fit.labels$prob, 1, paste0, collapse = "; "),
#                                 frame_label_short = apply(fit.labels_short$prob, 1, paste0, collapse = "; "))
#   
#   
#   ########
#   # Merge topic labels onto data
#   ########
#   topicsFrames.df <- 
#     topicsFrames.df %>% 
#     dtplyr::lazy_dt() %>% 
#     left_join(topic_labels.df, by = "topic") %>% 
#     as.data.frame()
#   
#   ########
#   # Merge frame labels onto data
#   ########
#   topicsFrames.df <- 
#     topicsFrames.df %>% 
#     dtplyr::lazy_dt() %>% 
#     left_join(frame_labels.df, by = "frame") %>% 
#     as.data.frame()
#   
#   
# } ##################################  End label_statement_topics.fns function
# 
# 
# 
# 
# 
# ###############
# # Apply topic label function
# ##############
# set.seed(7777)
# topicsFrames.R <-
#   label_statement_topics.fns(
#     fit = fit.R,
#     out = out.R)
# 
# # Remove 2021 press releases (there are only 8, compared to 20k for the others)
# topicsFrames.R <-
#   topicsFrames.R %>% 
#   filter(date_pr < ymd("2021-01-01"))
# 
# 
# # Table: Examples of Statement title, Topic, Frame
# # Pull random examples:
# sample_n(topicsFrames.R, 1) %>% select(title_pr, topic_label, frame_label)
# 
# 
# 
# 
# 
# 
# 
# ##########################################################
# # Most Common Frames
# # Make DF for diffusion network
# ##########################################################
# 
# 
# # For each topic, what is the most common frame?
# mostCommonFrames.R <-
#   topicsFrames.R %>% 
#   group_by(topic) %>% 
#   count(frame) %>% 
#   summarize(frame = as.character(which.max(n))) 
# 
# 
# ####
# # Pull the unique descriptions of topics/frames
# ####
# 
# # Topic descriptions
# topicDescs.df <-
#   topicsFrames.R %>% 
#   select(topic, topic_label, topic_label_short) %>% 
#   distinct()
# 
# # Frame descriptions
# frameDescs.df <-
#   topicsFrames.R %>% 
#   select(frame, frame_label, frame_label_short) %>% 
#   distinct()
#   
# # Merge TOPIC descriptions onto table
# mostCommonFrames.R <-
#   mostCommonFrames.R %>% 
#   left_join(., topicDescs.df
#             , by = "topic"
#             )
# 
# # Merge FRAME descriptions onto table
# mostCommonFrames.R <-
#   mostCommonFrames.R %>% 
#   left_join(., frameDescs.df,
#             by = "frame")




################################################################
# Analysis:
# Descriptive Figures/Tables: Frames
################################################################


# Clean/organize table for publish

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













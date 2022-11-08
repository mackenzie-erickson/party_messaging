###############################

# TopicsDF_create_sessionSlices.R
# Author: Mackenzie Weiler
# Date: 2022-04
# Description: Load fitted STM model and original document data, 
  # get in cascade format, and fit diffusion network


################################

rm(list = ls())

# Packages
packages <- c("NetworkInference", "tidyverse", "igraph", "ggplot2", "lubridate",
              "stringr", "stm", "quanteda", 
              "texreg", "knitr", "kableExtra"
              , "data.table"
              , "plm")

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
# Get the first use of each topic by each member

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

  
estimateNetwork_outputScore.fns <- function(first.obs){
  
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
       , trans_mod = "exponential"
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
# Apply networks and centrality function
###
set.seed(1776)
score_congressNet.list.R <- estimateNetwork_outputScore.fns(first.obs.R)
score_congressNet.list.D <- estimateNetwork_outputScore.fns(first.obs.D)

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



###############################################################

# Add in congress covariates; add in number of press releases; join with centrality df

###
# Number PRs - Function
###

numPR.fns <- function(topics.df, first.obs, score_congressNet.df) {
  # Number the PRs (total)
  numPR_total <-
    topics.df %>% 
    group_by(member_id, congress) %>% 
    count() %>% 
    rename(numPR_total = n) 
  
  numPR_total <-
    numPR_total %>% 
    mutate(numPR_total_ln = log(numPR_total))
  
  # Number of PRs (first.obs)
  numPR_firstObs <- 
    first.obs %>% 
    group_by(member_id, congress) %>% 
    count() %>% 
    rename(numPR_firstobs = n)
  
  numPR_firstObs <-
    numPR_firstObs %>% 
    mutate(numPR_firstobs_ln = log(numPR_firstobs))
  
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
# Apply function
###
new.score.df.R <- numPR.fns(topics_df.R, first.obs.R, score_congressNet.df.R)
new.score.df.D <- numPR.fns(topics_df.D, first.obs.D, score_congressNet.df.D)



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
    new.score.df.R
    , covariates.df.R,
    by = c("member_id", "congress")
  )

scoreCongressNets_legCovs.D <-
  left_join(
    new.score.df.D
    , covariates.df.D
    , by = c("member_id", "congress")
  )


###
# Merge Democrats and Republicans together
###

dat_congressNets <- rbind(scoreCongressNets_legCovs.D, scoreCongressNets_legCovs.R)


###
# Make member names pretty for figure purposes
###
dat_congressNets$bioname <- str_to_title(dat_congressNets$bioname)


  


##########################################################################

####################################################
# DESCRIPTIVE STATS - Appendix
####################################################

# Distribution of pagerank scores
ggplot(dat_congressNets) +
  geom_boxplot(aes(pagerank_REVERSE)) +
  theme_minimal() +
  labs(x = "PageRank score") +
  ggtitle("Distribution of PageRank scores")

# Distribution of relative pagerank scores
ggplot(dat_congressNets) +
  geom_boxplot(aes(pagerank_DIFF)) +
  theme_minimal() +
  labs(x = "Relative PageRank score") +
  ggtitle("Distribution of Relative PageRank scores")

# Distribution of eigencentrality scores
ggplot(dat_congressNets) +
  geom_boxplot(aes(eigen)) +
  theme_minimal() +
  labs(x = "Eigenvector Centrality") +
  ggtitle("Distribution of Eigenvector Centrality")


###########################################################################







##########################################################
# Regressions: 
###########################################################


# Transform score to make more readable (multiple by 100)
dat_congressNets <-
  dat_congressNets %>% 
  mutate(pr_100 = pagerank_REVERSE * 1000
         , prDiff_100 = pagerank_DIFF * 1000
         , eigen_100 = eigen * 1000)

# Filter to just Dems and Repubs (remove one independent)
dat_congressNets <- filter(dat_congressNets, party_code %in% c(100, 200))



####
# Models
####

# Prepare variables

# Set up all variables 
vars0 <- with(dat_congressNets
              , data.frame(
                "PageRank" = pr_100
                , "PageRank diff" = prDiff_100
                , "Eigencentrality" = eigen_100
                , "member_id" = member_id
                , "Congress" = congress
                , "Press release count" = numPR_total
                , "Bills sponsored" = bills_sponsored
                , "Bills cosponsored" = bills_cosponsored
                , "Pct. votes with party" = votes_with_party_pct
                , "Caucus member" = factor(caucus_member, levels = 0:1, labels = c("No", "Yes"))
                , "Speeches daily" = n_speeches_daily
                , "Party" = factor(party_code, levels = c(100, 200), labels = c("Democrat", "Republican"))
                , "NOMINATE Dim 1" = nominate_dim1
                , "NOMINATE Dim 2" = nominate_dim2
                , "Senioriy" = seniority
                , "Gender" = factor(gender, levels = c("man", "woman"), labels = c("Male", "Female"))
                , "White" = factor(white, levels = 0:1, labels = c("No", "Yes"))
                , "Majority" = factor(majority_party, levels = 0:1, labels = c("No", "Yes"))
                , "Party leadership" = factor(party_leadership, levels = 0:1, labels = c("No", "Yes"))
                , "Committee leadership" = factor(committee_leadership, levels = 0:1, labels = c("No", "Yes"))
                , "LES" = les
                , "Unopposed" = factor(unopposed, levels = 0:1, labels = c("No", "Yes"))
                ))
                



######
# Panel data models
######



# Make pdata.frame
vars0.pdf <- pdata.frame(vars0
                         , index = c("member_id", "Congress"))



# Simpler version of PageRank with random individual fixed effects
plm.pr1 <- plm(PageRank ~ 
                    Party.leadership 
                 + Committee.leadership 
                 # + Senioriy
                 + NOMINATE.Dim.1 
                 + NOMINATE.Dim.2 
                 + LES
                 + White 
                 + Bills.sponsored 
                 + Bills.cosponsored 
                 + Pct..votes.with.party
                 + Caucus.member
                 + Gender 
                 # + Press.release.count
                 + Speeches.daily
                 + Unopposed
                 + Majority
                 + Party
                 , data = vars0
                 , effect = "individual"
                 , model = "random"
                 , index = c("member_id", "Congress"))

# PageRank with all controls with random individual fixed effects
plm.pr2 <- plm(PageRank ~ 
                 Party.leadership 
               + Committee.leadership 
               + Senioriy
               + NOMINATE.Dim.1 
               + NOMINATE.Dim.2 
               + LES
               + White 
               + Bills.sponsored 
               + Bills.cosponsored 
               + Pct..votes.with.party
               + Caucus.member
               + Gender 
               + Press.release.count
               + Speeches.daily
               + Unopposed
               + Majority
               + Party
               , data = vars0
               , effect = "individual"
               , model = "random"
               , index = c("member_id", "Congress"))

# Simpler version of Relative PageRank with random individual fixed effects
plm.prRelative1 <- plm(PageRank.diff ~ 
                 Party.leadership 
               + Committee.leadership 
               # + Senioriy
               + NOMINATE.Dim.1 
               + NOMINATE.Dim.2 
               + LES
               + White 
               + Bills.sponsored 
               + Bills.cosponsored 
               + Pct..votes.with.party
               + Caucus.member
               + Gender 
               # + Press.release.count
               + Speeches.daily
               + Unopposed
               + Majority
               + Party
               , data = vars0
               , effect = "individual"
               , model = "random"
               , index = c("member_id", "Congress"))

# Relative PageRank with all controls with random individual fixed effects
plm.prRelative2 <- plm(PageRank.diff ~ 
                 Party.leadership 
               + Committee.leadership 
               + Senioriy
               + NOMINATE.Dim.1 
               + NOMINATE.Dim.2 
               + LES
               + White 
               + Bills.sponsored 
               + Bills.cosponsored 
               + Pct..votes.with.party
               + Caucus.member
               + Gender 
               + Press.release.count
               + Speeches.daily
               + Unopposed
               + Majority
               + Party
               , data = vars0
               , effect = "individual"
               , model = "random"
               , index = c("member_id", "Congress"))

# Simpler version of EigenCentrality with random individual fixed effects
plm.eigen1 <- plm(Eigencentrality ~ 
                         Party.leadership 
                       + Committee.leadership 
                       # + Senioriy
                       + NOMINATE.Dim.1 
                       + NOMINATE.Dim.2 
                       + LES
                       + White 
                       + Bills.sponsored 
                       + Bills.cosponsored 
                       + Pct..votes.with.party
                       + Caucus.member
                       + Gender 
                       # + Press.release.count
                       + Speeches.daily
                       + Unopposed
                       + Majority
                       + Party
                       , data = vars0
                       , effect = "individual"
                       , model = "random"
                       , index = c("member_id", "Congress"))

# Eigencentrality with all controls with random individual fixed effects
plm.eigen2 <- plm(Eigencentrality ~ 
                         Party.leadership 
                       + Committee.leadership 
                       + Senioriy
                       + NOMINATE.Dim.1 
                       + NOMINATE.Dim.2 
                       + LES
                       + White 
                       + Bills.sponsored 
                       + Bills.cosponsored 
                       + Pct..votes.with.party
                       + Caucus.member
                       + Gender 
                       + Press.release.count
                       + Speeches.daily
                       + Unopposed
                       + Majority
                       + Party
                       , data = vars0
                       , effect = "individual"
                       , model = "random"
                       , index = c("member_id", "Congress"))


summary(plm.pr1)
summary(plm.pr2)



#####################
# Create LaTeX table
texreg(l = list(plm.pr2, plm.prRelative2,  plm.eigen2)
       
       , custom.model.names = c("PageRank", "Relative PageRank", "Eigencentrality"),
       custom.coef.map = list(
         "Party.leadershipYes" = "Party leadership"
         , "Committee.leadershipYes" = "Committee leadership"         
         , "Senioriy" = "Seniority"         
         , "NOMINATE.Dim.1" = "NOMINATE Dim1"
         , "NOMINATE.Dim.2" = "NOMINATE Dim2"                    
         , "LES" = "LES"               
         , "WhiteYes" = "White"        
         , "Bills.sponsored" = "Bills sponsored"      
         , "Bills.cosponsored" = "Bills cosponsored"
         , "Pct..votes.with.party" = "Votes w/ party pct."       
         , "Caucus.memberYes" = "Caucus member"           
         , "GenderFemale"  = "Female"   
         , "Press.release.count" = "Press release count"         
         , "Speeches.daily" = "Speeches daily"
         , "UnopposedYes" = "Unopposed"            
         , "MajorityYes" = "Majority"        
         , "PartyRepublican" = "Republican"
       )
       , bold = 0.05
       , stars = numeric(0)
       # , custom.note = "Table shows the effect of member characteristics on their
       # likelihood to be central to their party. All models include individual random effects
       # to account for unobserved features such as a member's proclivity to discuss press releases with colleagues."
       , caption = "Linear Models of Member Influence Scores"
       , caption.above = TRUE
       , fontsize = "small"
       , custom.gof.rows = list("Random effects" = c("YES", "YES", "YES"))
       , float.pos = "h"
       )






###############################################################################
###############################################################################
# Network Figures
###############################################################################
###############################################################################

# This section is using first.obs data
# Here's a pre-run copy of it from Nov 11
first.obs.D <- readRDS(paste0(getwd(), "/Data/output/firstobs_D_Nov11"))
first.obs.R <- readRDS(paste0(getwd(), "/Data/output/firstobs_R_Nov11"))

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
    , trans_mod = "exponential"
    , p_value_cutoff = 0.1
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



###
# Plot: Improvements (gain in model fit) from each added edge
###
library(cowplot)
library(grid)
library(gridExtra)
library(egg)

# Create plot objects
impD113.pl <- plot(D113.network, type = "improvement") + theme(axis.title = element_blank()) + labs(subtitle = "113th Congress")
impD114.pl <- plot(D114.network, type = "improvement") + theme(axis.title = element_blank()) + labs(subtitle = "114th Congress")
impD115.pl <- plot(D115.network, type = "improvement") + theme(axis.title = element_blank()) + labs(subtitle = "115th Congress")
impD116.pl <- plot(D116.network, type = "improvement") + theme(axis.title = element_blank()) + labs(subtitle = "116th Congress")

impR113.pl <- plot(R113.network, type = "improvement") + theme(axis.title = element_blank()) + labs(subtitle = " ")
impR114.pl <- plot(R114.network, type = "improvement") + theme(axis.title = element_blank()) + labs(subtitle = " ")
impR115.pl <- plot(R115.network, type = "improvement") + theme(axis.title = element_blank()) + labs(subtitle = " ")
impR116.pl <- plot(R116.network, type = "improvement") + theme(axis.title = element_blank()) + labs(subtitle = " ")

###
# Create "master" axis grobs
###

# Master y axis
y.grobImprovement <- textGrob("Improvement", rot = 90)

# Master x axis
x.grobEdgeNum <- textGrob("Edge Number")

# Super axis - party
x.grobParty <- textGrob("Democrats                                  Republicans"
                        , gp = gpar(fontsize = 14, fontface = "bold"))

# Arrange axis grobs and plot objects
grid.arrange(arrangeGrob(impD113.pl, impR113.pl
                         , impD114.pl, impR114.pl
                         , impD115.pl, impR115.pl
                         , impD116.pl, impR116.pl
                         , ncol = 2
                         , left = y.grobImprovement, top = x.grobParty, bottom = x.grobEdgeNum))




###################################
# Plot: p-value from the Vuong test associated with each edge addition
###################################
# Create plot objects
vuongD113.pl <- plot(D113.network, type = "p-value") + theme(axis.title = element_blank()) + labs(subtitle = "113th Congress")
vuongD114.pl <- plot(D114.network, type = "p-value") + theme(axis.title = element_blank()) + labs(subtitle = "114th Congress")
vuongD115.pl <- plot(D115.network, type = "p-value") + theme(axis.title = element_blank()) + labs(subtitle = "115th Congress")
vuongD116.pl <- plot(D116.network, type = "p-value") + theme(axis.title = element_blank()) + labs(subtitle = "116th Congress")

vuongR113.pl <- plot(R113.network, type = "p-value") + theme(axis.title = element_blank()) + labs(subtitle = " ")
vuongR114.pl <- plot(R114.network, type = "p-value") + theme(axis.title = element_blank()) + labs(subtitle = " ")
vuongR115.pl <- plot(R115.network, type = "p-value") + theme(axis.title = element_blank()) + labs(subtitle = " ")
vuongR116.pl <- plot(R116.network, type = "p-value") + theme(axis.title = element_blank()) + labs(subtitle = " ")

###
# Create "master" axis grobs
###

# Master y axis
y.grobPValue <- textGrob("P-Value", rot = 90)

# Master x axis
x.grobEdgeNum <- textGrob("Edge Number")

# Super axis - party
x.grobParty <- textGrob("Democrats                                  Republicans"
                        , gp = gpar(fontsize = 14, fontface = "bold"))

# Arrange axis grobs and plot objects
grid.arrange(arrangeGrob(vuongD113.pl, vuongR113.pl
                         , vuongD114.pl, vuongR114.pl
                         , vuongD115.pl, vuongR115.pl
                         , vuongD116.pl, vuongR116.pl
                         , ncol = 2
                         , left = y.grobPValue, top = x.grobParty, bottom = x.grobEdgeNum))




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


# Get list of all ego networks in each congress, and induce them each into subgraphs
R114.egoList <- lapply(ego(R114.graph), function(x) induced_subgraph(R114.graph, x))
D116.egoList <- lapply(ego(D116.graph), function(x) induced_subgraph(D116.graph, x))

# Index for min/max influential in their respective ego lists
V(R114.graph)$name
which(V(R114.graph)$name == most_influential[1])

plot(R114.egoList[[3]])

# This occurs when the vertex attribute list does not contain a name entry, 
# but you can still set it using syntax of the form 
# vertex_attr(g, "name") <- c("vector", "of", "names") 
# optionally selecting a subset of vertices with the index argument, 
# which defaults to index = V(graph).

###
# Pull out specific graphs
###

# Most influential


component_distribution(R115.graph)
is_connected(R115.graph)
decompose(R115.graph, mode = "weak", min.vertices = 2)

tmp <- igraph::make_ego_graph(R115.graph, nodes = c("D000619"))
tmp2 <- induced_subgraph(R115.graph)
plot(tmp)





# Convert to graph
D113.graph <- igraph::graph_from_data_frame(d = D114.network[, 1:2])



##### TO DO Tuesday:
# Make ego-net graphs! Just pick two and make them, and be done with it
# You can always change them later, just do it and move on
# Probably move on to descriptive graphs of who is most/least influential
# Absolute pagerank - who has the most influence, regardless of if they are
# also being influenced




########################################################################
















##################################
# Look at bar charts to see if anything stands out as highly correlated (factors)

# Party leadership
dat %>%
  filter(!is.na(party_leadership)) %>% 
  ggplot( aes(x=eigen_100, fill=as.factor(party_leadership))) +
  geom_histogram( color="#e9ecef", alpha=0.6, position = 'identity', bins = 30) +
  scale_fill_manual(values=c("#69b3a2", "#404080")) +
  labs(fill="Party leadership", x = "Eigencentrality")

# Committee leadership
dat %>%
  filter(!is.na(committee_leadership)) %>% 
  ggplot( aes(x=eigen_100, fill=as.factor(committee_leadership))) +
  geom_histogram( color="#e9ecef", alpha=0.6, position = 'identity', bins = 30) +
  scale_fill_manual(values=c("#69b3a2", "#404080")) +
  labs(fill="Committee leadership", x = "Eigencentrality")

# Relationship with NOMINATE and centrality - null results
dat %>% 
  filter(party_code == 100) %>% 
  ggplot() +
  geom_point(aes(x = nominate_dim2, y = eigen_100), color = "blue") +
  geom_smooth(aes(x = nominate_dim2, y = eigen_100))



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













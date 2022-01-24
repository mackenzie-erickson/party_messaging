
# Step1_importEtc.R - Load data that was preliminarily cleaned in Python

# AUTHOR: Mackenzie Weiler
# REVISION HISTORY:
#   01/11/2021: Read in data


# General libraries
library(tidyverse)
library(dtplyr) # Translation to data.table
library(data.table)
library(ggplot2)
library(lubridate)
library(stringr)


# Specific to Step 1
library(feather) # To load feather data from Python
library(rjson)
library(jsonlite)



# Load data - Press Releases
file_path <- "~/OneDrive - The Ohio State University/Party_messaging/Data/Press_releases/Statements_All_df.feather"
full_data <- read_feather(file_path)


# Load data - ProPublica (uncleaned)
file_path_propub_unclean <- "~/OneDrive - The Ohio State University/Party_messaging/Data/Press_releases/ProPublica_Unclean.feather"
propub <- read_feather(file_path_propub_unclean)
# Detach java packages to help memory/java functionality
detach("package:feather", unload = TRUE)

# Load data - Legislators (https://github.com/unitedstates/congress-legislators)
leg_current <- fread("~/OneDrive - The Ohio State University/Party_messaging/Data/Legislator_info/legislators_current2022.csv")
leg_historical <- fread("~/OneDrive - The Ohio State University/Party_messaging/Data/Legislator_info/legislators_historical2022.csv")
leg_all <- rbind(leg_current, leg_historical)

# Load data - leadership positions
# Self-produced from https://history.house.gov/People/
leadership <- readxl::read_xlsx("~/OneDrive - The Ohio State University/Party_messaging/Data/Other/HouseLeadership113_117.xlsx", sheet = 1)

# Load data - Sessions of Congress dates
# Save for full data - merge based on date
sessionDates <- readxl::read_xlsx("~/OneDrive - The Ohio State University/Party_messaging/Data/Other/congressSessionDates113_117.xlsx", sheet = 1)





######################
# Wrangle - Full statement data
######################

# Add PR (press release) suffix to all variable names for clarity
full_data <- full_data %>% 
  setNames(paste0(names(.), "_pr"))

# Remove unnecessary variables and correct classes
full_data0 <- 
  full_data %>% 
  # Remove unnecessary variables
  select(-c(index_pr, id_pr)) %>% 
  # Correct classes
  mutate(date_pr = ymd(date_pr),
         congress_id_pr = as.numeric(congress_id_pr))


##############
# Merge - with Legislator info (more complete name information)
#############

# Create lazy DTs using dtplyr
full_data_DT <- lazy_dt(full_data0)
leg_all_DT <- lazy_dt(leg_all)

full_data_withBio <-
  left_join(full_data_DT, leg_all_DT,
            by = c("member_id_pr" = "bioguide_id"), ) %>% 
  as_tibble()







####################
# Wrange - ProPublica member data (by session)
#####################


# Flatten nested data
members_flat <-
  propub %>% 
  jsonlite::flatten()

# Clean up var names
members_flat <-
  members_flat %>% 
  rename_with(~ str_remove(., "roles."), starts_with("roles."))

# Filter to congresses 113:117
members113_117 <-
  members_flat %>% 
  filter(congress %in% c("113", "114", "115", "116", "117"))


####
# Join propub data with leadership data
###
# Remove current leadership role variable (incorrect/incomplete)
members113_117 <- 
  members113_117 %>% 
  select(-leadership_role)

# Merge with external leadership info (for the House only)
members113_117 <-
  members113_117 %>% 
  mutate(congress = as.integer(congress)) %>% 
  left_join(., select(leadership, # external leadership info
                      congress,
                      leadership_role,
                      member_id,
                      date_elected),
            by = c("id" = "member_id",
                   "congress" = "congress"), 
            keep = FALSE)


####
# Clean up members113_117 for merging with PR data
####

# Remove committee and subcommittee nested tables
members113_117_clean0 <-
  members113_117 %>% 
  select(-c(committees, subcommittees))

# Remove unnecessary variables
members113_117_clean1 <-
  members113_117_clean0 %>% 
  select(-c(
    index,
    id,
    url,
    times_topics_url,
    youtube_account,
    google_entity_id,
    rss_url,
    phone,
    senate_class,
    title,
    short_title,
    last_updated,
    fax,
    contact_form, 
    times_tag, 
    ocd_id, 
    office, 
    state_rank
  ))

# Correct variable classes
members113_117_clean2 <-
  members113_117_clean1 %>% 
  # as Date
  mutate(across(c(
    date_of_birth,
    most_recent_vote,
    end_date,
    start_date,
    date_elected
    )
    , ymd),
    # as Numeric
    across(c(
      next_election,
      missed_votes_pct,
      seniority  
    )
    , as.numeric))







####################
# Merge - Statements with Member Info
###################

# Create lazy DTs using dtplyr
full_data_DT <- lazy_dt(full_data_withBio)
members113_117_DT <- lazy_dt(members113_117_clean2)

# Merge 
merged_data <-
  left_join(full_data_DT, members113_117_DT,
            by = c("member_id_pr" = "member_id",
                   "congress_id_pr" = "congress")) %>% 
  as_tibble()

# Clean up variable names
merged_data0 <-
  merged_data %>% 
  rename(member_id = member_id_pr,
         congress = congress_id_pr,
         leadership_dateElected = date_elected,
         term_startDate = start_date,
         term_endDate = end_date)



# Remove extra name columns (keeping only the complete data from leg_all)
merged_data1 <-
  merged_data0 %>% 
  select(-c(
    # Names from leg_all are most complete
    member_name_pr, first_name.y, middle_name.y, last_name.y, suffix.y,
            # Districts from PR data are most complete
            gender.y, district.x, district.y, state_pr, state.y,
    # party_pr is party at the time, party.x(from leg_all) is most recent party
    party.y, current_party,
    # Remove versions with more missingness
    lis_id.y, govtrack_id.y, icpsr_id.y, cspan_id.y, votesmart_id.y))

# Rename for clarity
merged_data2 <-
  merged_data1 %>% 
  rename(first_name = "first_name.x",
         middle_name = "middle_name.x",
         last_name = "last_name.x",
         suffix = "suffix.x",
         gender = "gender.x",
         district = district_pr,
         state = "state.x",
         party_atPR = party_pr,
         current_party = "party.x", # current party as of Jan 2018 (from Github)
         lis_id = "lis_id.x",
         govtrack_id = "govtrack_id.x",
         icpsr_id = "icpsr_id.x",
         cspan_id = "cspan_id.x",
         votesmart_id = "votesmart_id.x"
         ) 





# # Save a copy
# saveRDS(merged_data2, "~/OneDrive - The Ohio State University/Party_messaging/Data/Press_releases/StatementsWithBioClean_18jan2022.RDS")



###############################################################
###############################################################
# START HERE:
###############################################################
###############################################################
merged_data2 <- readRDS("~/OneDrive - The Ohio State University/Party_messaging/Data/Press_releases/StatementsWithBioClean_18jan2022.RDS")


# Simplify to immediately relevant variables
simple_df <-
  merged_data2 %>% 
  select(member_id, congress, title_pr, date_pr, title_pr, body_pr, url_pr,
         party_atPR, current_party, state, district,
         last_name, first_name, middle_name, suffix, full_name, birthday,
         gender, type, leadership_role, leadership_dateElected,
         at_large, bills_cosponsored, bills_sponsored,
         dw_nominate, cook_pvi, term_startDate, term_endDate, votes_against_party_pct,
         votes_with_party_pct, next_election)

# Standardize 'party' variables
# Party_PR contains the party at the time of publication
# Party_x contains the most up to date current party

# Standardize party naming convention -  function
stand_party_func <- function(x){
  new_value = ifelse((x == "D" | x == "Democrat"), "D",
                     ifelse((x == "I" | x == "ID" | x == "IND" | x == "Independent"), "ID",
                            ifelse((x == "R" | x == "REP" | x == "Republican"), "R",
                                   ifelse(x == "Libertarian", "LIB",
                                          # else
                                          x
                                   ))))
} # end party function

# Standardize party naming convention - Apply function
simple_df <-
  simple_df %>% 
  lazy_dt() %>% 
  mutate(across(c(party_atPR, current_party),
                stand_party_func)) %>% 
  as_tibble()
###################################


# Add in Congress session dates for my own reference
sessionDatesWide <- 
  sessionDates %>% 
  pivot_wider(names_from = congress_session,
              values_from = c(session_startDate, session_endDate)) %>% 
  rename(session_1_startDate = session_startDate_1,
         session_1_endDate = session_endDate_1,
         session_2_startDate = session_startDate_2,
         session_2_endDate = session_endDate_2)

# Merge with simple_df
simple_df_sessionDates <-
  left_join(lazy_dt(simple_df), lazy_dt(sessionDatesWide),
            by = c("congress" = "congress")) %>% 
  as_tibble()
  


# Dates - remove observations where the PR date > session 2 end date
  # Where PR data < session 1 start date, they're usually right before (like day or two)
  # So keep them for now, if they're between election (Nov 8) and start of session
simple_df_correctDates <-
  simple_df_sessionDates %>% 
  lazy_dt() %>% 
  filter(date_pr < session_2_endDate) %>% 
  filter(date_pr > mdy(paste0("11-08-", as.character(year(session_1_startDate) - 1)))) %>% 
  as_tibble()



# Plots to look at frequency of press releases and check dates, etc
# For each member, plot frequency of PRs by month,
# And assign title (e.g Zoe Lofgren (D): 113-116)

# List of unique member ids
unique_member_ids <- unique(simple_df_correctDates$member_id)

# For loop to plot 5 at a time
for (mem in sample(unique_member_ids, 5, replace = FALSE)){

  # Plot title - paste full_name, party, congress tenure
  mytitle <- paste0(
    (simple_df_correctDates %>% 
      filter(member_id == mem) %>% 
      select(full_name) %>% 
      unique() %>% 
      pull()
     ), "(",
    (simple_df_correctDates %>% 
       filter(member_id == mem) %>% 
       select(party_atPR) %>% 
       unique() %>% 
       pull()
    ),
    "):",
    (simple_df_correctDates %>% 
      filter(member_id == mem) %>% 
      filter(congress == min(congress, na.rm = TRUE)) %>% 
      select(congress) %>% 
      unique() %>% 
      pull()), "-",
    (simple_df_correctDates %>% 
       filter(member_id == mem) %>% 
       filter(congress == max(congress, na.rm = TRUE)) %>% 
       select(congress) %>% 
       unique() %>% 
       pull()))
  
  (simple_df_correctDates %>% 
    filter(member_id == mem) %>% 
    ggplot() +
    geom_histogram(aes(x = date_pr), binwidth = 30) + # monthly (30 days)
    ggtitle(mytitle)) %>% 
    print()
}


####
# Average PRs per month
####

# Group by member and month; count PRs per month
avg_monthly_df <-
    simple_df_correctDates %>%
    group_by(member_id,
             month = lubridate::floor_date(date_pr, "month")) %>% 
    summarize(member_id = unique(member_id),
              current_party = unique(current_party),
              first_name = unique(first_name),
              last_name = unique(last_name),
              full_name = unique(full_name),
              PRs_per_month = sum(length(date_pr))) %>% 

# Summarize into monthly average by member
    summarize(member_id = unique(member_id),
              current_party = unique(current_party),
              first_name = unique(first_name),
              last_name = unique(last_name),
              full_name = unique(full_name),
              avg_monthly_pr = mean(PRs_per_month)) %>% 
  ungroup()


# Frequency distributions of monthly averages

# All
ggplot(avg_monthly_df) +
  geom_histogram(aes(x = avg_monthly_pr), 
                 bins = 50,
                 fill = "white",
                 color = "black")

# Fewer than 50/month - current_party
ggplot(filter(avg_monthly_df, avg_monthly_pr < 50)) +
  geom_density(aes(x = avg_monthly_pr, color = current_party))


# Who publishes the most frequently?
avg_monthly_df %>% 
  slice_max(avg_monthly_pr, n = 10)



###### TO DO:
# Time to start on pre-processing!
# Everything else is cleaned up - time start cleaning the text :-O!










  
  
  
  
  
  
  































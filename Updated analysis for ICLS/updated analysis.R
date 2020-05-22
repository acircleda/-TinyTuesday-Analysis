
# Pacakges ----
  # uncomment and install as needed
# install.packages("tidyverse")
# install.packages("rtweet")
# install.packages("gsheet")

# Load Packages ----
library(tidyverse)
library(rtweet)
library(gsheet)
library(lubridate)

# Access TAGS data from GoogleSheets ----

tags_data <- gsheet2tbl("https://docs.google.com/spreadsheets/d/1yucj-9lR-yaac2A2tuUCPcURZ90-adEKl87mlhdzSMk/edit#gid=400689247") 

# Use statuses from TAGS to get full Twitter dataset ----
twitter_data <- lookup_statuses(tags_data$id_str)

# use data to develop a list of possible participants to contact ----
possible_participants <- twitter_data %>%
  filter(is_retweet == FALSE) %>% #select initial tweets not retweets
  filter(!is.na(ext_media_url)) %>% #select individuals who contributed an image
  mutate(year = year(created_at)) %>%
  group_by(year) %>%
  mutate(week = week(created_at),
         year_week = paste0(year, " ", week)) %>%
  ungroup() %>%
  group_by(screen_name) %>%
  summarize(n = n()) %>%
    arrange(desc(n)) %>%
  filter(n > 6)
  
  
filtered_twitter_data <- twitter_data %>%
  filter(is_retweet == FALSE) %>% #select initial tweets not retweets
  filter(!is.na(ext_media_url)) %>%
  filter(screen_name %in% possible_participants$screen_name) %>%
  mutate(year = year(created_at)) %>%
  group_by(year) %>%
  mutate(week = week(created_at),
         year_week = paste0(year, " ", week)) %>%
  ungroup() %>%
  unnest_wider(ext_media_url, names_sep = "_") %>%
  janitor::clean_names() %>%
  mutate(profile = paste0("http://www.twitter.com/", screen_name)) %>%
  group_by(screen_name) %>%
  mutate(tweet_number = row_number()) %>%
  select(screen_name, profile, tweet_number, created_at, year, week, year_week, text, ext_media_url_1, ext_media_url_2, ext_media_url_3, ext_media_url_4, status_id)
  
write.csv(filtered_twitter_data, file="Updated analysis for ICLS/Tidy Tuesday Participants.csv")


# Questions we discussed ----

# How much code are you sharing?
# Sophistication of code?
  # Tidycode
  # Code Linting
# Visualizations
  # Create an app for rating in Shiny
    # Repurpose existing app?
    # Tidycode classifier?
  # Permission required
  # Choose most representative
  # Criteria of judgement
  # inlcusion criteria
  # sustained number of weeks ~ 6 months
# Survey

# Tasks
  # IDing sample of participants - Micahel / Anthony
  # Criteria of judgement of visualizations - Josh
    # Epistemic considerations
  # Shiny App - ?
  
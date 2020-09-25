
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

# get list of participants we chose for inclusion
selected_participants <- gsheet2tbl("https://docs.google.com/spreadsheets/d/1uEbxH83Ww-AJtZl4M_XJubbqg9P_nRyhlVOwf5bcJOQ/edit#gid=1191274417") %>%
  filter(`Include?` == "Include") %>%
  filter(`Agree to Participate` == "Agree") %>%
  select(screen_name)


# get files ready for dl
participant_images <- gsheet2tbl("https://docs.google.com/spreadsheets/d/1uEbxH83Ww-AJtZl4M_XJubbqg9P_nRyhlVOwf5bcJOQ/edit#gid=821913014") %>%
  filter(screen_name %in% selected_participants$screen_name) %>%
  select(screen_name, tweet_number:week, ext_media_url_1:status_id) %>%
  group_by(screen_name) %>%
  pivot_longer(ext_media_url_1:ext_media_url_4,
               names_to="media", values_to="url") %>%
  drop_na(url) %>%
  mutate(
    filename=paste0("survey/images/", screen_name, "-", tweet_number, "-", basename(url)),
    stat = case_when(
      tweet_number == 1 ~"1",
      tweet_number == 2 ~"2",
      tweet_number == round(median(tweet_number), digits=0)-1 ~"3",
      tweet_number == round(median(tweet_number), digits=0) ~"4",
      tweet_number == max(tweet_number)-1 ~ "5",
      tweet_number == max(tweet_number) ~ "6"
    )) %>%
  drop_na(stat)

sample <- participant_images

# download images

# for (i in 1:length(sample$url)){
#   download.file(sample$url[i], destfile =  sample$filename[i], mode = 'wb')
# }
# 
# # if interrupted or other error:
# `%notin%` <- Negate(`%in%`)
# file_list <- data.frame(files = list.files())
# 
# remain <- selected_participants %>%
#   filter(filename %notin% file_list$files)

for (i in 1:length(sample$url)){
  
  skip_to_next <- FALSE
  
  tryCatch(
  download.file(sample$url[i], destfile =  sample$filename[i], mode = 'wb'), error = function(e) { skip_to_next <<- TRUE})
  
  if(skip_to_next) { next }     
}


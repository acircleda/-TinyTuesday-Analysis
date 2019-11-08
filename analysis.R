# analysis.r

library(tidyverse)
library(lubridate)
library(ggplot2)
library(rtweet)

## authenticate via access token

t <- read_tsv("https://raw.githubusercontent.com/nsgrantham/tidytuesdayrocks/master/data/tweets.tsv")
t$status_id <- str_split(t$status_url, "/") %>%
   map_chr(~ .[[6]])

s <- lookup_statuses(t$status_id, token = token)
 
write_rds(s, "processed-tweets.rds")

pre <- read_rds("processed-tweets.rds")

flat <- flatten(pre)

#get number of tweets by username over 2 tweets
user_tweet_counts <- flat %>% 
  filter(!is_retweet) %>% 
  count(screen_name) %>% 
  arrange(desc(n)) %>%
  subset(n > 2)

#subset of data
tweets <- flat %>% select(
  screen_name, created_at, text, urls_expanded_url, ext_media_url, favorite_count, retweet_count, is_retweet
) %>%
  mutate(
    date = as_date(created_at),
    week(date)
  ) %>% filter(
    !is_retweet, #not a retweet
    screen_name %in% user_tweet_counts$screen_name, #tweets > 2
    str_detect(ext_media_url, 'http') #a tweet with media is likely a contribution
  ) %>%
  subset(screen_name != "thomas_mock" | screen_name != "R4DScommunity" | screen_name != "tidypod") #this line not working - exclude these as they are the same person


#count how many weeks contributed per user (over 2 contributions MIN).
by_week <- tweets %>% 
  group_by(screen_name, `week(date)`) %>% 
  count(`week(date)`) %>% 
  ungroup() %>% 
  group_by(screen_name) %>% 
  mutate(x = row_number()) %>% 
  summarise(max = max(x)) %>% 
  arrange(desc(max))

#dowload images organized into folders by username, week-number as prefix, for anyone over n contributions

#########################################

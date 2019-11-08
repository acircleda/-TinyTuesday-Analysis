# analysis.r

library(tidyverse)
library(lubridate)
library(ggplot2)

pre <- read_rds("processed-data/processed-data.rds")

pre <- pre %>% unnest(urls_expanded_url)

shorteners <- c("bit.ly", "ow.ly", "buff.ly", "goo.gl", "ln.is", "tinyurl.com", "share.es", "ht.ly", "fb.me", "wp.me", "ift.tt")

# flat <- flatten(pre)

#get number of tweets by username over 2 tweets
user_tweet_counts <- pre %>% 
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

# process URLs

pre <- pre %>% 
  mutate(domains = urltools::domain(d$urls_expanded_url),
         need_to_expand = domains %in% shorteners)

pre_expanded <- pre %>% 
  mutate(processed_url = ifelse(need_to_expand == 1, expand_urls(urls_expanded_url), urls_expanded_url))

pre_expanded <- pre_expanded %>% 
  mutate(domain = domain(urls_expanded_url),
         is_github_link = str_detect(domain, "github.com") & !str_detect(domain, "gist"),
         is_gist_link = str_detect(domain, "gist.github.com"))

write_rds(pre_expanded, "/processed-data/processed_data_with_processed_urls.rds")

#dowload images organized into folders by username, week-number as prefix, for anyone over n contributions

#########################################

# analysis.r

library(tidyverse)
library(lubridate)
library(ggplot2)
library(urltools)
library(longurl)

pre <- read_rds("processed-data/processed-data.rds")

pre <- pre %>% unnest(urls_expanded_url)

# flat <- flatten(pre)

#get number of tweets by username over 2 tweets
user_tweet_counts <- pre %>% 
  filter(!is_retweet) %>% 
  count(screen_name) %>% 
  arrange(desc(n)) %>%
  subset(n > 10)

user_tweet_counts

#subset of data
tweets <- pre %>% select(
  screen_name, status_url, created_at, text, urls_expanded_url, ext_media_url, favorite_count, retweet_count, is_retweet
) %>%
  mutate(
    date = as_date(created_at),
    week(date)
  ) %>% filter(
    !is_retweet, #not a retweet
    screen_name %in% user_tweet_counts$screen_name, #tweets > 2
    # the warning is because this seems to be a list-column; but, this still seems to work
    str_detect(ext_media_url, 'http') #a tweet with media is likely a contribution
  ) %>%
  # can the following line be a filter?
  filter(screen_name != "thomas_mock" | screen_name != "R4DScommunity" | screen_name != "tidypod") #this line not working - exclude these as they are the same person

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

shorteners <- c("bit.ly", "ow.ly", "buff.ly", "goo.gl", "ln.is", "tinyurl.com", "share.es", "ht.ly", "fb.me", "wp.me", "ift.tt")

pre <- pre %>% 
  mutate(domains = urltools::domain(urls_expanded_url),
         need_to_expand = domains %in% shorteners)

# this takes about 5 minutes to run
expanded_urls <- pre %>% 
  filter(need_to_expand) %>% 
  pull(urls_expanded_url) %>% 
  unique() %>% 
  expand_urls()

pre_expanded <- pre %>% 
  mutate(index = 1:nrow(.)) %>% 
  rename(orig_url = urls_expanded_url) %>% 
  left_join(expanded_urls) %>% 
  mutate(processed_url = ifelse(need_to_expand == 1, expanded_url, orig_url),
         domain = domain(processed_url),
         is_github_link = str_detect(domain, "github.com") & !str_detect(domain, "gist"),
         is_gist_link = str_detect(domain, "gist.github.com")) %>% 
  select(status_id, status_url, screen_name, created_at, processed_url, is_github_link, is_gist_link)

write_csv(pre_expanded, "processed-data/processed_urls.csv")

#dowload images organized into folders by username, week-number as prefix, for anyone over n contributions

tweets <- tweets %>% 
  unnest(ext_media_url) %>% 
  mutate(url_name = paste0(screen_name, " - ", `week(date)`, " - ", ext_media_url))

#url1
for (i in 1:length(tweets$url1)){
  download.file(tweets$url1[i], destfile =  tweets$url1_name[i], mode = 'wb')
}

#url2
for (i in 1:length(tweets$url2)){
  download.file(tweets$url2[i], destfile =  tweets$url2_name[i], mode = 'wb')
}


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


# Possible Q's:
#   Who's contributing the most?
#   What % of contributors contribte repeatedly?
#   What % of contributors sustain contribution over time?
#   Does code become more sophisticated?
#   Do visualizations become "better"?
#   Which visuals get the most likes each week, and why?
#   
  
#dowload images organized into folders by username, week-number as prefix, for anyone over n contributions

tweets <- tweets %>% separate(ext_media_url, into = c("url1", "url2"), sep = " ") %>% mutate(
  url1_name = paste0(screen_name, " - ", week, " - ", basename(url1)),
  url2_name = paste0(screen_name, " - ", week, " - ", basename(url2))
)

#url1
for (i in 1:length(tweets$url1)){
  download.file(tweets$url1[i], destfile =  tweets$url1_name[i], mode = 'wb')
}

#url2
for (i in 1:length(tweets$url2)){
  download.file(tweets$url2[i], destfile =  tweets$url2_name[i], mode = 'wb')
}

# analysis.r

library(tidyverse)
library(lubridate)
library(ggplot2)
library(rtweet)
library(urltools)
library(longurl)
library(splitstackshape)
library(stringr)
library(gghighlight)


data <- read_rds("processed-data/processed-data.rds")

pre <- data %>% unnest(urls_expanded_url)

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
  select(status_id, status_url, screen_name, created_at, processed_url)

write_csv(pre_expanded, "processed-data/processed_urls.csv")

# process data for descriptives

tweets <- flatten(data)

tweets <- data %>% mutate(
  ext_media_url_copy = ext_media_url) %>% #cSplit removes this column, but I need it later to subset correctly
  cSplit("ext_media_url", sep=" ") #using unnest adds ~3000 more rows, which may throw off row counts later

#get number of tweets by username over 2 tweets
user_tweet_counts <- tweets %>% 
  filter(!is_retweet) %>% 
  count(screen_name) %>% 
  arrange(desc(n)) %>%
  subset(n > 2) %>%
  filter(screen_name != "thomas_mock", 
         screen_name != "R4DScommunity",
         screen_name != "tidypod")

#subset of data
subset_tweets <- tweets %>% select(
  screen_name, created_at, text, favorite_count, retweet_count, is_retweet, ext_media_url_copy, ext_media_url_1, ext_media_url_2, ext_media_url_3, ext_media_url_4) %>%
  mutate(
    date = as_date(created_at),
    week = week(date),
    ext_media_url_1 = str_remove_all(ext_media_url_1, 'c\\("|",'),
    ext_media_url_2 = str_remove_all(ext_media_url_2, '"|"\\)|,"'),
    ext_media_url_3 = str_remove_all(ext_media_url_3, '"|"\\)|,"'),
    ext_media_url_4 = str_remove_all(ext_media_url_4, '"|"\\)|,"')
        ) %>% 
  filter(
    !is_retweet, #not a retweet
    screen_name %in% user_tweet_counts$screen_name,
    str_detect(ext_media_url_copy, 'http'))


#count how many weeks contributed per user (over 2 contributions MIN).
by_week <- subset_tweets %>% 
  group_by(screen_name, week) %>% 
  count(week) %>% 
  ungroup() %>% 
  group_by(screen_name) %>% 
  mutate(x = row_number()) %>% 
  summarise(total_weeks = max(x)) %>% 
  arrange(desc(total_weeks))


summary(user_tweet_counts) # tweets: mean 8.79 median 5
summary(by_week) #participation weeks: mean 6.278 median 4

weekly_contribution <- subset_tweets %>% group_by(week) %>% count(screen_name) %>% left_join(by_week, by ="screen_name")

#line graph with top 5 contrbutors highlighted
ggplot(weekly_contribution) +
  geom_line(aes(x=week, y=n, group=screen_name, color=screen_name))+
  gghighlight(max(total_weeks), max_highlight = 5, unhighlighted_colour = "grey80")

#line graph with top 10 contrbutors highlighted
ggplot(weekly_contribution) +
  geom_line(aes(x=week, y=n, group=screen_name, color=screen_name))+
  gghighlight(max(max), max_highlight = 10, unhighlighted_colour = "grey80")

# weekly_contribution_mean <- weekly_contribution %>% group_by(screen_name) %>% summarize(mean_weekly_contibution = mean(n))
# summary(weekly_contribution_mean)


#dowload images organized by username, week-number as prefix, for anyone over n contributions

tweets_pivot <- subset_tweets %>% group_by(screen_name) %>% pivot_longer(ext_media_url_1:ext_media_url_4, names_to = "media_num", values_to = "url") %>% mutate(
  filename=paste0(screen_name, "-", week, "-", basename(url))
  ) %>% drop_na(url)


for (i in 1:length(tweets_pivot$url)){
  download.file(tweets_pivot$url[i], destfile =  tweets_pivot$filename[i], mode = 'wb')
}
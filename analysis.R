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
    ext_media_url_4 = str_remove_all(ext_media_url_4, '"|"\\)|,"')) %>% 
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


n_distinct(data$user_id) #1,231 unique contributors
n_distinct(data$status_id) #4,418 unique tweets
n_distinct(subset_tweets$ext_media_url_copy) #2,224 possible contributions

#contributed in the first 4 weeks
month_first <- subset_tweets %>% filter(week < 5) %>% select(screen_name) %>% group_by(screen_name) %>% count(screen_name)

#contributed in the middle 5 weeks
month_mid <- subset_tweets %>% filter(week > 23 | week < 29 ) %>% select(screen_name) %>% group_by(screen_name) %>% count(screen_name)

#contributed in the last 4 weeks
month_last<- subset_tweets %>% filter(week > 48) %>% select(screen_name)  %>% group_by(screen_name) %>% count(screen_name)

contributors_first_last <- month_first %>% inner_join(month_last, by = "screen_name")

contributors_first_mid_last <- contributors_first_last %>% inner_join(month_mid, by = "screen_name")

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


#dowload images organized by username, week-number as prefix, for anyone over n contributions

tweets_pivot <- subset_tweets %>% group_by(screen_name) %>% pivot_longer(ext_media_url_1:ext_media_url_4, names_to = "media_num", values_to = "url") %>% mutate(
  filename=paste0(screen_name, "-", week, "-", basename(url)) %>% drop_na(url)

for (i in 1:length(tweets_pivot$url)){
  download.file(tweets_pivot$url[i], destfile =  tweets_pivot$filename[i], mode = 'wb')
}

#likes and retweets
subset_tweets <- subset_tweets %>% mutate(
  pop = retweet_count + favorite_count)

#who has the most popular tweets
likes <- subset_tweets %>% group_by(screen_name) %>% summarize(likes = sum(pop)) %>%
  arrange(desc(likes))

#top 25 contributors with the most likes
top_25 <- likes %>% top_n(25)

#top 17 contributors who are top likes
contributors_first_mid_last %>% inner_join(likes, by = "screen_name")

#which prolific #TT contributors have the most likes?
contributors_first_mid_last %>% inner_join(top_25, by ="screen_name") %>% arrange(desc(likes)) %>% view()

#which tweets are the most popular
likes %>% arrange(desc(likes)) %>% view()

ggplot(subset_tweets) +
  geom_line(aes(x=week, y=pop))

#do likes follow retweets?
ggplot(subset_tweets) +
  geom_line(aes(x=week, y=favorite_count), color="blue")+
  geom_line(aes(x=week, y=retweet_count), color="red")

#likes seem to be more common than retweets


#are likes and retweets correlated?
cor(subset_tweets$retweet_count, subset_tweets$favorite_count, method = c("pearson"))
#.95 strong correlation



# #random sample for content analysis
# sample <- sample_n(subset_tweets, 50)
# write.csv(sample, "50 random tweets.csv")
# 
# samples <- bind_rows(replicate(10, subset_tweets %>% select(text) %>% sample_n(50), simplify=F), .id="Sample")
# samples$Sample <- as.numeric(samples$Sample)
# 
# samples %>% filter(Sample == 1) %>% write.csv("50-random-tweets-1.csv")
# samples %>% filter(Sample == 2) %>% write.csv("50-random-tweets-2.csv")
# samples %>% filter(Sample == 3) %>% write.csv("50-random-tweets-3.csv")
# samples %>% filter(Sample == 4) %>% write.csv("50-random-tweets-4.csv")
# samples %>% filter(Sample == 5) %>% write.csv("50-random-tweets-5.csv")

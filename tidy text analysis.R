#text analysis
library(tidyverse)
library(lubridate)
library(splitstackshape)
library(stringr)
library(tidytext)
library(textdata)

data <- read_rds("processed-data/processed-data.rds")

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
    str_detect(ext_media_url_copy, 'http')) %>%
  select(screen_name, text)

clean_tweets <- subset_tweets %>% mutate(
  clean = str_replace_all(text,"#[a-z,A-Z]*","")
)

tidy_tweets <- clean_tweets %>% unnest_tokens(word, text) %>% anti_join(stop_words)

top_words <- tidy_tweets %>% count(word, sort = T)

#add to stopwords
stop <- c("t.co", "https", "tidytuesday", "rstats", "code", "week", "r4ds")

stop <- as_tibble(stop)
stop <- stop %>% rename(word = value)

stop_words_new <- stop_words %>% left_join(stop, by="word")

tidy_tweets <- clean_tweets %>% unnest_tokens(word, clean) %>% anti_join(stop_words_new)

top_words <- tidy_tweets %>% count(word, sort = T)

ngram2 <- tidy_tweets %>% unnest_tokens(bigram, text, token="ngrams", n=2) %>% count(bigram, sort = T)

ngram3 <- subset_tweets %>% unnest_tokens(bigram, text, token="ngrams", n=3) %>% count(bigram, sort = T)

ngram5 <- subset_tweets %>% unnest_tokens(bigram, text, token="ngrams", n=5) %>% count(bigram, sort = T)

nrc <- get_sentiments("nrc")

tidy_tweets %>% inner_join(nrc, by = "word")

tidy_tweets %>% 
  sentistrength
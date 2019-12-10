library(tidyverse)
library(tidycode)

td <- read_rds("processed-data/processed-data.rds")
tweet_data <- read_csv("processed-data/processed_urls.csv")
mapping <- read_csv("mapping.csv")
tidycode_output <- read_rds("processed-data/processed-tidycode-data.rds")

# tweets

nu <- td %>% 
  filter(!is_retweet) %>% 
  count(screen_name) %>% 
  arrange(desc(n)) %>% 
  filter(n > 1) # change back to 10

tds <- td %>% 
  semi_join(nu)

tds <- tds %>% 
  left_join(select(tweet_data, status_url, is_github_link, processed_url), by = "status_url") %>% 
  mutate(day = lubridate::round_date(created_at, unit = "day"),
         is_github_link = ifelse(is.na(is_github_link), FALSE, is_github_link))

tds %>% 
  count(screen_name, day, is_github_link, status_url) %>% 
  ggplot(aes(x = day, y = n, shape = is_github_link)) +
  geom_point() +
  facet_wrap(~screen_name)

# code

tidycode_output$on_disk <- str_split(tidycode_output$file, "/") %>% 
  map_chr(~.[[6]])

mapping$on_disk <- str_split(mapping$on_disk, "|") %>% 
  map_chr(~.[[3]])

tidycode_output <- tidycode_output %>% 
  anti_join(tidycode::get_stopfuncs()) %>% 
  inner_join(get_classifications(lexicon = "crowdsource", include_duplicates = FALSE)) %>% 
  left_join(mapping) %>% 
  rename(processed_url = url)

small_out <- tidycode_output %>% 
  distinct(processed_url, classification, func, .keep_all = TRUE)

small_out_n <- small_out %>% 
  count(processed_url, classification) %>% 
  group_by(processed_url) %>% 
  mutate(prop = n / sum(n)) %>% 
  spread(classification, n) %>% 
  mutate_if(is.numeric, replace_na, 0) %>% 
  distinct()

small_out_p <- small_out %>% 
  count(processed_url, classification) %>% 
  group_by(processed_url) %>% 
  mutate(prop = n / sum(n)) %>% 
  mutate(total_n_funcs = sum(n)) %>% 
  select(-n) %>% 
  spread(classification, prop) %>% 
  mutate_if(is.numeric, replace_na, 0) %>% 
  distinct()

# Merging

sum_tab <- tds %>% 
  left_join(small_out_p) %>% 
  filter(!is.na(communication)) %>% 
  group_by(screen_name) %>% 
  arrange(created_at) %>% 
  mutate(tweet_num = row_number()) %>% 
  ungroup() %>% 
  select(screen_name, status_url, created_at, favorite_count, retweet_count, tweet_num, total_n_funcs, communication:visualization) %>% 
  mutate(recognitions = favorite_count + retweet_count) %>% 
  select(-favorite_count, -retweet_count) %>% 
  mutate_if(is.numeric, replace_na, 0) %>% 
  arrange(screen_name, tweet_num) %>% 
  select(tweet_num, total_n_funcs, recognitions, everything())

small_out_p %>% 
  ungroup() %>%
  summarize_if(is.numeric, mean)

sum_tab %>% 
  select_if(is.numeric) %>% 
  as.matrix() %>% 
  Hmisc::rcorr()

write_csv(sum_tab, "summary-table-top-10-tweeters.csv")

to_keep <- sum_tab %>% 
  count(screen_name) %>% 
  arrange(desc(n)) %>% 
  filter(!(screen_name %in% c("jakekaupp", "geomaramanis", "thomas_mock"))) %>% 
  filter(n > 10)

sum_tab %>% 
  semi_join(to_keep) %>% 
  ggplot(aes(x = created_at, y = total_n_funcs)) +
  geom_point() +
  facet_wrap(~screen_name)

sum_tab %>% 
  semi_join(to_keep) %>% 
  mutate(tweet_num_s = scale(tweet_num)) %>% 
  group_split(screen_name) %>% 
  map(select_if, is.numeric) %>% 
  map(corrr::correlate)

# allison_horst:
# -

# geokaramanis
# - 

# kigtembu
# - more communication, exporting, and importing over time, less clecaning, less exploring

# othomn
# - 
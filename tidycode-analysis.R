# tidycode-analysis

library(tidycode)
library(tidyverse)

# dir <-"/Users/jrosenb8/github-content-scraper/cache"
# 
# full_dir <- list.files(dir, full.names = T)
# 
# full_dir <- full_dir[tools::file_ext(full_dir) == "R"]
# 
# l <- full_dir %>%
#   map_df(possibly(read_rfiles, data.frame(file = ., expr = NA, line = NA)))
# 
# f <- function(x) {
#   NA
# }
# 
# is_null <- function(x) {
#   x == "NULL"
# }
# 
# bool <- l$expr %>% 
#   modify_if(is.language, f) %>% 
#   is.na()
# 
# ll <- l[bool, ] %>% 
#   filter(expr != "NULL") %>% 
#   unnest_calls(expr)
# 
# write_rds(ll, "processed-data/processed-tidycode-data.rds")
#
# lll <- ll %>% 
#   anti_join(get_stopfuncs())
# 
# lll %>% 
#   inner_join(get_classifications("crowdsource", include_duplicates = FALSE)) %>%
#   select(func, classification) %>% 
#   count(classification) %>% 
#   arrange(desc(n)) %>% 
#   mutate(prop = n / sum(n))

tidycode_output <- read_rds("processed-data/processed-tidycode-data.rds")
tidycode_output$on_disk <- str_split(tidycode_output$file, "/") %>% map_chr(~.[[6]])

tidycode_output <- tidycode_output %>% 
  anti_join(tidycode::get_stopfuncs()) %>% 
  left_join(get_classifications())

tweet_data <- read_csv("processed-data/processed_urls.csv")

mapping <- read_csv("mapping.csv")

mapping <- mapping %>% 
  rename(processed_url = url) %>% 
  distinct(processed_url, .keep_all = TRUE)

tweet_data %>% 
  semi_join(mapping) %>% 
  left_join(mapping) %>% 
count(screen_name) %>% 
  arrange(desc(n)) %>% 
  filter(n > 10)

u <- tweet_data %>% 
  semi_join(mapping) %>% 
  left_join(mapping) %>% 
  filter(str_detect(processed_url, ".R")) %>% 
  count(screen_name) %>% 
  arrange(desc(n)) %>% 
  filter(n > 10)

analyze_me <- tweet_data %>% 
  left_join(mapping) %>% 
  filter(str_detect(processed_url, ".R")) %>% 
  semi_join(u)

tidycode_output %>% 
  right_join(analyze_me)
  count(url, classification)

tweet_data <- rename(tweet_data, url = processed_url)



# tidycode_s <- tidycode_output %>% 
#   left_join(mapping) %>% 
#   count(url, classification)
# 
# proc <- tidycode_s %>% 
#   group_by(url) %>% 
#   summarize(total_n = sum(n)) %>% 
#   right_join(tidycode_s) %>% 
#   spread(classification, n)
# 
# dd <- tweet_data %>% 
#   left_join(proc)
# 
# dd <- dd %>% 
#   select(status_url, total_n:`<NA>`)


pre <- td %>% 
  left_join(dd) %>% 
  group_by(screen_name) %>% 
  arrange(created_at) %>% 
  mutate(tweet_num = row_number())

pre %>% 
  group_by(screen_name) %>% 
  filter(any(tweet_num>5)) %>% 
  select(screen_name, tweet_num, communication:visualization) %>% 
  mutate_if(is.numeric, scale) %>% 
  ungroup() %>% 
  select(-screen_name) %>% 
  corrr::correlate()

tweet_
td %>% count(screen_name, )

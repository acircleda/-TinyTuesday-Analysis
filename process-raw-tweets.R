# process-raw-tweets

library(tidyverse)
library(rtweet)

t <- read_tsv("data/tweets.tsv") # this file comes from this repo: https://github.com/nsgrantham/tidytuesdayrocks
t$status_id <- str_split(t$status_url, "/") %>%
  map_chr(~ .[[6]])

s <- lookup_statuses(t$status_id, token = token)

write_rds(s, "processed-data/processed-data.rds")
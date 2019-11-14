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

tweet_data <- read_csv("processed-data/processed_urls.csv")


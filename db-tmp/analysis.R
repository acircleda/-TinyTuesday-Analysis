# reanalysis

library(tidyverse)
library(ggplot2)
library(tidycode)
library(matahari)
load("db-tmp/cleaned database.RData")

more_than_one <- final_set %>%
  group_by(username) %>%
  count() %>%
  filter(n > 1)

raw <- final_set %>%
  filter(username %in% more_than_one$username) %>%
  group_by(username) %>%
  mutate(tweet_num = row_number(),
         content = as.character(content))

tmp<-final_set$content[1] %>% paste((collapse = '\n'))
save(tmp, file="tmp.r")


(d <- read_rfiles(
  tidycode_example("example_plot.R"),
  tidycode_example("example_analysis.R")
))

library(tidyverse)
library(tidycode)
library(lubridate)
library(gghighlight)

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
  mutate(total_n_funcs = sum(n)) %>% 
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

# Overall descriptives

# means

sum_tab %>% 
  ungroup() %>%
  summarize_if(is.numeric, mean) %>% View()

# corrs

sum_tab %>% 
  select_if(is.numeric) %>% 
  mutate_if(is.numeric, scale) %>% 
  as.matrix() %>% 
  Hmisc::rcorr()

# write_csv(sum_tab, "summary-table-top-10-tweeters.csv")

to_keep <- sum_tab %>% 
  count(screen_name) %>% 
  arrange(desc(n)) %>% 
  filter(!(screen_name %in% c("jakekaupp", "geomaramanis", "thomas_mock"))) %>% 
  filter(n > 10)

# sum_tab %>% 
#   semi_join(to_keep) %>% 
#   ggplot(aes(x = created_at, y = total_n_funcs)) +
#   geom_point() +
#   facet_wrap(~screen_name)

split_tab <- sum_tab %>% 
  semi_join(to_keep) %>% 
  mutate(tweet_num_s = scale(tweet_num)) %>% 
  group_split(screen_name) %>% 
  map(select_if, is.numeric)

sum_tab <- tds %>% 
  left_join(small_out_n) %>% 
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

sum_tab_s <- sum_tab %>% 
  semi_join(to_keep) 

sum_tab_s %>% 
  inner_join(sum_tab_s) %>% 
  mutate(screen_name= factor(screen_name, labels = str_c("Individual ", toupper(c("a", "b", "c", "d"))))) %>% 
  filter(screen_name == "Individual D")

sum_tab_s %>%   

  mutate(tweet_num_s = scale(tweet_num)) %>% 
  gather(key, val, communication:visualization)  %>% 
  group_by(screen_name, tweet_num) %>% 
  arrange(screen_name, tweet_num) %>% 
  mutate(row_num = row_number()) %>% 
  mutate(first_recognition = ifelse(row_num == 1, recognitions, NA)) %>% 
  mutate(key = tools::toTitleCase(key)) %>% 
  ungroup() %>% 
  ggplot(aes(x = tweet_num, y = val, group = key, fill = key, label = first_recognition)) +
  geom_text(size = 2.5, position = "stack", vjust = -1) +
  geom_col() +
  ggthemes::scale_fill_calc(NULL) +
  theme_bw() +
  xlab("Tweet Number") +
  ylab("Number of Functions in Code") +
  labs(caption = "The value above each bar is for the number of recognitions (favorites and retweets) the Tweet received")

ggsave(width = 10, height = 10, "tweets-by-user.png")

split_tab %>% 
  map(select_if, is.numeric) %>% 
  map(summarize_if, is.numeric, mean)

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
#####################################Follow-up Analyses
# Questions to answer:
#   1. Do users' overall functions increase?
#     Compare average of first three and average of last three.
#     Compute averages across all users)
#   2. Do specific functions increase over time per user/overall?
#     Above analysis per function
#   3. Do vizzes with more code/less code correlate to likes?
#     Regression model with likes as DV, functions overall as IV
#     Regression model with likes as DV, each function as IV


#number of functions per week overall (not by user)
n_of_functions_overall <- tds %>% left_join(small_out_n) %>% 
  select(screen_name, status_url, created_at, favorite_count, retweet_count, total_n_funcs, communication:visualization) %>% 
  mutate(week = week(created_at)) %>% 
  pivot_longer(communication:visualization, names_to = "function_type", values_to = "function_count") %>% 
  group_by(screen_name, week, function_type) %>% 
  summarize(sum = sum(function_count)) %>% 
  group_by(week, function_type) %>% 
  mutate_if(is.numeric, replace_na, 0) %>% 
  summarize(function_count = sum(sum)) %>% 
  arrange(week, function_type) #%>%
  # group_by(function_type) #%>% 
  # mutate(lag = lag(function_count),
  #        pct.change = (function_count - lag) / lag) %>%
  # select(week, function_type, pct.change) #%>% 
  # pivot_wider(names_from = week, values_from = pct.change)

#functions over time faceted by type - general decrease in functions
  ggplot(n_of_functions_overall)+
  geom_point(aes(x=week, y=function_count))+
  geom_smooth(method='lm', aes(x=week, y=function_count))+
    facet_wrap(~function_type)
  
#number of total functions overall
n_of_functions_overall %>% group_by(week) %>% 
  summarize(sum = sum(function_count)) %>%
  ggplot()+
  geom_line(aes(x=week, y=sum))
  


#per user total functions/week - not disaggregated by function type
per_user_total_functions <- tds %>% left_join(small_out_n) %>% 
  select(screen_name, status_url, created_at, favorite_count, retweet_count, total_n_funcs, communication:visualization) %>% 
  mutate(week = week(created_at)) %>% 
  pivot_longer(communication:visualization, names_to = "function_type", values_to = "function_count") %>% 
  group_by(screen_name, week) %>% 
  summarize(sum = sum(function_count)) %>%
  ungroup() %>%
  arrange(week) %>%
  pivot_wider(names_from = week, values_from = sum)

#find first two weeks of data and last two weeks of data per user

per_user_total_functions[, c("first", "second")] <- t(apply(per_user_total_functions[, 2:50], 1, 
                                              function(x) c(x[which(x != "")[1]], x[which(x != "")[2]])))

per_user_total_functions[, c("last", "second_to")] <- t(apply(per_user_total_functions[, 50:2], 1, 
                                                                     function(x) c(x[which(x != "")[1]], x[which(x != "")[2]])))

first_and_last_two <- per_user_total_functions %>% 
  select(screen_name, first, second, last, second_to) %>% 
  drop_na %>% 
  rowwise() %>% 
  mutate(mean1 = mean(c(first,second)),
         mean2 = mean(c(last,second_to))) %>%
  pivot_longer("mean1":"mean2", names_to = "time", values_to = "mean") %>% select(screen_name, time, mean)

#Quick RM Anova of first_and_last_two differences
summary(aov(mean ~ time, data = first_and_last_two))

#no significant differences between time points for first and last (n = 40 observations)

#following https://www.r-bloggers.com/how-to-do-repeated-measures-anovas-in-r/

summary(aov(function_count ~ function_type * week + Error(function_type), data=n_of_functions_overall))
library(lmerTest)
fit <- lmer(function_count ~ function_type * week + (1|function_type), data=n_of_functions_overall)
anova(fit)

get_contrasts(fit, "function_type") #can't get contrasts by week?

##How does proporition of code change over time? (overall by function)
prop_overall <- n_of_functions_overall %>% group_by(week) %>% mutate(
  prop = prop.table(function_count))

ggplot(prop_overall)+
  geom_point(aes(x=week, y=prop, color=function_type))+
  geom_smooth(method='lm', aes(x=week, y=prop))+
  facet_wrap(~function_type)

ggplot(prop_overall)+
  coord_flip()+
  geom_bar(aes(x=week, y=prop, fill=function_type), stat="identity", position="fill")+
  scale_x_reverse()

#########using sum_tab

ggplot(sum_tab)+
  geom_bar(aes(x=tweet_num, y=mean(total_n_funcs)), stat="identity")

##proportion of code analysis by tweet number not week number
#stacked 100% bar
sum_tab %>% pivot_longer(communication:visualization, names_to="function_type", values_to = "function_count") %>% group_by(tweet_num, function_type) %>%
  summarize(mean = mean(function_count)) %>%
  mutate(prop = prop.table(mean)) %>%
  ggplot()+
  geom_bar(aes(x=tweet_num, y=prop, fill=function_type), stat="identity", position="fill")

#faceted line chart
sum_tab %>% pivot_longer(communication:visualization, names_to="function_type", values_to = "function_count") %>% group_by(screen_name, tweet_num, function_type) %>%
  summarize(mean = mean(function_count)) %>%
  mutate(prop = prop.table(mean)) %>%
  ggplot()+
  geom_line(aes(x=tweet_num, y=prop, color=function_type))+
  facet_wrap(~function_type)+
  geom_smooth(method='lm', aes(x=tweet_num, y=prop))


##proportions of code for first five and last five tweets
sum_tab %>% pivot_longer(communication:visualization, names_to="function_type", values_to = "function_count") %>% group_by(tweet_num, function_type) %>%
  summarize(sum = mean(function_count)) %>%
  mutate(prop = prop.table(sum)) %>%
  select(tweet_num, function_type, prop) %>%
  pivot_wider(names_from = tweet_num, values_from = prop) %>%
  rowwise() %>%
  mutate(firstfive = mean(c(`1`,`2`,`3`,`4`,`5`)),
         lastfive = mean(c(`31`,`32`,`33`,`34`,`35`))
         ) %>% select(function_type, firstfive, lastfive) %>%
  pivot_longer(firstfive:lastfive) %>% ggplot()+
  geom_bar(aes(x=name, y=value, fill=function_type), stat="identity")

#first five and last five by mean functions overall count
sum_tab %>% pivot_longer(communication:visualization, names_to="function_type", values_to = "function_count") %>% group_by(tweet_num) %>%
  summarize(sum = mean(total_n_funcs)) %>% 
  pivot_wider(names_from = tweet_num, values_from = sum) %>% rowwise() %>%
  mutate(firstfive = mean(c(`1`,`2`,`3`,`4`,`5`)),
         lastfive = mean(c(`31`,`32`,`33`,`34`,`35`))
  ) %>% select(firstfive, lastfive) %>%
  pivot_longer(firstfive:lastfive) %>% ggplot()+
  geom_bar(aes(x=name, y=value), stat="identity")


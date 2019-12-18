library(tidyverse)
library(tidycode)
library(lubridate)
data <- read.csv("summary-table-all.csv")

#length of code
data %>%
  pivot_longer(communication:visualization, names_to = "function_type", values_to = "function_count") %>%
  group_by(tweet_num) %>%
  summarize(mean = mean(function_count)) %>% #average length of code
  ggplot(aes(x=tweet_num, y=mean))+
  geom_col()

#function trends by tweet number overall
data %>%
  pivot_longer(communication:visualization, names_to = "function_type", values_to = "function_count") %>%
  group_by(tweet_num, function_type) %>%
  summarize(mean = mean(function_count)) %>% #mean n per function
  ggplot()+
  geom_line(aes(x=tweet_num, y=mean))+
  geom_smooth(method='lm', aes(x=tweet_num, y=mean))+
  facet_wrap(~function_type)

#proportion of code by tweet number overall
data %>%
  pivot_longer(communication:visualization, names_to = "function_type", values_to = "function_count") %>%
  group_by(screen_name, tweet_num, function_type) %>%
  summarize(sum = sum(function_count)) %>%
  mutate(prop = prop.table(sum)) %>%
  ggplot()+
  geom_bar(aes(x=tweet_num, y=prop, fill=function_type), stat="identity", position="fill")

#proportion trends
data %>%
  pivot_longer(communication:visualization, names_to = "function_type", values_to = "function_count") %>%
  group_by(screen_name, tweet_num, function_type) %>%
  summarize(sum = sum(function_count)) %>%
  mutate(prop = prop.table(sum)) %>%
  ggplot()+
  geom_line(aes(x=tweet_num, y=prop))+
  geom_smooth(method='lm', aes(x=tweet_num, y=prop))+
    facet_wrap(~function_type)

#number of functions per week overall (not by user)
n_of_functions_overall <- tds %>% left_join(small_out_n) %>% 
  select(screen_name, status_url, created_at, favorite_count, retweet_count, total_n_funcs, communication:visualization) %>% 
  mutate(week = week(created_at)) %>% 
  pivot_longer(communication:visualization, names_to = "function_type", values_to = "function_count") %>% 
  group_by(screen_name, week, function_type) %>% 
  summarize(sum = sum(function_count)) %>% 
  group_by(week, function_type) %>% 
  mutate_if(is.numeric, replace_na, 0) %>% 
  summarize(function_count = mean(sum)) %>% 
  arrange(week, function_type)

#functions over time faceted by type - general decrease in functions
ggplot(n_of_functions_overall)+
  geom_point(aes(x=week, y=function_count))+
  geom_smooth(method='lm', aes(x=week, y=function_count))+
  facet_wrap(~function_type)

#number of total functions overall
n_of_functions_overall %>% group_by(week) %>% 
  summarize(mean = mean(function_count)) %>%
  ggplot()+
  geom_line(aes(x=week, y=mean))



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
  geom_bar(aes(x=tweet_num, y=prop, fill=function_type), stat="identity", position="stack")

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

rm_data <- sum_tab %>%
  mutate(
    tweet = paste0("tweet",tweet_num)
  ) %>% group_by(screen_name, tweet_num) %>%
  summarize(function_count = sum(total_n_funcs)) %>%
  pivot_wider(names_from = tweet_num, values_from = function_count)


rm_data[, c("first", "second")] <- t(apply(rm_data[, 2:32], 1, 
                                           function(x) c(x[which(x != "")[1]], x[which(x != "")[2]])))

rm_data[, c("last", "second_to")] <- t(apply(rm_data[, 35:3], 1, 
                                             function(x) c(x[which(x != "")[1]], x[which(x != "")[2]])))

rm_data <- rm_data %>% select(screen_name, first, second, last, second_to) %>%
  rowwise() %>%
  mutate(`1` = mean(c(first, second)),
         `2` = mean(c(last, second_to))
  ) %>%
  select(screen_name, `1`, `2`) %>% drop_na

rm_data <- tibble::rowid_to_column(rm_data, "ID")

rm_data <- rm_data %>% pivot_longer(`1`:`2`)


mean1 <- rm_data$`1`
mean2 <- rm_data$`2`

t.test(mean1, mean2, var.equal = TRUE)



# reanalysis

library(tidyverse)
library(ggplot2)
library(tidycode)
library(ggtext)
load("tidy-tuesday_data.Rdata")

#prep raw data ----
# load("db-tmp/cleaned database.RData")
# tidycode_output <- read_rds("processed-data/processed-tidycode-data.rds")
# 
# # import matahari? tibble
# tidycode_output$on_disk <- str_split(tidycode_output$file, "/") %>%
#   map_chr(~.[[6]])
# 
# #make sure there are at least 2 tweets per user
# more_than_one <- final_set %>%
#   group_by(username) %>%
#   count() %>%
#   filter(n > 1)
# 
# 
# #filter final set into a filtered set
# filtered_set <- final_set %>%
#   filter(username %in% more_than_one$username) %>%
#   group_by(username) %>%
#   mutate(tweet_num = row_number(),
#          on_disk = paste0(username, "|", repo, "|", path),
#          on_disk = str_replace_all(on_disk, "/", "__"))
# 

## set up dictionary ----
# get_classifications(lexicon = "crowdsource", include_duplicates = FALSE) %>%
#   write.csv("tiycode dictionary.csv")
# 
# #unmatched - add these to TidyDictionary
# unmatched <- tidycode_output %>%
#   anti_join(tidycode::get_stopfuncs()) %>%
#   anti_join(get_classifications(lexicon = "crowdsource", include_duplicates = FALSE)) %>%
#   group_by(func) %>% count() %>%
#   write.csv("tidycode_unmatched.csv")
# 
# #use: mutate_each(funs(empty_as_na))
# 
# empty_as_na <- function(x){
#   if("factor" %in% class(x)) x <- as.character(x) ## since ifelse wont work with factors
#   ifelse(as.character(x)!="", x, NA)
# }
# 
# dictionary<-read.csv("db-tmp/tiycode dictionary.csv") %>% 
#   filter(classification != "other") %>% 
#   rbind(read.csv("db-tmp/tidycode_unmatched.csv") %>%
#           distinct(func, .keep_all = T)) %>%
#   mutate(classification = 
#            as.character(classification)) %>%
#   mutate_each(funs(empty_as_na))

# tidycode_processed <- tidycode_output %>%
#   anti_join(tidycode::get_stopfuncs()) %>%
#   inner_join(dictionary)
# 
# github_data <- filtered_set %>% left_join(tidycode_processed) %>%
#   select(id, username, tweet_num, on_disk, line, func, classification )
# 
# save(github_data, tidycode_output, final_set, file="tidy-tuesday_data.Rdata")

## ANALYSIS BEGINS ----

## individual contributors ----
github_data %>% distinct(username) %>% summarize(n = n()) %>% count()

#   91 users

## unique contributions ----
github_data %>% 
  filter(username != "jkaupp") %>%
  distinct(username, tweet_num, .keep_all = T) %>%
  count() %>% summary()

#   min 2 / max 236 / mean 11 / median 5

## function examples

func_example <-  function(name) {
github_data %>% ungroup() %>%
  filter(classification == name) %>%
  select(func) %>%
  sample_n(10)
}

func_example("data cleaning")
func_example("visualization")
func_example("communication")
func_example("import")
func_example("export")
func_example("exploratory")
func_example("evaluation")
func_example("modeling")
func_example("setup")

##anti-join

## average number of functions per contribution ----
github_data %>% 
  filter(username != "jkaupp") %>%
  group_by(username, tweet_num) %>%
  count() %>%
  ungroup() %>%
  group_by(tweet_num) %>%
  summarize(mean = mean(n)) %>%
  ggplot()+
  geom_bar(aes(x=tweet_num, y=mean, fill=mean), stat="identity")+
  geom_smooth(method='lm', aes(x=tweet_num, y=mean), se = FALSE, color="black")+
  scale_fill_distiller(palette = "Blues", direction = 1)+
  scale_y_continuous(position = "right")+
  xlab("Tweet Number")+
  labs(title = "Average Number of Functions Over Time",
       subtitle = "Pearson r = .54, p<.001")+
  theme_void()+
  theme(
    axis.text = element_text(size=15),
    axis.title.x = element_text(size=15),
    legend.position = "none",
    plot.title = element_text(size=25),
    plot.subtitle = element_text(size=18)
  )

## correlation between tweetnumber and mean functions ----
cor_data<-github_data %>% 
  filter(username != "jkaupp") %>%
  group_by(username, tweet_num) %>%
  count() %>%
  ungroup() %>%
  group_by(tweet_num) %>%
  summarize(mean = mean(n))

cor.test(cor_data$tweet_num, cor_data$mean)

#   r = .533, p < .001

## mean number of functions over time ----

github_data %>% 
  filter(username != "jkaupp") %>%
  filter(classification != "import" &
           classification != "export" &
           classification != "setup") %>%
  group_by(username, tweet_num, classification) %>%
  count() %>%
  ungroup() %>%
  drop_na %>%
  group_by(tweet_num, classification) %>%
  summarize(mean = mean(n))%>%
  ggplot()+
  geom_bar(aes(x=tweet_num, y=mean, fill=classification), stat="identity", position=position_fill())+
  scale_fill_viridis_d(option = "magma")

github_data %>% 
  filter(username != "jkaupp") %>%
  filter(classification != "import" &
           classification != "export" &
           classification != "setup") %>%
  group_by(username, tweet_num, classification) %>%
  count() %>%
  ungroup() %>%
  drop_na %>%
  group_by(tweet_num, classification) %>%
  summarize(mean = mean(n)) %>%
  ggplot()+
  geom_line(aes(x=tweet_num, y=mean))+
  geom_smooth(method="lm", aes(x=tweet_num, y=mean))+
  facet_wrap(~classification)

#   communication has been increasing
#   data cleaning has creased but recently lots of increase
#   data visualization is of course steady or increasing

## proporition of functions over time ----

github_data %>% 
  filter(username != "jkaupp") %>%
  group_by(username, tweet_num, classification) %>%
  count() %>%
  ungroup() %>%
  group_by(username, tweet_num) %>%
  drop_na %>%
  mutate(prop = prop.table(n)) %>%
  ungroup() %>%
  group_by(tweet_num, classification) %>%
  summarize(mean = mean(prop))%>%
  ggplot()+
  geom_bar(aes(x=tweet_num, y=mean, fill=classification), stat="identity", position=position_fill())+
  scale_fill_viridis_d(option = "magma")

github_data %>% 
  filter(username != "jkaupp") %>%
  filter(classification == "communication" |
           classification == "data cleaning" | classification == "visualization") %>%
  group_by(username, tweet_num, classification) %>%
  count() %>%
  ungroup() %>%
  group_by(username, tweet_num) %>%
  drop_na %>%
  mutate(prop = prop.table(n)) %>%
  ungroup() %>%
  group_by(tweet_num, classification) %>%
  summarize(mean = mean(prop)) %>%
  ggplot()+
  geom_bar(aes(x=tweet_num, y=mean), stat="identity", fill="#eeeeee", size=1)+
  geom_smooth(se = FALSE, method="lm", aes(x=tweet_num, y=mean), color="#4D8DC9")+
  facet_wrap(~classification, ncol=1)+
  theme_void()+
  theme(
    strip.text = element_text(size=20)
  )

#   communication has been increasing
#   data cleaning has creased but recently lots of increase
#   data visualization is of course steady

# 


        
library(tidyverse)
library(readxl)
`%notin%` <- Negate(`%in%`)

data <- read.csv("github-database.csv") %>%
  mutate(path = as.character(path)) %>%
  select(-date_added) %>%
  rowid_to_column("id")

## get year data ----
year <- data %>%
  mutate(
  year = case_when(
    str_detect(path, "2018") ~ "2018",
    str_detect(path, "2019") ~ "2019",
    TRUE ~ "Other"
  )
) %>% filter(year != "Other")

##step get year/week data ----
year_week <- year %>% mutate(
  week_num_ext = str_extract(path, "week[0-9]+")
) %>%
  drop_na(week_num_ext) %>% mutate(
    week = str_extract(week_num_ext, "[0-9]+")
  )

#this is near complete - export to Excel for finalization
write.csv(year_week, "db-tmp/year_week.csv")

#final year/week ----
year_week <- read_excel("db-tmp/year_week.xlsx") %>% select(-`...1`)

##step 2: parse year data

year_no_week <-year %>% filter(id %notin% year_week$id)
write.csv(year_no_week, "db-tmp/year_no_week.csv")

year_no_week <- read_excel("db-tmp/year_no_week.xlsx") %>% select(-`...1`)

## final year/week 2 ----
year_week2 <- year_no_week %>% drop_na(week) %>%
  select(-date, -topic, -other_date)

##final files for ymd and dmy
ymd <- year_no_week %>% drop_na(date) %>%
  select(-week, -topic, -other_date) %>%
  mutate(date2 = ymd(date),
         week = week(date2)) %>%
  select(-date2, -date)

dmy <- year_no_week %>% drop_na(other_date) %>%
  select(-week, -topic, -date) %>%
  mutate(date2 = dmy(other_date),
         week = week(date2)) %>%
  select(-date2, -other_date)

## other data ----
other<-data %>%
  mutate(
    year = case_when(
      str_detect(path, "2018") ~ "2018",
      str_detect(path, "2019") ~ "2019",
      TRUE ~ "Other"
    )
  ) %>% filter(year == "Other")
write.csv(other, "db-tmp/other.csv")

other<- read_excel("db-tmp/other.xlsx") %>% select(-`...1`)

## final files ----
other_year_week <- other %>% select(-topic, -ymd, -mdy) %>% drop_na(week)

other_mdy <- other %>% select(-topic, -ymd, -week, -year) %>%
  drop_na(mdy) %>%
  mutate(date2 = mdy(mdy),
         week = week(date2),
         year = year(date2)) %>%
  select(-mdy, -date2)

other_ymd <- other %>% select(-topic, -mdy, -week, -year) %>%
  drop_na(ymd) %>%
  mutate(date2 = ymd(ymd),
         week = week(date2),
         year = year(date2)) %>%
  select(-ymd, -date2)

other_topic <- other %>% select(-ymd, -mdy, -week, -year) %>%
  mutate(topic = tolower(topic)) %>%
  drop_na(topic) %>%
  mutate(date = case_when(
         str_detect(topic, "train") ~ "2019-09",
         str_detect(topic, "fifa") ~ "2018-11",
         str_detect(topic, "policing") ~ "2019-12",
         str_detect(topic, "earnings") ~ "2019-10",
         str_detect(topic, "emperor") ~ "2019-33",  
         str_detect(topic, "bird") ~ "2019-18",
         str_detect(topic, "video") ~ "2019-31",
         str_detect(topic, "meteor") ~ "2019-24",
         str_detect(topic, "media") ~ "2019-27",
         str_detect(topic, "nobel") ~ "2019-20",
         str_detect(topic, "horror") ~ "2019-43",
         str_detect(topic, "nuclear") ~ "2019-34",
         str_detect(topic, "squirrel") ~ "2019-44",
         str_detect(topic, "lift") ~ "2019-41",
         str_detect(topic, "thanks") ~ "2018-34",
         str_detect(topic, "bridges") ~ "2018-35",
         str_detect(topic, "ufo") ~ "2019-26",
         str_detect(topic, "ramen") ~ "2019-23",
         str_detect(topic, "wildife") ~ "2019-30",
         str_detect(topic, "r4ds") ~ "2019-29",
         str_detect(topic, "wwc") ~ "2019-28",
         str_detect(topic, "unesco") ~ "2019-19",
         str_detect(topic, "transistor") ~ "2019-36",
         str_detect(topic, "anime") ~ "2019-17",
         str_detect(topic, "mortgage") ~ "2019-19",
         str_detect(topic, "moore") ~ "2019-36",
         str_detect(topic, "plastic") ~ "2019-21",
         str_detect(topic, "simpsons") ~ "2019-35",
         str_detect(topic, "student") ~ "2019-19",
         str_detect(topic, "cup") ~ "2019-28",
         str_detect(topic, "hurricane") ~ "2018-12",
         str_detect(topic, "space") ~ "2019-3",
         str_detect(topic, "seattle") ~ "2019-14",
         str_detect(topic, "tennis") ~ "2019-15",
         str_detect(topic, "fuel") ~ "2019-42",
         str_detect(topic, "mortality") ~ "2018-03",
         str_detect(topic, "bike") ~ "2018-10",
         TRUE ~ "Other")
         ) %>%
  filter(date != "Other") %>%
  select(-topic)

##create final set ----
set1 <- rbind(year_week, year_week, ymd, dmy, other_mdy, other_ymd, other_year_week) %>%
  mutate(date = paste0(year, "-", week)) %>%
  select(-year, -week)

final_set <- rbind(set1, other_topic)
save(final_set, file="db-tmp/cleaned database.RData")

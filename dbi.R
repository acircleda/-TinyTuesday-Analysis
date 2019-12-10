library(RSQLite)
library(DBI)
library(matahari)
library(tidyverse)

mydb <- dbConnect(RSQLite::SQLite(), "content.sqlite")

# dbDisconnect(mydb)

l <- dbGetQuery(mydb, "SELECT url, username, repo, path, content FROM content;")

ll <- l %>% 
  filter(str_ends(repo, ".r"))

map2()

my_dance_recital <- function(x, i) {
  message(i)
  dance_recital(x)
}

ll <- map2(l$content, .f = possibly(my_dance_recital, NA), .y = 1:length(l$content))

u <- unnest_calls(m, expr)

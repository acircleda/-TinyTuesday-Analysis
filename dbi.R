library(RSQLite)
library(DBI)

mydb <- dbConnect(RSQLite::SQLite(), "content.sqlite")

# dbDisconnect(mydb)

l <- dbGetQuery(mydb, "SELECT url, content FROM content;")


library(tidyverse)
library(rtweet)
# whatever name you assigned to your created app
appname <- "Anthony Teacher Feed"

## api key (example below is not a real key)
key <- "RI2Edm4hm3rEqRYXkVSKw"

## api secret (example below is not a real key)
secret <- "zwA58Iag9oZ5qnZNnmqBbpvPSw3iFfPxTyD7s02M"

access_token <- "285614027-9eCvQLT6MlEIh8BngPkEPmHLQBxRiOgkwyIuu4lo"


access_secret <- "xbJmOBuffVeOzENi16yZBO22Ado49gnzb3haONpnd4"

# create token named "twitter_token"
twitter_token <- create_token(
  app = appname,
  consumer_key = key,
  consumer_secret = secret,
  access_token = access_token,
  access_secret = access_secret)

tidy_tweets <- search_tweets(q = "#TidyTuesday", n=15000, retryonratelimit = TRUE)

write.csv(tidy_tweets, "tidy_tweets.csv")
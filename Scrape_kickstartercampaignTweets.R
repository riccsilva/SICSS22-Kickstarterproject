#install.packages("rtweet")

# load rtweet

library(rtweet)
library(readr)
library(dplyr)
library(tidyverse)

# create credentials as objects
# you need to replace them with your own.

api_key <- "REo4hjWP28HDg2bKDW5WvfIAV"
api_secret_key <- "gdRzTEmFQsnw6Nyw43IoKcZF4B2etRhBG7PMjTDIsg6EbgHUoJ"
access_token <- "842584099-5nlByQXA0WfmsKUHCGAOatg7k0QJc2D9f81kxYeM"
access_secret <- "VhVuy3VYk5bOdtKCVbXpH23f2SsBpbssk87gPEgkDQm1q"

#df<-read.csv("/Users/ricardocsilva/Desktop/SICSS/tabletop_games_sample_june.csv", header=TRUE)

token <- create_token(
  app = "SICSS_Lisbon_2022_RCS",
#  app = "TwitterAppSICSS",
  consumer_key = api_key,
  consumer_secret = api_secret_key,
  access_token = access_token,
  access_secret = access_secret
  )

#covid_19_tweets<-search_tweets("coronavirus", n=100)

#kickstarter_tweets = search_tweets("kickstarter", n=50000)

kickstarter_tweets_en = search_tweets("kickstarter", 
                                    n=15000, 
                                    type="recent",
                                    include_rts = FALSE,
                                    retryonratelimit = TRUE,
                                    lang='en'
                                    )

colnames(kickstarter_tweets_en)

kickstarter_tweets_en<-kickstarter_tweets_en[c('created_at','screen_name','text','favorite_count', 'retweet_count', 'hashtags')]

save(kickstarter_tweets_en, file = "tweets_kickstarter.Rdata")
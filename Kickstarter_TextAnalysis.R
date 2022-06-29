# Installing and importing libraries
# https://sicss.io/2020/materials/day3-text-analysis/basic-text-analysis/rmarkdown/Basic_Text_Analysis_in_R.html#text-pre-processing

#install.packages("tidytext")
#install.packages("tm")
#install.packages("SnowballC")
#install.packages("topicmodels")
#install.packages("reshape2")
#install.packages("doParallel")

library(tidyverse)
library(ggplot2)
library(tidytext)
library(dplyr)
library(tidyr)
library(tm)
library(SnowballC)
library(topicmodels)

#df<-read.csv("/Users/patriciamachado/Desktop/tabletop_games_june.csv", header=TRUE)

load(file = "/Users/ricardocsilva/Downloads/tweets_kickstarter.Rdata")

lapply(mylist, function(x) write.table( data.frame(x), 'tweets_kickstarter.csv'  , append= T, sep=',' ))

#just_tweets<-kickstarter_tweets_en[c(3)]
#just_ids<-kickstarter_tweets_en[c(1)]

#write.csv(just_ids,'/Users/ricardocsilva/Downloads/ids_kickstarter.csv')

tweets<-kickstarter_tweets_en
tweets

#tweets<-tweets[c('created_at','screen_name','text','favorite_count', 'retweet_count', 'hashtags')]

tidy_tweets<- tweets %>%
  select(created_at,screen_name,text) %>%
  unnest_tokens("word", text)
head(tidy_tweets)

#Counting frequency of most common words
tidy_tweets %>%
  count(word) %>%
  arrange(desc(n))

#I am importing and applying the typical stopwords list here

corpus <- Corpus(VectorSource(as.vector(tweets$text))) 
corpus <- tm_map(corpus, removeWords, stopwords("english"))

corpus

data("stop_words")
tidy_tweets<-tidy_tweets %>%
  anti_join(stop_words)

tidy_tweets %>%
  count(word) %>%
  arrange(desc(n))

#I am creating and applying another stopwords list here
word <- c("https", "t.co", "kickstarter", "check", "amp")
lexicon <- c("SMART", "SMART", "SMART", "SMART", "SMART")

other_words <- data.frame(word, lexicon)

tidy_tweets<-tidy_tweets %>%
  anti_join(other_words)

tidy_tweets %>%
  count(word) %>%
  arrange(desc(n))

#normalization and transformation of text
corpus <- tm_map(corpus, content_transformer(removePunctuation))
corpus <- tm_map(corpus, content_transformer(removeNumbers))
tidy_tweets<-tidy_tweets[-grep("\\b\\d+\\b", tidy_tweets$word),]
corpus <- tm_map(corpus,  content_transformer(tolower)) 
corpus <- tm_map(corpus, content_transformer(stripWhitespace))
tidy_tweets$word <- gsub("\\s+","",tidy_tweets$word)

corpus  <- tm_map(corpus, content_transformer(stemDocument), language = "english")

#Snowball stemming
tidy_tweets<-tidy_tweets %>%
  mutate_at("word", funs(wordStem((.), language="en")))

kickstarter_DTM <- DocumentTermMatrix(corpus, control = list(wordLengths = c(2, Inf)))

inspect(kickstarter_DTM[1:5,3:8])

tidy_kickstarter_DTM<-
  tidy_tweets %>%
  count(created_at, word) %>%
  cast_dtm(created_at, word, n)

tidy_kickstarter_DTM

# Text preprocessing ends here

# --------------------------------

# Topic Modelling starts here
# https://sicss.io/2020/materials/day3-text-analysis/topic-modeling/rmarkdown/Topic_Modeling.html

kickstarter_corpus <- Corpus(VectorSource(as.vector(tweets$text))) 
kickstarter_corpus

mat = matrix(ncol = 0, nrow = 0)
perplexity_scores <- data.frame(mat)

i <- 2
repeat {
  print(perplexity(LDA(tidy_kickstarter_DTM, k=i, control = list(seed = 321)),estimate_theta=FALSE))
  i <- i + 1
  if(i > 20)
    break
}

number_topics <- c(2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20)
perplexity_score <- c(4837.769, 4841.118, 4828.298, 4820.04, 2933.283, 2806.252, 2700.496, 2616.062, 2513.363, 2419.456, 2358.052, 2292.805, 2285.152, 2254.531, 2237.051, 2231.76, 2192.943, 2168.362, 2141.619)

perplexity_scores <- data.frame(number_topics, perplexity_score)

setNames(perplexity_scores,c("topics","perplexity"))

perplexity_scores

# Code below comes from: https://statisticsglobe.com/plot-line-in-r-graph-chart
# Change main title & axis labels
plot(x=perplexity_scores$number_topics, 
     y=perplexity_scores$perplexity_score,
     type = "b",
     main = "Perplexity score by number of topics",
     xlab = "Number of topics",
     ylab = "Perplexity score",
     col = "dark green",
     lwd = 5
)

# winning number of topics is k=6

kickstarter_topic_model<-LDA(tidy_kickstarter_DTM, k=6, control = list(seed = 321))

perplexity(kickstarter_topic_model,estimate_theta=FALSE)

#kickstarter_topic_model

kickstarter_topics <- tidy(kickstarter_topic_model, matrix = "beta")

kickstarter_topics

kickstarter_top_terms <- 
  kickstarter_topics %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

kickstarter_top_terms %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()

# Topic Modelling ends here



# --------------------------------

# Top Words and TF IDF Start here
# https://sicss.io/2020/materials/day3-text-analysis/dictionary-methods/rmarkdown/Dictionary-Based_Text_Analysis.html

top_words<-
  tidy_tweets %>%
  anti_join(stop_words) %>%
  anti_join(other_words) %>%
  count(word) %>%
  arrange(desc(n))

top_words

top_words %>%
  slice(1:20) %>%
  ggplot(aes(x=reorder(word, -n), y=n, fill=word))+
  geom_bar(stat="identity")+
  theme_minimal()+
  theme(axis.text.x = 
          element_text(angle = 60, hjust = 1, size=13))+
  theme(plot.title = 
          element_text(hjust = 0.5, size=18))+
  ylab("Frequency")+
  xlab("")+
  ggtitle("Most Frequent Words in Kickstarter-related Tweets")+
  guides(fill=FALSE)

tidy_kickstarter_tfidf<- tweets %>%
  select(created_at,text) %>%
  unnest_tokens("word", text) %>%
  anti_join(stop_words) %>%
  count(word, created_at) %>%
  bind_tf_idf(word, created_at, n)

tidy_kickstarter_tfidf

top_tfidf<-tidy_kickstarter_tfidf %>%
  arrange(desc(tf_idf))

top_tfidf$word[1]


# Top Words and TF IDF end here
# -----------------

# Sentiment analysis of tweets starts here

head(get_sentiments("bing"))

kickstarter_tweet_sentiment <- tidy_tweets %>%
  inner_join(get_sentiments("bing")) %>%
  count(created_at, sentiment) 

head(kickstarter_tweet_sentiment)

tidy_tweets$date<-as.Date(tidy_tweets$created_at, 
                          format="%Y-%m-%d %x")

#kickstarter_negative_sentiment_plot

kickstarter_negative_sentiment_plot <-
  tidy_tweets %>%
  inner_join(get_sentiments("bing")) %>% 
  filter(sentiment=="negative") %>%
  count(date, sentiment)

#kickstarter_positive_sentiment_plot

kickstarter_positive_sentiment_plot <-
  tidy_tweets %>%
  inner_join(get_sentiments("bing")) %>% 
  filter(sentiment=="positive") %>%
  count(date, sentiment)


#?left_join

kickstarter_sentiment_plot<-full_join(
  kickstarter_negative_sentiment_plot,
  kickstarter_positive_sentiment_plot,
  by = c("date"),
)

kickstarter_sentiment_plot

kickstarter_sentiment_plot<-bind_rows(kickstarter_negative_sentiment_plot, 
                                      kickstarter_positive_sentiment_plot) %>% 
  ggplot(aes(x=date, y=n, group=sentiment ,color= sentiment)) + 
  geom_line()+
  theme_minimal()+
  theme(axis.text.x = 
          element_text(angle = 0, hjust = 1, size=10))+
  theme(plot.title = 
          element_text(hjust = 0.5, size=18))+
  ylab("Number of Words")+
  xlab("Date")+
  ggtitle("Sentiment in Kickstarter-related Tweets")+
  theme(aspect.ratio=1/2)

kickstarter_sentiment_plot

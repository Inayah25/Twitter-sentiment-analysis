# importing required libraries
library(rtweet)
library(ggplot2)
library(dplyr)
library(tidytext)
library(tidyverse)
library(RTextTools)
library(SnowballC)
library(caTools)
library(rpart)
library(rpart.plot)
library(ROCR)
library(randomForest)
library(caret)


# Authenticating to twitter
appname <- "eno"
key <- "WiLKEQiW8ZKkSnBoHmEm63RGE"
secret <- "DxyPTgFUaxDHoOsKQf6nzQCZa0Cuj5ZSOyQb00NMDYiLIJJHYp"
access_token = "1100787382754910208-b2zZQdhdlmAD3Rudwm9acInjsUCjyX"
access_secret = "SPQphbBc3TbGQnJ5GwJ1rRx3ywbHoGbs9ZJSfEWoJzcCY"

# create token 
twitter_token <- rtweet::create_token(
  app = appname,
  consumer_key = key,
  consumer_secret = secret,
  access_token = access_token,
  access_secret = access_secret,
  set_renv = TRUE)

get_token()

# get tweets
tweets <- search_tweets("Oscars", n=1000, lang = "en")
oscartweet <- tweets


# create dataframe
tweetsdf <- data.frame(tweets$user_id,tweets$status_id,tweets$created_at,
                      tweets$screen_name,tweets$text,tweets$source,
                      tweets$display_text_width, tweets$reply_to_status_id,
                      tweets$reply_to_user_id, tweets$reply_to_screen_name,
                      tweets$is_quote, tweets$is_retweet, tweets$favorite_count,
                      tweets$retweet_count, tweets$quote_count, tweets$reply_count,
                      tweets$ext_media_type, tweets$lang, tweets$quoted_status_id,
                      tweets$quoted_text, tweets$quoted_created_at, tweets$quoted_source,
                      tweets$quoted_favorite_count, tweets$quoted_retweet_count,
                      tweets$quoted_user_id, tweets$quoted_screen_name,tweets$name,
                      tweets$quoted_followers_count, tweets$quoted_friends_count,
                      tweets$quoted_statuses_count, tweets$quoted_location,
                      tweets$quoted_description, tweets$quoted_verified, tweets$retweet_status_id,
                      tweets$retweet_text, tweets$retweet_created_at, tweets$retweet_source,
                      tweets$retweet_favorite_count, tweets$retweet_retweet_count,
                      tweets$retweet_user_id, tweets$retweet_screen_name, tweets$retweet_name,
                      tweets$retweet_followers_count, tweets$retweet_friends_count,
                      tweets$retweet_statuses_count, tweets$retweet_location, tweets$retweet_description,
                      tweets$retweet_verified, tweets$place_url, tweets$place_name,
                      tweets$place_full_name, tweets$place_type, tweets$country,
                      tweets$country_code)

# creating csv file 
is.data.frame(tweetsdf)
write_as_csv(tweetsdf, 'oscars.csv')
     #############################################

# read oscars.csv
osc <- rtweet::read_twitter_csv(file.choose())
str(osc)


#Building corpus
library(tm)

corpus <- iconv(tweets$text, to="UTF-8")
corpus <- Corpus(VectorSource(corpus))
inspect(corpus[1:5])


# Cleaning Text (Oscar tweets)

corpus <- tm_map(corpus, tolower)
inspect(corpus[1:5])

corpus <- tm_map(corpus, removePunctuation)
inspect(corpus[1:5])

corpus <- tm_map(corpus, removeNumbers)
inspect(corpus[1:10])

cleanset <- tm_map(corpus, removeWords, stopwords('english'))
inspect(cleanset[1:10])

removeURL <- function(x) gsub('http[[:alnum:]]*', '', x)
cleanset <- tm_map(cleanset, content_transformer(removeURL))
inspect(cleanset[1:10])

cleanset <- tm_map(cleanset, removeWords, c('oscars', 'Oscars', 'oscar', 'Oscar'))
inspect(cleanset[1:6])

cleanset <- tm_map(cleanset, gsub, pattern="\n", replacement = "")
cleanset <- tm_map(cleanset, gsub, pattern="\u009d", replacement = "")
cleanset <- tm_map(cleanset, gsub, pattern="smiths", replacement = "smith")
cleanset <- tm_map(cleanset, gsub, pattern="sadhgurujv", replacement = "")
cleanset <- tm_map(cleanset, gsub, pattern="sadhguru", replacement = "")
cleanset <- tm_map(cleanset, stripWhitespace)
inspect(cleanset[1:6])


# Term Document Matrix (Oscar)
tdm <- TermDocumentMatrix(cleanset)
tdm
tdm <- as.matrix(tdm)
tdm[1:5, 1:10]


# Bar Plot (Oscar)
w <- rowSums(tdm)
w
w <- subset(w, w>=25)
w
barplot(w,
        las = 2,
        col = rainbow(50))


#word cloud (Oscars)
library(wordcloud)

w <- sort(rowSums(tdm), decreasing = TRUE)
set.seed(222)
wordcloud(words = names(w), 
          freq = w,
          max.words = 150,
          random.order = FALSE,
          min.freq = 5,
          colors = brewer.pal(8, 'Dark2'),
          scale = c(3, 0.8))

library(wordcloud2)

w <- data.frame(names(w), w)
colnames(w) <- c('word', 'freq')
wordcloud2::wordcloud2(w,
           size = 0.5,
           shape = 'circle',
           minSize = 1)


# Sentiment Analysis
library(lubridate)
library(syuzhet)
library(scales)
library(reshape2)


# Read the file 
#read oscars.csv
osctw <- rtweet::read_twitter_csv(file.choose())
ostweet <-  iconv(osctw$tweets.text,to="UTF-8")


# Obtain sentiment scores

o <- get_nrc_sentiment(ostweet)
head(o)
ostweet[6]
get_nrc_sentiment('decline')
#score
o$score <- o$positive - o$negative
o$score 
oscartweet$score <- o$score
#o$text <- osctw$tweets.text

#written
write.csv(o, file = "c:/Users/KIIT/Desktop/finalscoreos.csv")

review_score_oscar <- colSums(o[,])
review_score_oscar


# Bar Plot
barplot(colSums(o),
        las = 2,
        col = rainbow(10),
        ylab = 'Count',
        main = 'Sentiment for oscars tweets')

qplot(o$score, geom="histogram", binwidth=5, fill=I("blue")) 


library(tidyr)
library(textdata)
library(purrr)

tweet.tweets = tweets %>%
  select(screen_name,text)
tweet.tweets
head(tweet.tweets$text)

tweet.tweets$stripped_text1 <- gsub("http\\S+","",tweet.tweets$text)

tweet.tweets_stem <- tweet.tweets %>%
  select(stripped_text1) %>%
  unnest_tokens(word, stripped_text1)
head(tweet.tweets_stem)

cleaned_tweets.tweets <- tweet.tweets_stem %>%
  anti_join(stop_words)
head(cleaned_tweets.tweets)

head(tweet.tweets$text)

# top 20 words in oscar tweets
cleaned_tweets.tweets %>%
  count(word, sort = TRUE) %>%
  top_n(20) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x=word, y = n)) +
  geom_col() +
  coord_flip() +
  theme_classic() +
  labs(x="Count",
       y="Unique word",
       tittle="Unique word counts found in Tweets")

# top 10 words in oscar tweets
cleaned_tweets.tweets %>%
  count(word, sort = TRUE) %>%
  top_n(10) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x=word, y = n)) +
  geom_col() +
  coord_flip() +
  theme_classic() +
  labs(x="Count",
       y="Unique word",
       tittle="Unique word counts found in Tweets")

bing_tweets = cleaned_tweets.tweets %>% inner_join(get_sentiments("bing")) %>% 
  count(word, sentiment, sort = TRUE) %>% ungroup()

bing_tweets %>% group_by(sentiment) %>% top_n(10) %>% ungroup() %>% 
  mutate(word = reorder(word, n)) %>% ggplot(aes(word, n, fill = sentiment))+ 
  geom_col(show.legend = FALSE) + facet_wrap(~sentiment, scales = "free_y") + 
  labs(title = "Tweets containing '#smith'", y = "Contribution to sentiment", 
       x = NULL) + coord_flip() + theme_bw()


sentiment_bing = function(twt){
  twt_tbl = tibble(text = twt) %>%
    mutate(
      stripped_text = gsub("http\\S+","",text)
    )%>%
    unnest_tokens(word, stripped_text) %>%
    anti_join(stop_words) %>%
    inner_join(get_sentiments("bing")) %>%
    count(word,sentiment, sort = TRUE) %>%
    ungroup() %>%
    mutate(
      score = case_when(
        sentiment == 'negative'~n*(-1),
        sentiment == 'positive'~n*1)
    )
  sent.score = case_when(
    nrow(twt_tbl)==0~0, 
    nrow(twt_tbl)>0~sum(twt_tbl$score) 
  )
  
  zero.type = case_when(
    nrow(twt_tbl)==0~"Type 1", 
    nrow(twt_tbl)>0~"Type 2" 
  )
  list(score = sent.score, type = zero.type, twt_tbl = twt_tbl)
}

tweets_sent = lapply(tweets["text"], function(x){sentiment_bing(x)}) 
tweets_sent

tweets_sentiment = bind_rows(tibble(oscars = '#will', 
                                  score = unlist(map(tweets_sent,'score')), 
                                  type = unlist(map(tweets_sent, 'type'))))

tweets_sentiment

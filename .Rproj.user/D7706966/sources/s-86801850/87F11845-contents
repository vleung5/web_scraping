library(tidyverse)
library(ggplot2)
library(lubridate)
library(tidyr)
library(scales)
set.seed(1)

url <- 'http://www.trumptwitterarchive.com/data/realdonaldtrump/%s.json'
trump_tweets <- map(2009:2017, ~sprintf(url, .x)) %>%
  map_df(jsonlite::fromJSON, simplifyDataFrame = TRUE) %>%
  filter(!is_retweet & !str_detect(text, '^"')) %>%
  mutate(created_at = parse_date_time(created_at, orders = "a b! d! H!:M!:S! z!* Y!", tz="EST")) 

library(dslabs)
data("trump_tweets")

head(trump_tweets)

names(trump_tweets)

trump_tweets %>% select(text) %>% head

trump_tweets %>% count(source) %>% arrange(desc(n))

trump_tweets %>% 
  extract(source, "source", "Twitter for (.*)") %>%
  count(source) 

#We are interested in what happened during the campaign, so for the analysis here we will focus on what was 
#tweeted between the day Trump announced his campaign and election day. So we define the following table:
campaign_tweets <- trump_tweets %>% 
  extract(source, "source", "Twitter for (.*)") %>%
  filter(source %in% c("Android", "iPhone") &
           created_at >= ymd("2015-06-17") & 
           created_at < ymd("2016-11-08")) %>%
  filter(!is_retweet) %>%
  arrange(created_at)


#We can now use data visualization to explore the possibility that two different groups were tweeting 
#from these devices. For each tweet, we will extract the hour, in the east coast (EST), it was tweeted 
#then compute the proportion of tweets tweeted at each hour for each device.
ds_theme_set()
campaign_tweets %>%
  mutate(hour = hour(with_tz(created_at, "EST"))) %>%
  count(source, hour) %>%
  group_by(source) %>%
  mutate(percent = n / sum(n)) %>%
  ungroup %>%
  ggplot(aes(hour, percent, color = source)) +
  geom_line() +
  geom_point() +
  scale_y_continuous(labels = percent_format()) +
  labs(x = "Hour of day (EST)",
       y = "% of tweets",
       color = "")


#install.packages("tidytext")
library(tidytext)

example <- data_frame(line = c(1, 2, 3, 4),
                      text = c("Roses are red,", "Violets are blue,", "Sugar is sweet,", "And so are you."))
example
example %>% unnest_tokens(word, text)

#Now let's look at a quick example with a tweet number 3008
i <- 3008
campaign_tweets$text[i]
campaign_tweets[i,] %>% 
  unnest_tokens(word, text) %>%
  select(word)

pattern <- "([^A-Za-z\\d#@']|'(?![A-Za-z\\d#@]))"

#We can now use the unnest_tokens function with the regex option and appropriately extract the hashtags 
#and mentions:
campaign_tweets[i,] %>% 
  unnest_tokens(word, text, token = "regex", pattern = pattern) %>%
  select(word)


#Another minor adjustment we want to make is remove the links to pictures:
campaign_tweets[i,] %>% 
  mutate(text = str_replace_all(text, "https://t.co/[A-Za-z\\d]+|&amp;", ""))  %>%
  unnest_tokens(word, text, token = "regex", pattern = pattern) %>%
  select(word)


#Now we are now read to extract the words for all our tweets.
tweet_words <- campaign_tweets %>% 
  mutate(text = str_replace_all(text, "https://t.co/[A-Za-z\\d]+|&amp;", ""))  %>%
  unnest_tokens(word, text, token = "regex", pattern = pattern) 

#And we can now answer questions such as "what are the most commonly used words?"
tweet_words %>% 
  count(word) %>%
  arrange(desc(n))


#Use stop_words from the tidytext package to filter out commonly used words.
stop_words

#If we filter out rows representing stop words with filter(!word %in% stop_words$word)
tweet_words <- campaign_tweets %>% 
  mutate(text = str_replace_all(text, "https://t.co/[A-Za-z\\d]+|&amp;", ""))  %>%
  unnest_tokens(word, text, token = "regex", pattern = pattern) %>%
  filter(!word %in% stop_words$word ) 

#we end up with a much more informative set of top 10 tweeted words
tweet_words %>% 
  count(word) %>%
  top_n(10, n) %>%
  mutate(word = reorder(word, n)) %>%
  arrange(desc(n))

#There are some tokens with just numbers (ie: years).  Let's remove them with regex ^d+$.
#Also some tokens come from a quote and they start with '.  Let's remove them with str_replace.
tweet_words <- campaign_tweets %>% 
  mutate(text = str_replace_all(text, "https://t.co/[A-Za-z\\d]+|&amp;", ""))  %>%
  unnest_tokens(word, text, token = "regex", pattern = pattern) %>%
  filter(!word %in% stop_words$word &
           !str_detect(word, "^\\d+$")) %>%
  mutate(word = str_replace(word, "^'", ""))

#Now, let's explore which words are more common when comparing Android or iPhone.
#Let's say each divice and a given word is y.
#We compute the odds or the ratio between the proportion of words that are y and not y,
#and compute the ratio of those odds.
android_iphone_or <- tweet_words %>%
  count(word, source) %>%
  spread(source, n, fill = 0) %>%
  mutate(or = (Android + 0.5) / (sum(Android) - Android + 0.5) / 
           ( (iPhone + 0.5) / (sum(iPhone) - iPhone + 0.5)))
android_iphone_or %>% arrange(desc(or))
android_iphone_or %>% arrange(or)


#Given that several of these words are overall low frequency words we can impose a 
#filter based on the total frequency like this:
android_iphone_or %>% filter(Android+iPhone > 100) %>%
  arrange(desc(or))

android_iphone_or %>% filter(Android+iPhone > 100) %>%
  arrange(or)


#Let's get the sentiments of these words using nrc lexicon:
nrc <- sentiments %>%
  filter(lexicon == "nrc") %>%
  select(word, sentiment)

#Let's comine the words and sentiments using inner_join
tweet_words %>% inner_join(nrc, by = "word") %>% 
  select(source, word, sentiment) %>% sample_n(10)

#Now we are ready for quantitative analysis comparing Android and iPhone by comparing the sentiments
#of the tweets posted from each device.
sentiment_counts <- tweet_words %>%
  left_join(nrc, by = "word") %>%
  count(source, sentiment) %>%
  spread(source, n) %>%
  mutate(sentiment = replace_na(sentiment, replace = "none"))
sentiment_counts

#Because more words were used on the Android than on the phone:
tweet_words %>% group_by(source) %>% summarize(n = n())

#For each sentiment we can compute the odds of being in the device: proportion of words with sentiment 
#versus proportion of words without and then compute the odds ratio comparing the two devices
sentiment_counts %>%
  mutate(Android = Android / (sum(Android) - Android) , 
         iPhone = iPhone / (sum(iPhone) - iPhone), 
         or = Android/iPhone) %>%
  arrange(desc(or))

#So we do see some difference and the order is interesting: the largest three sentiments are disgust, 
#anger, and negative! But are they statistically significant?


#To answer that question we can compute, for each sentiment, an odds ratio and confidence interval. We will 
#add the two values we need to form a two-by-two table and the odd rat
library(broom)
log_or <- sentiment_counts %>%
  mutate( log_or = log( (Android / (sum(Android) - Android)) / (iPhone / (sum(iPhone) - iPhone))),
          se = sqrt( 1/Android + 1/(sum(Android) - Android) + 1/iPhone + 1/(sum(iPhone) - iPhone)),
          conf.low = log_or - qnorm(0.975)*se,
          conf.high = log_or + qnorm(0.975)*se) %>%
  arrange(desc(log_or))

log_or

#A graphical visualization shows some sentiments that are clearly overrepresented:
log_or %>%
  mutate(sentiment = reorder(sentiment, log_or),) %>%
  ggplot(aes(x = sentiment, ymin = conf.low, ymax = conf.high)) +
  geom_errorbar() +
  geom_point(aes(sentiment, log_or)) +
  ylab("Log odds ratio for association between Android and sentiment") +
  coord_flip()

#We see that the disgust, anger, negative, sadness and fear sentiments are associated with the Android 
#in a way that is hard to explain by chance alone. Words not associated to a sentiment were strongly 
#associated with the iPhone source, which is in agreement with the original claim about hyperbolic tweets.

#If we are interested in exploring which specific words are driving these differences, we can back to our 
#android_iphone_or object:
android_iphone_or %>% inner_join(nrc) %>%
  filter(sentiment == "disgust" & Android + iPhone > 10) %>%
  arrange(desc(or))

#And make a graph:
android_iphone_or %>% inner_join(nrc, by = "word") %>%
  mutate(sentiment = factor(sentiment, levels = log_or$sentiment)) %>%
  mutate(log_or = log(or)) %>%
  filter(Android + iPhone > 10 & abs(log_or)>1) %>%
  mutate(word = reorder(word, log_or)) %>%
  ggplot(aes(word, log_or, fill = log_or < 0)) +
  facet_wrap(~sentiment, scales = "free_x", nrow = 2) + 
  geom_bar(stat="identity", show.legend = FALSE) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) 
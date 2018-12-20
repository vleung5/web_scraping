library(tidytext)
#table(sentiments$lexicon)

#The bing lexicon divides words into positive and negative. We can see this using the tidytext function 
#get_sentiments:
get_sentiments("bing")

#The AFINN lexicon assigns a score between -5 and 5, with -5 the most negative and 5 the most positive.
get_sentiments("afinn")

#The loughran and nrc lexicons provide several different sentiments:
get_sentiments("loughran") %>% count(sentiment)

get_sentiments("nrc") %>% count(sentiment)

#For the analysis here we are interested in exploring the different sentiments of each tweet so we will 
#use the nrc lexicon:
nrc <- sentiments %>%
  filter(lexicon == "nrc") %>%
  select(word, sentiment)
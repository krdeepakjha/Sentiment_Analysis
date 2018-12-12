
# Sentiment analysis for the different twitter data 
# Later will come up with different datasets from tv programs and some more datasets. 

#####################
# Will be using the tidy data principle for this analysis. So will be loading the different packages for this
# because will use them in this course of analysis. 

library(dplyr)
library(tidytext)

# There will be datasets from tidytext which will be taken from here 
# i.e The datasets are afinn, bing, nrc

#############
# Choose the bing lexicon
# There are three datasets for the get_sentiments

get_sentiments("bing")

# Choose the nrc lexicon
get_sentiments("nrc") %>%
  count(sentiment) # Count words by sentiment

#######################

##################

# loading the geocoded_tweets workspace
# be careful as the older version of the r data workspace being provided earlier so try to change the extension
# from .rda to .RData

load(file = "/Users/deepajha/Documents/Sentiment_Analysis/geocoded_tweets.RData")

# geocoded_tweets has been pre-defined
geocoded_tweets

# Access bing lexicon: bing
bing <- get_sentiments("bing")

# Use data frame with text data
geocoded_tweets %>%
  # With inner join, implement sentiment analysis using `bing`
  inner_join(bing)


##
# finding out some of the common sad words for the text analysis
# can be found for every category and arranging them according to the frequency. 
# Which is very handy for the overall analysis. 
load(file = "/Users/deepajha/Documents/Sentiment_Analysis/geocoded_tweets.RData")

tweets_nrc <- geocoded_tweets %>%
  inner_join(get_sentiments("nrc"))

#tweets_nrc <- get_sentiments("nrc")

tweets_nrc %>%
  filter(sentiment == "sadness") %>%
  group_by(word) %>%
  summarize(freq = mean(freq)) %>%
  arrange(desc(freq))


############
# finding out the most common joy words

tweets_nrc <- geocoded_tweets %>%
  inner_join(get_sentiments("nrc"))


joy_words <- tweets_nrc %>%
  # filter to choose only words which are only being categorised as "joy". Then it can be classified with the 
  # frequency as how many time they are being showing up 
  filter(sentiment == "joy") %>%
  # group by the words 
  group_by(word) %>%
  # getting the frequency of certain words for the sentiment data
  summarize(freq = mean(freq)) %>%
  # arranging the words in the descending order so can get the table for most frequently used words
  arrange(desc(freq))
  
# loading ggplot for the visualization for the tweets data  
         
library(ggplot2)

joy_words %>%
  top_n(20) %>%
  mutate(word = reorder(word, freq)) %>%
#
ggplot(aes(word, freq)) + geom_col() + coord_flip()
# make a bar charts with the geom_com


## now filtering the words for different states and the words being tweeted by them. 
# will start with the word "joy"

# loading the tweets data with the inner join

tweets_nrc <- geocoded_tweets %>%
  inner_join(get_sentiments("nrc"))

tweets_nrc %>%
  #finding the words of state utah and the word joy for it. 
  
  filter(state == "utah", sentiment == "joy") %>%
  arrange(desc(freq))


# can be also found out for the different states  
  

# Now trying to find out the positive tweets from the different states 
tweets_bing <- geocoded_tweets %>%
  inner_join(get_sentiments("bing"))

library(tidyr)
library(ggplot2)
library(dplyr)
tweets_bing

tweets_bing %>%
  group_by(state, sentiment) %>%
  summarize(freq = mean(freq)) %>%
  spread(sentiment, freq)%>%
  ungroup() %>%
  mutate(ratio = positive/ negative,
         state = reorder(state, ratio)) %>%
  ggplot(aes(state, ratio)) + geom_point() + coord_flip()


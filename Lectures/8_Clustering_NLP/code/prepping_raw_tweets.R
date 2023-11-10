rm(list = ls())
gc()
require(tidyverse)

raw <- read_csv('../data/trump_tweets.csv')

raw <- raw %>%
  select(id,content = text,is_deleted,is_flagged,datetime,retweets,favorites,Tweeting.date = date) %>%
  mutate(Tweeting.hour = as.character(lubridate::hour(datetime)),
         Tweeting.year = as.factor(lubridate::year(datetime))) %>%
  arrange(Tweeting.date)


# Based on https://github.com/joshclinton/DSCI1000/blob/main/Lectures/Topic10_Clustering/ReadTrumpTweetData.R
clean_tweets <- function(x) {
  x %>%
    # Remove URLs
    #    str_remove_all(" ?(f|ht)(tp)(s?)(://)(.*)[.|/](.*)") %>%
    # Remove mentions e.g. "@my_account"
    str_remove_all("@[[:alnum:]_]{4,}") %>%
    # Remove mentions e.g. "@ my_account"
    str_remove_all("@ [[:alnum:]_]{4,}") %>%
    # Remove hashtags
    str_remove_all("#[[:alnum:]_]+") %>%
    # Remove hashtags
    str_remove_all("# [[:alnum:]_]+") %>%
    # Remove twitter references
    str_remove_all("twitter[[:alnum:]_]+") %>%
    # Remove twitter pics references
    str_remove_all("pictwitter[[:alnum:]_]+") %>%
    # Replace "&" character reference with "and"
    str_replace_all("&amp;", "and") %>%
    # Remove puntucation, using a standard character class
    str_remove_all("[[:punct:]]") %>%
    # Remove "RT: " from beginning of retweets
    str_remove_all("^RT:? ") %>%
    # Replace any newline characters with a space
    str_replace_all("\\\n", " ") %>%
    # Make everything lowercase
    str_to_lower() %>%
    # Remove any trailing whitespace around the text
    str_trim("both")
}

# may need to revisit above if passing thru links
raw$content <- clean_tweets(raw$content)

library(tidytext)
library(qdapRegex)
library(tm)
require(RTextTools)
reg <- "([^A-Za-z\\d#@']|'(?![A-Za-z\\d#@]))"

library(SnowballC)

tweet_words <- raw %>%
  filter(!str_detect(content, '^"')) %>%
  unnest_tokens(word, content, token = "regex", pattern = reg)  %>%
  filter(!word %in% stop_words$word,str_detect(word, "[a-z]")) %>%
  mutate(word = str_replace_all(word, "\\d+", "")) %>%  # drop numbers
  mutate(word = str_replace_all(word, "twitter[A-Za-z\\d]+|&amp;", "")) %>%
  mutate(word = str_replace_all(word, "pictwitter[A-Za-z\\d]+|&amp;", "")) %>%
  mutate(word = str_replace_all(word, "pic[A-Za-z\\d]+|&amp;", "")) %>%
  mutate(word = str_replace_all(word, "pic", "")) %>%
  mutate(word = str_replace_all(word, "againpic[A-Za-z\\d]+|&amp;", "")) %>%
  # mutate(word = wordStem(word)) %>%
  mutate(document = id) %>%
  select(-id) %>%
  filter(word != "")   # drop any empty strings

write_rds(raw,file = '../data/Trumptweets.Rds')
write_rds(tweet_words,file = '../data/Trump_tweet_words.Rds')
getwd()

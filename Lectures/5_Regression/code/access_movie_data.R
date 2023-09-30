# Accessing Data on Movies
# Will Doyle
# 2021-09-24

#  This code accesses the following datasets to create our analysis dataset for class:
# CPI data from the Bureau of Labor Stats to inflation adjust dollar amounts
# Bechdel Ratings of Movies
# Data from IMDB on every title in the database
# Data from IMDB on every rating in the database
# Cleaned up data from IMDB including budgets


library(tidyverse)
library(omdbapi)
library(tidyquant)
library(plotly)

## Enter API Key, can get one here:http://www.omdbapi.com/

omdb_api_key(force = TRUE)

#####################################################
# Access and Create Datasets
#####################################################

## CPI
## Need annualized CPI deflator

get_cpi<-FALSE

if (get_cpi){
  
  cpi <-
    tq_get("CPIAUCSL",
           get = "economic.data",
           from = "1980-01-01",
           to = "2021-09-15") %>%
    mutate(year = year(date)) %>% ## pull just year from date
    group_by(year) %>%
    summarize(cpi = mean(price)) %>% ## annualized by year
    arrange(year) %>%
    mutate(adj = last(cpi) / cpi) #inflator is ratio of future to current
  
  save(cpi, file = "cpi.Rdata")
}
load("cpi.Rdata")

names(cpi)<-tolower(names(cpi))


## Bechdel data
## This data is from a Tidy Tuesday competition with data based on a 538 article

## Bechdel score

get_bechdel<-FALSE

if (get_bechdel){
  
  bechdel <-
    readr::read_csv(
      'https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-03-09/raw_bechdel.csv'
    ) %>%
    rename(bechdel_score = rating)
  
  save(bechdel, file = "bechdel.Rdata")
}

load("bechdel.Rdata")

## Access imdb titles and ratings
## From:https://datasets.imdbws.com/
imdb_titles <- read_tsv("title.basics.tsv")

imdb_ratings <- read_tsv("title.ratings.tsv")

## Only select media with more than 1000 votes
imdb_ratings <-
  imdb_ratings %>%
  filter(numVotes > 1e4)

## Combine ratings and titles
imdb_c <- imdb_ratings %>%
  left_join(imdb_titles, by = "tconst")

## Wrangle dataset
imdb_c <- imdb_c %>%
  filter(isAdult == 0,  ## Keeping it family friendly
         titleType == "movie", ## Only movies
         startYear > 1980) %>%## Only since 2000)
  mutate(runtime = as.numeric(runtimeMinutes)) %>% ## runtime is character to start
  filter(!is.na(runtime),  ## Drop na
         runtime > 75) %>% ## Keep feature-length only
  select(-endYear,-isAdult) ## Drop unneeded variables

imdb_id_list <- imdb_c %>%
  select(tconst) %>%
  as_vector() ## generate list of imdb ids

## This will take a long time to run!
get_movie_data <- FALSE ## Creates  a "switch"

if (get_movie_data) {
  ## only run if above is TRUE
  mv_df <- NULL #initialize data frame
  for (id in imdb_id_list) {
    results <- find_by_id(id) ## find_by_id is key command
    mv_df <-
      bind_rows(mv_df, results) ## bind results as we go
  }
  
  save(mv_df, file = "mv_df.Rdata")
}

load("mv_df.Rdata")

## Wrangle downloaded dataset

#names(mv_df_a)<-tolower(names_mv_df_a)

mv_df_a <-
  mv_df %>%
  group_by(imdbID) %>%
  summarize(across(.cols = everything(), .fns = first)) %>% ## Keep only first unique record
  ungroup() %>%
  mutate(boxoffice_a = parse_number(BoxOffice)) %>% ## Turn box office into numeric
  filter(boxoffice_a > 0) %>% ## had to make some $
  mutate(Year = as.numeric(Year)) %>% #
  rename_all(.fun=tolower)%>%
  left_join(cpi, by = "year") %>%
  mutate(boxoffice_a = boxoffice_a * adj) %>%
  select(-boxoffice)


## This is the actual data I'm going to use: scraped from IMDB, includes gross

## From: https://www.kaggle.com/danielgrijalvas/movies?select=movies.csv


mv2 <- read_csv("movies.csv") %>%
  rename(title = name) %>%
  left_join(cpi, by = "year") %>%
  mutate(budget = budget * adj,
         gross = gross * adj)%>%
  select(-cpi,-adj)
## Join the bechdel data

mv2 <- mv2 %>%
  left_join(bechdel, by = c("title", "year"))

names(mv_df_a) <- tolower(names(mv_df_a))

mv2 <- mv2 %>%
  left_join(mv_df_a %>% select(title,
                               year,
                               boxoffice_a,
                               language),
            by = c("title", "year"))

write_rds(mv2,file="mv.Rds")
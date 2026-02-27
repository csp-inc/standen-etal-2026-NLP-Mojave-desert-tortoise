## ---------------------------
## 
## Project: TLD NLP
## 
## Script name: 04_Weekly_Aggregation.R
##
## Purpose of script: this script creates datasets representing weekly aggregations for tweet and article metrics
##
## Author: Madeline Standen
##
## Date last updated: 2/27/2026
##
## Email contact: standenmadeline[at]gmail.com, madi[at]csp-inc.org
##
## ---------------------------

# Clear memory (if necessary)
rm(list=ls())

# Load packages
list.of.packages <- c("lubridate","dplyr","parallel","stringr", "ggplot2","zoo","readr","ggpubr","scales","patchwork","cowplot", "Rtsne", "nlme", "mgcv","Deriv",
                      "ggpmisc","gridExtra","MASS","msm","sandwich","foreign", "performance", "broom", "gplots", "RColorBrewer", "clValid","dendextend", "gratia",
                      "ggeffects","DHARMa","pscl","AICcmodavg","lmtest","viridis","trend","tidytext","tidyr","stringr","wordcloud","VennDiagram","tseries","funtimes","systemfonts",
                      "tm", "SnowballC", "wordcloud", "cluster", "factoextra", "topicmodels","tm","text", "textTinyR", "dplyr", "ggplot2", "tm", "text2vec", "uwot","textstem")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = TRUE)


# Articles -----
articles <- read.csv("./data_final/NFWF_articles_final_filter2_topics.csv")[,-1]

articles <- articles %>% mutate(Date = as.Date(date),
                                article_id = as.character(article_id),
                                Week_of_start = floor_date(Date, unit = "week"),
                                Week_of_end = ceiling_date(Date, unit = "week") - 1,.after = "Date")

weekly_articles <- articles %>%
  group_by(Week_of_start,Week_of_end) %>%
  summarise(Article_Count = n(),Vader_articles = mean(Raw_Sentiment_Score),Article_topic_n = length(unique(topic)) ,.groups = "drop")

# Using the old dataset to pull in the week column (for ease)
week_key <- read.csv("./data_final/Week_key.csv")[,-1]
week_key$Week_of_start <- as.Date(week_key$Week_of)
# Removes empty rows at the end
week_key <- week_key %>%
  filter(!is.na(Week_of_start))

week_key <- week_key %>% arrange(Week_of_start)

setdiff(week_key$Week_of,weekly_articles$Week_of_start)

weekly_articles <- weekly_articles %>% left_join(week_key)
weekly_articles$Week[weekly_articles$Week == "Other"] <- "Other week"
weekly_articles <- weekly_articles %>% dplyr::select(Article_Count,Vader_articles,Article_topic_n,Week,Week_of_start)
write.csv(weekly_articles,"./data_final/NFWF_weekly_articles.csv")

## Tweets-----

tweet_raw <- read.csv("./data_final/NFWF_tweets_final_topics.csv") %>% arrange(Date)
str(tweet_raw)
tweet_raw <- tweet_raw %>% mutate(Date = as.Date(Date),
                                  User = as.character(User),
                                  Week_of_start = floor_date(Date, unit = "week"),
                                  Week_of_end = ceiling_date(Date, unit = "week") - 1,.after = "Date")
max(tweet_raw$Week_of_start)
length(unique(tweet_raw$User))

first_seen <- tweet_raw %>%
  group_by(User) %>%
  summarise(First_Date = min(Date), .groups = "drop") %>%
  mutate(Week_of_first = floor_date(First_Date, unit = "week"))


tweet_raw <- tweet_raw %>% left_join(first_seen,by = join_by(User))
tweet_raw <- tweet_raw %>% mutate(Users_new = ifelse(Week_of_first == Week_of_start, 1, 0))

tweet_raw <- tweet_raw %>%
  group_by(User) %>%
  mutate(Users_new = ifelse(Users_new == 1 & cumsum(Users_new == 1) == 1, 1, 0)) %>%
  ungroup()

# Re-create the weekly summarized tweet data
weekly_tweets <- tweet_raw %>%
  group_by(Week_of_start,Week_of_end) %>%
  summarise(Tweet_Count = n(), Vader = mean(Raw_Sentiment_Score),Users = length(unique(User)),Users_new = sum(Users_new), tweet_topic_n = length(unique(topic)),.groups = "drop")

weekly_tweets <- weekly_tweets %>% mutate(Year = year(Week_of_start),Month = month(Week_of_start))
weekly_tweets <- weekly_tweets %>% arrange(Week_of_start)



# Using the old dataset to pull in the week column (for ease)
week_key <- read.csv("./data_final/Week_key.csv")
week_key$Week_of <- as.Date(week_key$Week_of)
# Removes empty rows at the end
week_key <- week_key %>%
  filter(!is.na(Week_of))
# Filter out one week with no raw tweet data
week_key <- week_key %>% filter(Week_of != "2024-08-11")
week_key <- week_key %>% arrange(Week_of)

setdiff(week_key$Week_of,weekly_tweets$Week_of_start)

weekly_tweets$Week <- week_key$Week
weekly_tweets$Week[weekly_tweets$Week == "Other"] <- "Other week"

rm(week_key)

write.csv(weekly_tweets,"./data_final/NFWF_weekly_tweets.csv")

# Events -----

long_with_years <- read.csv("./data_final/NFWF_events_final.csv")[,-1]
length(unique(long_with_years$Name))
sort(unique(long_with_years$Year))

weekly_events <- long_with_years %>% 
  group_by(Week, Month, Year) %>%
  summarise(
    Occurrence = n(),
    Unique_occurrence = sum(length(unique(Name))),
    Event = sum(Type == "Event", na.rm = TRUE),
    Program = sum(Type == "Program", na.rm = TRUE),
    .groups = "drop"
  )

write.csv(weekly_events,"./data_final/NFWF_weekly_events.csv")

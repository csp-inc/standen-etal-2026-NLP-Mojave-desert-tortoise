## ---------------------------
## 
## Project: TLD NLP
## 
## Script name: 01_Tweet_Cleaning.R
##
## Purpose of script: this script processes cleans, filters and applies sentiment analysis to tweet data
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
list.of.packages <- c("lubridate","dplyr","parallel","stringr", "ggplot2","zoo","readr","ggpubr","scales", "vader",
                      "ggpmisc","gridExtra","MASS","msm","sandwich","foreign", "performance", "broom", "gplots",
                      "ggeffects","DHARMa","pscl","AICcmodavg","lmtest","viridis","trend","tidytext","tidyr","stringr","wordcloud","VennDiagram")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = TRUE)


# Create the negate function
`%notin%` <- Negate(`%in%`)

# Part 1: Checking out the raw tweet data -----
tweet_raw_all <- read.csv("./data_final/NFWF_alltweets_raw.csv") %>% filter(Date >= "2012-01-01")
nrow(tweet_raw_all)
colnames(tweet_raw_all)

# Filtering out tweets missing values in essential columns
tweet_raw_all <- tweet_raw_all[complete.cases(tweet_raw_all[, c("Date", "User", "Tweet")]), ]

# Adjusting the time and date information
tweet_raw_all$Time <- as.POSIXlt(tweet_raw_all$Date, tz = "", format="%Y-%m-%d %H:%M:%OS")
tweet_raw_all$Date <- as.Date(tweet_raw_all$Date)
tweet_raw_all$Year <- year(tweet_raw_all$Date)
tweet_raw_all$Month <- month(tweet_raw_all$Date)
tweet_raw_all$User <- as.character(tweet_raw_all$User)

# Part 2: Cleaning -----
# Cleaning tweet text using standard NLP cleaning protocols and running vader analyses in R on this cleaned text

tweet_raw_all <- tweet_raw_all %>% 
  filter(Date >= "2012-01-01") %>%
  mutate(Tweet_cleaned = gsub("\\s+", " ", Tweet), # Clean extra white space within text
    Tweet_cleaned = gsub("http\\S+|www\\S+", "", Tweet_cleaned),     # Remove URLs
    Tweet_cleaned = gsub("@\\S+", "", Tweet_cleaned),     # Remove  mentions
    Tweet_cleaned = gsub("#","", Tweet_cleaned), # Turn hashtags into normal text
    Tweet_cleaned = str_replace_all(Tweet_cleaned, "[\U0001F600-\U0001F64F]", ""),  # Remove Emojis (emoticons)
    Tweet_cleaned = str_replace_all(Tweet_cleaned, "[\U0001F300-\U0001F5FF]", ""),  # Remove Misc symbols and pictographs
    Tweet_cleaned = str_replace_all(Tweet_cleaned, "[\U0001F680-\U0001F6FF]", ""),  # Remove Transport and map symbols
    Tweet_cleaned = str_replace_all(Tweet_cleaned, "[\U0001F700-\U0001F77F]", ""),  # Remove Alchemical symbols
    Tweet_cleaned = str_replace_all(Tweet_cleaned, "[\U0001F780-\U0001F7FF]", ""),  # Remove Geometric shapes extended
    Tweet_cleaned = str_replace_all(Tweet_cleaned, "[\U0001F800-\U0001F8FF]", ""),  # Remove Supplemental arrows
    Tweet_cleaned = str_replace_all(Tweet_cleaned, "[\U0001F900-\U0001F9FF]", ""),  # Remove Supplemental symbols and pictographs
    Tweet_cleaned = str_replace_all(Tweet_cleaned, "[\U0001FA00-\U0001FA6F]", ""),  # Remove Chess symbols, etc.
    Tweet_cleaned = str_replace_all(Tweet_cleaned, "[\U0001FA70-\U0001FAFF]", ""),  # Remove Symbols and pictographs extended-A
    Tweet_cleaned = str_replace_all(Tweet_cleaned, "^\\p{Punct}+", ""),            # Remove leading punctuation
    Tweet_cleaned = str_replace_all(Tweet_cleaned, "[:\\-—;]+$", ""),             # Remove trailing punctuation only if : ; - --
    Tweet_cleaned = gsub("\\s+", " ", Tweet_cleaned),                          # Remove extra spaces
    Tweet_cleaned = gsub("&amp;?", "&", Tweet_cleaned),
    Tweet_cleaned = gsub(" amp ", " & ", Tweet_cleaned),
    Tweet_cleaned = trimws(Tweet_cleaned)) %>%                              # Trim leading or trailing spaces
    filter(!str_detect(Tweet_cleaned,"^(RT|Rt|rt|retweet)"))            # Remove tweets that have rt at start
    
  
# Removing tweets that are now empty and removing exact dupicates, keeping only first instance
tweet_raw <- tweet_raw_all %>% filter(nchar(Tweet_cleaned) > 0) 
tweet_raw <- tweet_raw %>%
  group_by(Tweet_cleaned,Year,Month) %>%
  slice_min(Time, n = 1, with_ties = FALSE) %>%
  ungroup()

# Part 3: Applying VADER -----

Cleaned_Sentiment_Score_R <- vader_df(tweet_raw$Tweet_cleaned)

tweet_raw$Cleaned_Sentiment_Score_R <- Cleaned_Sentiment_Score_R$compound

tweet_raw <- tweet_raw %>% filter(!is.na(Cleaned_Sentiment_Score_R))

write.csv(tweet_raw, "./data_final/NFWF_tweets_cleaned_with_vader.csv")

tweet_raw <- read.csv("./data_final/NFWF_tweets_cleaned_with_vader.csv")[,-1]

# Part 4: Relevancy filtering -----
# Checking if tweets have desert tortoise key terms
tweet_tort <- tweet_raw %>% filter(grepl("Mojave Desert Tortoise|Gopherus agassizii|Gopherusagassizii|Desert Tortoise|DesertTortoise|MojaveDesertTortoise",Tweet,ignore.case = TRUE))

# Checking if tweets are about tortoise but do not about desert tortoise
tweet_tort_other <- tweet_raw %>% filter(grepl("sonoran|morafkai|african|centrochelys|sulcata|sinaloan|morafka's|goode's|thornscrub",Tweet,ignore.case = TRUE)) %>% filter(!grepl("agassizii|mojave|agassiz's",Tweet,ignore.case = TRUE))
tweet_raw <- tweet_raw %>% filter(Tweet %notin% tweet_tort_other$Tweet)

# Remove marketing 
tweet_raw_marketing <- tweet_raw %>% filter(grepl("khaki|glasses|artwork",Tweet,ignore.case = TRUE))
tweet_raw <- tweet_raw %>% filter(!grepl("khaki|glasses|artwork",Tweet,ignore.case = TRUE))
nrow(tweet_raw)
rm(tweet_raw_marketing)

# Remove pets, but keep those that have to do with legal adoptions in conujunction with government agencies
tweet_raw_pet_keep <- tweet_raw %>% filter(grepl("pet",Tweet,ignore.case = TRUE)) %>% filter(grepl("adopt|adoption",Tweet,ignore.case = TRUE)) %>% filter(grepl("facilitate|facilitated|agency|NPS|legal|fish|dept",Tweet,ignore.case = TRUE))
tweet_raw_pet_remove <- tweet_raw %>% filter(grepl("pet",Tweet,ignore.case = TRUE)) %>% filter(grepl("adopt|adoption",Tweet,ignore.case = TRUE)) %>% filter(!grepl("facilitate|facilitated|agency|NPS|legal|fish|dept",Tweet,ignore.case = TRUE))

tweet_raw <- tweet_raw %>% filter(Tweet %notin% tweet_raw_pet_remove$Tweet)
nrow(tweet_raw)
rm(tweet_raw_pet_remove,tweet_raw_pet_keep)

# Pulling hashtags (adding them to another column)
tweet_raw <- tweet_raw %>%
  mutate(
    Hashtags = str_extract_all(Tweet, "#\\w+")          # Extract hashtags
  )

# Removing mentions (adding them to another column)
tweet_raw <- tweet_raw %>%
  mutate(
    Tagged   = str_extract_all(Tweet, "@\\w+")          # Extract mentions
  )


# Removing white space in the hashtag and tagged rows
tweet_raw <- tweet_raw %>%
  mutate(
    Hashtags = sapply(Hashtags, function(x) paste(str_replace_all(x, "\\s+", ""), collapse = ",")),
    Tagged   = sapply(Tagged, function(x) paste(str_replace_all(x, "\\s+", ""), collapse = ","))
  )


# Remove tweets with the hashtag desertortoiseforum, this is a pet website
tweet_raw_forum <- tweet_raw %>% filter(grepl("deserttortoiseforum|forum",Hashtags,ignore.case = TRUE))
tweet_raw <- tweet_raw %>% filter(Tweet %notin% tweet_raw_forum$Tweet)


tweet_raw <- tweet_raw %>% dplyr::select(Date,Time ,User, Tweet,Tweet_cleaned, Cleaned_Sentiment_Score_R,Hashtags, Tagged)
colnames(tweet_raw) <- c("Date","Time","User", "Tweet","Tweet_cleaned", "Raw_Sentiment_Score","Hashtags", "Tagged")
nrow(tweet_raw)

tweet_raw <- tweet_raw %>% filter(!is.na(Raw_Sentiment_Score))
# Saving output
write.csv(tweet_raw,"./data_final/NFWF_tweets_final.csv")


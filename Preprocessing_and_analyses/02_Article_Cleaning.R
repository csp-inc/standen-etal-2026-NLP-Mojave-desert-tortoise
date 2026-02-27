## ---------------------------
## 
## Project: TLD NLP
## 
## Script name: 02_Article_Cleaning.R
##
## Purpose of script: this script processes cleans, filters and applies sentiment analysis to article data
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
list.of.packages <- c("lubridate","dplyr","parallel","stringr", "ggplot2","zoo","readr","ggpubr","scales", "vader", "Matrix",
                      "ggpmisc","gridExtra","MASS","msm","sandwich","foreign", "performance", "broom", "gplots","text2vec",
                      "ggeffects","DHARMa","pscl","AICcmodavg","lmtest","viridis","trend","tidytext","tidyr","stringr","wordcloud","VennDiagram")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = TRUE)


# Create the negate function
`%notin%` <- Negate(`%in%`)

# Part 1: Joining and filtering articles for keywords and relevance -----
## Article data -----

gdelt_v1 <- read.csv("./data_final/GDELT_V1.csv")
gdelt_v1$version <- "V1"

gdelt_v2 <- read.csv("./data_final/GDELT_V2.csv")
gdelt_v2 <- gdelt_v2 %>% rename(title = X)
gdelt_v2$version <- "V2"

# Align column names and format columns

gdelt_all <- rbind(gdelt_v1,gdelt_v2)
gdelt_all$date <- as.Date(gdelt_all$date, "%Y-%m-%d")
gdelt_all$month <- month(gdelt_all$date)

gdelt_all[] <- lapply(gdelt_all, function(x) {
  if (is.character(x)) {
    x[grepl("^\\s*$", x)] <- NA
  }
  x
})

gdelt_all <- gdelt_all %>% filter(!is.na(text))
colSums((is.na(gdelt_all)))
# Only some missing title, this is okay

# Creating cleaned version of text
gdelt_all <- gdelt_all %>%
  mutate(text_og = text,
         text = gsub("\\s+", " ", text), # Clean extra white space within text
         text = gsub("http\\S+|www\\S+", "", text),     # Remove URLs
         text = str_replace_all(text, "[\U0001F600-\U0001F64F]", ""),  # Remove Emojis (emoticons)
         text = str_replace_all(text, "[\U0001F300-\U0001F5FF]", ""),  # Remove Misc symbols and pictographs
         text = str_replace_all(text, "[\U0001F680-\U0001F6FF]", ""),  # Remove Transport and map symbols
         text = str_replace_all(text, "[\U0001F700-\U0001F77F]", ""),  # Remove Alchemical symbols
         text = str_replace_all(text, "[\U0001F780-\U0001F7FF]", ""),  # Remove Geometric shapes extended
         text = str_replace_all(text, "[\U0001F800-\U0001F8FF]", ""),  # Remove Supplemental arrows
         text = str_replace_all(text, "[\U0001F900-\U0001F9FF]", ""),  # Remove Supplemental symbols and pictographs
         text = str_replace_all(text, "[\U0001FA00-\U0001FA6F]", ""),  # Remove Chess symbols, etc.
         text = str_replace_all(text, "[\U0001FA70-\U0001FAFF]", ""),  # Remove Symbols and pictographs extended-A
         text = gsub("\\s+", " ", text),                          # Remove extra spaces
         text = trimws(text))                              # Trim leading or trailing spaces

# Selecting articles that have the word tortoise 
gdelt_all <- gdelt_all %>% filter(grepl("tortoise",text,ignore.case = TRUE))

# Removing articles with identical text, keeping only the first instance
gdelt_all <- gdelt_all %>% arrange(text,date)
# keeping only the first instance of duplicated text
gdelt_all <- gdelt_all %>%
  group_by(text) %>%
  slice_min(date, n = 1, with_ties = FALSE) %>%
  ungroup()
nrow(gdelt_all)

# Removing articles that are specifically about other types of desert tortoise
gdelt_all_other_species <- gdelt_all %>% filter(grepl("sonoran|morafkai|african|centrochelys|sulcata|sinaloan|morafka's|goode's|thornscrub",text,ignore.case = TRUE)) %>% filter(!grepl("agassizii|mojave|agassiz's",text,ignore.case = TRUE))
nrow(gdelt_all_other_species) # 1131 were about other species of desert tortoise but didnt mention mojave or scientific name
gdelt_all <- gdelt_all %>% filter(text %notin% gdelt_all_other_species$text)

# Filtering by keywords
gdelt_all_relevant <- gdelt_all %>% filter(grepl("desert tortoise|agassizii|mojave|agassiz's",text,ignore.case = TRUE))

write.csv(gdelt_all_relevant,"./data_final/NFWF_articles_filter2.csv")


# Part 2: Cosine filtering -----

gdelt_all <- read.csv("./data_final/NFWF_articles_filter2.csv")[,-1]

gdelt_all <- gdelt_all %>%
  mutate(
    text_LDA = tolower(text),
    article_id = as.character(1:nrow(gdelt_all))
  ) 

tfidf_tbl <- gdelt_all %>%
  dplyr::select(article_id, text_LDA) %>%
  unnest_tokens(word, text_LDA) %>%
  anti_join(stop_words, by = "word") %>%
  count(article_id, word) %>%
  bind_tf_idf(word, article_id, n)

tfidf_matrix <- tfidf_tbl %>%
  dplyr::select(article_id, word, tf_idf) %>%
  cast_sparse(article_id, word, tf_idf)

# Compute cosine similarity
cos_sim <- sim2(tfidf_matrix, method = "cosine", norm = "l2")

# Restrict comparisons to articles within 2 months
meta <- gdelt_all %>%
  dplyr::select(article_id, date)

pairs <- summary(cos_sim) %>%
  as_tibble() %>%
  rename(
    article_1 = i,
    article_2 = j,
    similarity = x
  ) %>%
  filter(article_1 != article_2)

# Join article ids
id_lookup <- tibble(
  row_id = seq_len(nrow(tfidf_matrix)),
  article_id = rownames(tfidf_matrix)
)

pairs <- pairs %>%
  left_join(id_lookup, by = c("article_1" = "row_id")) %>%
  left_join(id_lookup, by = c("article_2" = "row_id"), suffix = c("_1", "_2")) %>%
  dplyr::select(article_1 = article_id_1,
         article_2 = article_id_2,
         similarity)

pairs <- pairs %>%
  left_join(meta, by = c("article_1" = "article_id")) %>%
  left_join(meta, by = c("article_2" = "article_id"),
            suffix = c("_1", "_2")) %>%
  filter(abs(difftime(date_1, date_2, units = "days")) <= 60)

# Flag syndicated articles (similarity > 0.95)
syndicated <- pairs %>%
  filter(similarity > 0.95) %>%
  mutate(
    syndicated_id = if_else(date_1 > date_2, article_1, article_2),
    original_id   = if_else(date_1 > date_2, article_2, article_1)
  ) %>%
  distinct(syndicated_id)

# Mark or remove syndicated articles

gdelt_all <- gdelt_all %>%
  mutate(
    is_syndicated_raw = article_id %in% syndicated$syndicated_id
  )


#Part 3: Identifying text snippets for LDA and VADER -----
keywords <- c("tortoise")   # replace with yours
pattern <- str_c(keywords, collapse = "|")

gdelt_all <- gdelt_all %>%
  rowwise() %>%
  mutate(
    subset_text_VADER = {
      sentences <- str_split(text, "(?<=[.!?])\\s+")[[1]]
      hits <- str_detect(sentences, regex(pattern, ignore_case = TRUE))
      
      selected <- sentences[
        hits | c(FALSE, hits[-length(hits)])
      ]
      
      if (length(selected) == 0) NA_character_ else str_c(selected, collapse = " ")
    }
  ) %>%
  ungroup()

gdelt_all <- gdelt_all %>%
  rowwise() %>%
  mutate(
    subset_text_LDA = {
      sentences <- str_split(text, "(?<=[.!?])\\s+", simplify = FALSE)[[1]]
      hits <- which(str_detect(sentences, regex(pattern, ignore_case = TRUE)))
      
      if (length(hits) == 0) {
        NA_character_
      } else {
        # Expand to 2 sentences before and after
        expanded_idx <- unlist(lapply(hits, function(i) {
          seq(pmax(i - 2, 1), pmin(i + 2, length(sentences)))
        }))
        # Keep unique indices in order
        expanded_idx <- sort(unique(expanded_idx))
        str_c(sentences[expanded_idx], collapse = " ")
      }
    }
  ) %>%
  ungroup()



gdelt_all <- gdelt_all %>% filter(!is.na(subset_text_VADER))

gdelt_all_clean <- gdelt_all %>%
  filter(!is_syndicated_raw)

write.csv(gdelt_all_clean,"./data_final/NFWF_articles_final_filter2.csv")


# Part 4: Applying VADER -----
gdelt_all_clean <- read.csv("./data_final/NFWF_articles_final_filter2.csv")[,-1]

Cleaned_Sentiment_Score_R <- vader_df(gdelt_all_clean$subset_text_VADER)
gdelt_all_clean$Raw_Sentiment_Score <- Cleaned_Sentiment_Score_R$compound

write.csv(gdelt_all_clean,"./data_final/NFWF_articles_final_filter2.csv")


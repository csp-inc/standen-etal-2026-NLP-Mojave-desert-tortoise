## ---------------------------
## 
## Project: TLD NLP
## 
## Script name: 03_Topic_Modeling.R
##
## Purpose of script: this script applies LDA topic modeling and UMAP projection to tweet and article data
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
list.of.packages <- c("lubridate","dplyr","parallel","stringr", "ggplot2","zoo","readr","ggpubr","scales","patchwork","cowplot", "Rtsne", "nlme", "mgcv","Deriv", "Matrix", "slam",
                      "ggpmisc","gridExtra","MASS","msm","sandwich","foreign", "performance", "broom", "gplots", "RColorBrewer", "clValid","dendextend", "gratia", "vader", "ggforce",
                      "ggeffects","DHARMa","pscl","AICcmodavg","lmtest","viridis","trend","tidytext","tidyr","stringr","wordcloud","VennDiagram","tseries","funtimes","systemfonts",
                      "tm", "SnowballC", "wordcloud", "cluster", "factoextra", "topicmodels","tm","text", "textTinyR", "dplyr", "ggplot2", "tm", "text2vec", "uwot","textstem","textmineR")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = TRUE)

if(dir.exists("./figures") == FALSE){dir.create("./figures")}

# Article data -----
# LDA topic modeling
gdelt_all_clean <- read.csv("./data_final/NFWF_articles_final_filter2.csv")[,-1]
gdelt_all_clean <- gdelt_all_clean %>% dplyr::select(-text_LDA,-text_LDA2)
gdelt_all_clean <- gdelt_all_clean %>% arrange(date,title,text)
colSums(is.na(gdelt_all_clean))

gdelt_all_clean$article_id_old <- as.character(gdelt_all_clean$article_id)
gdelt_all_clean$article_id <- c(1:nrow(gdelt_all_clean))

# Basic text preprocessing
# Remove custom stop words
custom_stops <- c(
  "the", "deserttortoise", "sonoran", "their", "more", "some","tortoise", "gopherus", "agassizii",
  "mojave", "desert", "tortoises", "and", "for", "with", "that", "this", "have", "im", "hes", "las", "vegas",
  "from", "your", "just", "like", "when", "will", "they", "them", "there", "wildlife", "species", "were",
  "what", "about", "which", "would", "could", "via", "amp", "do", "dont", "can", "got","go",
  "here", "turtle", "also","these", "than", "been", "because", "us", "fish", "get", "one",
  "said", "says", "according", "news", "report"
)

# create tokens
article_tokens <- gdelt_all_clean %>%
  mutate(subset_text_LDA = tolower(subset_text_LDA)) %>%
  mutate(subset_text_LDA = lemmatize_strings(subset_text_LDA)) %>%
  dplyr::select(article_id, subset_text_LDA) %>%
  unnest_tokens(word, subset_text_LDA) %>%
  anti_join(stop_words, by = "word") %>%
  filter(str_detect(word, "[a-z]"))%>%
  filter(nchar(word) > 2)%>%
  filter(!word %in% custom_stops)

# Good
length(unique(article_tokens$article_id))
gdelt_all_clean_og <- gdelt_all_clean

# Create a Document-Term Matrix (DTM)
token_count <- article_tokens %>%
  count(article_id, word) 

article_dtm <- token_count %>%
  cast_dtm(article_id, word, n)

# Topic number selection via coherence (textmineR) -----

set.seed(1234)

# Function to fit LDA and compute mean topic coherence
calc_coherence_for_k <- function(k, dtm) {

  lda_model <- LDA(
    dtm,
    k = k,
    method = "Gibbs",
    control = list(
      burnin = 1000,
      iter = 2000,
      thin = 100,
      seed = 1234
    )
  )

  # Extract topic-word probabilities safely
  beta_df <- tidy(lda_model, matrix = "beta")
  beta_df <- beta_df %>% rename(t0pic = topic)
  # Build phi matrix (topics x words)
  phi <- beta_df %>%
    pivot_wider(
      id_cols = t0pic,
      names_from = term,
      values_from = beta,
      values_fill = 0
    ) %>%
    arrange(t0pic) %>%
    dplyr::select(-t0pic) %>%
    as.matrix()

  # Step 1: extract the underlying triplet matrix
  dtm_triplet <- slam::as.simple_triplet_matrix(dtm)

  # Step 2: convert to dgCMatrix
  dtm_tm <- Matrix::sparseMatrix(
    i = dtm_triplet$i,
    j = dtm_triplet$j,
    x = dtm_triplet$v,
    dims = c(dtm_triplet$nrow, dtm_triplet$ncol),
    dimnames = dtm_triplet$dimnames
  )
  # Probabilistic topic coherence (Mimno et al. 2011)
  coherence <- CalcProbCoherence(
    phi = phi,
    dtm = dtm_tm,
    M = 10
  )

  mean(coherence)
}

# Run models for K = 2 to 30
k_values <- 2:30

coherence_results <- data.frame(
  k = k_values,
  coherence = sapply(k_values, calc_coherence_for_k, dtm = article_dtm)
)

# Select optimal K
optimal_k <- coherence_results$k[
  which.max(coherence_results$coherence)
]

print(optimal_k)

write.csv(coherence_results,"./results/coherence_2_30_articles.csv")
coherence_results <- read.csv("./results/coherence_2_30_articles.csv")

# Plot coherence by K
ggplot(coherence_results, aes(x = k, y = coherence)) +
  geom_line() +
  geom_point() +
  labs(
    x = "Number of Topics (K)",
    y = "Mean Topic Coherence",
    title = "Topic Coherence by Number of Topics"
  ) +
  theme_minimal()

p_article_coherence <- ggplot(coherence_results, aes(x = k, y = coherence)) +
  geom_line() +
  geom_point() +
  geom_point(
    data = subset(coherence_results, k == 11),
    aes(x = k, y = coherence),
    color = "red",
    size = 3
  ) +
  geom_point(
    data = subset(coherence_results, k == 7),
    aes(x = k, y = coherence),
    color = "pink",
    size = 3
  ) +
  labs(
    x = "Number of Topics (K)",
    y = "Mean Topic Coherence",
    title = "Articles"
  ) +
  theme_minimal()
p_article_coherence
ggsave('./figures/Article_topic_coherence.png',width = 7, height = 4)


### Refit final LDA model with optimal K -----

optimal_k <- 7

final_lda_model <- LDA(
  article_dtm,
  k = optimal_k,
  method = "Gibbs",
  control = list(
    burnin = 1000,
    iter = 2000,
    thin = 100,
    seed = 1234
  )
)

# Examine topics
topics_terms <- tidy(final_lda_model, matrix = "beta")

top_terms <- topics_terms %>%
  group_by(topic) %>%
  slice_max(beta, n = 5) %>%
  ungroup()

top_terms %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip() +
  scale_x_reordered()

ggsave('./figures/Article_topic_betas.png',width = 7, height = 4)

# Assign topic probabilities to documents
doc_topics <- tidy(final_lda_model, matrix = "gamma")

# Attach dominant topic to each article
doc_topics_max <- doc_topics %>%
  group_by(document) %>% arrange(-gamma) %>%
  slice(1) %>%
  ungroup() %>% 
  rename(article_id = document,
         topic = topic,
         topic_prob = gamma) %>% mutate(article_id = as.integer(article_id)) %>% arrange(article_id)


gdelt_all_clean <- gdelt_all_clean %>%
  full_join(doc_topics_max, by = "article_id")

gdelt_topics_time <- gdelt_all_clean %>% group_by(year,topic) %>% summarise(count = n())

ggplot(gdelt_topics_time, aes(x = year, y = count, color = factor(topic))) +
  geom_line(size = 1) +
  # geom_point(size = 2) +
  labs(
    x = "Year",
    y = "Number of Articles",
    color = "Topic",
    title = "Number of Articles per Topic Over Time"
  ) +
  theme_minimal() +
  scale_x_continuous(breaks = unique(gdelt_topics_time$year)) +
  theme(
    legend.position = "right",
    plot.title = element_text(size = 14, face = "bold")
  )

ggsave('./figures/Article_topic_trends.png',width = 7, height = 4)

gdelt_all_clean <- gdelt_all_clean
write.csv(gdelt_all_clean,"./data_final/NFWF_articles_final_filter2_topics.csv")

### UMAP visualization -----

# Create a TF-IDF
tfidf_tbl  <- article_tokens %>%
  count(article_id, word) %>%
  bind_tf_idf(word, article_id, n)
max(tfidf_tbl$article_id)

words_500 <- tfidf_tbl %>% group_by(word) %>% summarise(count = n()) %>% slice_max(order_by = count,n = 500)

tfidf_tbl <- tfidf_tbl %>% filter(word %in% words_500$word)
length(unique(tfidf_tbl$article_id))

  
tfidf_matrix <- tfidf_tbl %>% filter(word %in% words_500$word) %>%
  dplyr::select(article_id, word, tf_idf) %>%
  cast_sparse(article_id,word, tf_idf)


tfidf <- TfIdf$new()
dtm_tfidf_umap <- tfidf$fit_transform(tfidf_matrix)
dtm_tfidf_umap_matrix <- as.matrix(dtm_tfidf_umap)
dtm_tfidf_umap_unique <- dtm_tfidf_umap_matrix[!duplicated(dtm_tfidf_umap_matrix), ]
duplicate_indexes <- which(duplicated(dtm_tfidf_umap_matrix))

# Project using UMAP
set.seed(123)  # R-level reproducibility

projection <- uwot::umap(
  dtm_tfidf_umap_matrix,
  n_neighbors = 15,
  min_dist = 0.1,
  n_components = 2,
  seed = 123
)
# projection <- umap(dtm_matrix, n_neighbors = 15, min_dist = 0.1, n_components = 2)
projection_df <- as.data.frame(projection)
colnames(projection_df) <- c("x", "y")
projection_df$article_id <- rownames(dtm_tfidf_umap_matrix)
# Adding info to the projection dataset
gdelt_all_clean <- gdelt_all_clean %>% mutate(article_id = as.character(article_id))
projection_df <- projection_df %>% left_join(gdelt_all_clean)
write.csv(projection_df,"./results/UMAP_articles.csv")


ggplot(projection_df, aes(x = x, y = y, colour = factor(topic))) +
  geom_point(alpha = 0.6, size = 2) +
  theme_minimal() +
  labs(
    title = "Article UMAP visualization",
    x = "Component 1",
    y = "Component 2",
    colour = "Topic"
  )

n_topics <- 7

summary(projection_df$topic_prob)

ggplot(
  projection_df,
  aes(
    x = x,
    y = y,
    colour = factor(topic),
    shape  = factor(topic)
  )
) +
  geom_point(alpha = 0.6, size = 2) +
  theme_minimal() +
  labs(
    title = "Article UMAP visualization",
    x = "Component 1",
    y = "Component 2",
    colour = "Topic",
    shape  = "Topic"
  ) +
  scale_shape_manual(
    values = c(
      16, # solid circle
      17, # solid triangle
      15, # solid square
      10,  # plus
      7,  # square cross
      8,  # star
      9  # diamond plus
    )
  )

ggsave('./figures/Article_topic_UMAP.png',width = 7, height = 4)

# Twitter data -----

# This dataset represents the cleaned twitter data with the vader sentiment scores
tweet_raw <- read.csv("./data_final/NFWF_tweets_final.csv")[,-1] %>% arrange(Time,User)
str(tweet_raw)
# Adjusting column type and adding time information
tweet_raw <- tweet_raw %>% mutate(Time = as.POSIXlt(Time, tz = "", format="%Y-%m-%d %H:%M:%OS"),
                                  Date = as.Date(Date),
                                  Year = year(Date),
                                  User = as.character(User),
                                  Month = month(Date),
                                  Week_of_start = floor_date(Date, unit = "week"),
                                  Week_of_end = ceiling_date(Date, unit = "week"),.after = "Date")


tweet_raw$tweet_id <- c(1:nrow(tweet_raw))

# Basic text preprocessing
# Remove custom stop words
custom_stops <- c(
  "the", "deserttortoise", "sonoran", "their", "more", "some","tortoise", "gopherus", "agassizii",
  "mojave", "desert", "tortoises", "and", "for", "with", "that", "this", "have", "im", "hes", "las", "vegas",
  "from", "your", "just", "like", "when", "will", "they", "them", "there", "wildlife", "species", "were",
  "what", "about", "which", "would", "could", "via", "amp", "do", "dont", "can", "got","go",
  "here", "turtle", "also","these", "than", "been", "because", "us", "fish", "get", "one",
  "said", "says", "according", "news", "report"
)

tweet_tokens <- tweet_raw %>%
  mutate(Tweet_cleaned = tolower(Tweet_cleaned)) %>%
  mutate(Tweet_cleaned = lemmatize_strings(Tweet_cleaned)) %>%
  dplyr::select(tweet_id, Tweet_cleaned) %>%
  unnest_tokens(word, Tweet_cleaned) %>%
  anti_join(stop_words, by = "word") %>%
  filter(str_detect(word, "[a-z]"))%>%
  filter(nchar(word) > 2)%>%
  filter(!word %in% custom_stops)

length(unique(tweet_tokens$tweet_id))

tweet_raw_og <- tweet_raw
tweet_raw <- tweet_raw %>% filter(tweet_id %in% unique(tweet_tokens$tweet_id)) %>% rename(tweet_id_og = tweet_id) %>% mutate(tweet_id = 1:length(unique(tweet_tokens$tweet_id)))

tweet_tokens <- tweet_raw %>%
  mutate(Tweet_cleaned = tolower(Tweet_cleaned)) %>%
  mutate(Tweet_cleaned = lemmatize_strings(Tweet_cleaned)) %>%
  dplyr::select(tweet_id, Tweet_cleaned) %>%
  unnest_tokens(word, Tweet_cleaned) %>%
  anti_join(stop_words, by = "word") %>%
  filter(str_detect(word, "[a-z]"))%>%
  filter(nchar(word) > 2)%>%
  filter(!word %in% custom_stops)

# Create a Document-Term Matrix (DTM)
token_count <- tweet_tokens %>%
  count(tweet_id, word) 

max(token_count$tweet_id)

tweet_dtm <- token_count %>%
  cast_dtm(tweet_id, word, n)

max(as.integer(unique(tweet_dtm$dimnames$Docs)))
length(unique(tweet_dtm$dimnames$Docs))

# Topic number selection via coherence (textmineR) -----

set.seed(1234)

# Function to fit LDA and compute mean topic coherence
calc_coherence_for_k <- function(k, dtm) {

  lda_model <- LDA(
    dtm,
    k = k,
    method = "Gibbs",
    control = list(
      burnin = 1000,
      iter = 2000,
      thin = 100,
      seed = 1234
    )
  )

  # Extract topic-word probabilities safely
  beta_df <- tidy(lda_model, matrix = "beta")
  beta_df <- beta_df %>% rename(t0pic = topic)
  # Build phi matrix (topics x words)
  phi <- beta_df %>%
    pivot_wider(
      id_cols = t0pic,
      names_from = term,
      values_from = beta,
      values_fill = 0
    ) %>%
    arrange(t0pic) %>%
    dplyr::select(-t0pic) %>%
    as.matrix()

  # Step 1: extract the underlying triplet matrix
  dtm_triplet <- slam::as.simple_triplet_matrix(dtm)

  # Step 2: convert to dgCMatrix
  dtm_tm <- Matrix::sparseMatrix(
    i = dtm_triplet$i,
    j = dtm_triplet$j,
    x = dtm_triplet$v,
    dims = c(dtm_triplet$nrow, dtm_triplet$ncol),
    dimnames = dtm_triplet$dimnames
  )
  # Probabilistic topic coherence (Mimno et al. 2011)
  coherence <- CalcProbCoherence(
    phi = phi,
    dtm = dtm_tm,
    M = 10
  )

  mean(coherence)
}

# Run models for K = 2 to 30
k_values <- 2:30

coherence_results <- data.frame(
  k = k_values,
  coherence = sapply(k_values, calc_coherence_for_k, dtm = tweet_dtm)
)

# Select optimal K
optimal_k <- coherence_results$k[
  which.max(coherence_results$coherence)
]

print(optimal_k)

write.csv(coherence_results,"./results/coherence_2_30_tweets.csv")
coherence_results <- read.csv("./results/coherence_2_30_tweets.csv")


p_tweet_coherence <- ggplot(coherence_results, aes(x = k, y = coherence)) +
  geom_line() +
  geom_point() +
  geom_point(
    data = subset(coherence_results, k == 28),
    aes(x = k, y = coherence),
    color = "red",
    size = 3
  ) +
  geom_point(
    data = subset(coherence_results, k == 7),
    aes(x = k, y = coherence),
    color = "pink",
    size = 3
  ) +
  labs(
    x = "Number of Topics (K)",
    y = "Mean Topic Coherence",
    title = "Tweets"
  ) +
  theme_minimal()
p_tweet_coherence
ggsave('./figures/Tweet_topic_coherence.png',width = 7, height = 4)

coherence_all <- grid.arrange(p_tweet_coherence,p_article_coherence, nrow = 2, ncol = 1)

ggsave('./figures/All_topic_coherence.png',coherence_all,width = 7, height = 4, units = "in", dpi = 300,bg = "transparent")



### Refit final LDA model with optimal K -----

optimal_k <- 7

final_lda_model <- LDA(
  tweet_dtm,
  k = optimal_k,
  method = "Gibbs",
  control = list(
    burnin = 1000,
    iter = 2000,
    thin = 100,
    seed = 1234
  )
)

# Examine topics
topics_terms <- tidy(final_lda_model, matrix = "beta")

top_terms <- topics_terms %>%
  group_by(topic) %>%
  slice_max(beta, n = 5) %>%
  ungroup()

top_terms %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip() +
  scale_x_reordered()

ggsave('./figures/Tweets_topic_betas.png',width = 7, height = 4)


# Assign topic probabilities to documents
doc_topics <- tidy(final_lda_model, matrix = "gamma")
length(unique(doc_topics$document))

# Attach dominant topic to each article
doc_topics_max <- doc_topics %>%
  group_by(document) %>% arrange(-gamma) %>%
  slice(1) %>%
  ungroup() %>% 
  rename(tweet_id = document,
         topic = topic,
         topic_prob = gamma) %>% mutate(tweet_id = as.integer(tweet_id)) %>% arrange(tweet_id)

colSums(is.na(doc_topics_max))
max(doc_topics_max$tweet_id)

tweet_raw <- tweet_raw %>%
  full_join(doc_topics_max, by = "tweet_id")

table(tweet_raw$topic)
table(tweet_raw$topic, tweet_raw$Year)

tweet_raw_time <- tweet_raw %>% filter(!is.na(topic))%>%group_by(Year,topic) %>% summarise(count = n())

ggplot(tweet_raw_time, aes(x = Year, y = count, color = factor(topic))) +
  geom_line(size = 1) +
  # geom_point(size = 2) +
  labs(
    x = "Year",
    y = "Number of Tweets",
    color = "Topic",
    title = "Number of Tweets per Topic Over Time"
  ) +
  theme_minimal() +
  scale_x_continuous(breaks = unique(tweet_raw_time$Year)) +
  theme(
    legend.position = "right",
    plot.title = element_text(size = 14, face = "bold")
  )

ggsave('./figures/Article_topic_trends.png',width = 7, height = 4)

colSums(is.na(tweet_raw))

tweet_topics <- tweet_raw %>% dplyr::select(tweet_id,tweet_id_og,topic,topic_prob)
tweet_raw_og2 <- tweet_raw_og %>% rename(tweet_id_og = tweet_id) %>% left_join(tweet_topics, by = "tweet_id_og")
tweet_raw_og2 <- tweet_raw_og2 %>% dplyr::select(-tweet_id)
write.csv(tweet_raw_og2,"./data_final/NFWF_tweets_final_topics.csv")

### UMAP visualization -----
# Create a TF-IDF
tfidf_tbl  <- tweet_tokens %>%
  count(tweet_id, word) %>%
  bind_tf_idf(word, tweet_id, n)
max(tfidf_tbl$tweet_id)
length(unique(tfidf_tbl$tweet_id))

words_500 <- tfidf_tbl %>% group_by(word) %>% summarise(count = n()) %>% slice_max(order_by = count,n = 500)

tfidf_tbl <- tfidf_tbl %>% filter(word %in% words_500$word) 
length(unique(tfidf_tbl$tweet_id))

tweet_raw <- tweet_raw %>% filter(tweet_id %in% unique(tfidf_tbl$tweet_id)) %>% mutate(tweet_id = 1:length(unique(tfidf_tbl$tweet_id)))

tweet_tokens <- tweet_raw %>%
  mutate(Tweet_cleaned = tolower(Tweet_cleaned)) %>%
  mutate(Tweet_cleaned = lemmatize_strings(Tweet_cleaned)) %>%
  dplyr::select(tweet_id, Tweet_cleaned) %>%
  unnest_tokens(word, Tweet_cleaned) %>%
  anti_join(stop_words, by = "word") %>%
  filter(str_detect(word, "[a-z]"))%>%
  filter(nchar(word) > 2)%>%
  filter(!word %in% custom_stops)

tfidf_tbl  <- tweet_tokens %>%
  count(tweet_id, word) %>%
  bind_tf_idf(word, tweet_id, n)
max(tfidf_tbl$tweet_id)
length(unique(tfidf_tbl$tweet_id))

words_500 <- tfidf_tbl %>% group_by(word) %>% summarise(count = n()) %>% slice_max(order_by = count,n = 500)

tfidf_tbl <- tfidf_tbl %>% filter(word %in% words_500$word) 
length(unique(tfidf_tbl$tweet_id))

tfidf_matrix <- tfidf_tbl %>%
  dplyr::select(tweet_id, word, tf_idf) %>%
  cast_sparse(tweet_id,word, tf_idf)

tfidf <- TfIdf$new()
dtm_tfidf_umap <- tfidf$fit_transform(tfidf_matrix)
dtm_tfidf_umap_matrix <- as.matrix(dtm_tfidf_umap)
dtm_tfidf_umap_unique <- dtm_tfidf_umap_matrix[!duplicated(dtm_tfidf_umap_matrix), ]
duplicate_indexes <- which(duplicated(dtm_tfidf_umap_matrix))

# Project using UMAP
set.seed(123)  # R-level reproducibility

projection <- uwot::umap(
  dtm_tfidf_umap_matrix,
  n_neighbors = 15,
  min_dist = 0.1,
  n_components = 2,
  seed = 123
)
# projection <- umap(dtm_matrix, n_neighbors = 15, min_dist = 0.1, n_components = 2)
projection_df <- as.data.frame(projection)
colnames(projection_df) <- c("x", "y")
projection_df$tweet_id <- rownames(dtm_tfidf_umap_matrix)

tweet_raw <- tweet_raw %>% mutate(tweet_id = as.character(tweet_id))
# Adding info to the projection dataset 
projection_df <- projection_df %>% left_join(tweet_raw)

write.csv(projection_df,"./results/UMAP_tweets.csv")


# ggplot(projection_df, aes(x = x, y = y, colour = factor(topic))) +
#   geom_point(alpha = 0.6, size = 2) +
#   theme_minimal() +
#   labs(
#     title = "Article UMAP visualization",
#     x = "Component 1",
#     y = "Component 2",
#     colour = "Topic"
#   )
projection_df <- read.csv("./results/UMAP_tweets.csv")
n_topics <- 7
hist(projection_df$topic_prob)


ggplot(
  projection_df,
  aes(
    x = x,
    y = y,
    colour = factor(topic),
    shape  = factor(topic)
  )
) +
  geom_point(alpha = 0.6, size = 2) +
  theme_minimal() +
  labs(
    title = "Tweet UMAP visualization",
    x = "Component 1",
    y = "Component 2",
    colour = "Topic",
    shape  = "Topic"
  ) +
  scale_shape_manual(
    values = c(
      16, # solid circle
      17, # solid triangle
      15, # solid square
      10,  # plus
      7,  # square cross
      8,  # star
      9  # diamond plus
      # 3, # circle plus
      # 12, # triangle down
      # 13, # square triangle
      # 4   # x
    )
  )

ggsave('./figures/Tweet_topic_UMAP.png',width = 7, height = 4)




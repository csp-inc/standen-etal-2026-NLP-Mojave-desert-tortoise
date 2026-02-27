## ---------------------------
## 
## Project: TLD NLP
## 
## Script name: 05_Analyses_Final.R
##
## Purpose of script: this script runs all analyses included in the associated manuscript
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
list.of.packages <- c("lubridate","dplyr","parallel","stringr", "ggplot2","zoo","readr","ggpubr","scales","patchwork","cowplot", "Rtsne", "nlme", "mgcv","Deriv", "zoo", "jpeg",
                      "ggpmisc","gridExtra","MASS","msm","sandwich","foreign", "performance", "broom", "gplots", "RColorBrewer", "clValid","dendextend", "gratia", "perm", "fastDummies",
                      "ggeffects","DHARMa","pscl","AICcmodavg","lmtest","viridis","trend","tidytext","tidyr","stringr","wordcloud","VennDiagram","tseries","funtimes","systemfonts",
                      "tm", "SnowballC", "wordcloud", "cluster", "factoextra", "topicmodels","tm","text", "textTinyR", "dplyr", "ggplot2", "tm", "text2vec", "uwot","textstem","forcats","forecast")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = TRUE)

if(dir.exists("./figures") == FALSE){dir.create("./figures")}
## Loading data -----
### Raw datasets -----
# This dataset represents the cleaned twitter data with the vader sentiment scores
tweet_raw <- read.csv("./data_final/NFWF_tweets_final_topics.csv")[,-1]
# Adjusting column type and adding time information
tweet_raw <- tweet_raw %>% mutate(Time = as.POSIXlt(Time, tz = "", format="%Y-%m-%d %H:%M:%OS"),
                                  Date = as.Date(Date),
                                  Year = year(Date),
                                  User = as.character(User),
                                  Month = month(Date),
                                  Week_of_start = floor_date(Date, unit = "week"),
                                  Week_of_end = ceiling_date(Date, unit = "week"),.after = "Date")

# Add sentiment category using cutoffs 
tweet_raw <- tweet_raw %>%
  mutate(VADER_Sentiment = case_when(
    Raw_Sentiment_Score < -0.05 ~ 'negative',  # If Vader is less than -0.05, it's Negative
    Raw_Sentiment_Score > 0.05 ~ 'positive',   # If Vader is greater than 0.05, it's Positive
    TRUE ~ 'neutral'             # Otherwise, it's Neutral
  ))

# Identify when users first appear 
first_seen <- tweet_raw %>%
  group_by(User) %>%
  summarise(First_Date = min(Date), .groups = "drop") %>%
  mutate(Week_of_first = floor_date(First_Date, unit = "week"))

tweet_raw <- tweet_raw %>% left_join(first_seen,by = join_by(User))
tweet_raw <- tweet_raw %>% mutate(Users_new = ifelse(Week_of_first == Week_of_start, 1, 0))

tweet_raw <- tweet_raw %>%
  group_by(User) %>%
  mutate(Users_new = ifelse(Users_new == 1 & cumsum(Users_new == 1) == 1, 1, 0)) %>%
  ungroup() %>% dplyr::select(-First_Date,-Week_of_first)

rm(first_seen)


article_raw <- read.csv("./data_final/NFWF_articles_final_filter2_topics.csv")[,-1]

# Add sentiment category using cutoffs 
article_raw <- article_raw %>%
  mutate(VADER_Sentiment = case_when(
    Raw_Sentiment_Score < -0.05 ~ 'negative',  # If Vader is less than -0.05, it's Negative
    Raw_Sentiment_Score > 0.05 ~ 'positive',   # If Vader is greater than 0.05, it's Positive
    TRUE ~ 'neutral'             # Otherwise, it's Neutral
  ))


### Weekly datasets -----
# This dataset represents weekly aggregated metrics of twitter data and event data for the entire study period
tweet_counts <- read.csv("./data_final/NFWF_weekly_tweets.csv")[,-1]
tweet_counts <- tweet_counts %>% mutate(Week_of_start = as.Date(Week_of_start),
                                        Week_of_end = as.Date(Week_of_end))


max(tweet_counts$Week_of_start)

# Adding sentiment categories using cutoffs as factor for analyses
tweet_counts <- tweet_counts %>%
  mutate(Sentiment = case_when(
    Vader < -0.05 ~ 1,  # If Vader is less than -0.05, it's Negative
    Vader > 0.05 ~ 3,   # If Vader is greater than 0.05, it's Positive
    TRUE ~ 2             # Otherwise, it's Neutral
  )) %>% mutate(Sentiment = factor(Sentiment,
                                   levels = 1:3,
                                   labels = c("Negative",
                                              "Neutral",
                                              "Positive")))

article_counts <- read.csv("./data_final/NFWF_weekly_articles.csv")[,-1] %>% mutate(Week_of_start = as.Date(Week_of_start)) %>% dplyr::select(-Week)


# Adding sentiment categories using cutoffs as factor for analyses
article_counts <- article_counts %>%
  mutate(Sentiment_articles = case_when(
    Vader_articles < -0.05 ~ 1,  # If Vader is less than -0.05, it's Negative
    Vader_articles > 0.05 ~ 3,   # If Vader is greater than 0.05, it's Positive
    TRUE ~ 2             # Otherwise, it's Neutral
  )) %>% mutate(Sentiment_articles = factor(Sentiment_articles,
                                            levels = 1:3,
                                            labels = c("Negative",
                                                       "Neutral",
                                                       "Positive")))

event_counts <- read.csv("./data_final/NFWF_weekly_events.csv")[,-1]

weekly_dat_all <- tweet_counts %>% left_join(article_counts, by = c("Week_of_start")) %>% left_join(event_counts, by = c("Week","Month","Year"))

weekly_dat_all <- weekly_dat_all %>% mutate(Article_Count = if_else(is.na(Article_Count),0,Article_Count)) %>%
  mutate(Article_topic_n = if_else(is.na(Article_topic_n),0,Article_topic_n)) %>%
  mutate(Unique_occurrence = if_else(is.na(Unique_occurrence),0,Unique_occurrence)) %>%
  mutate(Occurrence = if_else(is.na(Occurrence),0,Occurrence))

# Normalizing users based on temporal twitter trends

twitter_users_year <- read.csv("./data_final/Statista_Twitter_use_data_Yearly_users.csv")
colnames(twitter_users_year) <- c("Year","Users_ALL")
twitter_users_year$Users_ALL_real <- twitter_users_year$Users_ALL*1000000

weekly_dat_all <- weekly_dat_all %>% left_join(twitter_users_year)
weekly_dat_all <- weekly_dat_all %>% mutate(Users_new = if_else(Users_new == 0,1,Users_new))
weekly_dat_all <- weekly_dat_all %>% mutate(Users_adjusted = Users/Users_ALL_real) %>% mutate(Users_new_adjusted = Users_new/Users_ALL_real)
weekly_dat_all <- weekly_dat_all %>% mutate(Tweet_per_user = Tweet_Count/Users)
weekly_dat_all <- weekly_dat_all %>% arrange(Week_of_start) %>% mutate(log_Tweet_Count = log(Tweet_Count),log_Article_Count = log(Article_Count+1),log_Users = log(Users),log_Users_new = log(Users_new),log_Users_adjusted = log(Users_adjusted),log_Users_new_adjusted = log(Users_new_adjusted),log_Tweets_per_user = log(Tweet_per_user))
weekly_dat_all <- weekly_dat_all %>% mutate(Prop_new = Users_new/Users, log_Prop_new = log(Prop_new+1))


tweet_raw <- tweet_raw %>% left_join(twitter_users_year)


## Mann-Kendall (MK) test for trends -----
# Adding 1 to prevent log of 0
article_counts <- article_counts %>% mutate(log_Article_Count = log(Article_Count))


mk.test(weekly_dat_all$Vader)
mk.test(weekly_dat_all$Users_adjusted) # significant and decreasing
mk.test(weekly_dat_all$Users_new_adjusted) # significant and decreasing
mk.test(weekly_dat_all$Tweet_per_user) # significant and increasing


mk.test(weekly_dat_all$Users)
mk.test(weekly_dat_all$Users_new)
mk.test(weekly_dat_all$Tweet_Count)

mk.test(article_counts$Vader_articles)
mk.test(article_counts$Article_Count)


## Chi-squared test to compare distribution of sentiment -----
### Tweets -----
mean(tweet_raw$Raw_Sentiment_Score)
sd(tweet_raw$Raw_Sentiment_Score)

groups <- levels(factor(tweet_raw$VADER_Sentiment))
combos <- combn(groups, 2, simplify = FALSE)

results <- lapply(combos, function(pair) {
  df <- tweet_raw %>% filter(VADER_Sentiment %in% pair)
  subset_table <- table(df$VADER_Sentiment)
  test <- chisq.test(subset_table)
  data.frame(
    group1 = pair[1],
    group2 = pair[2],
    p_value = test$p.value,
    x2 = test$statistic
  )
})

results_df <- do.call(rbind, results)
results_df$p_adj <- p.adjust(results_df$p_value, method = "bonferroni")
print(results_df)


p_tweet_bar <- ggplot(
  tweet_raw,
  aes(x = as.factor(VADER_Sentiment), fill = as.factor(VADER_Sentiment))
) +  
  geom_bar() +
  geom_text(
    stat = "count",
    aes(label = after_stat(count)),
    color = "black",
    vjust = -0.3
  ) +
  # scale_fill_manual(values = c("#bd242a", "#d14f2b", "#f1c40f")) +
  scale_fill_manual(values = c("gray", "gray", "gray")) +
  scale_x_discrete(
    breaks = c("negative", "neutral", "positive"),
    labels = c("Negative", "Neutral", "Positive")
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  theme_minimal() +
  labs(x = "", y = "Count", title = "(a) Tweets") +
  theme(legend.position = "none",
        plot.title = element_text(size = 14, face = "bold")
  ) 

p_tweet_bar

ggsave('./figures/Chi-squared_bar_tweets.png',width = 7, height = 4)


### Articles -----
mean(article_raw$Raw_Sentiment_Score)
sd(article_raw$Raw_Sentiment_Score)

groups <- levels(factor(article_raw$VADER_Sentiment))
combos <- combn(groups, 2, simplify = FALSE)

results <- lapply(combos, function(pair) {
  df <- article_raw %>% filter(VADER_Sentiment %in% pair)
  subset_table <- table(df$VADER_Sentiment)
  test <- chisq.test(subset_table)
  data.frame(
    group1 = pair[1],
    group2 = pair[2],
    p_value = test$p.value,
    x2 = test$statistic
  )
})

results_df <- do.call(rbind, results)
results_df$p_adj <- p.adjust(results_df$p_value, method = "bonferroni")
print(results_df)


p_article_bar <- ggplot(
  article_raw,
  aes(x = as.factor(VADER_Sentiment), fill = as.factor(VADER_Sentiment))
) +  
  geom_bar() +
  geom_text(
    stat = "count",
    aes(label = after_stat(count)),
    color = "black",
    vjust = -0.3
  ) +
  # scale_fill_manual(values = c("#bd242a", "#d14f2b", "#f1c40f")) +
  scale_fill_manual(values = c("gray", "gray", "gray")) +
  scale_x_discrete(
    breaks = c("negative", "neutral", "positive"),
    labels = c("Negative", "Neutral", "Positive")
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  theme_minimal() +
  labs(x = "", y = NULL, title = "(b) Articles") +
  theme(legend.position = "none",
        plot.title = element_text(size = 14, face = "bold"))

p_article_bar
ggsave('./figures/Chi-squared_bar_articles.png',width = 7, height = 4)


p_all_bar <- grid.arrange(p_tweet_bar,p_article_bar, nrow = 1, ncol = 2,widths = c(0.42,0.4))
ggsave("./figures/Chi-squared_bar.jpeg", p_all_bar,
       width = 7, height = 4, units = "in", dpi = 300)

## Finding top words in each sentiment category -----
stopwords <- c(
  "the", "deserttortoise", "sonoran", "their", "more", "some","tortoise", "gopherus", "agassizii",
  "mojave", "desert", "tortoises", "and", "for", "with", "that", "this", "have", "im", "hes", "las", "vegas",
  "from", "your", "just", "like", "when", "will", "they", "them", "there", "wildlife", "species", "were",
  "what", "about", "which", "would", "could", "via", "amp", "do", "dont", "can", "got","go","don",
  "here", "turtle", "also","these", "than", "been", "because", "us", "fish", "get", "one"
)

# Function to clean and tokenize and remove stop words and words with numerics
clean_text <- function(text, stopwords) {
  words <- str_extract_all(tolower(text), "\\b\\w+\\b")[[1]]
  words <- words[!grepl("[0-9]", words)]      # Remove words with digits
  words <- words[nchar(words) > 2]            # Remove 1- and 2-letter words
  words[!words %in% c(stopwords, stopwords("en"))]
}

# Sentiments to process
sentiments <- c("positive", "negative", "neutral")
sentiment_top25_list <- vector(mode = "list", length = 3)
names(sentiment_top25_list) <- sentiments

### Identify the top words in each sentiment category for tweets -----
for (sentiment in sentiments) {
  # Filter tweets for this sentiment
  sentiment_tweets <- tweet_raw %>%
    filter(VADER_Sentiment == sentiment)
  
  # Extract and clean words from tweets
  all_words <- unlist(lapply(sentiment_tweets$Tweet_cleaned, clean_text, stopwords = stopwords))
  
  # Get word frequencies
  word_freq <- table(all_words)
  word_freq_sorted <- sort(word_freq, decreasing = TRUE)
  
  # Top 50 for word cloud
  top_terms <- head(word_freq_sorted, 50)
  
  # Generate word cloud
  wordcloud(
    words = names(top_terms),
    freq = as.numeric(top_terms),
    min.freq = 1,
    max.words = 50,
    random.order = FALSE,
    colors = brewer.pal(8, "Dark2")
  )
  title(paste("Word Cloud for", tools::toTitleCase(sentiment), "Sentiment"))
  
  # Top 25 words list
  top_25 <- head(word_freq_sorted, 25)
  sentiment_top25_list[[sentiment]] <- as.data.frame(top_25)
  
  cat("\nTop 25 Words for", tools::toTitleCase(sentiment), "Sentiment:\n")
  print(data.frame(word = names(top_25), count = as.numeric(top_25)))
  cat("\n----------------------------\n")
}
rm(sentiment_tweets)
saveRDS(sentiment_top25_list,"./top_25_words_tweets.RDS")

# Sentiments to process
sentiments <- c("positive", "negative", "neutral")
sentiment_top25_list <- vector(mode = "list", length = 3)
names(sentiment_top25_list) <- sentiments


### Identify the top words in each sentiment category for articles -----
for (sentiment in sentiments) {
  # Filter tweets for this sentiment
  sentiment_articles <- article_raw %>%
    filter(VADER_Sentiment == sentiment)
  
  # Extract and clean words from tweets
  all_words <- unlist(lapply(sentiment_articles$subset_text_VADER, clean_text, stopwords = stopwords))
  
  # Get word frequencies
  word_freq <- table(all_words)
  word_freq_sorted <- sort(word_freq, decreasing = TRUE)
  
  # Top 50 for word cloud
  top_terms <- head(word_freq_sorted, 50)
  
  # Generate word cloud
  wordcloud(
    words = names(top_terms),
    freq = as.numeric(top_terms),
    min.freq = 1,
    max.words = 50,
    random.order = FALSE,
    colors = brewer.pal(8, "Dark2")
  )
  title(paste("Word Cloud for", tools::toTitleCase(sentiment), "Sentiment"))
  
  # Top 25 words list
  top_25 <- head(word_freq_sorted, 25)
  sentiment_top25_list[[sentiment]] <- as.data.frame(top_25)
  
  cat("\nTop 25 Words for", tools::toTitleCase(sentiment), "Sentiment:\n")
  print(data.frame(word = names(top_25), count = as.numeric(top_25)))
  cat("\n----------------------------\n")
}
rm(sentiment_articles)
saveRDS(sentiment_top25_list,"./top_25_words_articles.RDS")

## Creating venn diagram of top words -----

sentiment_top25_list <- readRDS("./top_25_words_tweets.RDS")

### Create venn diagram to show overlapping top words -----
# Prepare a palette of 3 colors with R colorbrewer:
myCol <- brewer.pal(3, "Pastel2")

# Chart
venn.diagram(
  x = list(sentiment_top25_list$positive$all_words, sentiment_top25_list$negative$all_words, sentiment_top25_list$neutral$all_words),
  category.names = c("Positive" , "Negative" , "Neutral"),
  filename = './figures/25Sentiment_venn_diagramm.png',
  output=TRUE,
  
  # Output features
  imagetype="png" ,
  height = 480 , 
  width = 480 , 
  resolution = 300,
  compression = "lzw",
  
  # Circles
  lwd = 2,
  lty = 'blank',
  fill = myCol,
  
  # Numbers
  cex = .6,
  fontface = "bold",
  fontfamily = "sans",
  
  # Set names
  cat.cex = 0.6,
  cat.fontface = "bold",
  cat.default.pos = "outer",
  cat.pos = c(-27, 27, 135),
  cat.dist = c(0.055, 0.055, 0.085),
  cat.fontfamily = "sans",
  rotation = 1
)


# Example word sets
positive <- sentiment_top25_list$positive$all_words
negative <- sentiment_top25_list$negative$all_words
neutral  <- sentiment_top25_list$neutral$all_words

# Compute overlaps
venn_words <- list(
  Positive = positive,
  Negative = negative,
  Neutral  = neutral
)

# Helper function to get region words
get_region_words <- function(a, b, c) {
  ab <- intersect(a, b)
  ac <- intersect(a, c)
  bc <- intersect(b, c)
  abc <- Reduce(intersect, list(a, b, c))
  a_only <- setdiff(setdiff(a, b), c)
  b_only <- setdiff(setdiff(b, a), c)
  c_only <- setdiff(setdiff(c, a), b)
  ab_only <- setdiff(ab, abc)
  ac_only <- setdiff(ac, abc)
  bc_only <- setdiff(bc, abc)
  
  list(
    A = paste(a_only, collapse = "\n"),
    B = paste(b_only, collapse = "\n"),
    C = paste(c_only, collapse = "\n"),
    AB = paste(ab_only, collapse = "\n"),
    AC = paste(ac_only, collapse = "\n"),
    BC = paste(bc_only, collapse = "\n"),
    ABC = paste(abc, collapse = "\n")
  )
}

labels <- get_region_words(positive, negative, neutral)


# Now build the diagram using custom labels
venn.plot <- draw.triple.venn(
  area1 = length(positive),
  area2 = length(negative),
  area3 = length(neutral),
  n12 = length(intersect(positive, negative)),
  n23 = length(intersect(negative, neutral)),
  n13 = length(intersect(positive, neutral)),
  n123 = length(Reduce(intersect, list(positive, negative, neutral))),
  category = c("Positive", "Negative", "Neutral"),
  fill = c("#f1c40f", "#bd242a","#d14f2b"),
  cex = .8,
  cat.cex = 1,
  fontface = "bold",
  cat.fontface = "bold",
  cat.fontfamily = "arial",
  fontfamily = "arial",
  label.col = "black",
  lty = "blank"
)
dev.off()
# Open JPEG device
jpeg("./figures/custom_venn_diagram_tweets.jpeg", width = 2000, height = 2000, res = 300)
# Overwrite the label values manually
venn.plot[[7]]$label <- labels$A
venn.plot[[9]]$label <- labels$B
venn.plot[[8]]$label <- labels$AB
venn.plot[[13]]$label <- labels$C
venn.plot[[10]]$label <- labels$AC
venn.plot[[12]]$label <- labels$BC
venn.plot[[11]]$label <- labels$ABC

# Save it
ven <- grid.draw(venn.plot)
dev.off()

img1 <- rasterGrob(readJPEG("./figures/custom_venn_diagram_tweets.jpeg"))
img2 <- rasterGrob(readJPEG("./figures/custom_venn_diagram_articles.jpeg"))


title1 <- textGrob("Tweets", gp = gpar(fontsize = 14, face = "bold"))
title2 <- textGrob("Articles", gp = gpar(fontsize = 14, face = "bold"))

all_ven <- grid.arrange(img1, img2, ncol = 2)

all_ven <- grid.arrange(
  arrangeGrob(title1, img1, ncol = 1, heights = c(0.05, 1)),
  arrangeGrob(title2, img2, ncol = 1, heights = c(0.05, 1)),
  ncol = 2
)

ggsave("./figures/custom_venn_diagram_all.jpeg", all_ven,
       width = 10, height = 7, units = "in", dpi = 300)

## Comparing sentiment in weeks with and without occurance -----

weekly_dat_all <- weekly_dat_all %>% mutate(Occurrence_binary = if_else(Occurrence >=1,1,0))

perm_test <- function(df,variable) {
  permTS(
    x = df[[variable]][df$Occurrence_binary == 1],
    y = df[[variable]][df$Occurrence_binary == 0],
    method = 'exact.mc',
    alternative = "two.sided",
    control = permControl(p.conf.level = 0.95, nmc = 10000, seed = 123)
  )
}

perm_vader <- perm_test(weekly_dat_all,"Vader")
perm_new_users_adj <- perm_test(weekly_dat_all,"Users_new_adjusted")
perm_tweet_per <- perm_test(weekly_dat_all,"Tweet_per_user")
perm_users_adj <- perm_test(weekly_dat_all,"Users_adjusted")

perm_vader
perm_users_adj
perm_new_users_adj
perm_tweet_per

weekly_dat_all <- weekly_dat_all %>% mutate(Group = if_else(Occurrence_binary == 1,"Event","Non-event"))


mean_median_df <- weekly_dat_all %>%
  group_by(Group) %>%
  summarise(
    mean   = mean(Vader, na.rm = TRUE),
    median = median(Vader, na.rm = TRUE),
    max = quantile(Vader, na.rm = TRUE, c(.75)),
    .groups = "drop"
  )

vader_perm_box <- ggplot(
  weekly_dat_all,
  aes(
    x = factor(Group, levels = c("Event","Non-event")),
    y = Vader,
    fill = Group
  )
) +
  geom_boxplot(alpha = 0.6, outlier.shape = NA) +
  scale_fill_manual(
    values = c("Event" = "gray", "Non-event" = "white")
  ) +
  labs(
    title = "(a) Sentiment",
    x = NULL,
    y = "Mean sentiment"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5,size = 16, face = "bold"),
    legend.position = "none",
    axis.text.y = element_text(size = 12),
    axis.text.x = element_text(size = 12)
  )

vader_perm_box

mean_median_df <- weekly_dat_all %>%
  group_by(Group) %>%
  summarise(
    mean   = mean(log_Users_new_adjusted, na.rm = TRUE),
    median = median(log_Users_new_adjusted, na.rm = TRUE),
    max = quantile(log_Users_new_adjusted, na.rm = TRUE, c(.75)),
    .groups = "drop"
  )

users_new_perm_box <- ggplot(
  weekly_dat_all,
  aes(
    x = factor(Group, levels = c("Event","Non-event")),
    y = log_Users_new_adjusted,
    fill = Group
  )
) +
  geom_boxplot(alpha = 0.6, outlier.shape = NA) +
  scale_fill_manual(
    values = c("Event" = "gray", "Non-event" = "white")
  ) +
  labs(
    title = "(c) New Users",
    x = NULL,
    y = "Standardized count (log)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5,size = 16, face = "bold"),
    legend.position = "none",
    axis.text.y = element_text(size = 12),
    axis.text.x = element_text(size = 12)
  )

users_new_perm_box

mean_median_df <- weekly_dat_all %>%
  group_by(Group) %>%
  summarise(
    mean   = mean(log_Users_adjusted, na.rm = TRUE),
    median = median(log_Users_adjusted, na.rm = TRUE),
    max = quantile(log_Users_adjusted, na.rm = TRUE, c(.75)),
    .groups = "drop"
  )

users_perm_box <- ggplot(
  weekly_dat_all,
  aes(
    x = factor(Group, levels = c("Event","Non-event")),
    y = log_Users_adjusted,
    fill = Group
  )
) +
  geom_boxplot(alpha = 0.6, outlier.shape = NA) +
  scale_fill_manual(
    values = c("Event" = "gray", "Non-event" = "white")
  ) +
  labs(
    title = "(b) Users",
    x = NULL,
    y = "Standardized count (log)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5,size = 16, face = "bold"),
    legend.position = "none",
    axis.text.y = element_text(size = 12),
    axis.text.x = element_text(size = 12)
  )

users_perm_box

mean_median_df <- weekly_dat_all %>%
  group_by(Group) %>%
  summarise(
    mean   = mean(Tweet_per_user, na.rm = TRUE),
    median = median(Tweet_per_user, na.rm = TRUE),
    max = quantile(Tweet_per_user, na.rm = TRUE, c(.75)),
    .groups = "drop"
  )

tweets_perm_box <- ggplot(
  weekly_dat_all,
  aes(
    x = factor(Group, levels = c("Event","Non-event")),
    y = Tweet_per_user,
    fill = Group
  )
) +
  geom_boxplot(alpha = 0.6, outlier.shape = NA) +
  scale_fill_manual(
    values = c("Event" = "gray", "Non-event" = "white")
  ) +
  labs(
    title = "(d) Tweets per User",
    x = NULL,
    y = "Count"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5,size = 16, face = "bold"),
    legend.position = "none",
    axis.text.y = element_text(size = 12),
    axis.text.x = element_text(size = 12)
  )

tweets_perm_box

ttest_results <- grid.arrange(vader_perm_box,users_perm_box,users_new_perm_box,tweets_perm_box, ncol = 4)

ggsave('./figures/Permutation_box_all_nolabs.png',ttest_results,width = 11, height = 4)


## Finding weekly outliers -----

identify_outliers <- function(df,variable) {
  # Calculate quartiles and IQR
  Q1 <- quantile(df[[variable]], 0.25, na.rm = TRUE)
  Q3 <- quantile(df[[variable]], 0.75, na.rm = TRUE)
  IQR_value <- Q3 - Q1
  
  lower_bound <- Q1 - 1.5 * IQR_value
  upper_bound <- Q3 + 1.5 * IQR_value
  
  # Create and rename the new column in one step
  df <- df %>%
    mutate(
      !!paste0(variable, "_Outlier") := case_when(
        .data[[variable]] < lower_bound ~ "Lower Outlier",
        .data[[variable]] > upper_bound ~ "Upper Outlier",
        TRUE ~ "Not Outlier"
      )
    )
  
  return(df)
}



weekly_dat_all <- identify_outliers(weekly_dat_all,"Vader")
weekly_dat_all <- identify_outliers(weekly_dat_all,"Tweet_per_user")
weekly_dat_all <- identify_outliers(weekly_dat_all,"Users_adjusted")
weekly_dat_all <- identify_outliers(weekly_dat_all,"Users_new_adjusted")
weekly_dat_all <- identify_outliers(weekly_dat_all,"Article_Count")
weekly_dat_all <- identify_outliers(weekly_dat_all,"Vader_articles")

tweet_counts_outliers_sentiment <- weekly_dat_all %>% filter(Vader_Outlier != "Not Outlier")
tweet_counts_outliers_count <- weekly_dat_all %>% filter(Tweet_per_user_Outlier != "Not Outlier")
tweet_counts_outliers_users <- weekly_dat_all %>% filter(Users_adjusted_Outlier != "Not Outlier")
tweet_counts_outliers_users_new <- weekly_dat_all %>% filter(Users_new_adjusted_Outlier != "Not Outlier")
article_counts_outliers_count <- weekly_dat_all %>% filter(Article_Count_Outlier != "Not Outlier")
article_counts_outliers_sentiment <- weekly_dat_all %>% filter(Vader_articles_Outlier != "Not Outlier")


# Filter data by year and date
dual_plot_data <- weekly_dat_all %>%
  rename(Average_Sentiment = Vader,User_Count = Users_adjusted, log_User_Count = log_Users_adjusted,Users_New = Users_new_adjusted,log_Users_New = log_Users_new_adjusted)

dual_plot_data <- dual_plot_data %>%
  mutate(Average_Sentiment_articles = na.approx(Vader_articles,na.rm = FALSE))

x_limits <- range(dual_plot_data$Week_of_start)
x_breaks <- seq(min(dual_plot_data$Week_of_start), max(dual_plot_data$Week_of_start), by = "1 year")

tweet_sentiment_outliers_p <- ggplot(dual_plot_data, aes(x=Week_of_start)) +
  geom_hline(
    aes(yintercept = 0),
    linetype = "solid", size = .5,color = 'gray'
  ) +
  geom_line( aes(y=Average_Sentiment), size=.6, color="black") + 
  geom_point( data = tweet_counts_outliers_sentiment, aes(y=Vader,color=Vader_Outlier), size=2) +
  scale_color_manual(values = c("Lower Outlier" = "#B23AEE", "Upper Outlier" = "#FFD700")) + 
  scale_x_date(limits = x_limits, breaks = x_breaks, date_labels = "%Y") + labs(x = NULL, y = "Tweet Sentiment", title = NULL)+
  theme_classic() + theme(legend.position = "none") +
  theme(
    plot.background  = element_rect(fill = NA, color = NA),
    panel.background = element_rect(fill = NA, color = NA),
    legend.background = element_rect(fill = NA, color = NA),
    legend.box.background = element_rect(fill = NA, color = NA)
  )

tweet_sentiment_outliers_p


article_sentiment_outliers_p <- ggplot(dual_plot_data, aes(x=Week_of_start)) +
  geom_hline(
    aes(yintercept = 0),
    linetype = "solid", size = .5,color = 'gray'
  ) +
  geom_line( aes(y=Average_Sentiment_articles), size=.6, color="black") + 
  geom_point( data = article_counts_outliers_sentiment, aes(y=Vader_articles,color=Vader_articles_Outlier), size=2) +
  scale_color_manual(values = c("Lower Outlier" = "#B23AEE", "Upper Outlier" = "#FFD700")) + 
  scale_x_date(limits = x_limits, breaks = x_breaks, date_labels = "%Y") + labs(x = NULL, y = "Article Sentiment", title = NULL)+
  theme_classic() + theme(legend.position = "none") +
  theme(
    plot.background  = element_rect(fill = NA, color = NA),
    panel.background = element_rect(fill = NA, color = NA),
    legend.background = element_rect(fill = NA, color = NA),
    legend.box.background = element_rect(fill = NA, color = NA)
  )

article_sentiment_outliers_p

tweet_count_outliers_p <- ggplot(dual_plot_data, aes(x=Week_of_start)) +
  geom_line( aes(y=Tweet_per_user), size=.6, color="black") + 
  geom_point( data = tweet_counts_outliers_count, aes(y=Tweet_per_user,color=Tweet_per_user_Outlier), size=2) +
  scale_color_manual(values = c("Lower Outlier" = "#B23AEE", "Upper Outlier" = "#FFD700")) + 
  scale_x_date(limits = x_limits, breaks = x_breaks, date_labels = "%Y") + labs(x = NULL, y = "Tweets per User", title = NULL)+
  theme_classic() + theme(legend.position = "none") +
  theme(
    plot.background  = element_rect(fill = NA, color = NA),
    panel.background = element_rect(fill = NA, color = NA),
    legend.background = element_rect(fill = NA, color = NA),
    legend.box.background = element_rect(fill = NA, color = NA)
  )

tweet_count_outliers_p

article_count_outliers_p <- ggplot(dual_plot_data, aes(x=Week_of_start)) +
  geom_line( aes(y=Article_Count), size=.6, color="black") + 
  geom_point( data = article_counts_outliers_count, aes(y=Article_Count,color=Article_Count_Outlier), size=2) +
  scale_color_manual(values = c("Lower Outlier" = "#B23AEE", "Upper Outlier" = "#FFD700")) + 
  scale_x_date(limits = x_limits, breaks = x_breaks, date_labels = "%Y") + labs(x = NULL, y = "Article Count", title = NULL)+
  theme_classic() + theme(legend.position = "none") +
  theme(
    plot.background  = element_rect(fill = NA, color = NA),
    panel.background = element_rect(fill = NA, color = NA),
    legend.background = element_rect(fill = NA, color = NA),
    legend.box.background = element_rect(fill = NA, color = NA)
  )

article_count_outliers_p

tweet_users_outliers_p <- ggplot(dual_plot_data, aes(x=Week_of_start)) +
  geom_line( aes(y=log_User_Count), size=.6, color="black") +
  geom_point( data = tweet_counts_outliers_users, aes(y=log_Users_adjusted,color=Users_adjusted_Outlier), size=2) +
  scale_color_manual(values = c("Lower Outlier" = "#B23AEE", "Upper Outlier" = "#FFD700")) + 
  scale_x_date(limits = x_limits, breaks = x_breaks, date_labels = "%Y") + labs(x = NULL, y = "Users (log)", title = NULL)+
  theme_classic() + theme(legend.position = "none") +
  theme(
    plot.background  = element_rect(fill = NA, color = NA),
    panel.background = element_rect(fill = NA, color = NA),
    legend.background = element_rect(fill = NA, color = NA),
    legend.box.background = element_rect(fill = NA, color = NA)
  )

tweet_users_outliers_p

tweet_users_new_outliers_p <- ggplot(dual_plot_data, aes(x=Week_of_start)) +
  # geom_line( aes(y=log_Users_New), size=.75, color="#528970") + 
  geom_line( aes(y=log_Users_New), size=.6, color="black") + 
  geom_point( data = tweet_counts_outliers_users_new, aes(y=log_Users_new_adjusted,color=Users_new_adjusted_Outlier), size=2) +
  # geom_line( aes(y=Users_New), size=.75, color="#528970") + 
  # geom_point( data = tweet_counts_outliers_users_new, aes(y=Users_new_adjusted,color=Users_new_adjusted_Outlier), size=2) +
  scale_color_manual(values = c("Lower Outlier" = "#B23AEE", "Upper Outlier" = "#FFD700")) + 
  scale_x_date(limits = x_limits, breaks = x_breaks, date_labels = "%Y") + labs(x = NULL, y = "New Users (log)", title = NULL)+
  theme_classic() + theme(legend.position = "none") +
  theme(
    plot.background  = element_rect(fill = NA, color = NA),
    panel.background = element_rect(fill = NA, color = NA),
    legend.background = element_rect(fill = NA, color = NA),
    legend.box.background = element_rect(fill = NA, color = NA)
  )

tweet_users_new_outliers_p


occurrence_p <- ggplot(dual_plot_data, aes(x = Week_of_start, y = Unique_occurrence)) +
  geom_line( aes(y=Unique_occurrence), size=.6, color="black") +
  scale_x_date(
    limits = x_limits,
    breaks = x_breaks,
    date_labels = "%Y"
  )+
  labs(
    x = "Year",
    y = "Event Count",
    title = NULL
  ) +
  theme_classic() +
  theme(
    plot.background  = element_rect(fill = NA, color = NA),
    panel.background = element_rect(fill = NA, color = NA),
    legend.background = element_rect(fill = NA, color = NA),
    legend.box.background = element_rect(fill = NA, color = NA)
  )



final_outliers_plot_all <- plot_grid(tweet_sentiment_outliers_p, article_sentiment_outliers_p,tweet_count_outliers_p, article_count_outliers_p,tweet_users_outliers_p,tweet_users_new_outliers_p,occurrence_p,ncol = 1, align = "v", axis = "lr",rel_heights = c(.4,.4, .4, .4, .4, .4, .4))
final_outliers_plot_all

ggsave('./figures/outliers_trend_adjusted_all.png',final_outliers_plot_all,width = 10, height = 10, units = "in", dpi = 300,bg = "transparent")
ggsave('./figures/outliers_trend_adjusted_all.jpeg',final_outliers_plot_all,width = 10, height = 10, units = "in", dpi = 300,bg = "transparent")


## Time series modeling -----
# Load in events data
events_data <- read_csv("./data_final/NFWF_events_final.csv")[,-1]

weekly_dat_all$uniq <- paste0(weekly_dat_all$Week,weekly_dat_all$Month,weekly_dat_all$Year)

# Create data for arima modeling

# Creating dummy numeric variables for use later
Employer_type_id <- as.data.frame(table(events_data$Employer_type))
colnames(Employer_type_id) <- c("Employer_type","Freq")
Employer_type_id$Employer_type_id <- c(1:nrow(Employer_type_id))

Org_size_id <- as.data.frame(table(events_data$Org_size))
colnames(Org_size_id) <- c("Org_size","Freq")
Org_size_id <- Org_size_id[c(1,5,3,4,6,2),]
Org_size_id$Org_size_id <- c(1:nrow(Org_size_id))

Staff_id <- as.data.frame(table(events_data$Staff))
colnames(Staff_id) <- c("Staff","Freq")
Staff_id <- Staff_id[c(3,1,5,4,2),]
Staff_id$Staff_id <- c(1:nrow(Staff_id))

Attendance_id <- as.data.frame(table(events_data$Attendance))
colnames(Attendance_id) <- c("Attendance","Freq")
Attendance_id <- Attendance_id[c(3,1,4,5,2,6),]
Attendance_id$Attendance_id <- c(1:nrow(Attendance_id))

# Adding dummy columns
events_data <- events_data %>%
  left_join(Org_size_id, by = "Org_size") %>% left_join(Employer_type_id, by = "Employer_type") %>% 
  left_join(Staff_id, by = "Staff") %>% left_join(Attendance_id, by = "Attendance") %>% dplyr::select(-Freq.x,-Freq.y, -Freq.x.x, -Freq.y.y)

# Creating unique column to filter events for weeks actually used in final time series
events_data$uniq <- paste0(events_data$Week,events_data$Month,events_data$Year)
events_data_arima <- events_data %>% filter(uniq %in% weekly_dat_all$uniq)

events_data_arima <- fastDummies::dummy_cols(
  events_data_arima,
  select_columns = c("Org_size", "Employer_type","Staff","Attendance"),  # columns to convert
  remove_first_dummy = FALSE,   # keep all unique values
  remove_selected_columns = FALSE  # remove original categorical columns
)


# Deal with weeks that had multiple events in them
events_data_arima_week <- split(events_data_arima,events_data_arima$uniq)
# Split into two lists based on length of the items
more_than_one <- events_data_arima_week[sapply(events_data_arima_week, nrow) > 1]
one_or_less <- events_data_arima_week[sapply(events_data_arima_week, nrow) <= 1]

single_events_df <- bind_rows(one_or_less)
for(i in 1:length(more_than_one)){
  week_events <- as.data.frame(more_than_one[[i]])
  # Paste unique data for the first 10 columns to retain for records
  for(j in c("Respondent ID","Name","Name_Aggregated","Employer_type","Title","Role","event_id")){
    week_events[[j]] <- paste(sort(unique(week_events[[j]])), collapse = "|")
  }
  
  for(j in c(12:24,36:57)){
    week_events[,j] <- sum(week_events[,j])
  }
  
  week_events$Org_size_id <- max(week_events$Org_size_id, na.rm = TRUE)
  week_events$Staff_id <- max(week_events$Staff_id, na.rm = TRUE)
  week_events$Attendance_id <- max(week_events$Attendance_id, na.rm = TRUE)
  
  week_events$n_events <- nrow(week_events)
  
  week_events$Employer_type_n <- length(unique(week_events$Employer_type_id))
  week_events$Employer_type_id <- mode(week_events$Employer_type_id)
  more_than_one[[i]] <- week_events[1,]
}

single_events_df$n_events <- 1
single_events_df$Employer_type_n <- 1

multiple_events_df <- bind_rows(more_than_one)
events_used <- rbind(multiple_events_df,single_events_df) %>% arrange(uniq) %>% dplyr::select(-Info_fixed,-Frequency,-Staff,-Attendance,-Type,-Week,-month,-Year,-Month,-Employer_type,-Org_size)

Employer_type_id <- rbind(Employer_type_id,Employer_type_id[nrow(Employer_type_id),])
Employer_type_id[nrow(Employer_type_id),'Employer_type_id'] <- 'numeric'
levels(Employer_type_id$Employer_type) <- c(levels(Employer_type_id$Employer_type), "Multiple")
Employer_type_id[nrow(Employer_type_id),'Employer_type'] <- "Multiple"

events_used <- events_used %>%
  left_join(Org_size_id, by = "Org_size_id") %>% left_join(Employer_type_id, by = "Employer_type_id") %>% 
  left_join(Staff_id, by = "Staff_id") %>% left_join(Attendance_id, by = "Attendance_id") %>% dplyr::select(-Freq.x,-Freq.y, -Freq.x.x, -Freq.y.y)

arima_dat <- weekly_dat_all %>% left_join(events_used)


### Temporal cross correlations -----
# Create time series objects with the weekly tweet data for sentiment and count
# https://atsa-es.github.io/atsa-labs/sec-tslab-time-series-plots.html
str(tweet_counts)
arima_dat <- arima_dat %>% arrange(Week_of_start)

table(arima_dat$Year)

# Adding variable that denotes which week of 52 the week is in each year
arima_dat_weeks <- split(arima_dat, arima_dat$Year)
for(i in 1:length(arima_dat_weeks)){
  arima_dat_weeks[[i]] <- arima_dat_weeks[[i]] %>% mutate(week_num = 1:nrow(arima_dat_weeks[[i]]))
}
arima_dat <- bind_rows(arima_dat_weeks)

arima_dat <- arima_dat %>% mutate(Vader_articles = if_else(is.na(Vader_articles),0,Vader_articles))

# Create time series objects with the weekly tweet data for sentiment and count
tweet_sentiment_ts <- ts(arima_dat$Vader,frequency = 52, start = c(arima_dat[1, "Year"],
                                                                   arima_dat[1, "week_num"]))


tweet_count_ts <- ts(arima_dat$log_Tweets_per_user,frequency = 52, start = c(arima_dat[1, "Year"],
                                                                             arima_dat[1, "week_num"]))


tweet_users_ts <- ts(arima_dat$log_Users_adjusted,frequency = 52, start = c(arima_dat[1, "Year"],
                                                                            arima_dat[1, "week_num"]))


tweet_users_new_ts <- ts(arima_dat$log_Users_new_adjusted,frequency = 52, start = c(arima_dat[1, "Year"],
                                                                                    arima_dat[1, "week_num"]))


event_ts <- ts(arima_dat$Occurrence_binary,frequency = 52, start = c(arima_dat[1, "Year"],
                                                                     arima_dat[1, "week_num"]))


# Plotting all time series to visualize
tweet_ts <- ts.union(tweet_sentiment_ts, tweet_count_ts, tweet_users_ts, tweet_users_new_ts)
plot.ts(tweet_ts, yax.flip = TRUE)


# Create time series objects with the weekly tweet data for sentiment and count
article_sentiment_ts <- ts(arima_dat$Vader_articles,frequency = 52, start = c(arima_dat[1, "Year"],
                                                                              arima_dat[1, "week_num"]))


article_count_ts <- ts(arima_dat$log_Article_Count,frequency = 52, start = c(arima_dat[1, "Year"],
                                                                             arima_dat[1, "week_num"]))


article_ts <- ts.union(article_sentiment_ts, article_count_ts)
plot.ts(article_ts, yax.flip = TRUE)


# Cross correlation between events and response variables

# -----------------------------
# 1. Compute CCFs (raw)
# -----------------------------
cc_plot_list <- list(
  "(a) Tweet Sentiment" = ccf(event_ts, tweet_sentiment_ts, plot = FALSE),
  "(d) Tweets per User"    = ccf(event_ts, tweet_count_ts,     plot = FALSE),
  "(b) Users"     = ccf(event_ts, tweet_users_ts,     plot = FALSE),
  "(c) New Users" = ccf(event_ts, tweet_users_new_ts, plot = FALSE)
)

# -----------------------------
# 2. Tidy CCF output
# -----------------------------
ccf_df <- bind_rows(
  lapply(names(cc_plot_list), function(p) {
    data.frame(
      lag   = as.numeric(cc_plot_list[[p]]$lag),
      ccf   = as.numeric(cc_plot_list[[p]]$acf),
      panel = p
    )
  })
)

# -----------------------------
# 3. 95% CI (base ccf default)
# -----------------------------
ci <- qnorm(0.975) / sqrt(cc_plot_list[[1]]$n.used)

ccf_df <- ccf_df %>%
  mutate(significant = abs(ccf) > ci)

# -----------------------------
# 4. Dot colors (significant only)
# -----------------------------
sig_colors <- c(
  "(a) Tweet Sentiment" = "#0FB2D3",
  "(d) Tweets per User"    = "#0FB2D3",
  "(b) Users"     = "#0FB2D3",
  "(c) New Users" = "#0FB2D3"
)

# -----------------------------
# 5. Plot
# -----------------------------

ccf_df<- ccf_df %>%
  filter(lag >= -.26 & lag <= .26)

ccf_df$panel <- factor(ccf_df$panel,
                       levels = c("(a) Tweet Sentiment", 
                                  "(b) Users", 
                                  "(c) New Users",
                                  "(d) Tweets per User"))

ccf_tweet_events <- ggplot(ccf_df, aes(x = lag, y = ccf)) +
  geom_hline(yintercept = 0, color = "black", linewidth = 0.6) +
  geom_hline(yintercept = c(-ci, ci),
             linetype = "dashed",
             color = "red",
             linewidth = 0.6) +
  # Black bars for all non-zero lags
  geom_segment(
    data = subset(ccf_df, lag != 0),
    aes(xend = lag, yend = 0),
    color = "darkgray",
    linewidth = 0.7
  ) +
  # Colored bar at lag = 0 if significant
  geom_segment(
    data = subset(ccf_df, lag == 0 & significant),
    aes(xend = lag, yend = 0, color = panel),
    linewidth = 0.9
  ) +
  geom_segment(
    data = subset(ccf_df, lag == 0 & !significant),
    aes(xend = lag, yend = 0),
    color = "darkgray",
    linewidth = 0.7
  ) +
  # Other significant dots (filled)
  geom_point(
    aes(color = panel),
    size = 2,
    shape = 16,
    data = subset(ccf_df, significant & lag != 0)
  ) +
  # Lag = 0 dot (open circle)
  geom_point(
    aes(color = panel),
    shape = 16,          # open circle
    size = 2,
    data = subset(ccf_df, lag == 0 & significant)
  ) +
  # Non-significant dots (black filled)
  geom_point(
    color = "darkgray",
    size = 2,
    shape = 16,
    data = subset(ccf_df, !significant)
  ) +
  facet_wrap(~panel, nrow = 1) +
  scale_color_manual(values = sig_colors) +
  scale_x_continuous(
    breaks = c(-.26,-.13,0,.13,.26),
    labels = c("-13","-7","0","7","13")
  ) +
  coord_cartesian(ylim = c(-0.2, 0.2)) +
  labs(
    x = "Lag",
    y = "Outreach events"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    strip.background = element_blank(),
    strip.text = element_text(face = "bold", size = 16),
    axis.line = element_line(color = "black"),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12)
  )

ccf_tweet_events

ccf_all <- grid.arrange(ccf_tweet_events,nrow = 1, ncol = 1)

ggsave('./figures/CCF_plot.png',ccf_all,width = 10, height = 4, units = "in", dpi = 300,bg = "transparent")


## ARIMAX Modeling -----

# Select variables to test
arima_dat_test <- arima_dat
nrow(arima_dat_test)
str(arima_dat_test)
# Fill in 0 and NA 
arima_dat_test <- arima_dat_test %>% 
  mutate(
    across(where(is.numeric), ~ replace(., is.na(.), 0)),
    across(where(is.factor),  ~ forcats::fct_na_value_to_level(., level = "Missing")),
    Occurrence_binary = as.integer(Occurrence_binary)
  )

#### Null -----
# Run the null model for comparison
fit_null <- auto.arima(tweet_sentiment_ts, xreg = NULL, stepwise=FALSE, approximation=FALSE, parallel = TRUE, ic = "aicc", num.cores = detectCores() - 1)
fit_null
saveRDS(fit_null,"./results/arima_sentiment_null.RDS")
fit_null <- readRDS("./results/arima_sentiment_null.RDS")
summary(fit_null)

checkresiduals(fit_null)

#### Tweet sentiment ~ occurrences -----
#### 1) Org size -----
# Create the model data
x.interact <- model.matrix(
  ~ 0 + Occurrence_binary:(`Org_size_<5 employees`+ `Org_size_5-9 employees` + `Org_size_10-19 employees` + `Org_size_20-49 employees` + `Org_size_50-99 employees`+ `Org_size_>100 employees`),
  data = arima_dat_test
)
x.interact_clean <- x.interact[, qr(x.interact)$pivot[1:qr(x.interact)$rank]]

fit_sentiment_orgsize <- auto.arima(tweet_sentiment_ts, xreg = x.interact_clean, stepwise=FALSE, approximation=FALSE, parallel = FALSE, ic = "aicc", num.cores = detectCores() - 2)
summary(fit_sentiment_orgsize)

saveRDS(fit_sentiment_orgsize,"./results/arima_sentiment_orgsize.RDS")
fit_sentiment_orgsize <- readRDS("./results/arima_sentiment_orgsize.RDS")

autoplot(fit_sentiment_orgsize)
fitted_vals <- fitted(fit_sentiment_orgsize)
plot(tweet_sentiment_ts, type = "l", col = "black")
lines(fitted_vals, col = "blue")

# Ljung-Box test
checkresiduals(fit_sentiment_orgsize)

# Extract coefficients and standard errors
coefs <- coef(fit_sentiment_orgsize)
se <- sqrt(diag(vcov(fit_sentiment_orgsize)))

# Filter to xreg terms related to Org_size
xreg_idx <- grepl("Org_size", names(coefs))
xreg_coefs <- coefs[xreg_idx]
xreg_se <- se[xreg_idx]

# Build data frame
coef_df <- data.frame(
  Term = names(xreg_coefs),
  Estimate = xreg_coefs,
  SE = xreg_se
) %>%
  mutate(
    Lower = Estimate - 1.96 * SE,
    Upper = Estimate + 1.96 * SE,
    Significant = ifelse(Lower > 0 | Upper < 0, "Yes", "No")
  )

coef_df$Term <- factor(coef_df$Term, levels = c(
  "Occurrence_binary:`Org_size_<5 employees`", "Occurrence_binary:`Org_size_5-9 employees`", "Occurrence_binary:`Org_size_10-19 employees`", "Occurrence_binary:`Org_size_20-49 employees`", "Occurrence_binary:`Org_size_50-99 employees`",  "Occurrence_binary:`Org_size_>100 employees`"# Your desired order (top to bottom)
))

# Plot
org_size_arima_plot <- ggplot(coef_df, aes(x = Term, y = Estimate, color = Significant)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = Lower, ymax = Upper), width = 0.2, size = 1) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray40") +
  scale_color_manual(values = c("Yes" = "#0FB2D3", "No" = "black")) +
  scale_x_discrete(labels = c(
    "Occurrence_binary:`Org_size_<5 employees`" = "<5 employees",
    "Occurrence_binary:`Org_size_5-9 employees`" = "5-9 employees",
    "Occurrence_binary:`Org_size_10-19 employees`" = "10-19 employees",
    "Occurrence_binary:`Org_size_20-49 employees`" = "20-49 employees",
    "Occurrence_binary:`Org_size_50-99 employees`" = "50-100 employees",
    "Occurrence_binary:`Org_size_>100 employees`" = ">100 employees"
  )) +
  coord_flip() +
  labs(
    title = "Org Size",
    x = NULL,
    y = "Coefficient Estimate",
    color = "Sig"
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "none")

org_size_arima_plot

#### 2) staff -----
# Create the model data
x.interact <- model.matrix(
  ~ 0 + Occurrence_binary:(`Staff_0` + `Staff_<5`+ `Staff_6-10` + `Staff_11-20` + `Staff_>20`),
  data = arima_dat_test
)
x.interact_clean <- x.interact[, qr(x.interact)$pivot[1:qr(x.interact)$rank]]

fit_sentiment_staff <- auto.arima(tweet_sentiment_ts, xreg = x.interact_clean, stepwise=FALSE, approximation=FALSE, parallel = TRUE, ic = "aicc", num.cores = detectCores() - 1)
summary(fit_sentiment_staff)
saveRDS(fit_sentiment_staff,"./results/arima_sentiment_staff.RDS")
fit_sentiment_staff <- readRDS("./results/arima_sentiment_staff.RDS")

autoplot(fit_sentiment_staff)
fitted_vals <- fitted(fit_sentiment_staff)
plot(tweet_sentiment_ts, type = "l", col = "black")
lines(fitted_vals, col = "blue")

# Ljung-Box test
checkresiduals(fit_sentiment_staff)

# Extract coefficients and standard errors
coefs <- coef(fit_sentiment_staff)
se <- sqrt(diag(vcov(fit_sentiment_staff)))

# Filter to xreg terms related to Org_size
xreg_idx <- grepl("Staff", names(coefs))
xreg_coefs <- coefs[xreg_idx]
xreg_se <- se[xreg_idx]

# Build data frame
coef_df <- data.frame(
  Term = names(xreg_coefs),
  Estimate = xreg_coefs,
  SE = xreg_se
) %>%
  mutate(
    Lower = Estimate - 1.96 * SE,
    Upper = Estimate + 1.96 * SE,
    Significant = ifelse(Lower > 0 | Upper < 0, "Yes", "No")
  )

coef_df$Term <- factor(coef_df$Term, levels = c(
  "Occurrence_binary:Staff_0", "Occurrence_binary:`Staff_<5`", "Occurrence_binary:`Staff_6-10`", "Occurrence_binary:`Staff_11-20`", "Occurrence_binary:`Staff_>20`"# Your desired order (top to bottom)
))

# Plot
staff_arima_plot <- ggplot(coef_df, aes(x = Term, y = Estimate, color = Significant)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = Lower, ymax = Upper), width = 0.2, size = 1) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray40") +
  scale_color_manual(values = c("Yes" = "#0FB2D3", "No" = "black")) +
  scale_x_discrete(labels = c(
    "Occurrence_binary:Staff_0" = "No staff present",
    "Occurrence_binary:`Staff_<5`" = "<6 Staff present",
    "Occurrence_binary:`Staff_6-10`" = "6-10 Staff present",
    "Occurrence_binary:`Staff_11-20`" = "11-20 Staff present",
    "Occurrence_binary:`Staff_>20`" = ">20 Staff present"
  )) +
  coord_flip() +
  labs(
    title = "Staff Presence",
    x = NULL,
    y = "Coefficient Estimate",
    color = "Sig"
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "none")

staff_arima_plot

#### 3) Type -----
# Create the model data
x.interact <- model.matrix(
  ~ 0 + Occurrence_binary:(`Employer_type_County/local government` + `Employer_type_Federal agency`+ `Employer_type_Non-profit organization` + `Employer_type_Private business` + `Employer_type_State agency`),
  data = arima_dat_test
)
x.interact_clean <- x.interact[, qr(x.interact)$pivot[1:qr(x.interact)$rank]]



fit_sentiment_orgtype <- auto.arima(tweet_sentiment_ts, xreg = x.interact_clean, stepwise=FALSE, approximation=FALSE, parallel = TRUE, ic = "aicc", num.cores = detectCores() - 1)
summary(fit_sentiment_orgtype)
saveRDS(fit_sentiment_orgtype,"./results/arima_sentiment_orgtype.RDS")
fit_sentiment_orgtype <- readRDS("./results/arima_sentiment_orgtype.RDS")

autoplot(fit_sentiment_orgtype)
fitted_vals <- fitted(fit_sentiment_orgtype)
plot(tweet_sentiment_ts, type = "l", col = "black")
lines(fitted_vals, col = "blue")

# Ljung-Box test
checkresiduals(fit_sentiment_orgtype)

# Extract coefficients and standard errors
coefs <- coef(fit_sentiment_orgtype)
se <- sqrt(diag(vcov(fit_sentiment_orgtype)))

# Filter to xreg terms related to Org_size
xreg_idx <- grepl("Employer", names(coefs))
xreg_coefs <- coefs[xreg_idx]
xreg_se <- se[xreg_idx]

# Build data frame
coef_df <- data.frame(
  Term = names(xreg_coefs),
  Estimate = xreg_coefs,
  SE = xreg_se
) %>%
  mutate(
    Lower = Estimate - 1.96 * SE,
    Upper = Estimate + 1.96 * SE,
    Significant = ifelse(Lower > 0 | Upper < 0, "Yes", "No")
  )

coef_df$Term <- factor(coef_df$Term, levels = c(
  "Occurrence_binary:`Employer_type_Federal agency`", "Occurrence_binary:`Employer_type_State agency`", "Occurrence_binary:`Employer_type_County/local government`", "Occurrence_binary:`Employer_type_Non-profit organization`", "Occurrence_binary:`Employer_type_Private business`"# Your desired order (top to bottom)
))
coef_df$Sig_color <- with(coef_df,
                          ifelse(Significant == "No", "No",
                                 ifelse(Term %in% c(
                                   "Occurrence_binary:`Employer_type_Non-profit organization`"
                                 ), "Yes_1", "Yes_2"))
)

# Plot
org_type_arima_plot <- ggplot(coef_df, aes(x = Term, y = Estimate, color = Sig_color)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = Lower, ymax = Upper), width = 0.2, size = 1) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray40") +
  scale_color_manual(values = c("Yes_2" = "#0FB2D3", "Yes_1" = "#0FB2D3", "No" = "black")) +
  scale_x_discrete(labels = c(
    "Occurrence_binary:`Employer_type_Federal agency`" = "Federal agency",
    "Occurrence_binary:`Employer_type_State agency`" = "State agency",
    "Occurrence_binary:`Employer_type_County/local government`" = "County/local government",
    "Occurrence_binary:`Employer_type_Non-profit organization`" = "Non-profit organization",
    "Occurrence_binary:`Employer_type_Private business`" = "Private business"
  )) +
  coord_flip() +
  labs(
    title = "Org Type",
    x = NULL,
    y = "Coefficient Estimate",
    color = "Sig"
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "none")

org_type_arima_plot


#### 4) Attendence -----
# Create the model data
x.interact <- model.matrix(
  ~ 0 + Occurrence_binary:(`Attendance_0` + `Attendance_<20`+ `Attendance_20-50` + `Attendance_50-100` + `Attendance_>100` + `Attendance_unknown`),
  data = arima_dat_test
)
x.interact_clean <- x.interact[, qr(x.interact)$pivot[1:qr(x.interact)$rank]]


fit_sentiment_attendence <- auto.arima(tweet_sentiment_ts, xreg = x.interact_clean, stepwise=FALSE, approximation=FALSE, parallel = TRUE, ic = "aicc", num.cores = detectCores() - 1)
summary(fit_sentiment_attendence)
saveRDS(fit_sentiment_attendence,"./results/arima_sentiment_attendence.RDS")
fit_sentiment_attendence <- readRDS("./results/arima_sentiment_attendence.RDS")


autoplot(fit_sentiment_attendence)
fitted_vals <- fitted(fit_sentiment_attendence)
plot(tweet_sentiment_ts, type = "l", col = "black")
lines(fitted_vals, col = "blue")

# Ljung-Box test
checkresiduals(fit_sentiment_attendence)

# Extract coefficients and standard errors
coefs <- coef(fit_sentiment_attendence)
se <- sqrt(diag(vcov(fit_sentiment_attendence)))

# Filter to xreg terms related to Org_size
xreg_idx <- grepl("Attendance", names(coefs))
xreg_coefs <- coefs[xreg_idx]
xreg_se <- se[xreg_idx]

# Build data frame
coef_df <- data.frame(
  Term = names(xreg_coefs),
  Estimate = xreg_coefs,
  SE = xreg_se
) %>%
  mutate(
    Lower = Estimate - 1.96 * SE,
    Upper = Estimate + 1.96 * SE,
    Significant = ifelse(Lower > 0 | Upper < 0, "Yes", "No")
  )

coef_df$Term <- factor(coef_df$Term, levels = c(
  "Occurrence_binary:Attendance_0", "Occurrence_binary:`Attendance_<20`", "Occurrence_binary:`Attendance_20-50`", "Occurrence_binary:`Attendance_50-100`", "Occurrence_binary:`Attendance_>100`","Occurrence_binary:Attendance_unknown"# Your desired order (top to bottom)
))

# Plot
attend_arima_plot <- ggplot(coef_df, aes(x = Term, y = Estimate, color = Significant)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = Lower, ymax = Upper), width = 0.2, size = 1) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray40") +
  scale_color_manual(values = c("Yes" = "#0FB2D3", "No" = "black")) +
  scale_x_discrete(labels = c(
    "Occurrence_binary:Attendance_0" = "None",
    "Occurrence_binary:`Attendance_<20`" = "Attendance <20",
    "Occurrence_binary:`Attendance_20-50`" = "Attendance 20-49",
    "Occurrence_binary:`Attendance_50-100`" = "Attendance 50-100",
    "Occurrence_binary:`Attendance_>100`" = "Attendance >100",
    "Occurrence_binary:Attendance_unknown" = "Attendance Unknown"
  )) +
  coord_flip() +
  labs(
    title = "Attendence",
    x = NULL,
    y = "Coefficient Estimate",
    color = "Sig"
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "none")
attend_arima_plot

#### 5) Event type -----
# Create the model data
x.interact <- model.matrix(
  ~ 0 + Occurrence_binary:(Social_media_info_campaign + Onsite_engagement + `Off-site_community_engagement` + School_visit + Service_volunteer_community_science + Family_friendly_activities + Other_type),
  data = arima_dat_test
)

x.interact_clean <- x.interact[, qr(x.interact)$pivot[1:qr(x.interact)$rank]]

fit_sentiment_campaign <- auto.arima(tweet_sentiment_ts, xreg = x.interact_clean, stepwise=FALSE, approximation=FALSE, parallel = TRUE, ic = "aicc", num.cores = detectCores() - 1)
summary(fit_sentiment_campaign)
saveRDS(fit_sentiment_campaign,"./results/arima_sentiment_campaign.RDS")
fit_sentiment_campaign <- readRDS("./results/arima_sentiment_campaign.RDS")


autoplot(fit_sentiment_campaign)
fitted_vals <- fitted(fit_sentiment_campaign)
plot(tweet_sentiment_ts, type = "l", col = "black")
lines(fitted_vals, col = "blue")

# Ljung-Box test
checkresiduals(fit_sentiment_campaign)

# Extract coefficients and standard errors
coefs <- coef(fit_sentiment_campaign)
se <- sqrt(diag(vcov(fit_sentiment_campaign)))

# Filter to xreg terms related to Org_size
xreg_idx <- names(coefs)[3:9]
xreg_coefs <- coefs[xreg_idx]
xreg_se <- se[xreg_idx]

# Build data frame
coef_df <- data.frame(
  Term = names(xreg_coefs),
  Estimate = xreg_coefs,
  SE = xreg_se
) %>%
  mutate(
    Lower = Estimate - 1.96 * SE,
    Upper = Estimate + 1.96 * SE,
    Significant = ifelse(Lower > 0 | Upper < 0, "Yes", "No")
  )

coef_df$Term <- factor(coef_df$Term, levels = c(
  "Occurrence_binary:Other_type", "Occurrence_binary:Onsite_engagement", "Occurrence_binary:`Off-site_community_engagement`", "Occurrence_binary:Service_volunteer_community_science", "Occurrence_binary:Family_friendly_activities","Occurrence_binary:School_visit","Occurrence_binary:Social_media_info_campaign"# Your desired order (top to bottom)
))

coef_df$Sig_color <- with(coef_df,
                          ifelse(Significant == "No", "No",
                                 ifelse(Term %in% c(
                                   "Occurrence_binary:Social_media_info_campaign"
                                 ), "Yes_1", "Yes_2"))
)

# Plot
type_arima_plot <- ggplot(coef_df, aes(x = Term, y = Estimate, color = Sig_color)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = Lower, ymax = Upper), width = 0.2, size = 1) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray40") +
  scale_color_manual(values = c("Yes_1" = "#0FB2D3", "Yes_2" = "#0FB2D3","No" = "black")) +
  scale_x_discrete(labels = c(
    "Occurrence_binary:Social_media_info_campaign" = "Social media campaign",
    "Occurrence_binary:Onsite_engagement" = "On site",
    "Occurrence_binary:`Off-site_community_engagement`" = "Off site",
    "Occurrence_binary:School_visit" = "School visit",
    "Occurrence_binary:Service_volunteer_community_science" = "Community science",
    "Occurrence_binary:Family_friendly_activities" = "Family friendly",
    "Occurrence_binary:Other_type" = "Other"
  )) +
  coord_flip() +
  labs(
    title = "Event Type",
    x = NULL,
    y = "Coefficient Estimate",
    color = "Sig"
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "none")
type_arima_plot

#### 6) Intended audience -----
# Create the model data
x.interact <- model.matrix(
  ~ 0 + Occurrence_binary:(Business_owners + General_public + Property_owners + Recreationists + `Youth_family-friendly` + Other_audience),
  data = arima_dat_test
)

x.interact_clean <- x.interact[, qr(x.interact)$pivot[1:qr(x.interact)$rank]]

fit_sentiment_audience <- auto.arima(tweet_sentiment_ts, xreg = x.interact_clean, stepwise=FALSE, approximation=FALSE, parallel = TRUE, ic = "aicc", num.cores = detectCores() - 1)
summary(fit_sentiment_audience)
saveRDS(fit_sentiment_audience,"./results/arima_sentiment_audience.RDS")
fit_sentiment_audience <- readRDS("./results/arima_sentiment_audience.RDS")


autoplot(fit_sentiment_audience)
fitted_vals <- fitted(fit_sentiment_audience)
plot(tweet_sentiment_ts, type = "l", col = "black")
lines(fitted_vals, col = "blue")

# Ljung-Box test
checkresiduals(fit_sentiment_audience)

# Extract coefficients and standard errors
coefs <- coef(fit_sentiment_audience)
se <- sqrt(diag(vcov(fit_sentiment_audience)))

# Filter to xreg terms related to Org_size
xreg_idx <- names(coefs)[3:8]
xreg_coefs <- coefs[xreg_idx]
xreg_se <- se[xreg_idx]

# Build data frame
coef_df <- data.frame(
  Term = names(xreg_coefs),
  Estimate = xreg_coefs,
  SE = xreg_se
) %>%
  mutate(
    Lower = Estimate - 1.96 * SE,
    Upper = Estimate + 1.96 * SE,
    Significant = ifelse(Lower > 0 | Upper < 0, "Yes", "No")
  )

coef_df$Term <- factor(coef_df$Term, levels = c(
  "Occurrence_binary:Other_audience", "Occurrence_binary:General_public", "Occurrence_binary:Business_owners", "Occurrence_binary:Property_owners","Occurrence_binary:Recreationists", "Occurrence_binary:`Youth_family-friendly`"# Your desired order (top to bottom)
))

# Plot
audience_arima_plot <- ggplot(coef_df, aes(x = Term, y = Estimate, color = Significant)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = Lower, ymax = Upper), width = 0.2, size = 1) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray40") +
  scale_color_manual(values = c("Yes" = "#0FB2D3", "No" = "black")) +
  scale_x_discrete(labels = c(
    "Occurrence_binary:Business_owners" = "Business owners",
    "Occurrence_binary:General_public" = "General public",
    "Occurrence_binary:Property_owners" = "Property owners",
    "Occurrence_binary:Recreationists" = "Recreationists",
    "Occurrence_binary:`Youth_family-friendly`" = "Youth/families",
    "Occurrence_binary:Other_audience" = "Other"
  )) +
  coord_flip() +
  labs(
    title = "Target Audience",
    x = NULL,
    y = "Coefficient Estimate",
    color = "Sig"
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "none")
audience_arima_plot

arima_plots <- grid.arrange(org_type_arima_plot,org_size_arima_plot,staff_arima_plot,attend_arima_plot,type_arima_plot,audience_arima_plot, ncol = 3, nrow = 2)
ggsave('./figures/ARIMA_plots.png',arima_plots,width = 12, height = 6, units = "in", dpi = 600,bg = "transparent")

arima_plots <- grid.arrange(org_type_arima_plot,org_size_arima_plot,type_arima_plot,audience_arima_plot, ncol = 2, nrow = 2)
ggsave('./figures/ARIMA_plots_sig.png',arima_plots,width = 10, height = 6, units = "in", dpi = 300,bg = "transparent")

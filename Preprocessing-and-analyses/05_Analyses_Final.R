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
## Date last updated: 10/19/2026
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

# Tweets
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

# Articles
# This dataset represents the cleaned article data with the vader sentiment scores
article_raw <- read.csv("./data_final/NFWF_articles_final_filter2_topics.csv")[,-1]

# Add sentiment category using cutoffs 
article_raw <- article_raw %>%
  mutate(VADER_Sentiment = case_when(
    Raw_Sentiment_Score < -0.05 ~ 'negative',  # If Vader is less than -0.05, it's Negative
    Raw_Sentiment_Score > 0.05 ~ 'positive',   # If Vader is greater than 0.05, it's Positive
    TRUE ~ 'neutral'             # Otherwise, it's Neutral
  ))

### Weekly datasets -----
# This dataset represents weekly aggregated metrics of twitter data and for the entire study period
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


weekly_dat_all <- tweet_counts %>% left_join(article_counts, by = c("Week_of_start")) 

weekly_dat_all <- weekly_dat_all %>% mutate(Article_Count = if_else(is.na(Article_Count),0,Article_Count)) %>%
  mutate(Article_topic_n = if_else(is.na(Article_topic_n),0,Article_topic_n))

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

article_counts <- article_counts %>% mutate(log_Article_Count = log(Article_Count))


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


## Mann-Kendall (MK) test for trends -----

mk.test(weekly_dat_all$Vader) # significant and increasing
mk.test(weekly_dat_all$Users_adjusted) # significant and decreasing
mk.test(weekly_dat_all$Users_new_adjusted) # significant and decreasing
mk.test(weekly_dat_all$Tweet_per_user) # significant and increasing


mk.test(weekly_dat_all$Users)
mk.test(weekly_dat_all$Users_new)
mk.test(weekly_dat_all$Tweet_Count)

mk.test(article_counts$Vader_articles)
mk.test(article_counts$Article_Count)



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


final_outliers_plot_all <- plot_grid(tweet_sentiment_outliers_p, article_sentiment_outliers_p,tweet_count_outliers_p, article_count_outliers_p,tweet_users_outliers_p,tweet_users_new_outliers_p,ncol = 1, align = "v", axis = "lr",rel_heights = c(.4,.4, .4, .4, .4, .4))
final_outliers_plot_all

ggsave('./figures/outliers_trend_adjusted_all.png',final_outliers_plot_all,width = 10, height = 10, units = "in", dpi = 300,bg = "transparent")
ggsave('./figures/outliers_trend_adjusted_all.jpeg',final_outliers_plot_all,width = 10, height = 10, units = "in", dpi = 300,bg = "transparent")


## Trends in sentiment departure and proportional contribution by topic -----

monthly_totals <- tweet_raw %>% filter(!is.na(topic)) %>%
  mutate(month = make_date(Year, Month, 1)) %>%
  count(month, name = "total")

tweet_time_month <- tweet_raw %>% filter(!is.na(topic)) %>%
  mutate(month = make_date(Year, Month, 1)) %>%
  filter(month >= as.Date("2012-01-01"), month <= as.Date("2024-12-01")) %>%
  group_by(topic) %>%
  mutate(topic_mean = mean(Raw_Sentiment_Score, na.rm = TRUE)) %>%
  group_by(topic, month) %>%
  summarise(
    n_topic  = n(),
    sent_dev = mean(Raw_Sentiment_Score, na.rm = TRUE) - first(topic_mean),
    .groups  = "drop"
  ) %>%
  complete(
    topic,
    month = seq(as.Date("2012-01-01"), as.Date("2024-12-01"), by = "month"),
    fill  = list(n_topic = 0)
  ) %>%
  left_join(monthly_totals, by = "month") %>%
  mutate(count = if_else(!is.na(total) & total > 0, n_topic / total, 0))


tweet_time_month <- tweet_time_month %>% as.data.frame() %>% mutate(topic = factor(topic, levels = c(1,2,3,4,5,6,7), labels = c("Tortoise conservation","Emotional and personal connections to tortoises","Renewable energy development",
                                                                                                                                "Tortoise movement and observation","Outreach and education","Ranching and human- wildlife conflict","California tortoise protection")))

tweet_time_month %>% group_by(topic) %>% summarise(mean_freq = mean(count),
                                                   med_freq = median(count),
                                                   sd_freq = sd(count),
                                                   cv_freq = mean_freq/sd_freq)

# Scale factor so the largest sentiment deviation reaches ~half the max count
k1 <- 0.5 * max(tweet_time_month$count) / max(abs(tweet_time_month$sent_dev), na.rm = TRUE)

p1 <- ggplot(tweet_time_month, aes(x = month)) +
  geom_ribbon(aes(ymin = 0, ymax = pmax(sent_dev, 0) * k1, fill = "Above topic mean"),
              alpha = 0.6, na.rm = TRUE) +
  geom_ribbon(aes(ymin = pmin(sent_dev, 0) * k1, ymax = 0, fill = "Below topic mean"),
              alpha = 0.6, na.rm = TRUE) +
  geom_hline(yintercept = 0, linewidth = 0.3, color = "grey60") +
  geom_line(aes(y = count), linewidth = 0.5, show.legend = FALSE) +
  facet_wrap(~ topic, ncol = 2) +
  # scale_color_manual(values = pal_topic1) +
  scale_fill_manual(values = c("Above topic mean" = "#3987e5",
                               "Below topic mean" = "#e34948")) +
  scale_y_continuous(
    name     = "Proportion of monthly total tweets",
    sec.axis = sec_axis(~ . / k1, name = "Sentiment departure")
  ) +
  scale_x_date(
    limits      = as.Date(c("2012-01-01", "2024-12-31")),
    date_breaks = "2 years",
    date_labels = "%Y",
    expand      = c(0, 0)
  ) +
  labs(x = NULL, fill = NULL, title = "(a) Tweets") +
  theme_minimal() +
  theme(
    legend.position   = "none",
    legend.text       = element_text(size = 10),
    legend.key.width  = unit(0.5, "cm"),
    legend.key.height = unit(0.25, "cm"),
    strip.text        = element_text(size = 10, face = "bold"),
    panel.grid.minor  = element_blank(),
    plot.title        = element_text(size = 14, face = "bold")
  )

p1

monthly_totals_art <- article_raw %>% filter(!is.na(topic)) %>%
  mutate(month = make_date(year, month, 1)) %>%
  count(month, name = "total")

article_time_month <- article_raw %>% filter(!is.na(topic)) %>%
  mutate(month = make_date(year, month, 1)) %>%
  filter(month >= as.Date("2012-01-01"), month <= as.Date("2024-12-01")) %>%
  group_by(topic) %>%
  mutate(topic_mean = mean(Raw_Sentiment_Score, na.rm = TRUE)) %>%
  group_by(topic, month) %>%
  summarise(
    n_topic  = n(),
    sent_dev = mean(Raw_Sentiment_Score, na.rm = TRUE) - first(topic_mean),
    .groups  = "drop"
  ) %>%
  complete(
    topic,
    month = seq(as.Date("2012-01-01"), as.Date("2024-12-01"), by = "month"),
    fill  = list(n_topic = 0)
  ) %>%
  left_join(monthly_totals_art, by = "month") %>%
  mutate(count = if_else(!is.na(total) & total > 0, n_topic / total, 0))

article_time_month <- article_time_month %>% as.data.frame() %>% mutate(topic = factor(topic, levels = c(1,2,3,4,5,6,7), labels = c("Renewable energy development",
                                                                                                                                    "Roads and habitat fragmentation",
                                                                                                                                    "Habitat use on military bases",
                                                                                                                                    "Emotional and personal connections to tortoises",
                                                                                                                                    "Public lands and protected landscapes",
                                                                                                                                    "Raven predation",
                                                                                                                                    "Science/research"
)))

# Scale factor so the largest sentiment deviation reaches ~half the max count
k2 <- 0.5 * max(article_time_month$count) / max(abs(article_time_month$sent_dev), na.rm = TRUE)

p2 <- ggplot(article_time_month, aes(x = month)) +
  geom_ribbon(aes(ymin = 0, ymax = pmax(sent_dev, 0) * k2, fill = "Above topic mean"),
              alpha = 0.6, na.rm = TRUE) +
  geom_ribbon(aes(ymin = pmin(sent_dev, 0) * k2, ymax = 0, fill = "Below topic mean"),
              alpha = 0.6, na.rm = TRUE) +
  geom_hline(yintercept = 0, linewidth = 0.3, color = "grey60") +
  geom_line(aes(y = count), linewidth = 0.5, show.legend = FALSE) +
  facet_wrap(~ topic, ncol = 2) +
  # scale_color_manual(values = pal_topic1) +
  scale_fill_manual(values = c("Above topic mean" = "#3987e5",
                               "Below topic mean" = "#e34948")) +
  scale_y_continuous(
    name     = "Proportion of monthly total articles",
    sec.axis = sec_axis(~ . / k2, name = "Sentiment departure")
  ) +
  scale_x_date(
    limits      = as.Date(c("2012-01-01", "2024-12-31")),
    date_breaks = "2 years",
    date_labels = "%Y",
    expand      = c(0, 0)
  ) +
  labs(x = NULL, fill = NULL, title = "(b) Articles") +
  theme_minimal() +
  theme(
    legend.position   = "none",
    legend.text       = element_text(size = 10),
    legend.key.width  = unit(0.5, "cm"),
    legend.key.height = unit(0.25, "cm"),
    strip.text        = element_text(size = 10, face = "bold"),
    panel.grid.minor  = element_blank(),
    plot.title        = element_text(size = 14, face = "bold")
  )

p2

p_all <- grid.arrange(p1,p2, nrow = 2, ncol = 1,heights = c(0.4,0.4))

ggsave("./figures/Topic_trends_sentiment.jpeg", p_all,
       width = 8, height = 9, units = "in", dpi = 600)



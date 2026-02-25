## ---------------------------
## 
## Project: TLD NLP
## 
## Script name: 03-GDELT-v2-combine-article-text-dates.R
##
## Purpose of script: this script combines all article data gathered via GDELT v2
##                    with its associated date information
##
## Author: Mae Lacey
##
## Date last updated: 12/8/2025
##
## Email contact: mae[at]csp-inc.org
##
## ---------------------------

setwd("C:/Users/Mae Lacey/Documents/github-repos/TLD-NLP/GDELT-article-extraction/GDELT-v2/gdelt_v2_stage1/")

urls_all <- read.csv("urls_all.csv")
article_text <- read.csv("GDELT-v2-final/article_text_master.csv")

all_article_data <- article_text %>%
  left_join(urls_all, by = "url")

#write.csv(all_article_data, "GDELT-v2-final/article_text_master_wDates.csv")

# Filter for records containing any of the target phrases in the text OR title
#keywords <- c("mojave desert tortoise", "gopherus agassizii", "mojavedeserttortoise", "desert tortoise") 
keywords <- c("mojave desert tortoise", "gopherus agassizii", "mojavedeserttortoise") 
#keywords <- c("tortoise") 

# then filter
text_filtered <- all_article_data %>%
  filter(
    str_detect(
      tolower(text),
      str_c(keywords, collapse = "|") # OR pattern
    )
  )

# get count of URLs identified
nrow(urls_all)
# get count of articles with text returned
nrow(all_article_data)
# get count of articles remaining post-filter
nrow(text_filtered)

# export final filtered dataset
write.csv(text_filtered, "GDELT-v2-final/gdelt_v2_gkg_final_filtered_2016_2024.csv")

## ---------------------------
## 
## Project: TLD NLP
## 
## Script name: 03-GDELT-v1-process-outputs.R
##
## Purpose of script: this script processes all article data gathered via GDELT v1
##
## Author: Mae Lacey
##
## Date last updated: 12/8/2025
##
## Email contact: mae[at]csp-inc.org
##
## ---------------------------


library(stringr)
library(dplyr)

setwd("C:/Users/Mae Lacey/Documents/github-repos/TLD-NLP/GDELT-article-extraction/GDELT-v1/")

# read in CSV
GDELTv1_text <- read.csv("gdelt_v1_final/gdelt_v1_gkg_master_text.csv")
GDELTv1_urls <- read.csv("gdelt_v1_final/gdelt_v1_gkg_master_urls.csv")

# Filter for records containing any of the target phrases in the text OR title
#keywords <- c("mojave desert tortoise", "gopherus agassizii", "mojavedeserttortoise", "desert tortoise") 
keywords <- c("mojave desert tortoise", "gopherus agassizii", "mojavedeserttortoise") 
#keywords <- c("tortoise") 

# then filter
text_filtered <- GDELTv1_text %>%
  filter(
    str_detect(
      tolower(paste(title, text)), # combine title + text
      str_c(keywords, collapse = "|") # OR pattern
    )
  )

# get count of URLs identified
nrow(GDELTv1_urls)
# get count of articles with text returned
nrow(GDELTv1_text)
# get count of articles remaining post-filter
nrow(text_filtered)

# export final filtered dataset
write.csv(text_filtered, "NFWF-spyder/gdelt_v1_final/gdelt_v1_gkg_final_filtered_2013_2015.csv")

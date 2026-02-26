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
## Date last updated: 2/25/2026
##
## Email contact: mae[at]csp-inc.org
##
## ---------------------------


library(stringr)
library(dplyr)

setwd("standen-etal-2026-NLP-Mojave-desert-tortoise/GDELT-article-extraction/GDELT-v1/")

# read in CSV
GDELTv1_text <- read.csv("gdelt_v1_final/gdelt_v1_gkg_master_text.csv")
GDELTv1_urls <- read.csv("gdelt_v1_final/gdelt_v1_gkg_master_urls.csv")

# filter for records containing any of the target phrases in the text OR title
# set keywords
keywords <- c("mojave desert tortoise", "gopherus agassizii", "mojavedeserttortoise") 

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

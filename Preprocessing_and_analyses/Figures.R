## ---------------------------
## 
## Project: TLD NLP
## 
## Script name: Figures.R
##
## Purpose of script: this script generates additional figures for the associated manuscript
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
list.of.packages <- c("lubridate","dplyr","parallel","stringr", "ggplot2","zoo","readr","ggpubr","scales","patchwork","cowplot", "Rtsne", "nlme", "mgcv","Deriv", "zoo", "jpeg", "tidyr",
                      "ggpmisc","gridExtra","MASS","msm","sandwich","foreign", "performance", "broom", "gplots", "RColorBrewer", "clValid","dendextend", "gratia", "perm", "fastDummies",
                      "ggeffects","DHARMa","pscl","AICcmodavg","lmtest","viridis","trend","tidytext","tidyr","stringr","wordcloud","VennDiagram","tseries","funtimes","systemfonts",
                      "tm", "SnowballC", "wordcloud", "cluster", "factoextra", "topicmodels","tm","text", "textTinyR", "dplyr", "ggplot2", "tm", "text2vec", "uwot","textstem","forcats","forecast")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = TRUE)

## Figures for article and tweet topic frequencies over time -----
tweet_raw <- read.csv("./data_final/NFWF_tweets_final_topics.csv")[,-1]

tweet_time <- tweet_raw %>% filter(!is.na(topic))%>%group_by(Year,topic) %>% summarise(count = n()) %>% arrange(Year,topic) 
tweet_time <- tweet_time %>% as.data.frame() %>% mutate(topic = factor(topic, levels = c(1,2,3,4,5,6,7), labels = c("Tortoise conservation","Emotional and personal connections to tortoises","Renewable energy development",
                                                                                                                    "Tortoise movement and observation","Outreach and education","Ranching and human- wildlife conflict","California tortoise protection")))


pal_topic1 <- c("#EFB83D","#B49C13","#D6264F","#E1B0A7","#9484B1","#B9C7E2","#3F564F")
pal_topic2 <- c("#E69512","#BDD0A2","#ac181d","#E38377","#6D397D","#6C91BD","#7E8C69")

p1 <- ggplot(tweet_time, aes(x = Year, y = count, color = topic)) +
  geom_line(size = 0.6) +
  # geom_point(size = 2) +
  labs(
    x = "Year",
    y = "Count",
    color = "Topic",
    title = "(a) Tweets"
  ) +
  theme_minimal() +
  scale_color_manual(values = pal_topic1) +
  scale_y_continuous(
    limits = c(0, 800),
    breaks = c(0,200,400,600,800)
  )+
  scale_x_continuous(
    limits = c(2012, 2024),
    breaks = 2012:2024
  )+
  guides(color = guide_legend(ncol = 2)) +
  theme(
    legend.position = "bottom",
    legend.text = element_text(size = 10),      # smaller legend font
    legend.key.width = unit(0.5, "cm"),        # shorter legend lines
    legend.spacing.y = unit(0.01, "cm"),  # tighter legend spacing
    legend.key.height = unit(0.25, "cm"),
    plot.title = element_text(size = 14, face = "bold")
  )

article_raw <- read.csv("./data_final/NFWF_articles_final_filter2_topics.csv")[,-1]

article_time <- article_raw %>% group_by(year,topic) %>% summarise(count = n()) %>% rename(Year = year) %>% arrange(Year,topic) %>% mutate(topic = factor(topic, levels = c(1,2,3,4,5,6,7), labels = c("Renewable energy development",
                                                                                                                                                                                                       "Roads and habitat fragmentation",
                                                                                                                                                                                                       "Habitat use on military bases",
                                                                                                                                                                                                       "Emotional and personal connections to tortoises",
                                                                                                                                                                                                       "Public lands and protected landscapes",
                                                                                                                                                                                                       "Raven predation",
                                                                                                                                                                                                       "Science/research"
                                                                                                                                                                                                       )))



p2 <- ggplot(article_time, aes(x = Year, y = count, color = factor(topic))) +
  geom_line(size = 0.6) +   # thinner lines
  labs(
    x = "Year",
    y = "Count",
    color = "Topic",
    title = "(b) Articles"
  ) +
  theme_minimal() +
  scale_color_manual(values = pal_topic2) +
  scale_x_continuous(
    limits = c(2012, 2024),
    breaks = 2012:2024
  )+
  guides(color = guide_legend(ncol = 2)) +
  theme(
    legend.position = "bottom",
    legend.text = element_text(size = 10),      # smaller legend font
    legend.key.width = unit(0.5, "cm"),        # shorter legend lines
    legend.spacing.y = unit(0.01, "cm"),  # tighter legend spacing
    legend.key.height = unit(0.25, "cm"),
    plot.title = element_text(size = 14, face = "bold")
  )

p_all <- grid.arrange(p1,p2, nrow = 2, ncol = 1,heights = c(0.4,0.4))

ggsave("./figures/Topic_trends.jpeg", p_all,
       width = 7, height = 7, units = "in", dpi = 600)


## Figures for UMAP projections for article and tweet topics -----
projection_df_articles <- read.csv("./results/UMAP_articles.csv")[,-1] %>% dplyr::select(x,y,topic) %>% mutate(panel = "Articles") %>% mutate(topic = factor(topic, levels = c(1,2,3,4,5,6,7), labels = c("Renewable energy development",
                                                                                                                                                                                                          "Roads and habitat fragmentation",
                                                                                                                                                                                                          "Habitat use on military bases",
                                                                                                                                                                                                          "Connections to tortoises",
                                                                                                                                                                                                          "Public lands and protected landscapes",
                                                                                                                                                                                                          "Raven predation",
                                                                                                                                                                                                          "Science/research"))) %>% arrange(topic)

n_topics <- 7


p_1 <- ggplot(
  projection_df_articles,
  aes(
    x = x,
    y = y,
    colour = topic,
    shape  = topic
  )
) +
  geom_point(alpha = 0.6, size = 2) +
  theme_minimal() +
  guides(color = guide_legend(ncol = 2)) +
  labs(
    title = "Articles",
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
  )+
  scale_color_manual(values = pal_topic2) +
  theme(
    legend.position = "bottom",
    legend.text = element_text(size = 8),      # smaller legend font
    legend.key.width = unit(0.5, "cm"),        # shorter legend lines
    legend.spacing.y = unit(0.02, "cm"),  # tighter legend spacing
    legend.key.height = unit(0.25, "cm"),
    plot.title = element_text(size = 14, face = "bold")
  )

p_1

p_1_faceted <- ggplot(
  projection_df_articles,
  aes(
    x = x,
    y = y,
    colour = topic,
    shape  = topic
  )
) +
  geom_point(alpha = 0.6, size = 2) +
  theme_minimal() +
  guides(color = guide_legend(ncol = 2)) +
  labs(
    title = "Articles",
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
      10, # plus
      7,  # square cross
      8,  # star
      9   # diamond plus
    )
  ) +
  scale_color_manual(values = pal_topic2) +
  facet_wrap(~ topic, nrow = 2, ncol = 4) +  # 2 rows x 4 columns
  theme(
    legend.position = "none",
    legend.text = element_text(size = 10),      # smaller legend font
    legend.key.width = unit(0.5, "cm"),        # shorter legend lines
    legend.spacing.y = unit(0.04, "cm"),  # tighter legend spacing
    legend.key.height = unit(0.25, "cm"),
    legend.title = element_text(face = "bold"),
    plot.title = element_text(size = 14, face = "bold"),
    strip.text = element_text(size = 10,face = "bold", hjust = 0)   # bold + left-aligned)
  )



projection_df_tweets <- read.csv("./results/UMAP_tweets.csv")[,-1] %>% dplyr::select(x,y,topic) %>% mutate(panel = "Tweets") %>% mutate(topic = factor(topic, levels = c(1,2,3,4,5,6,7), labels = c("Tortoise conservation","Connections to tortoises","Renewable energy development",
                                                                                                                                                                                                    "Tortoise movement and observation","Outreach and education","Ranching and human- wildlife conflict","California tortoise protection"))) %>% arrange()




n_topics <- 7

p_2 <-ggplot(
  projection_df_tweets,
  aes(
    x = x,
    y = y,
    colour = topic,
    shape  = topic
  )
) +
  geom_point(alpha = 0.6, size = 2) +
  theme_minimal() +
  guides(color = guide_legend(ncol = 2)) +
  labs(
    title = "Tweets",
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
  )+
  scale_color_manual(values = pal_topic1) +
  theme(
    legend.position = "bottom",
    legend.text = element_text(size = 8),      # smaller legend font
    legend.key.width = unit(0.5, "cm"),        # shorter legend lines
    legend.spacing.y = unit(0.02, "cm"),  # tighter legend spacing
    legend.key.height = unit(0.25, "cm"),
    plot.title = element_text(size = 14, face = "bold")
  )

p_2


p_2_faceted <- ggplot(
  projection_df_tweets,
  aes(
    x = x,
    y = y,
    colour = topic,
    shape  = topic
  )
) +
  geom_point(alpha = 0.6, size = 2) +
  theme_minimal() +
  guides(color = guide_legend(ncol = 2)) +
  
  labs(
    title = "Tweets",
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
      10, # plus
      7,  # square cross
      8,  # star
      9   # diamond plus
    )
  ) +
  scale_color_manual(values = pal_topic1) +
  facet_wrap(~ topic, nrow = 2, ncol = 4) +  # 2 rows x 4 columns
  theme(
    legend.position = "none",
    legend.text = element_text(size = 10),      # smaller legend font
    legend.key.width = unit(0.5, "cm"),        # shorter legend lines
    legend.spacing.y = unit(0.04, "cm"),  # tighter legend spacing
    legend.key.height = unit(0.25, "cm"),
    legend.title = element_text(face = "bold"),
    plot.title = element_text(size = 14, face = "bold"),
    strip.text = element_text(size = 10,face = "bold", hjust = 0)   # bold + left-aligned)
  )

pall <- grid.arrange(p_2,p_1,nrow = 2, ncol = 1,heights = c(0.4,0.4))
ggsave("./figures/Topic_UMAP.jpeg", pall,
       width = 9, height = 6, units = "in", dpi = 300)

pall_facet <- grid.arrange(p_2_faceted,p_1_faceted,nrow = 2, ncol = 1,heights = c(0.4,0.4))
ggsave("./figures/Topic_UMAP_facet.jpeg", pall_facet,
       width = 12, height = 7, units = "in", dpi = 300)


df_all <- bind_rows(
  projection_df_tweets,
  projection_df_articles
)

df_all <- df_all %>% arrange(panel,topic,x,y)

p_all_facet <- ggplot(
  df_all,
  aes(
    x = x,
    y = y,
    colour = topic,
    shape  = topic
  )
) +
  geom_point(alpha = 0.6, size = 2) +
  theme_minimal() +
  labs(
    title = NULL,
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
      10, # plus
      7,  # square cross
      8,  # star
      9   # diamond plus
    )
  ) +
  scale_color_manual(values = pal_topic) +
  facet_wrap(~ panel,ncol = 1, nrow = 2,"free") +
  theme(
    legend.position = "right",
    legend.text = element_text(size = 10),      # smaller legend font
    legend.key.width = unit(0.5, "cm"),        # shorter legend lines
    legend.spacing.y = unit(0.04, "cm"),  # tighter legend spacing
    legend.key.height = unit(0.25, "cm"),
    legend.title = element_text(face = "bold"),
    plot.title = element_text(size = 14, face = "bold"),
    strip.text = element_text(size = 14,face = "bold", hjust = 0)   # bold + left-aligned)
  )
p_all_facet

ggsave("./figures/Topic_UMAP.jpeg", p_all_facet,
       width = 7, height = 6, units = "in", dpi = 300)

## Figures for breakdown of events and event charachteristics -----
events_data <- read_csv("./data_final/NFWF_events_final.csv")[,-1]
length(unique(events_data$event_id))
length(unique(events_data$`Respondent ID`))

events_data_summary <- events_data %>% group_by(event_id) %>% summarise(ntimes = n(),
                                                                        nyears = length(unique(Year)),
                                                                        Employer_type = unique(Employer_type),
                                                                        Org_size = unique(Org_size),
                                                                        Staff = unique(Staff),
                                                                        Attendance = unique(Attendance),
                                                                        Business_owners = max(Business_owners),
                                                                        General_public = max(General_public),
                                                                        Property_owners = max(Property_owners),
                                                                        Recreationists = max(Recreationists),
                                                                        Youth_family_friendly = max(`Youth_family-friendly`),
                                                                        Other_audience = max(Other_audience),
                                                                        Social_media_info_campaign = max(Social_media_info_campaign),
                                                                        Onsite_engagement = max(Onsite_engagement),
                                                                        Off_site_community_engagement = max(`Off-site_community_engagement`),
                                                                        School_visit = max(School_visit),
                                                                        Service_volunteer_community_science = max(Service_volunteer_community_science),
                                                                        Family_friendly_activities = max(Family_friendly_activities),
                                                                        Other_type = max(Other_type))

events_data_summary <- events_data_summary %>%
  mutate(
    Org_size = factor(
      Org_size,
      levels = c(
        "<5 employees",
        "5-9 employees",
        "10-19 employees",
        "20-49 employees",
        "50-99 employees",
        ">100 employees"), 
      labels = c("<5",
                 "5-9",
                  "10-19",
                  "20-49",
                   "50-99",
                    ">100")
    )
  )

events_data_summary <- events_data_summary %>%
  mutate(
    Staff = factor(
      Staff,
      levels = c(
        "0",
        "<5",
        "6-10",
        "11-20",
        ">20"
      )
    )
  )

events_data_summary <- events_data_summary %>%
  mutate(
    Staff = factor(
      Staff,
      levels = c(
        "0",
        "<5",
        "6-10",
        "11-20",
        ">20"
      )
    )
  )

events_data_summary <- events_data_summary %>%
  mutate(
    Attendance = factor(
      Attendance,
      levels = c(
        "0",
        "<20",
        "20-50",
        "50-100",
        ">100",
        "unknown"
      )
    )
  )

# devtools::install_github("an-bui/calecopal")
library(calecopal)
names(cal_palettes)
pal1 <- calecopal::cal_palette(name = "chaparral3", n = length(unique(events_data$Employer_type)), type = "discrete")
pal2 <- calecopal::cal_palette(name = "sbchannel", n = length(unique(events_data$Org_size)), type = "continuous")
pal3 <- calecopal::cal_palette(name = "eschscholzia", n = length(unique(events_data$Staff)), type = "continuous")
pal4 <- calecopal::cal_palette(name = "calochortus", n = length(unique(events_data$Attendance)), type = "continuous")


p1 <- ggplot(events_data_summary,
             aes(x = "", fill = Employer_type)) +
  geom_bar(width = 0.8) +
  scale_fill_manual(
    values = pal1,
    guide = guide_legend(ncol = 1)
  ) +
  labs(
    title = "Org Type",
    x = NULL,
    y = "Frequency",
    fill = NULL
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    legend.position = "bottom",
    legend.box = "vertical"
  )
p1


p2 <- ggplot(events_data_summary,
             aes(x = "", fill = Org_size)) +
  geom_bar(width = 0.8) +
  scale_fill_manual(
    values = pal2,
    guide = guide_legend(ncol = 1)
  ) +
  labs(
    title = "Org Size",
    x = NULL,
    y = NULL,
    fill = NULL
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    legend.position = "bottom",
    legend.box = "vertical"
  )
p2

p3 <- ggplot(events_data_summary,
             aes(x = "", fill = Staff)) +
  geom_bar(width = 0.8) +
  scale_fill_manual(
    values = pal3,
    guide = guide_legend(ncol = 1)
  ) +
  labs(
    title = "Staff Presence",
    x = NULL,
    y = NULL,
    fill = NULL
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    legend.position = "bottom",
    legend.box = "vertical"
  )
p3

p4 <- ggplot(events_data_summary,
             aes(x = "", fill = Attendance)) +
  geom_bar(width = 0.8) +
  scale_fill_manual(
    values = pal4,
    guide = guide_legend(ncol = 1)
  ) +
  labs(
    title = "Attendance",
    x = NULL,
    y = NULL,
    fill = NULL
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    legend.position = "bottom",
    legend.box = "vertical"
  )
p4


library(patchwork)
all_stacks <- p1 | p2 | p3 | p4
all_stacks
ggsave("./figures/Event_summaries.jpeg", all_stacks,
       width = 8, height = 4, units = "in", dpi = 300)



binary_cols <- c("Business_owners", "General_public", "Property_owners", "Recreationists",
                 "Youth_family_friendly","Other_audience")

df_sums <- events_data_summary %>%
  dplyr::select(binary_cols) %>%    # <- just pass the vector directly
  pivot_longer(
    cols = everything(),
    names_to = "variable",
    values_to = "value"
  ) %>%
  group_by(variable) %>%
  summarise(total = sum(value, na.rm = TRUE), .groups = "drop")

df_sums <- df_sums %>%
  mutate(
    variable = factor(
      variable,
      levels = c("Other_audience",
        "General_public",
        "Property_owners",
        "Business_owners",
        "Recreationists",
        "Youth_family_friendly"), 
      labels = c(
                 "Other audience",
                 "General public",
                 "Property owners",
                 "Business owners",
                 "Recreationists",
                 "Youth/family friendly")
    )
  )


pal5 <- c("gray","gray","gray","gray","gray","gray")

p5 <- ggplot(df_sums, aes(x = variable, y = total, fill = variable)) +
  geom_col(width = 0.7) +
  scale_fill_manual(values = pal5) +
  labs(x = NULL, y = "Frequency", fill = NULL, title = "Target Audience") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none"
  )


binary_cols <- c("Social_media_info_campaign", "Onsite_engagement", "Off_site_community_engagement", "School_visit",
                 "Service_volunteer_community_science","Family_friendly_activities","Other_type")

df_sums <- events_data_summary %>%
  dplyr::select(binary_cols) %>%    # <- just pass the vector directly
  pivot_longer(
    cols = everything(),
    names_to = "variable",
    values_to = "value"
  ) %>%
  group_by(variable) %>%
  summarise(total = sum(value, na.rm = TRUE), .groups = "drop")

df_sums <- df_sums %>%
  mutate(
    variable = factor(
      variable,
      levels = c("Other_type",
        "Social_media_info_campaign", "Onsite_engagement", "Off_site_community_engagement", "School_visit",
        "Service_volunteer_community_science","Family_friendly_activities"), 
      labels = c("Other type",
        "Social media campaign", "On site engagement", "Off site engagement", "School visit",
        "Community science","Family friendly activities")
    )
  )


pal6 <- c("gray","gray","gray","gray","gray","gray","gray")

p6 <- ggplot(df_sums, aes(x = variable, y = total, fill = variable)) +
  geom_col(width = 0.7) +
  scale_fill_manual(values = pal6) +
  labs(x = NULL, y = NULL, fill = NULL, title = "Event Type") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none"
  )
p6

max_y <- max(
  max(ggplot_build(p5)$data[[1]]$y),
  max(ggplot_build(p6)$data[[1]]$y)
)

p5 <- p5 + ylim(0, max_y)
p6 <- p6 + ylim(0, max_y)

all_side <- p5 | p6
all_side
ggsave("./figures/Event_summaries2.jpeg", all_side,
       width = 8, height = 3, units = "in", dpi = 300)



all_grid <- (p1 | p2 | p3 | p4) /
  (p5 | p6)

all_grid
ggsave("./figures/Event_summaries_ALL.jpeg", all_grid,
       width = 8, height = 7, units = "in", dpi = 300)


top_row <- p1 | p2 | p3 | p4
bottom_row <- p5 | p6

top_row_boxed <- top_row +
  plot_annotation(tag_levels = list("a")) &
  theme(
    plot.tag.position = c(0.01, 0.99),
    plot.tag = element_text(face = "bold", size = 14),
    plot.background = element_rect(color = "black", fill = NA, linewidth = 0.8)
  )



bottom_row_boxed <- bottom_row +
  plot_annotation(tag_levels = list("b")) &
  theme(
    plot.tag.position = c(0.01, 0.99),
    plot.tag = element_text(face = "bold", size = 14),
    plot.background = element_rect(color = "black", fill = NA, linewidth = 0.8)
  )

final_plot <- top_row_boxed / bottom_row_boxed

final_plot <- top_row / bottom_row

final_plot
ggsave("./figures/Event_summaries_ALL.jpeg", final_plot,
       width = 8, height = 6, units = "in", dpi = 600)
       
       
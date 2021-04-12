library(tidyverse)
library(tidytext)
library(widyr)
library(ggraph)
library(igraph)
library(tidygraph)

### Reading in Data
a2021 <- read_csv("airline2021.csv")
df <- a2021 %>%
  select(status_id,created_at,text,
         mentions_screen_name,location,retweet_count, hashtags)
### Cleaning Data and unnesting Mentions
check <- df %>%
  mutate(mentions_screen_name = str_extract_all(mentions_screen_name, 
                                                '(?<=")[A-Za-z]+')) %>%
  unnest(., mentions_screen_name) %>%
  mutate(hashtags = str_extract_all(hashtags, '(?<=")[A-Za-z]+')) %>%
  unnest(., hashtags) %>%
  group_by(status_id) %>%
  mutate(mentions_screen_name =
           ifelse(is.na(mentions_screen_name),hashtags,mentions_screen_name)) %>%
  mutate(hashtags = 
           ifelse(is.na(hashtags), mentions_screen_name,hashtags)) %>%
  mutate(mentions_screen_name = tolower(mentions_screen_name))

### Filtering and Sorting Data by Airline
clean <- check %>%
  filter(str_detect(mentions_screen_name, 
                    'alaska|delta|united|southwest|americanair')) %>%
  group_by(status_id) %>%
  mutate(n = n(),
         is_dupe = ifelse(n > 1,1,0)) %>%
  filter(is_dupe == 0) %>%
  filter(row_number()==1) %>%
  select(status_id,created_at,text,
         mentions_screen_name,location,retweet_count) %>%
  rename(airline = mentions_screen_name) %>%
  mutate(airline = case_when(airline == "alaskaair" ~ "AlaskaAir",
                             airline %in% c("americanair","americanairlines",
                                            "americanairlnes") ~
                               "AmericanAir",
                             airline %in% c("delta", "deltaairline") ~ "DeltaAir",
                             airline == "southwestair" ~ "SouthwestAir",
                             airline == "united" ~ "United")) %>%
  ungroup()

saveRDS(clean, "Cleaned_Text2021.RData")


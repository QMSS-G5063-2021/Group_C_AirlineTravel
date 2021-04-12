library(tidyverse)
library(tidytext)
library(widyr)
library(ggraph)
library(igraph)
library(tidygraph)

## Load Data and Cleaning Strings
clean <- readRDS('Cleaned_Text2021.RData')
tidy_2021 <- clean %>%
  mutate(text = tolower(text)) %>%
  unnest_tokens(output = word, input = text) %>% 
  anti_join(bind_rows(stop_words, data.frame(word = c("rt", "https"), 
                                             lexicon = "TWITTER")), 
            by = "word") %>%
  mutate(word =  gsub("[[:punct:][:blank:]]+", "", word)) %>%
  mutate(word = gsub("[0-9]+", "", word)) %>%
  mutate(word =  gsub("*\\b[[:alpha:]]{1,2}\\b *", "", word)) %>%
  mutate(word =  gsub("\\b[A-Z]+\\b", "", word)) %>%
  mutate(word = gsub("^ +| +$|( ) +", "\\1", word)) %>%
  mutate(word = str_replace(word,"alaska|delta|united|southwest|americanair","")) %>%
  
  filter(word != "") %>%
  count(airline, word, sort = TRUE) 

## Total Word for String
total_2021 <- clean %>%
  mutate(text = tolower(text)) %>%
  unnest_tokens(output = word, input = text) %>% 
  anti_join(bind_rows(stop_words, data.frame(word = c("rt", "https"), 
                                             lexicon = "TWITTER")), 
            by = "word") %>%
  mutate(word =  gsub("[[:punct:][:blank:]]+", "", word)) %>%
  mutate(word = gsub("[0-9]+", "", word)) %>%
  mutate(word =  gsub("*\\b[[:alpha:]]{1,2}\\b *", "", word)) %>%
  mutate(word =  gsub("\\b[A-Z]+\\b", "", word)) %>%
  mutate(word = gsub("^ +| +$|( ) +", "\\1", word)) %>%
  mutate(word = str_replace(word,"alaska|delta|united|southwest|americanair","")) %>%
  
  filter(word != "") %>%
  count(airline, word, sort = TRUE) %>%
  group_by(airline) %>% 
  summarize(total = sum(n))

## Joining datadrame
tidy_2021 <- left_join(tidy_2021, total_2021, by = 'airline')

## Finding TF-IDF Score
airline_tf_idf <- tidy_2021 %>%
  bind_tf_idf(word, airline, n)

## Graph
airline_tf_idf %>%
  group_by(airline) %>%
  slice_max(tf_idf, n = 5) %>%
  ungroup() %>%
  ggplot(aes(tf_idf, fct_reorder(word, tf_idf), fill = airline)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~airline, ncol = 2, scales = "free") +
  scale_x_continuous(labels = scales::number_format(accuracy = 0.001,
                                                    decimal.mark = '.')) +
  labs(x = "Term Frequency–Inverse Document Frequency",
       y = 'Top 5 Words From Tweets') +
  ggtitle("TF-IDF Score by Airline Tweets") +
  theme(plot.title = element_text(vjust=2, hjust = 0.5),
        axis.title.x = element_text(vjust = -1),
        axis.title.y = element_text(vjust = 1))
library(tidyverse)
library(tidytext)
library(widyr)
library(ggraph)
library(igraph)
library(tidygraph)

## Load Data 
clean <- readRDS('Cleaned_Text2021.RData')

## Create Tidy

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


## Join Sentiment

valence <- inner_join(tidy_2021, get_sentiments("afinn"), by = "word")

## Graph
ggplot(valence) + 
  geom_boxplot(aes(x = airline, y = value, color = airline), show.legend = FALSE) + 
  ylim(-5, 5) +
  labs(x = "Airlines", y = "AFINN Values") +
  ggtitle("Sentiment Value Distribution By Airlines") +
  theme(plot.title = element_text(vjust=2, hjust = 0.5))
library(tidyverse)
library(tidytext)
library(plotly)

## Load Data 
setwd("~/dataviz2021/Group_Project/Andrew_Text/Main")
clean <- readRDS('Cleaned_Text2021.RData')

######################################
#Wrangling#
######################################

## Cleaning
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

######################################
#Sentiment Valence GRAPH#
######################################

valence <- inner_join(tidy_2021, get_sentiments("afinn"), by = "word")
violin_plot <- ggplot(valence, aes(x = airline, y = value, color = airline)) + 
  geom_violin( show.legend = FALSE) + 
  geom_boxplot(width=.1) +
  scale_y_continuous(breaks = seq(-5, 5, by = 1)) +
  labs(x = "Airlines", y = "AFINN Values") +
  ggtitle("Tweets Sentiment Value Distribution By Airlines") +
  theme(plot.title = element_text(vjust=2, hjust = 0.5),
        legend.position =  'none')

violin_plot


######################################
#Sentiment Valence Weight GRAPH#
######################################

weight_plot <- valence %>%
  mutate(contribution = n * value) %>%
  group_by(airline) %>%
  slice_head(n = 5) %>%
  arrange(((contribution))) %>%
  mutate(word = reorder(word, contribution)) %>%
  ggplot(aes(x = contribution, y = reorder(word, contribution), 
             fill = contribution > 0)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~airline, ncol = 2, scales = "free") +
  labs(x = "Sentiment Value * Number of Appearances",
       y = 'Top 5 Words From Tweets') +
  ggtitle("Sentiment Value Weighted by Frequency of Words in Tweets") +
  theme(plot.title = element_text(vjust=2, hjust = 0.5),
        axis.title.x = element_text(vjust = -1),
        axis.title.y = element_text(vjust = 1),
        legend.position =  'none')

weight_plot



######################################
#Emotion Category GRAPH#
######################################

nrc_graph <- inner_join(tidy_2021, get_sentiments("nrc"), by = "word")
emotion_plot <- nrc_graph  %>%
  group_by(airline, sentiment) %>%
  summarise(Freq=n(), .groups = 'drop') %>%
  ggplot(aes(Freq, reorder(sentiment, Freq), fill = airline)) +
  geom_col(show.legend = TRUE) +
  labs(x = "Frequency",
       y = 'Emotion Category',
       fill = "Airline") +
  ggtitle("Emotion Category by Frequency of Words in Tweets") +
  theme(plot.title = element_text(vjust=2, hjust = 0.5),
        axis.title.x = element_text(vjust = -1),
        axis.title.y = element_text(vjust = 1))
emotion_plot_int <- ggplotly(emotion_plot, tooltip = c("x"))


emotion_plot_int

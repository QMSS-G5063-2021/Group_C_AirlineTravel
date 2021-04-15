library(tidyverse)
library(tidytext)
library(widyr)
library(ggraph)
library(igraph)
library(tidygraph)

## Load Data 
setwd("~/dataviz2021/Group_Project/Andrew_Text/Main")
clean <- readRDS('Cleaned_Text2021.RData')

######################################
#TF-IDF GRAPH#
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

## Finding TF-IDF Score
airline_tf_idf <- tidy_2021 %>%
  bind_tf_idf(word, airline, n)

## Graph
tfidf_graph <- airline_tf_idf %>%
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

tfidf_graph

######################################
#Correlation GRAPH#
######################################


## Cleaning and preparing for correlation
word_cors <- clean %>%
  mutate(section = row_number() %/% 10) %>%
  filter(section > 0)  %>%
  unnest_tokens(output = word, input = text) %>% 
  anti_join(bind_rows(stop_words, data.frame(word = c("rt", "https"), 
                                             lexicon = "TWITTER")), 
            by = "word") %>%
  mutate(word =  gsub("[[:punct:][:blank:]]+", "", word)) %>%
  mutate(word = gsub("[0-9]+", "", word)) %>%
  mutate(word =  gsub("*\\b[[:alpha:]]{1,2}\\b *", "", word)) %>%
  mutate(word =  gsub("\\b[A-Z]+\\b", "", word)) %>%
  mutate(word = gsub("^ +| +$|( ) +", "\\1", word)) %>%
  filter(word != "") %>%   
  group_by(word) %>%
  filter(n() >= 20) %>%
  pairwise_cor(word, section, sort = TRUE) 

## Making Edge
textg <- word_cors %>%
  mutate(item1 = case_when(item1 == "alaskaair" ~ "AlaskaAir",
                           item1 %in% c("americanair","americanairlines",
                                        "americanairlnes") ~
                             "AmericanAir",
                           item1 %in% c("delta", "deltaairline") ~ "DeltaAir",
                           item1 == "southwestair" ~ "SouthwestAir",
                           item1 == "united" ~ "United")) %>%
  mutate(airline = case_when(item1 == "AlaskaAir" ~ "AlaskaAir",
                             item1 == "AmericanAir" ~ "AmericanAir",
                             item1 == "DeltaAir" ~ "DeltaAir",
                             item1 == "SouthwestAir" ~ "SouthwestAir",
                             item1 == "United" ~ "United")) %>%
  filter(airline %in% c("United", "SouthwestAir", "DeltaAir", 
                        "AlaskaAir", "AmericanAir")) %>%
  filter(correlation > 0.30) %>%
  graph_from_data_frame() 

## Categorizing by airline

V(textg)$airline <- as.character(clean$airline[match(V(textg)$name, 
                                                     as.character(clean$airline))])

g_tbl <- as_tbl_graph(textg)
node_from <- g_tbl %>%
  as_tibble() %>%
  mutate(from = row_number())
new_g_tbl <- g_tbl %>%
  activate(edges) %>%
  left_join(node_from)


### Graph
set.seed(2021)
windows.options(width=3.75, height=3.75)
cor_graph <- ggraph(new_g_tbl, layout = "fr") +
  geom_edge_link(aes(edge_alpha = correlation), show.legend = FALSE) +
  geom_node_point(aes(color = airline), size = 5) + 
  geom_node_text(aes(label = name), repel = TRUE) +
  ggtitle("Word Correlation Network By Airlines") + 
  theme(panel.background = element_blank(),
        strip.background = element_rect(colour=NA, fill=NA),
        legend.position =  'none',
        plot.title = element_text(vjust=2, hjust = 0.5))

cor_graph

######################################
#Sentiment Valence GRAPH#
######################################

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
valence_graph <- ggplot(valence) + 
  geom_boxplot(aes(x = airline, y = value, color = airline), show.legend = FALSE) + 
  ylim(-5, 5) +
  labs(x = "Airlines", y = "AFINN Values") +
  ggtitle("Sentiment Value Distribution By Airlines") +
  theme(plot.title = element_text(vjust=2, hjust = 0.5))

valence_graph
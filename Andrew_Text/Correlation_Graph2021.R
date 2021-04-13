library(tidyverse)
library(tidytext)
library(widyr)
library(ggraph)
library(igraph)
library(tidygraph)

## Load Data 
setwd("~/dataviz2021/Group_Project/Andrew_Text")
clean <- readRDS('Cleaned_Text2021.RData')
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


## Save
ggsave(cor_graph, file="cor_graph.png", width = 10, height = 6, dpi = 1000)
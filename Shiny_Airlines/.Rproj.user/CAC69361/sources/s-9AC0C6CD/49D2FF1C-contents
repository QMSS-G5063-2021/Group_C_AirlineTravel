library(shiny)
library(shinythemes)
library(tidyverse)
library(network)
library(urbnmapr)
library(sfnetworks)
library(sf)
library(sp)
library(tidygraph)
library(stplanr)
library(igraph)
library(tidygraph)
library(gganimate)
library(transformr)
library(ggthemes)
library(visNetwork)
library(magick)

##########################################################

#load("planes.RData") 
load("www/planesTop10Airlines.RData")
load("www/planesTop10Act.RData")
load("www/states_sf.RData")
load("www/net_perc.RData")
load("www/df_wider3.RData")
load("www/df_wider4.RData")

#states <- unique(planes$ORIGIN_STATE_NM)
#states <- sort(states)

month <- c("January","February","March","April","May","June")
startingMonth <- c("January","February","March","April","May")
endingMonth <- c("February","March","April","May","June")
network <- function(start=1,end=6,min_corr=0.9995, min_importance=6, min_pop=1e6){
  
  month <- c("January","February","March","April","May","June")  
  
  nodes <- read_tsv("NODES_COORDINATES.tsv",
                    col_types = list(col_number(),
                                     col_character(),
                                     col_number(),
                                     col_number()))
  
  nodes <- nodes %>% filter(nodes$label %in% df_wider3$ORIGIN_CITY_NAME | nodes$label %in% df_wider3$DEST_CITY_NAME) %>%
    mutate(id = 1:n())
  
  nodes1 <- nodes %>% left_join(df_wider4,by=c("label"="City"))
  cities_matrix <- nodes1 %>% filter(importance>= min_importance) %>% filter(annual >= min_pop) %>% select(label,importance, JAN:JUN)
  corr <- cor(t(cities_matrix[,(start+2):(end+2)]))
  
  upd_min_importance <- min(cities_matrix$importance)
  
  labels <- tibble(label = cities_matrix$label, id = 1:length(cities_matrix$label))
  
  cor_g <- graph_from_adjacency_matrix(corr, mode='undirected', weighted = 'correlation')
  cor_edge_list <- get.data.frame(cor_g)
  only_sig <- cor_edge_list[abs(cor_edge_list$correlation) > min_corr & abs(cor_edge_list$correlation) != 1, ]
  
  dim(only_sig)
  
  colors <- tibble(importance = 1:8, color = c("pink","purple","blue","aquamarine","green","#e6e600","orange","red")) %>% filter(importance >= upd_min_importance)
  
  nodes_sig <-  labels %>% left_join(cities_matrix,by="label") %>% left_join(colors,by="importance") %>% select(id,label,importance, color) 
  
  
  nodes_unique <-  data.frame(label = as.character(1:8),
                              shape = c( "circle"), color = c("pink","purple","blue","aquamarine","green","#e6e600","orange","red"), City = 1:8, size=10) %>%
    filter(City >= upd_min_importance) 
  nodes_unique$label <- (8-upd_min_importance+1):1
  nodes_unique <- nodes_unique %>% arrange(label)
  
  new_g <- graph_from_data_frame(d=only_sig,vertices=nodes_sig,directed= F)
  
  graph <- visNetwork(nodes_sig, only_sig) %>% #main="Patterns of Monthly Changes in Airline Passenger Volume of Top Airport Cities",
                      #submain=list(text=paste0("Network constructed based on high correlations of monthly changes in air traffic of cities from ",
                                               #month[start]," to ",month[end]," 2020."))) %>% #,
                      #footer = list(text=paste0("Only includes cities with a minimum 2020 air passenger traffic volume of: ", min_pop))) %>% 
    visIgraphLayout(layout = "layout_nicely",smooth=T,randomSeed=10) %>% 
    visEdges(arrows = "middle") %>%
    addFontAwesome() %>%
    visNodes(color=list(border="black")) %>%
    visLegend(main ="Airport City Traffic Volume Level",addNodes=nodes_unique,useGroups = F,ncol=2) %>%
    visOptions(highlightNearest = list(enabled = T, hover = T), nodesIdSelection = list(main="Select by city"))
  
  #visExport(graph,type="html",name=paste0("network_cities_",month[start],"_",month[end],".html"))
  
  return(graph)
  
}
min_corr1 <- .999


###############################################################################


#data <- read.csv( switch(company,"apple" = "appledata.csv","yahoo" = "yahoo_data.csv"))
  # convenient shortening of an if statement that chooses its value according to the value of another var

library(tidyverse)
library(ggmap)
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
library(igraph)

register_google(key = Sys.getenv('GOOGLE_API'))
setwd("C:/Users/jm445/Documents/")
# Read monthly csv files and focus on non-zero passengers flight
df <- data.frame()
for (file in list.files(path = 'DOT',
                        pattern = '[a-zA-Z]+[0-9]+',
                        full.names = T)) {
  print(file)
  this_df <- read_csv(file) %>% select(-X15) %>%
    mutate(YEAR = str_extract(string = file, pattern = '[0-9]+') %>% as.integer)
  df <- bind_rows(df, this_df)
}
df <- df %>% arrange(YEAR,MONTH) #%>% filter(PASSENGERS!=0)


df_sub <- df %>% filter(!DEST_STATE_ABR %in% c("AK","HI","PR","VI","TT")) %>%
  filter(!ORIGIN_STATE_ABR %in% c("AK","HI","PR","VI","TT")) %>%
  select(PASSENGERS,ORIGIN_CITY_NAME,ORIGIN_STATE_ABR,
         DEST_CITY_NAME,DEST_STATE_ABR, MONTH)

or <- df_sub %>% pull(ORIGIN_CITY_NAME)
dest <- df_sub %>% pull(DEST_CITY_NAME)
pairs <- c()

for(i in 1:length(or)){
  pair <- c(or[i],dest[i])
  pairs[i] <- paste(pair[order(pair)],collapse=", ")
  
}
df_sub$city_comb <- pairs

df_sub <- df_sub %>%
  group_by(city_comb,MONTH) %>%
  mutate(volume = sum(PASSENGERS,na.rm=T)) %>%
  ungroup() %>%
  select(-PASSENGERS) %>%
  distinct(city_comb,MONTH,.keep_all=T) %>%
  mutate(MONTH = as.factor(MONTH)) 

# Geocode cities (already done, no need to repeat unless non-continental states/territories wanted)
{
  sources <- df_sub %>%
    distinct(ORIGIN_CITY_NAME) %>%
    rename(label = ORIGIN_CITY_NAME)
  
  destinations <- df_sub %>%
    distinct(DEST_CITY_NAME) %>%
    rename(label = DEST_CITY_NAME)
  
  nodes <- full_join(sources, destinations, by = "label")
  nodes <- nodes %>% rowid_to_column("id")
  
  nodes_ggmap <- geocode(location = nodes$label, output = "latlon", source = "google")
  #nodes_ggmap <- geocode(location = cities[!cities %in% cities1], output = "latlon", source = "google")
  nodes <- nodes %>% mutate(lon = nodes_ggmap$lon) %>%
    mutate(lat = nodes_ggmap$lat)
  nodes %>% write_tsv("DOT/NODES_COORDINATES.tsv")
  }

############Edges
df_wider <- df_sub %>% pivot_wider(names_from=MONTH,values_from=volume) 
df_wider[is.na(df_wider)] <- 0

colnames(df_wider)[6:17] <- c("DEC","JAN","FEB","MAR","APR","MAY","JUN","JUL","AUG","SEP","OCT","NOV")

df_wider <- df_wider %>%
  mutate(annual = rowSums(across(where(is.numeric)))) %>%
  filter(annual > 1000) %>%
  mutate(importance = cut(log(annual),breaks=seq(min(log(annual)-0.01),max(log(annual)+0.01,na.rm=T),length=8+1))) %>%
  mutate(importance= factor(importance) %>% as.numeric())


df_wider3 <- df_wider %>%
  pivot_longer(DEC:NOV,names_to="MONTH",values_to="VOL") %>%
  group_by(ORIGIN_CITY_NAME,DEST_CITY_NAME) %>%
  mutate(pct.chg = 100*(VOL - lag(VOL))/lag(VOL)) %>%
  mutate(pct.chg = ifelse(pct.chg>100,100,pct.chg)) %>%
  mutate(pct.chg = ifelse(pct.chg< -100, -100, pct.chg)) %>%
  select(-VOL) %>%
  pivot_wider(names_from=MONTH,values_from=pct.chg) %>%
  drop_na(JAN:JUN)

cities1 <- unique(c(df_wider3$ORIGIN_CITY_NAME,df_wider3$DEST_CITY_NAME))
###############Nodes

node_vol <- matrix(NA,nrow=length(cities1),ncol=13)
node_vol[,1] <- cities1

k <- 1
for(i in node_vol[,1]){
  for(j in c(1:12)){
  node_vol[k,j+1] <- df_sub %>% filter(MONTH==j) %>%
    filter(ORIGIN_CITY_NAME==i | DEST_CITY_NAME==i) %>%
    pull(volume) %>% sum(na.rm=T)

  }
  k <- k+1
}
colnames(node_vol) <- c("City","JAN","FEB","MAR","APR","MAY","JUN","JUL","AUG","SEP","OCT","NOV","DEC")

df_wider2 <- as_tibble(node_vol) %>% select(City,DEC,JAN:NOV)
df_wider2[,-1] <- sapply(df_wider2[, -1], as.numeric)

df_wider2 <- df_wider2 %>%
  mutate(annual =  rowSums(across(where(is.numeric)))) %>%
  mutate(importance = cut(log(annual),breaks=seq(min(log(annual)-0.01,na.rm=T),max(log(annual)+0.01,na.rm=T),length=8+1))) %>%
  mutate(importance= factor(importance) %>% as.numeric()) 

df_wider4 <- df_wider2

df_wider4 <- df_wider4 %>%
  pivot_longer(DEC:NOV,names_to="MONTH",values_to="VOL") %>%
  group_by(City) %>%
  mutate(pct.chg = 100*(VOL - lag(VOL))/lag(VOL)) %>%
  mutate(pct.chg = ifelse(pct.chg>100,100,pct.chg)) %>%
  mutate(pct.chg = ifelse(pct.chg< -100, -100, pct.chg)) %>%
  select(-VOL) %>%
  pivot_wider(names_from=MONTH,values_from=pct.chg) %>%
  select(-DEC) %>%
  drop_na(JAN:JUN)

#Only routes (and cities) where more than 500 passengers
#df_wider - routes with total counts (all positive)
#df_wider2 - cities with total counts (all positive)
#df_wider3 - routes with monthly decreases
#df_wider4 - cities with monthly decreases



############## Create network's nodes

network_connection <- function(df_wider_routes,df_wider_nodes){

nodes <- read_tsv("DOT/NODES_COORDINATES.tsv",
                  col_types = list(col_number(),
                                   col_character(),
                                   col_number(),
                                   col_number()))

nodes <- nodes %>% filter(nodes$label %in% df_wider_routes$ORIGIN_CITY_NAME | nodes$label %in% df_wider_routes$DEST_CITY_NAME) %>%
  mutate(id = 1:n())


#cities <- unique(c(df_wider$ORIGIN_CITY_NAME,df_wider$DEST_CITY_NAME))
#cities1 <- unique(nodes$label)

nodes1 <- nodes %>% left_join(df_wider_nodes,by=c("label"="City"))

sp <- SpatialPoints(nodes[,3:4])
nodes_sf <- st_as_sf(sp)
nodes_sf <- st_set_crs(nodes_sf, 4326)
nodes_sf <- cbind(nodes$id,nodes_sf) %>% left_join(nodes1,by=c("nodes.id"="id"))

#Create network's edges

edges <- df_wider_routes  %>% 
  left_join(nodes, by = c("ORIGIN_CITY_NAME" = "label")) %>% 
  rename(from = id) %>%
  left_join(nodes, by = c("DEST_CITY_NAME" = "label")) %>% 
  rename(to = id)

edges1 <- edges %>%
  left_join(nodes,by = c("from" = "id")) %>%
  rename(from_lon = lon) %>% rename(from_lat = lat) %>%
  left_join(nodes,by = c("to" = "id")) %>%
  rename(to_lon = lon) %>% rename(to_lat = lat) %>%
  drop_na(JAN:JUN) %>%
  select(-starts_with("label")) %>%
  rename(`1`="JAN") %>% rename(`2`="FEB") %>% rename(`3`="MAR") %>% rename(`4`="APR") %>% rename(`5`="MAY") %>% rename(`6`="JUN")  %>%
  pivot_longer(`1`:`6`,names_to="MONTH") %>% mutate(MONTH=as.numeric(MONTH))


st_segment <- function(r){st_linestring(t(matrix(unlist(r), 2, 2)))}

edges1$geom <- st_sfc(sapply(1:nrow(edges1), 
                             function(i){st_segment(edges1[i,c("from_lon","from_lat","to_lon","to_lat")])},
                             simplify=FALSE))
edges_sf <- st_sf(edges1)
edges_sf <- st_set_crs(edges_sf, 4326) %>% mutate(dist=st_length(geom))%>%
  mutate(local = 1)

breaks <- quantile(edges_sf$dist, probs=c(0.25,0.50,0.75))
edges_sf$local <- 3
edges_sf$local[edges_sf$dist <= breaks[3]] <- 2
edges_sf$local[edges_sf$dist <= breaks[2]] <- 1
edges_sf$local[edges_sf$dist <= breaks[1]] <- 0
edges_sf$local <- as.factor(edges_sf$local)
levels(edges_sf$local) <- c("Short-Distance Flights", "Low-Mid Distance Flights","High-Mid Distance Flights","Long-Distance Flights")

# Establish network
net <- sfnetwork(nodes_sf, edges_sf, directed = TRUE,force=T)
return(net)
}

#net_count <- network_connection(df_wider,df_wider2)
net_perc <- network_connection(df_wider3,df_wider4)

###################### Spatial network maps

states_sf <- get_urbn_map("states", sf = TRUE)%>%
  filter(!state_name %in% c("Hawaii","Alaska"))



par(mfrow=c(6,2))
month <- c("January","February","March","April","May","June")
month_abb <- c("JAN","FEB","MAR","APR","MAY","JUN")

for (i in 1:6){
 # for(j in c("Short-Distance Flights","Low-Mid Distance Flights","High-Mid Distance Flights","Long-Distance Flights")){
title <- paste0("Monthly Change in Domestic Air Traffic Routes in ",month[i]," 2020")
graph <- states_sf %>% 
  ggplot(aes()) +
  geom_sf(fill = "grey36", color = "black")+
  geom_sf(data=activate(net_perc,"edges") %>% st_as_sf() %>%
            select(from,to,importance,local,MONTH,value)%>% filter(MONTH==i) %>%
 #           filter(local==j) %>%
            arrange(desc(value)),
          aes(color=value,alpha=importance,size=importance/8)) +
  geom_sf(data=activate(net_perc,"nodes") %>% st_as_sf(),
          aes(size=ifelse(importance>=6,(importance/4) + .4,0.2)),colour="black")+
  geom_sf(data=activate(net_perc,"nodes") %>% st_as_sf(),
          aes(size=ifelse(importance>=6,importance/4,0.15)),colour="white")+
  scale_color_gradient2(low="red",mid="white",high="blue",midpoint=0,limits=c(-100,100))+
 # scale_colour_viridis_c("% Change in Monthly Passenger Volume",option = 'plasma', direction=-1, limits=c(-100,100)) +
  scale_size_continuous(range = c(0.25,2))+
  guides(alpha=F, size=F)+
  facet_wrap(~local, nrow=2)+
  theme_map()+
  theme(legend.position = "bottom")+
  ggtitle(title)+
  labs(caption="Cities and routes are sized by traffic volume. \n Growth rates are capped at -100% and 100%.")

ggsave(paste0(month[i],"_.png"), graph, width = 12, height = 6, dpi = 1000)
  #}
}


####BELOW CODE NOT USED#############################

nodes <- read_tsv("DOT/NODES_COORDINATES.tsv",
                  col_types = list(col_number(),
                                   col_character(),
                                   col_number(),
                                   col_number()))

nodes <- nodes %>% filter(nodes$label %in% df_wider$ORIGIN_CITY_NAME | nodes$label %in% df_wider$DEST_CITY_NAME) %>%
  mutate(id = 1:n())

nodes1 <- nodes %>% left_join(df_wider4,by=c("label"="City"))


edges1 <- df_wider3  %>% 
  left_join(nodes, by = c("ORIGIN_CITY_NAME" = "label")) %>% 
  rename(from = id) %>%
  left_join(nodes, by = c("DEST_CITY_NAME" = "label")) %>% 
  rename(to = id) %>%
  left_join(nodes,by = c("from" = "id")) %>%
  rename(from_lon = lon) %>% rename(from_lat = lat) %>%
  left_join(nodes,by = c("to" = "id")) %>%
  rename(to_lon = lon) %>% rename(to_lat = lat) %>%
  drop_na(JAN:JUN) %>%
  select(-starts_with("label"))


nw_nodes <- nodes %>% select(id,label)
nw_edges <- edges1 %>% select(ORIGIN_CITY_NAME:from,to)
routes_tidy <- tbl_graph(nodes = nw_nodes, edges = nw_edges, directed = TRUE)

library(ggraph)
ggraph(routes_tidy, layout = "graphopt") + 
  geom_node_point() +
  geom_edge_link(aes(width = JAN), alpha = 0.8) + 
  scale_edge_width(range = c(0.2, 2)) +
  geom_node_text(aes(label =label), repel = TRUE) +
  labs(edge_width = "Letters") +
  theme_graph()


library(visNetwork)
visNetwork(nw_nodes, nw_edges[,c(16,17,18)])

#######Correlation matrix
cities_matrix <- nodes1 %>% filter(importance>=2) %>% select(label,importance, JAN:JUN)
cities_corr <- cor(t(cities_matrix[,3:8]))

cities_corr[cities_corr<0.995] <- 0
colnames(cities_corr) <- cities_matrix$label
# Make an Igraph object from this matrix:
network <- graph_from_adjacency_matrix( cities_corr, weighted=T, mode="undirected", diag=F)

# Basic chart
#plot(network)


library(RColorBrewer)
coul <- rev(brewer.pal(nlevels(as.factor(cities_matrix$importance)), "RdYlBu"))

# Map the color to cylinders
my_color <- coul[as.numeric(as.factor(cities_matrix$importance))]

# plot
png("correlation_network_perc_decr.png", width = 350, height = "350")
par(bg="white", mar=c(0,0,0,0))
set.seed(4)
plot(network, 
     vertex.size=10,
     vertex.color=my_color, 
     vertex.label.cex=0.7,
     vertex.label.color="black",
     vertex.frame.color="transparent"
)

text(0.5,0,"Airport network",col="black", cex=1.5)
legend(x=0.3, y=-0.1, 
       legend=paste("Volume Level:", levels(as.factor(cities_matrix$importance)), sep=""), 
       col = coul , 
       bty = "n", pch=20 , pt.cex = 2, cex = 1,
       text.col="black" , horiz = F)
dev.off()


edges_sub <- edges1[edges1$importance>=2,c("from","to","JAN")]  %>% drop_na() %>%
  rename("weight"="JAN")
nodes_sub <- nodes %>% filter(id %in% edges_sub$from | id %in% edges_sub$to)
routes_network <- network(edges_sub, vertex.attr = nodes_sub[,-2], matrix.type = "edgelist", ignore.eval = FALSE)
plot(routes_network, vertex.cex = 1)

routes_igraph <- graph_from_data_frame(d = edges_sub, vertices = nodes_sub, directed = TRUE)
plot(routes_igraph, edge.arrow.size = 0.2)

library(ggraph)
routes_igraph_tidy <- as_tbl_graph(routes_igraph)
routes_igraph_tidy %>% 
  activate(edges) %>% 
  arrange(desc(weight))

ggraph(routes_igraph_tidy, layout = "graphopt") + 
  geom_node_point() +
  geom_edge_link(aes(width = weight), alpha = 0.8) + 
  scale_edge_width(range = c(0.2, 2)) +
  geom_node_text(aes(label = label), repel = TRUE) +
  labs(edge_width = "Letters") +
  theme_graph()

library(visNetwork)
edges_sub <- edges1[edges1$importance>=4,c("from","to","JAN","importance")] %>% drop_na() %>%
  rename("weight"="JAN")
nodes_sub <- nodes %>% filter(id %in% edges_sub$from | id %in% edges_sub$to)

edges_sub <- mutate(edges_sub, width = importance/2 + 1)
visNetwork(nodes_sub, edges_sub) %>% 
  visIgraphLayout(layout = "layout_with_fr") %>% 
  visEdges(arrows = "middle")

############

#Graphs used for network plots

lvls <- levels(cut(log(df_wider3$annual),breaks=seq(min(log(df_wider3$annual)-0.01,na.rm=T),max(log(df_wider3$annual)+0.01,na.rm=T),length=8+1)))
exp(parse_number(lvls))

network <- function(start=1,end=6,min_corr=0.9995, min_importance=6, min_pop=1e6){

  month <- c("January","February","March","April","May","June")  

  nodes <- read_tsv("DOT/NODES_COORDINATES.tsv",
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

  nodes_sig <-  labels %>% left_join(cities_matrix,by="label") %>% left_join(colors,by="importance") %>% select(id,label,importance, color) %>%
    mutate(title=label)


  nodes_unique <-  data.frame(label = as.character(1:8),
                            shape = c( "circle"), color = c("pink","purple","blue","aquamarine","green","#e6e600","orange","red"), City = 1:8, size=50) %>%
  filter(City >= upd_min_importance)  %>%
    mutate(label = (8-upd_min_importance+1):1) %>%
    arrange(label) %>%
    mutate(label = paste0("Air Volume Level: ",label))

  new_g <- graph_from_data_frame(d=only_sig,vertices=nodes_sig,directed= F)

  graph <- visNetwork(nodes_sig, only_sig,main="Patterns of Monthly Changes in Air Passengers across Top Airport Cities",
                    submain=list(text=paste0("Network constructed based on high correlations of monthly changes in air traffic of cities from ",
                                             month[start]," to ",month[end]," 2020.")),
                    footer = list(text=paste0("Only includes cities with a minimum 2020 air passenger traffic volume of: ", min_pop))) %>% 
  visIgraphLayout(layout = "layout_nicely",smooth=T,randomSeed=10) %>% 
  visEdges(arrows = "middle") %>%
  addFontAwesome() %>%
  visNodes(color=list(border="black")) %>%
  visLegend(
    #main =list(text="City Air Traffic Level",
     #                  style='font-family:Georgia;font-weight:bold;font-size:20px;text-align:center;'),
                       addNodes=nodes_unique,useGroups = F,ncol=1, position ="left",stepY = 150, zoom=F) %>%
  visOptions(highlightNearest = list(enabled = T, hover = T), nodesIdSelection = list(main="Select by city"))

  #visExport(graph,type="html",name=paste0("network_cities_",month[start],"_",month[end],".html"))
  
  return(graph)
  
}
min_corr1 <- .999
network(1,3,min_corr1,4)
network(4,6,min_corr1,4)

############ BELOW CODE NOT USED###################################

airlines <- df %>%
  filter(!DEST_STATE_ABR %in% c("AK","HI","PR","VI","TT")) %>%
  filter(!ORIGIN_STATE_ABR %in% c("AK","HI","PR","VI","TT")) %>%
  select(PASSENGERS,UNIQUE_CARRIER,UNIQUE_CARRIER_NAME,MONTH) %>%
  filter(PASSENGERS > 25) %>%
  group_by(UNIQUE_CARRIER, MONTH) %>%
  mutate(volume = sum(PASSENGERS)) %>%
  ungroup() %>%
  drop_na(volume) %>%
  distinct(UNIQUE_CARRIER, MONTH,.keep_all=T)%>%
  mutate(MONTH = as.factor(MONTH)) %>%
  select(-PASSENGERS)

levels(airlines$MONTH) <- c("JAN","FEB","MAR","APR","MAY","JUN","JUL","AUG","SEP","OCT","NOV","DEC")
  

airlines <- airlines %>% pivot_wider(names_from = MONTH,values_from=volume)
#airlines[is.na(airlines)] <- 0
airlines <- airlines %>%
  drop_na(JAN:JUN) %>%
  mutate(annual =  rowSums(across(where(is.numeric)),na.rm=T)) %>%
  mutate(importance = cut(log(annual),breaks=seq(min(log(annual),na.rm=T)-1,max(log(annual),na.rm=T)+1,length=6+1))) %>%
  mutate(importance= factor(importance) %>% as.numeric()) %>%
  select(UNIQUE_CARRIER,UNIQUE_CARRIER_NAME, importance, annual,DEC:NOV) 



airlines_growth <- airlines %>%
  pivot_longer(DEC:NOV, names_to="MONTH",values_to="flights") %>%
  group_by(UNIQUE_CARRIER) %>%
  mutate(pct.chg = 100*(flights - lag(flights))/lag(flights)) %>%
  ungroup() %>%
  drop_na(pct.chg) %>%
  select(-annual,-flights,-UNIQUE_CARRIER) %>%
  pivot_wider(names_from=MONTH,values_from=pct.chg) 

airlines_matrix <- airlines_growth %>% filter(importance>=1) %>% select(UNIQUE_CARRIER_NAME,importance, JAN:JUN) %>% drop_na(JAN:JUN)
airlines_corr <- cor((t(airlines_matrix[,3:8])))

network_map <- function(corr,corr_min){

corr[corr<corr_min] <- 0
colnames(corr) <- airlines_matrix$UNIQUE_CARRIER_NAME

network <- graph_from_adjacency_matrix(corr, weighted=T, mode="undirected", diag=F)
coul <- rev(brewer.pal(nlevels(as.factor(airlines_matrix$importance)), "RdYlBu"))

# Map the color to cylinders
my_color <- coul[as.numeric(as.factor(airlines_matrix$importance))]

# plot
#png("correlation_network_perc_decr.png", width = 350, height = "350")
par(bg="white", mar=c(0,0,0,0))
set.seed(4)
gr <- plot(network, 
     vertex.size=airlines_matrix$importance*2.5,
     vertex.color=my_color, 
     vertex.label.cex=0.7,
     vertex.label.color="black",
     vertex.frame.color="transparent"
)
return(gr)
}
q1 <- cor((t(airlines_matrix[,3:5])))
q2 <- cor((t(airlines_matrix[,6:8])))
network_map(q1,0.95)
network_map(q2,0.95)


text(0.5,0,"Airport network",col="black", cex=1.5)
legend(x=0.3, y=-0.1, 
       legend=paste("Volume Level:", levels(as.factor(cities_matrix$importance)), sep=""), 
       col = coul , 
       bty = "n", pch=20 , pt.cex = 2, cex = 1,
       text.col="black" , horiz = F)
#dev.off()

network_pol <- function(corr,size1=3,title1){

cor_g <- graph_from_adjacency_matrix(corr, mode='undirected', weighted = 'correlation')
cor_edge_list <- as_data_frame(cor_g, 'edges')
only_sig <- cor_edge_list[abs(cor_edge_list$correlation) > .995 & abs(cor_edge_list$correlation) != 1, ]
new_g <- graph_from_data_frame(only_sig, F)

color <- tibble(value = c(1:6)*size1, color = c("purple","blue","green","#e6e600","orange","red"))

str_split_dba <- function(x){
  return((strsplit(x,"dba") %>% unlist())[1])
}
str_split_aka <- function(x){
  return((strsplit(x,"Aka") %>% unlist())[1])
}
names <- lapply(strsplit(airlines_growth$UNIQUE_CARRIER_NAME, "d/b/a"),str_split) %>% lapply(str_split_aka) %>% as.vector()

nodes_sig <- tibble(id=1:31,label=names, value = airlines_growth$importance*size1) %>%
  left_join(color,by="value")

nodes_unique <-  data.frame(label = as.character(1:6),
                            shape = c( "circle"), color = c("purple","blue","green","#e6e600","orange","red"),
                            title = paste0("Airline Volume Size",title1), id = 1:6)

visNetwork(nodes_sig, only_sig) %>% 
  visIgraphLayout(layout = "layout_nicely",smooth=T,randomSeed=5) %>% 
  visEdges(arrows = "middle") %>%
  visNodes(color=list(border="black")) %>%
#  visGroups(groupname = "1", color = "purple") %>%
#  visGroups(groupname = "2", color = "blue") %>%
#  visGroups(groupname = "3", color = "green") %>%
#  visGroups(groupname = "4", color = "#e6e600") %>%
#  visGroups(groupname = "5", color = "orange") %>%
#  visGroups(groupname = "6", color = "red") %>%
  visLegend(main = paste0("Airline Passenger Volume Magnitude in ",title1),addNodes=nodes_unique,useGroups = F,ncol=2) %>%
  visOptions(highlightNearest = list(enabled = T, hover = T), 
             nodesIdSelection = T)
}

network_pol(q1,title1="Q1 2020")
network_pol(q2, title1="Q2 2020")


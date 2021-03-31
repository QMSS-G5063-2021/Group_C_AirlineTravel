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

register_google(key = Sys.getenv('GOOGLE_API'))

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
         DEST_CITY_NAME,DEST_STATE_ABR, MONTH) %>%
  group_by(ORIGIN_CITY_NAME, DEST_CITY_NAME,MONTH) %>%
  mutate(volume = sum(PASSENGERS,na.rm=T)) %>%
  ungroup() %>%
  select(-PASSENGERS) %>%
  distinct(ORIGIN_CITY_NAME,DEST_CITY_NAME,MONTH,.keep_all=T) %>%
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

colnames(df_wider)[5:16] <- c("DEC","JAN","FEB","MAR","APR","MAY","JUN","JUL","AUG","SEP","OCT","NOV")

df_wider <- df_wider %>%
  mutate(annual = rowSums(across(where(is.numeric)))) %>%
  filter(annual >500) %>%
  mutate(importance = cut(annual,breaks=seq(500,max(annual,na.rm=T),length=6+1))) %>%
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

cities1 <- unique(c(df_wider$ORIGIN_CITY_NAME,df_wider$DEST_CITY_NAME))
###############Nodes

node_vol <- matrix(NA,nrow=length(cities1),ncol=13)
node_vol[,1] <- cities1

k <- 1
for(i in node_vol[,1]){
  for(j in c(12,1:11)){
  node_vol[k,j+1] <- df_sub %>% filter(MONTH==j) %>%
    filter(ORIGIN_CITY_NAME==i | DEST_CITY_NAME==i) %>%
    pull(volume) %>% sum()

  }
  k <- k+1
}
colnames(node_vol) <- c("City","JAN","FEB","MAR","APR","MAY","JUN","JUL","AUG","SEP","OCT","NOV","DEC")

df_wider2 <- as_tibble(node_vol) %>% select(City,DEC,JAN:NOV)
df_wider2[,-1] <- sapply(df_wider2[, -1], as.numeric)

df_wider2 <- df_wider2 %>%
  mutate(annual =  rowSums(across(where(is.numeric)))) %>%
  mutate(importance = cut(annual,breaks=seq(min(annual,na.rm=T),max(annual,na.rm=T),length=6+1))) %>%
  mutate(importance= factor(importance) %>% as.numeric()) 

df_wider4 <- df_wider2

df_wider4 <- df_wider4 %>%
  pivot_longer(DEC:NOV,names_to="MONTH",values_to="VOL") %>%
  group_by(City) %>%
  mutate(pct.chg = 100*(VOL - lag(VOL))/lag(VOL)) %>%
  mutate(pct.chg = ifelse(pct.chg>75,75,pct.chg)) %>%
  mutate(pct.chg = ifelse(pct.chg< -75, -75, pct.chg)) %>%
  select(-VOL) %>%
  pivot_wider(names_from=MONTH,values_from=pct.chg) %>%
  select(-DEC) %>%
  drop_na()

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
edges_sf$local[edges_sf$dist>median(edges_sf$dist)] <- 0


# Establish network
net <- sfnetwork(nodes_sf, edges_sf, directed = TRUE,)
return(net)
}

net_count <- network_connection(df_wider,df_wider2)
net_perc <- network_connection(df_wider3,df_wider4)

###################### Initial graph

states_sf <- get_urbn_map("states", sf = TRUE)%>%
  filter(!state_name %in% c("Hawaii","Alaska"))


par(mfrow=c(6,2))
for (i in 1:6){
graph <- states_sf %>% 
  ggplot(aes()) +
  geom_sf(fill = "gray", color = "#ffffff")+
  geom_sf(data=activate(net_perc,"edges") %>% st_as_sf() %>%
            select(from,to,importance,local,MONTH,value)%>% filter(MONTH==i) %>%arrange(desc(value)),
          aes(color=value,alpha=importance,size=importance)) +
  geom_sf(data=activate(net_perc,"nodes") %>% st_as_sf(),
          aes(size=importance+.5),colour="black")+
  geom_sf(data=activate(net_perc,"nodes") %>% st_as_sf(),
          aes(size=importance),colour="white")+
 scale_colour_viridis_c("% Change in Monthly Passenger Volume",option = 'plasma', direction=-1) +
guides(size=guide_legend(title=" Annual Traffic Level"),
       alpha=F
       )+
  facet_wrap(~local)+
  theme_map()+
  theme(legend.position = "bottom")
ggsave(paste0(i,".png"), graph, width = 12, height = 6, dpi = 500)
}


states_sf %>% 
  ggplot(aes()) +
  geom_sf(fill = "grey", color = "#ffffff")+
  geom_sf(data=activate(net,"edges") %>% st_as_sf() %>%
            select(from,to,importance,local) %>% filter(local==1) %>% arrange(importance),
          aes(alpha=importance)) +
  geom_sf(data=activate(net,"nodes") %>% st_as_sf() %>% select(JAN,importance),
          aes(color=JAN,size=importance),size=1)+
  scale_colour_viridis_c(option = 'inferno') 

#################################

nodes <- read_tsv("DOT/NODES_COORDINATES.tsv",
                  col_types = list(col_number(),
                                   col_character(),
                                   col_number(),
                                   col_number()))

nodes <- nodes %>% filter(nodes$label %in% df_wider$ORIGIN_CITY_NAME | nodes$label %in% df_wider$DEST_CITY_NAME) %>%
  mutate(id = 1:n())

nodes1 <- nodes %>% left_join(df_wider2,by=c("label"="City"))


edges1 <- df_wider  %>% 
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

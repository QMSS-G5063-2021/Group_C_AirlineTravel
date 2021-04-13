library(tidyverse)
#library(ggmap)
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

register_google(key = Sys.getenv('GOOGLE_API'))

# Read monthly csv files and focus on non-zero passengers flight
df <- data.frame()
for (file in list.files(path = '~/Documents/GitHub/Group_C_AirlineTravel/SpatialNetworks',
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
  
  nodes <- read_tsv("~/Documents/GitHub/Group_C_AirlineTravel/SpatialNetworks/NODES_COORDINATES.tsv",
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
  
  title <- paste0("Monthly Change in Domestic Air Traffic Routes in ",month[i]," 2020")
  graph <- states_sf %>% 
    ggplot(aes()) +
    geom_sf(fill = "black", color = "#ffffff")+
    geom_sf(data=activate(net_perc,"edges") %>% st_as_sf() %>%
              select(from,to,importance,local,MONTH,value)%>% filter(MONTH==i) %>%arrange(desc(value)),
            aes(color=value,alpha=importance,size=importance/4)) +
    geom_sf(data=activate(net_perc,"nodes") %>% st_as_sf(),
            aes(size=ifelse(importance>=6,importance/4,0.15)+.1),colour="black")+
    geom_sf(data=activate(net_perc,"nodes") %>% st_as_sf(),
            aes(size=ifelse(importance>=6,importance/4,0.15)),colour="white")+
    scale_colour_viridis_c("% Change in Monthly Passenger Volume",option = 'plasma', direction=-1, limits=c(-100,100)) +
    scale_size_continuous(range = c(0.25,2))+
    guides(alpha=F, size=F)+
    facet_wrap(~local, nrow=2)+
    theme_map()+
    theme(legend.position = "bottom")+
    ggtitle(title)+
    labs(subtitle="Growth rates are capped at -100% and 100%",
         caption="Cities and routes are sized by traffic volume.")
  
  ggsave(paste0(month[i],"_4.png"), graph, width = 12, height = 6, dpi = 1000)
}


#Graphs used for network plots

lvls <- levels(cut(log(df_wider3$annual),breaks=seq(min(log(df_wider3$annual)-0.01,na.rm=T),max(log(df_wider3$annual)+0.01,na.rm=T),length=8+1)))
exp(parse_number(lvls))

network <- function(start=1,end=6,min_corr=0.9995, min_importance=6, min_pop=1e6){
  
  month <- c("January","February","March","April","May","June")  
  
  nodes <- read_tsv("~/Documents/GitHub/Group_C_AirlineTravel/SpatialNetworks/NODES_COORDINATES.tsv",
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
  
  graph <- visNetwork(nodes_sig, only_sig,main="Patterns of Monthly Changes in Airline Passenger Volume of Top Airport Cities",
                      submain=list(text=paste0("Network constructed based on high correlations of monthly changes in air traffic of cities from ",
                                               month[start]," to ",month[end]," 2020.")),
                      footer = list(text=paste0("Only includes cities with a minimum 2020 air passenger traffic volume of: ", min_pop))) %>% 
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
network(1,3,min_corr1,4)
network(4,6,min_corr1,4)



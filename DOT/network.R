
library(tidyverse)
library(ggmap)
library(network)
library(urbnmapr)
library(sfnetworks)
library(sf)
library(tidygraph)
library(stplanr)
library(igraph)
library(tidygraph)


register_google(key = "key")

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
df <- df %>% arrange(YEAR,MONTH) %>% filter(PASSENGERS!=0)


# Subset for now
df_sub <- df %>% filter(MONTH==11) %>% filter(!DEST_STATE_ABR %in% c("AK","HI","PR","VI","TT")) %>%
  filter(!ORIGIN_STATE_ABR %in% c("AK","HI","PR","VI","TT"))

# Geocode cities (already done, no need to repeat unless non-continental states/territories wanted)
sources <- df_sub %>%
  distinct(ORIGIN_CITY_NAME) %>%
  rename(label = ORIGIN_CITY_NAME)

destinations <- df_sub %>%
  distinct(DEST_CITY_NAME) %>%
  rename(label = DEST_CITY_NAME)

nodes <- full_join(sources, destinations, by = "label")
nodes <- nodes %>% rowid_to_column("id")

nodes_ggmap <- geocode(location = nodes$label, output = "latlon", source = "google")
nodes <- nodes %>% mutate(lon = nodes_ggmap$lon) %>%
  mutate(lat = nodes_ggmap$lat)
nodes %>% write_tsv("DOT/NODES_COORDINATES.tsv")

# Create network's nodes
nodes <- read_tsv("DOT/NODES_COORDINATES.tsv",
                  col_types = list(col_number(),
                                   col_character(),
                                   col_number(),
                                   col_number()))
  

sp <- SpatialPoints(nodes[,3:4])
nodes_sf <- st_as_sf(sp)
nodes_sf <- st_set_crs(nodes_sf, 4326)


#Create network's edges

per_route <- df_sub %>%  
  group_by(ORIGIN_CITY_NAME, DEST_CITY_NAME) %>%
  summarise(weight = sum(PASSENGERS)) %>% 
  ungroup()

edges <- per_route %>% 
  left_join(nodes, by = c("ORIGIN_CITY_NAME" = "label")) %>% 
  rename(from = id)

edges <- edges %>% 
  left_join(nodes, by = c("DEST_CITY_NAME" = "label")) %>% 
  rename(to = id)

edges1 <- edges %>%
  select(-label) %>%
  left_join(nodes,by = c("from" = "id")) %>%
  rename(from_lon = lon) %>% rename(from_lat = lat) %>%
  left_join(nodes,by = c("to" = "id")) %>%
  rename(to_lon = lon) %>% rename(to_lat = lat) %>%
  select(-starts_with("label"))

st_segment <- function(r){st_linestring(t(matrix(unlist(r), 2, 2)))}

edges1$geom <- st_sfc(sapply(1:nrow(edges1), 
                        function(i){st_segment(edges1[i,4:7])},
                        simplify=FALSE))
edges_sf <- st_sf(edges1)
edges_sf <- st_set_crs(edges_sf, 4326)


# Establish network
net <- sfnetwork(nodes_sf, edges_sf, directed = TRUE)

# Initial graph
states_sf <- get_urbn_map("states", sf = TRUE)%>%
  filter(!state_name %in% c("Hawaii","Alaska"))

states_sf %>% 
  ggplot(aes()) +
  geom_sf(fill = "grey", color = "#ffffff")+
  geom_sf(data=activate(net,"edges") %>% st_as_sf() %>%
            filter(weight>5000),
          aes(alpha=weight*0.001)) +
  geom_sf(data=activate(net,"nodes") %>% st_as_sf(),
          color="black",size=1)


# Betweeness Centrality
net_c = net %>%
  activate("nodes") %>%
  mutate(bc = centrality_betweenness())

states_sf %>% 
ggplot(aes())+
  geom_sf(fill = "grey", color = "#ffffff")+
  geom_sf(data = activate(net_c, "edges") %>% st_as_sf() %>%
            filter(weight>5000),
          aes(alpha=weight*0.001)) + 
  geom_sf(data = activate(net_c, "nodes") %>% st_as_sf(),
          aes(col = bc, size = bc))+
  scale_color_continuous(limits=c(0, 30000),
                        breaks=seq(0, 30000, by=10000))+
  guides(color = guide_legend(title="Betweenness Centrality"),
         alpha = F,
         size = F)+
  scale_size_continuous(limits=c(0, 30000),
                        breaks=seq(0, 30000, by=10000))
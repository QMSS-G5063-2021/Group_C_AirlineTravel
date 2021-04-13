

states_sf <- get_urbn_map("states", sf = TRUE)%>%
  filter(!state_name %in% c("Hawaii","Alaska"))

month <- c("January","February","March","April","May","June")
month_abb <- c("JAN","FEB","MAR","APR","MAY","JUN")

j <- "January"

i <- match("January",month) # for January
  
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



############ RANDOM SCRATCH CODE. NOTHING RELEVANT ########################

library(tidyverse)

planes <- read_csv("airlines.csv") %>%
  filter(YEAR == 2020)

#planes <- planes %>% 
  #mutate(MONTH = 
           #case_when(
             #MONTH == 1 ~ "January",
             #MONTH == 2 ~ "February",
             #MONTH == 3 ~ "March",
             #MONTH == 4 ~ "April",
             #MONTH == 5 ~ "May",
             #MONTH == 6 ~ "June",
             #MONTH == 7 ~ "July",
             #MONTH == 8 ~ "August",
             #MONTH == 9 ~ "September",
             #MONTH == 10 ~ "October",
             #MONTH == 11 ~ "November",
             #MONTH == 12 ~ "December"))

#planes$MONTH <- factor(planes$MONTH, levels = c("January", "February", "March", "April", "May", "June", "July", "August",
                                                #"September", "October", "November", "December"))

planesTop10OriNum <- planes %>%
  group_by(MONTH, ORIGIN) %>%
  summarise(timesOri = n()) %>%
  rename(AIRPORT = ORIGIN)

planesTop10DestNum <- planes %>%
  group_by(MONTH, DEST) %>%
  summarise(timesDest = n()) %>%
  rename(AIRPORT = DEST)

planesTop10OriDest <- left_join(planesTop10OriNum, planesTop10DestNum, by = c("AIRPORT", "MONTH"))

planesTop10OriDest <- planesTop10OriDest %>%
  mutate(ACTIVITY = timesOri + timesDest)

planesTop10 <- planesTop10OriDest %>%
  group_by(AIRPORT) %>%
  summarise(ACTIVITY = sum(ACTIVITY))

Act <- arrange(planesTop10, desc(ACTIVITY))[1:10,1]

planesTop10Act <- planes %>%
  filter(ORIGIN %in% Act$AIRPORT) %>%
  group_by(MONTH, ORIGIN) %>%
  summarise(PASSENGERS = sum(PASSENGERS)) %>%
  rename(AIRPORT = ORIGIN)

Airline <- sort(table(planes$UNIQUE_CARRIER_NAME), decreasing = T)[1:10]

planesTop10Airlines <- planes %>%
  filter(UNIQUE_CARRIER_NAME %in% names(Airline)) %>%
  group_by(MONTH, UNIQUE_CARRIER_NAME) %>%
  summarise(PASSENGERS = sum(PASSENGERS)) %>%
  rename(AIRLINE = UNIQUE_CARRIER_NAME)

#####################################################################

df %>% # avg Passengers by month
  group_by(MONTH) %>%
  summarise(avgP = mean(PASSENGERS)) %>%
  ggplot(aes(MONTH, avgP)) +
  geom_col() +
  theme(axis.text.x = element_text(angle = 40, vjust = .75))

df %>% # total passengers by month
  filter(ORIGIN == "JFK") %>% #USE FILTER FUNCTION TO SUBSET IN INTERACTIVE?
  group_by(MONTH) %>%
  summarise(totalP = sum(PASSENGERS)) %>%
  ggplot(aes(MONTH, totalP)) +
  geom_col() +
  theme(axis.text.x = element_text(angle = 40, vjust = .75))

df %>% # total distance by month
  group_by(MONTH) %>%
  summarise(totalD = sum(DISTANCE)) %>%
  ggplot(aes(MONTH, totalD)) +
  geom_col() +
  theme(axis.text.x = element_text(angle = 40, vjust = .75))

#####################################################################



title <- paste0("Monthly Change in Domestic Air Traffic Routes in ",month[i]," 2020")
test<- activate(net_perc,"edges") %>% st_as_sf() %>%
  select(from,to,importance,local,MONTH,value)%>% filter(MONTH=="January") %>%arrange(desc(value))

states_sf %>% 
  ggplot(aes()) +
  geom_sf(fill = "black", color = "#ffffff")+
  geom_sf(data=activate(net_perc,"edges") %>% st_as_sf() %>%
            select(from,to,importance,local,MONTH,value)%>% filter(MONTH=="January") %>%arrange(desc(value)),
          aes(color=value,alpha=importance,size=importance/4)) +
  geom_sf(data=activate(net_perc,"nodes") %>% st_as_sf(),
          aes(size=ifelse(importance>=6,importance/4,0.15)+.1),colour="black")+
  geom_sf(data=activate(net_perc,"nodes") %>% st_as_sf(),
          aes(size=ifelse(importance>=6,importance/4,0.15)),colour="white")+
  scale_colour_viridis_c("% Change in Monthly Passenger Volume",option = 'plasma', direction=-1, limits=c(-100,100)) +
  scale_size_continuous(range = c(0.25,2))+
  guides(alpha=F, size=F)+
  #facet_wrap(~local, nrow=2)+
  theme_map()+
  theme(legend.position = "bottom")+
  ggtitle("Monthly Change in Domestic Air Traffic Routes in January 2020")+
  labs(subtitle="Growth rates are capped at -100% and 100%",
       caption="Cities and routes are sized by traffic volume.")

# states_sf, net_perc, 

load("states_sf.RData")
load("net_perc.RData")

i <- 1 # for January

# df_wider3, df_wider4 

# DEFINE UI
ui2 <- fluidPage(
  
  # Navbar structure for UI
  navbarPage("Pandemic Air Traffic", theme = shinytheme("lumen"),
             tabPanel("Passengers", fluid = TRUE,
                      selectInput(inputId = "State",
                                  label = "Choose a State", 
                                  choices = states),
                      plotOutput("hist")
             ), # tabPanel
             tabPanel("Flights", fluid = TRUE,
                      selectInput(inputId = "State2",
                                  label = "Choose a State",
                                  choices = states),
                      plotOutput("hist2")
             ), #tabPanel2
             tabPanel("Networks", fluid = TRUE,
                      selectInput(inputId = "Month1",
                                  label = "Choose a Month",
                                  choices = test),
                      plotOutput("maps")
             ), #tabPanel3
             tabPanel("Text", fluid = TRUE,
                      plotOutput("text")
             ) #TabPanel4
  ) # navbarPage
)# fluid page
  

# What is the most efficient way to store shiny UI, Server, data objects? 
# could look into SOURCE FILE


#adding styling with CSS
#tags$ part in fluid page
# "takes a little bit of time"

# better way...in lecture piece. couple of themes you could choose for overall

# fluidRow() and column, divide page into rows and columns. think of a table or grid as a visual analogue

# wellPanel() groups together inputs
# MANY MORE PANELS. see slides
# actually using sidebarPanel right now. look into other options
# same with tabsetPanel()

# flex dashboard and shiny dashboard packages

# look at flights shiny from ^ dashboard slide

# bootstrap project has free themes

# for spatial networks, can use fixedPanel, see his rmd from part 2

# figure out if visNetwork graphs go in shiny

# highchartOutput to make looks cooler slide 55 or 56?

# for Ian's airlines, can make it reacitve using filter(c(input$airlines)) (?)
  # have to figure out if you can select multiple


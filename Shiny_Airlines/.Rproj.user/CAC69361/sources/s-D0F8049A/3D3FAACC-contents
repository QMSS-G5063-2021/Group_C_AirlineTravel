
source('resources.R')

######################################## DEFINE UI ######################################## 

ui7 <- fluidPage(
  theme = shinytheme("sandstone"),
  #theme = "bootstrap.min.css", # not sure how to get this to work...
  
  navbarPage(
    title = "Airline Traffic",
    windowTitle = "Airline Traffic",
    
    # About Page -------------------------------------------------------------------------------------------
    tabPanel(
      "About",
      mainPanel(
        h4("by Andrew Lai, Ian Lightfoot, Charlie Levitz, Jason Mares"),
        p("We sought to explore how air traffic and the airline industry was impacted by the coronavirus...")
      )
    ),
    
    # Networks -------------------------------------------------------------------------------------------
    tabPanel(
      "Networks",
      tabsetPanel(
        type = "tabs",
        tabPanel(
          "Spatial Networks", fluid = TRUE,
          fluidRow(
            column(12, align = "center",
                   h4("Monthly Change in Domestic Air Traffic Routes in 2020"),
                   h5("Growth rates are capped at -100% and 100%"),
                   plotOutput("maps", height = "450px"),
                   selectInput(inputId = "Month1",
                               label = "Choose a Month",
                               choices = month)
            )
          ),
          fluidRow(
            column(12, align = "center",
                   p("This is text explaining the network to the left lets see how this will look if I type a lot of text I hope this doesn't look really bad i guess i'll find out soon aldkfja;dsf  askdfj ;askfd j askdf ja;lkfdj"
                   )
            )
          )
        ),
        tabPanel(
          "Interactive Networks", fluid = TRUE,
          fluidRow(
            column(12, #align = "center",
                   h4("Patterns of Monthly Changes in Airline Passenger Volume of Top Airport Cities"),
                   h5("Network constructed based on high correlations of monthly changes in air traffic of cities between months"),
                   h6("Try Zooming In!"),
                   visNetworkOutput("maps2", height = "450px"),
                   fluidRow(
                     column(6, align = "center",
                            selectInput(inputId = "Month2",
                                        label = "Choose Starting Month",
                                        choices = startingMonth)
                     ),
                     column(6, align = "center",
                            selectInput(inputId = "Month3",
                                        label = "Choose Ending Month",
                                        choices = endingMonth)
                     )
                   )
            )
          )
        )
      )
    ),
    
    # Static Graphs -------------------------------------------------------------------------------------------
    tabPanel(
      "Static",
      tabsetPanel(
        type = "tabs",
        tabPanel(
          "Flights", fluid = TRUE,
          fluidRow(
            column(12, align = "center",
                   h4("Passengers Per Month by Airline"),
                   plotOutput("hist2"),
                   p("This is test text where we will talk about a decrease in the number of passengers across all American airlines yada yada yada a;kdfj;asjkdf")
                   )
            )
          ),
        tabPanel(
          "Cities",
          sidebarPanel(
            sliderInput("year", 
                        label = "Year",
                        min = min(years), 
                        max = max(years), 
                        step = 1,
                        sep = "",
                        value = range(years)
            ),
            selectInput("Origin_City", 
                        label = "Origin City",
                        choices = cities1
            ),
            selectInput("Destination_City",
                        label = "Destination City",
                        choices = cities2
            )
          ),
          mainPanel(
            h4("Number of Passenger by Month Between Destinations"),
            plotOutput("yearCities", height = "500px")
          )
        )
      )
    ),
    
    # Text Analysis -------------------------------------------------------------------------------------------
    tabPanel("Text")
  )
)

######################################## DEFINE UI ######################################## 

server3 <- function(input, output) {
  
  citiesData <- reactive({
    if(input$Origin_City == "All" & input$Destination_City == "All") {
      planes_all %>%
        filter(YEAR >= input$year[1]) %>%
        filter(YEAR <= input$year[2]) %>%
        group_by(MONTH) %>%
        summarise(avg = mean(PASSENGERS))
    } else if(input$Origin_City == "All" & input$Destination_City != "All") {
      planes_all %>%
        filter(DEST_CITY_NAME == input$Destination_City) %>%
        filter(YEAR >= input$year[1]) %>%
        filter(YEAR <= input$year[2]) %>%
        group_by(MONTH) %>%
        summarise(avg = mean(PASSENGERS))
    } else if (input$Origin_City != "All" & input$Destination_City == "All") {
      planes_all %>%
        filter(ORIGIN_CITY_NAME == input$Origin_City) %>%
        filter(YEAR >= input$year[1]) %>%
        filter(YEAR <= input$year[2]) %>%
        group_by(MONTH) %>%
        summarise(avg = mean(PASSENGERS))
    } else {
      planes_all %>%
        filter(ORIGIN_CITY_NAME == input$Origin_City) %>%
        filter(DEST_CITY_NAME == input$Destination_City) %>%
        filter(YEAR >= input$year[1]) %>%
        filter(YEAR <= input$year[2]) %>%
        group_by(MONTH) %>%
        summarise(avg = mean(PASSENGERS))
      }
    })
  
  data3 <- reactive({
    i <- match(input$Month1, month)
  })
  
  title <- reactive({
    paste0("Monthly Change in Domestic Air Traffic Routes in ", input$Month1," 2020")
  })
  
  data4 <- reactive({
    i <- match(input$Month2, month)
  })
  
  data5 <- reactive({
    i <- match(input$Month3, month)
  })
  
  output$maps2 <- renderVisNetwork({
    network(data4(),data5(),min_corr1,4)
  })
  
  output$maps3 <- renderVisNetwork({
    network(1,6,min_corr1,4)
  })
  output$hist2 <- renderPlot({
    ggplot(planesTop10Airlines, aes(x = MONTH, y = PASSENGERS/1000000, fill = reorder(AIRLINE, -PASSENGERS), color = reorder(AIRLINE, -PASSENGERS))) +
      geom_line() +
      geom_point(shape=21, color="black", size=3) +
      xlab("Month") +
      ylab("Number of Passengers (in Millions)") +
      scale_x_continuous(breaks = c(1,2,3,4,5,6,7,8,9,10,11,12),
                         labels = c("January","February",
                                    "March","April","May","June",
                                    "July", "August", "September",
                                    "October", "November", "December")) +
      theme(plot.title = element_text(hjust = 0.5)) +
      theme(plot.title = element_text(face = "bold")) +
      theme(axis.text.x = element_text(angle=45, vjust = 0.5, size = 10)) +
      labs(fill = "Airline", color = "Airline")
  })
  
  output$maps <- renderPlot({
    states_sf %>% 
      ggplot(aes()) +
      geom_sf(fill = "black", color = "#ffffff")+
      geom_sf(data=activate(net_perc,"edges") %>% st_as_sf() %>%
                select(from,to,importance,local,MONTH,value)%>% filter(MONTH==data3()) %>%arrange(desc(value)),
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
      #ggtitle(title())+
      labs(#subtitle="Growth rates are capped at -100% and 100%",
        caption="Cities and routes are sized by traffic volume.")
  })
  
  output$yearCities <- renderPlot({
    citiesData() %>%
      ggplot(aes(x = MONTH, y = avg)) +
      geom_line() +
      geom_point(shape=21, color="black", size=3) +
      xlab("Month") +
      ylab("Number of Passengers (in Millions)") +
      scale_x_continuous(breaks = c(1,2,3,4,5,6,7,8,9,10,11,12),
                         labels = c("January","February",
                                    "March","April","May","June",
                                    "July", "August", "September",
                                    "October", "November", "December")) +
      theme(plot.title = element_text(hjust = 0.5)) +
      theme(plot.title = element_text(face = "bold")) +
      theme(axis.text.x = element_text(angle=45, vjust = 0.5, size = 10))
  })
  
}

shinyApp(ui = ui7, server = server3)

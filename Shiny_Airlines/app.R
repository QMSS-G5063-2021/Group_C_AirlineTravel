
source('resources.R')

######################################## DEFINE UI ######################################## 

ui7 <- fluidPage(
  theme = shinytheme("cerulean"),
  #theme = "bootstrap.min.css", # not sure how to get this to work...
  
  navbarPage(
    title = "Airline Traffic",
    windowTitle = "Airline Traffic",
    
    # About Page -------------------------------------------------------------------------------------------
    tabPanel(
      "About",
      mainPanel(
        h4("by Andrew Lai, Ian Lightfoot, Charlie Levitz, Jason Mares"),
        p("We sought to explore how air traffic and the airline industry was impacted by the coronavirus. To what extent did Americans stop flying during the spring of 2020? And how does this impact people's perception of the airlines and flying in the United States?"
          ),
        p("Airline Data from Bureau of Transportation Statistics: https://www.transtats.bts.gov/Fields.asp?gnoyr_VQ=FIL"
        ),
        p("Twitter data from Twitter API: https://developer.twitter.com/en/docs/twitter-api")
      )
    ),
    
    # Static Graphs -------------------------------------------------------------------------------------------
    tabPanel(
      "Passenger Traffic",
      tabsetPanel(
        type = "tabs",
        tabPanel(
          "Flights", fluid = TRUE,
          fluidRow(
            column(12, align = "center",
                   h4("Passengers Per Month by Airline"),
                   plotOutput("hist2"),
                   p("In March of 2020, Americans stopped flying as frequently as they had been in previous months. And in April, they stopped flying altogether. We see this effect independent of any specific airline. On the next tab, you may explore how this effect changes depending on the origin and destination cities. Notice how the pattern of total monthly passengers changes from month to month before the pandemic, and how the pandemic impacts how we view flight frequency over the last half-decade.")
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
            ),
            p("This graph displays the average passengers per month, and how that average changes depending on which years are being averaged. For example, the graph defaults to averaging from all of 2015-2020. If you adjust the right side of the slider to 2018, then the averages will exclude data from 2019 and 2020.  Pay special attention to the month of April when the year of 2020 is or is not included. And try manipulating the origin and destination cities to see how this effect may differ depending on the airport!"
              )
          ),
          mainPanel(
            h4("Number of Passenger by Month Between Destinations"),
            plotOutput("yearCities", height = "500px")
          )
        )
      )
    ),
    
    # Networks -------------------------------------------------------------------------------------------
    tabPanel(
      "Air Travel Networks",
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
                   p("The graphs above show the month-to-month change in passenger traffic along domestic routes throughout the continental United States. Maps are separated by the distances of the routes for the readability of the viewer but also to observe whether routes of different lengths were affected the same by the pandemic. Shorter flights may be viewed as safer since passengers do not spend as much time isolated in a cylindrical contained. In addition, long-distance flights are often taken by business passengers thus the changes in passenger traffic along these of flights may be correlated with the decline in employment seen in the U.S. during the first quarter of 2020. "
                     ),
                   p("After the holiday season in December 2019, air traffic volume rebounded to average levels throughout the country. Although the news of coronavirus reached Americans in early 2020, air traffic volume was not terribly impacted. In fact, the monthly changes in air passengers throughout domestic routes from January and February varied throughout the country, with travel decreasing from the end of the holiday season in January. However, March and April saw heavy hits in air traffic due to the nation-wide shutdown of businesses, schools, etc. May and June saw immediate recovery in most places as restrictions were slightly loosened in certain areas of the country. The opening of the country was attributed to shutdown weariness and anxiousness to spend time outside with summer beginning.")
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
    
    # Text Analysis -------------------------------------------------------------------------------------------
    tabPanel(
      "Airline Twitter Analysis",
      tabsetPanel(
        type = "tabs",
        tabPanel(
          "Tweet Sentiment",
          fluidRow(
            column(12, align = "center",
                   h4("Airline Tweets' Sentiment Value Distribution"),
                   plotOutput("sent1"),
                   p("To first understand social media user’s perceptions of major U.S airlines, we first use the AFINN dictionary to apply a polarity score on individual words from each tweet. Then, after categorizing by airline, we visualize a violin plot to look at the polarity distribution by each airline. What is interesting is that American, Delta, Southwest and United have a large distribution of negative polarity scores, and a median score of -1. In comparison, Alaska Air has a polarity score of 1 and a denser volume of positive polarity scores."
                     )
                   )
            )
          ),
        tabPanel(
          "Frequent Words",
          fluidRow(
            column(12, align = "center",
                   h4("Sentiment Value Weighted by Frequency of Words in Tweets"),
                   plotOutput("sent2"),
                   p("To closely examine words associated with each airline, we apply the AFINN dictionary to individual words and multiply each polarity score by the frequency of individual words to observe their overall contribution to the text. Like previously stated, we then categorize by airline and sample the top 5 words by the absolute value of word-contribution. Again, American, Delta, Southwest and United airlines have a largely negative word-contribution. With many words relating to boycotts and racism, this visualization indicates how interconnected social issues and politics have become with major U.S airlines. Government bailouts of the airline industry, Covid-19, and the Capitol riots in early January in conjunction with rising media coverage could be major reasons on why the airline industry has become embedded in political and social issues."
                     )
                   )
            )
          )
        )
      )
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
  
  output$hist2 <- renderPlot({
    planesTop10Airlines %>%
      filter(!AIRLINE == "Federal Express Corporation", !AIRLINE == "United Parcel Service") %>%
      ggplot(aes(x = MONTH, y = PASSENGERS/1000000, fill = reorder(AIRLINE, -PASSENGERS), color = reorder(AIRLINE, -PASSENGERS))) +
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
      geom_sf(fill = "grey36", color = "black")+
      geom_sf(data=activate(net_perc,"edges") %>% st_as_sf() %>%
                select(from,to,importance,local,MONTH,value)%>% filter(MONTH==data3()) %>%arrange(desc(value)),
              aes(color=value,alpha=importance,size=importance/8)) +
      geom_sf(data=activate(net_perc,"nodes") %>% st_as_sf(),
              aes(size=ifelse(importance>=6,(importance/4) + .4,0.2)),colour="black")+
      geom_sf(data=activate(net_perc,"nodes") %>% st_as_sf(),
              aes(size=ifelse(importance>=6,importance/4,0.15)),colour="white")+
      scale_color_gradient2(low="red",mid="white",high="blue",midpoint=0,limits=c(-100,100))+
      #scale_colour_viridis_c("% Change in Monthly Passenger Volume",option = 'plasma', direction=-1, limits=c(-100,100)) +
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
      geom_line(color = "steelblue") +
      geom_point(shape=21, fill="steelblue1", size=3) +
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
  
  output$sent1 <- renderPlot({
    ggplot(valence, aes(x = airline, y = value, color = airline)) + 
      geom_violin( show.legend = FALSE) + 
      geom_boxplot(width=.1) +
      scale_y_continuous(breaks = seq(-5, 5, by = 1)) +
      labs(x = "Airlines", y = "AFINN Values") +
      #ggtitle("Tweets Sentiment Value Distribution By Airlines") +
      theme(plot.title = element_text(vjust=2, hjust = 0.5),
            legend.position =  'none') +
      scale_color_discrete()
  })
  
  output$sent2 <- renderPlot({
    valence %>%
      mutate(contribution = n * value) %>%
      group_by(airline) %>%
      slice_head(n = 5) %>%
      arrange(((contribution))) %>%
      mutate(word = reorder(word, contribution)) %>%
      ggplot(aes(x = contribution, y = reorder(word, contribution), 
                 fill = contribution > 0)) +
      geom_col(show.legend = FALSE) +
      facet_wrap(~airline, ncol = 2, scales = "free") +
      labs(x = "Sentiment Value * Number of Appearances",
           y = 'Top 5 Words From Tweets') +
      #ggtitle("Sentiment Value Weighted by Frequency of Words in Tweets") +
      theme(plot.title = element_text(vjust=2, hjust = 0.5),
            axis.title.x = element_text(vjust = -1),
            axis.title.y = element_text(vjust = 1),
            legend.position =  'none') +
      scale_fill_manual(values = c("firebrick2", "steelblue1"))
    })
  
  output$emo <- renderPlot({
   emotion_plot <- nrc_graph  %>%
      group_by(airline, sentiment) %>%
      summarise(Freq=n(), .groups = 'drop') %>%
      ggplot(aes(Freq, reorder(sentiment, Freq), fill = airline)) +
      geom_col(show.legend = TRUE) +
      labs(x = "Frequency",
           y = 'Emotion Category',
           fill = "Airline") +
      ggtitle("Emotion Category by Frequency of Words in Tweets") +
      theme(plot.title = element_text(vjust=2, hjust = 0.5),
            axis.title.x = element_text(vjust = -1),
            axis.title.y = element_text(vjust = 1))
    #emotion_plot <- ggplotly(emotion_plot, tooltip = c("x"))
    emotion_plot
  })
}

shinyApp(ui = ui7, server = server3)


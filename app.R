#
# Shiny web application for Traffic Forecasting
#

library(shiny)
library(fpp3)
library(imputeTS)
library(arrow)
# Note: run below to make arrow work in Shiny Server
#Sys.setenv(LIBARROW_MINIMAL="false")
#install.packages("arrow")

# Set the system timezone to UTC
Sys.setenv(TZ = "UTC")
# Read dataset
dataset <- read_parquet("data.parquet")
units <- "1 hour"
# Define UI for application
ui <- fluidPage(
  titlePanel("Traffic Forecasting"),
  
  sidebarLayout(
    
    sidebarPanel(
      
      selectInput(
        "level",
        "Select Aggregation Level",
        c("15 minute", "60 minute", "day", "week", "month")),
      
      br(),
      
      selectInput(
        "location",
        "Select Traffic Signal Location",
        unique(dataset$TSSU)
      ),
      
      br(),
      
      dateRangeInput("daterange", "Date Range", start = "2020-01-01", end = "2022-04-01"),
      
      br(),
      
      selectInput(
        "datatype",
        "Choose Traffic Volumes or Travel Times",
        c("Volume", "Travel_Time")
      ),
      
      br(),
      
      sliderInput("steps", "Forecast Steps", 1, 300, 96, step = 1)
    ), 
  
  mainPanel(
    p("This app forecasts traffic volumes and travel times at traffic signals 
       using historical volume and travel time data. The forecasted 
       volumes and travel times can be used by traffic engineers to 
       determine how well traffic signal timing plans will work in 
       the future. The relationship between vehicle volumes and travel
       times can also reveal when a traffic signal system is likely to
       suffer from congestion."),
    tabsetPanel(type="tabs",
                tabPanel("Plot", plotOutput("testplot")),
                tabPanel("Summary", verbatimTextOutput("summary")),
                tabPanel("Table", tableOutput("table"))
                )
    )
  )
)


# Define server logic
server <- function(input, output) {
  # Initial data filtering/processing steps
  r_data <- reactive({
    dataset %>%
      filter(
        TSSU == input$location,
        TimeStamp >= input$daterange[1],
        TimeStamp <= input$daterange[2]
      ) %>%
      select(input$datatype) %>%
      fill_gaps() %>%
      na_locf() %>%
      group_by_key() %>%
      mutate(TimeStamp_level = floor_date(TimeStamp, unit = input$level))
  })
  
  # If the aggregation level is less than 1 day, keep TimeStamp as datetime
  # If >= 1 day, convert TimeStamp to date. Otherwise index_by won't work
  r_data2 <- reactive({
    if (substr(input$level, nchar(input$level) - 5, nchar(input$level)) == "minute")
    {
      r_data() %>% index_by(TimeStamp_level) %>%
        summarise(`value` = sum(.data[[input$datatype]]))
    }
    else
    {
      r_data() %>% index_by(TimeStamp_level = as.Date(TimeStamp_level)) %>%
        summarise(`value` = sum(.data[[input$datatype]]))
    }
  })
  
  # Create a plot
  output$testplot <- renderPlot({
    r_data2() %>%
      model(decomposition_model(STL(`value`, robust = TRUE),
                                NAIVE(season_adjust))) %>%
      forecast(h = input$steps) %>%
      autoplot(r_data2(), level = FALSE)
  })
  
  #Better Plot
  output$testplot2 <- renderPlot({
    forecast <- r_data2() %>%
      model(decomposition_model(STL(`value`, robust = TRUE),
                                NAIVE(season_adjust))) %>%
      forecast(h = input$steps) 
    
    r_data2() %>%
      ggplot(mapping=aes(
        x = TimeStamp_level,
        y = value
        #colour = factor(Phase)
      )) +
      #geom_line() +
      geom_line(forecast)
  })
}

# Run the application
shinyApp(ui = ui, server = server)

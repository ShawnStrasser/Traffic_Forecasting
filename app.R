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
  selectInput(
    "level",
    "Select Aggregation Level",
    c("15 minute", "60 minute", "day", "week", "month")
  ),
  selectInput(
    "location",
    "Select Traffic Signal Location",
    unique(dataset$TSSU)
  ),
  dateRangeInput("daterange", "Date Range", start = "2020-01-01", end = "2022-04-01"),
  selectInput("datatype", "Choose Traffic Volumes or Travel Times", c("Volume", "Travel_Time")),
  sliderInput("steps", "Forecast Steps", 1, 300, 96, step = 1),
  plotOutput("testplot"),
  textOutput("level")
)


# Define server logic
server <- function(input, output) {
  output$testplot <- renderPlot({
    if (substr(input$level, nchar(input$level) - 5, nchar(input$level)) == "minute")
    {
      data <- dataset %>%
        filter(
          TSSU == input$location,
          TimeStamp >= input$daterange[1],
          TimeStamp <= input$daterange[2]
        ) %>%
        select(input$datatype) %>%
        fill_gaps() %>%
        na_locf() %>%
        group_by_key() %>%
        mutate(TimeStamp_level = floor_date(TimeStamp, unit = input$level)) %>%
        index_by(TimeStamp_level) %>%
        summarise(`Traffic Counts` = sum(Volume)) 
      data %>%
        model(decomposition_model(
          STL(`Traffic Counts`, robust = TRUE),
          NAIVE(season_adjust)
        )) %>%
        forecast(h = input$steps) %>%
        autoplot(data, level=FALSE)
    }
    else
    {
      data <- dataset %>%
        filter(
          TSSU == input$location,
          TimeStamp >= input$daterange[1],
          TimeStamp <= input$daterange[2]
        ) %>%
        select(input$datatype) %>%
        fill_gaps() %>%
        na_locf() %>%
        group_by_key() %>%
        mutate(TimeStamp_level = as.Date(floor_date(TimeStamp, unit = input$level))) %>%
        index_by(TimeStamp_level) %>%
        summarise(`Traffic Counts` = sum(Volume)) 
      data %>%
        model(decomposition_model(
          STL(`Traffic Counts`, robust = TRUE),
          NAIVE(season_adjust)
        )) %>%
        forecast(h = input$steps) %>%
        autoplot(data, level=FALSE)
    }
  })
  
  
  
  
}

# Run the application
shinyApp(ui = ui, server = server)

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
dim <- readxl::read_xlsx("dimension_table.xlsx") %>%
  select(TSSU, Phase, Bearing, "Location Description")

dataset <-
  merge(
    x = read_parquet("data.parquet"),
    y = dim,
    by = c("TSSU", "Phase"),
    all.x = TRUE
  ) %>%
  select(-Phase,-TSSU) %>%
  mutate(Data = "Actual") %>%
  as_tsibble(
    index = TimeStamp,
    key = c(
      "Location Description",
      "Bearing",
      "Data"
    )
  ) %>%
  fill_gaps() %>%
  na_seasplit(algorithm = "locf", find_frequency = TRUE)

# Define UI for application
ui <- fluidPage( 
  theme = shinythemes::shinytheme('superhero'),
  
  titlePanel("Traffic Forecasting"),
  
  sidebarLayout(
    
    sidebarPanel(
      
      selectInput(
        "level",
        "Select Aggregation Level",
        c("15 minute", "60 minute", "day", "week")),
      
      br(),
      
      selectInput(
        "location",
        "Select Traffic Signal Location",
        unique(dataset$"Location Description")
      ),
      
      br(),
      
      dateRangeInput(
        "daterange",
        "Date Range (data avaiable from Jan 2020 through March 2022)",
        start = "2022-01-01",
        end = "2022-04-01"
      ),
      
      br(),
      
      selectInput(
        "datatype",
        "Choose Traffic Volumes or Travel Times",
        c("Volume", "Travel_Time")
      ),
      
      br(),
      
      sliderInput("steps", "Number of Weeks to Forecast", 1, 52, 2, step = 1)
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
                tabPanel("Plot", plotly::plotlyOutput("plot")),
                tabPanel("Summary", verbatimTextOutput("summary")),
                tabPanel("Table", tableOutput("table"))
                )
    )
  )
)


# Define server logic
server <- function(input, output) {
  
  # Reactive Plot Title
  title <- reactive({
    paste(input$datatype, "Forecast for", input$location, sep = " ")
  }) 
  
  # Reactive Y axis label
  y_axis <- reactive({
    if (input$datatype == "Volume")
    {
      paste("Total Vehicles per", input$level, sep = " ")
    }
    else
    {
      "Average Segment Travel Time Minutes"
    }
  })   
  
  # Reactive number of Forecast Steps from user input
  r_steps <- reactive({
    switch(
      input$level,
      "15 minute" = 7 * 24 * 4,
      "60 minute" = 7 * 24,
      "day" = 7,
      "week" = 1
    ) * input$steps
  })
  
  # Reactive Seasonal Periods for Decomposition
  # If aggregation level is less than day, use "day", else use "year"
  r_period <- reactive({
    if (substr(input$level, nchar(input$level) - 5, nchar(input$level)) == "minute")
    {
      "day"
    }
    else
    {
      "year"
    }
  })
  
  
  # Now, process the data
  # Frist filter per user selection
  r_data <- reactive({
    dataset %>%
      filter(
        `Location Description` == input$location,
        TimeStamp >= input$daterange[1],
        TimeStamp <= input$daterange[2]
      ) %>%
      select(input$datatype) %>%
      select(-"Location Description") %>%
      group_by_key() %>%
      mutate(TimeStamp_level = floor_date(TimeStamp, unit = input$level))
  })
  
  # If the aggregation level is less than 1 day, keep TimeStamp as datetime
  # If >= 1 day, convert TimeStamp to date. Otherwise index_by won't work
  r_data2 <- reactive({
    if (substr(input$level, nchar(input$level) - 5, nchar(input$level)) == "minute")
    {
      r_data() %>% index_by(TimeStamp_level)
    }
    else
    {
      r_data() %>% index_by(TimeStamp_level = as.Date(TimeStamp_level))
    }
  })
  
  # Summarize with Sum for Volume, but use Mean for Travel Time
  r_data3 <- reactive({
    if (input$datatype == "Volume")
    {
      r_data2() %>% summarise(`value` = sum(.data[[input$datatype]]))
    }
    else
    {
      r_data2() %>% summarise(`value` = mean(.data[[input$datatype]]))
    }
  })
  
  
  # Create a Plot
  final_plot <- function() {
    r_data3() %>% 
      model(decomposition_model(
      STL(
        `value` ~ season(period = "week") + season(period = r_period()),
        robust = TRUE
      ),
      NAIVE(season_adjust)
    )) %>%
      forecast(h = r_steps()) %>%
      as_tsibble() %>%
      select(-.model) %>%
      select(.mean) %>%
      rename("value" = .mean) %>%
      mutate(Data = "Forecast") %>%
      bind_rows(r_data3()) %>%
      autoplot(value) +
      scale_color_manual(values = c("light blue", "pink", "blue", "red")) +
      labs(title = title(), y = y_axis())
  }
  
  output$plot <- plotly::renderPlotly(final_plot())
  
}

# Run the application
shinyApp(ui = ui, server = server)

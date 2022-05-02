#
# Shiny web application for Traffic Forecasting
#

library(shiny)
library(fpp3)
library(imputeTS)
library(arrow)
library(DT)
library(markdown)
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
  mutate(Type = "Actual") %>%
  as_tsibble(
    index = TimeStamp,
    key = c(
      "Location Description",
      "Bearing",
      "Type"
    )
  ) %>%
  fill_gaps() %>%
  na_seasplit(algorithm = "locf", find_frequency = TRUE)

# Define UI for application
ui <- 
  navbarPage("Traffic Forecasting",
  theme = shinythemes::shinytheme('flatly'),
    tabPanel("Plot",
             
      sidebarLayout(
        
        sidebarPanel(
          
          selectInput(
            "level",
            "Select Aggregation Level",
            c("15 minute", "60 minute", "day")),
          br(),
          
          selectInput(
            "location",
            "Select Traffic Signal Location",
            unique(dataset$"Location Description")
          ),
          
          br(),
          
          dateRangeInput(
            "daterange",
            "Date Range (data available from Jan 2021 through March 2022)",
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
          
          sliderInput("steps", "Number of Weeks to Forecast", 1, 8, 2, step = 1),
          
          br(),
          
          downloadButton(outputId = "download_data", label="Download")
        ),
    
  mainPanel(
    h3("Background"),
    p("This app forecasts traffic volumes and travel times at traffic signals 
       using historical volume and travel time data. The forecasted 
       volumes and travel times can be used by traffic engineers to 
       determine how well traffic signal timing plans will work in 
       the future. The relationship between vehicle volumes and travel
       times can also reveal when a traffic signal system is likely to
       suffer from congestion."),
    hr(),
    h3("Data Definitions"),
    p("Travel Time - travel time from one data collection point to the next heading
    into an intersection. The distance over which the travel time occurs varies between locations
    but is usually between a quarter and half mile long."),
    p("Traffic Volume - total number of vehicles driving through an intersection during a given time period"),
    tabsetPanel(type="tabs",
                tabPanel("Plot", plotly::plotlyOutput("plot"),
                         plotly::plotlyOutput("scatter_plot")),
                tabPanel("Summary", verbatimTextOutput("summary")),
                tabPanel("Table", DT::dataTableOutput(outputId="table"))
                )
    )
  )),
  tabPanel("About Us",
           fluidPage(
             column(12,
                    p("This app was built for the DSCI-D590 Time Series Analysis class as
             part of the M.S. in Data Science program at Indiana University -
             Bloomington."),
                    hr(),
                    fluidRow(
                      column(6,
                             h3("Shawn Strasser"),img(src="shawn.png",height=150,
                             width=150),p("A traffic engineer and data enthusiast,
                             I look for actionable insights to alleviate traffic
                             congestion and enhance safety for all road users. I
                             develop and maintain automated reporting solutions 
                             for traffic signals at the Oregon Department of 
                             Transportation. This includes everything from 
                             equipment failures, to pedestrian activity, and even
                             monitoring red-light running. Being data-driven
                             means proactive maintenance and effective prioritization
                             of projects.")),
                      column(width=6,
                             h3("Sreeti Ravi"), img(src="sreeti.png",height=150,
                             width=150),p("I am currently working as a Senior 
                             Technology Solutions Consultant with a focus on data
                             engineering and data science in Chicago for a global
                             boutique consulting firm. I am driven by my interest
                             in big data to solve real world problems using the tools
                             I have gathered from my experiences and enjoy helping
                             companies find actionable insights from their data.
                             I obtained my B.S. in Informatics from IU in
                             May 2020 and will receive my M.S. in Data Science in May 2022."))
                    ))
           )
           ),
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
      #select(input$datatype) %>%
      select(-"Location Description") %>%
      group_by_key() %>%
      mutate(TimeStamp_Bin = floor_date(TimeStamp, unit = input$level))
  })
  
  # If the aggregation level is less than 1 day, keep TimeStamp as datetime
  # If >= 1 day, convert TimeStamp to date. Otherwise index_by won't work
  r_data2 <- reactive({
    if (substr(input$level, nchar(input$level) - 5, nchar(input$level)) == "minute")
    {
      r_data() %>% index_by(TimeStamp_Bin)
    }
    else
    {
      r_data() %>% index_by(TimeStamp_Bin = as.Date(TimeStamp_Bin))
    }
  })
  
  # Summarize with Sum for Volume, but use Mean for Travel Time
  r_data3 <- reactive({
    r_data2() %>% summarise(`Volume` = sum(.data[["Volume"]]), `Travel_Time` = mean(.data[["Travel_Time"]]))
    
    #if (input$datatype == "Volume")
    #{
    #  r_data2() %>% summarise(`value` = sum(.data[[input$datatype]]))
    #}
    #else
    #{
    #  r_data2() %>% summarise(`value` = mean(.data[[input$datatype]]))
    #}
  })
  
  r_data4 <- reactive({
    r_data3() %>% select(input$datatype, Type) %>%
      rename("value" = .data[[input$datatype]])
  })
  
  # Create a Plot
  final_plot <- function() {
    r_data4() %>% 
      #select(input$datatype) %>%
      model(decomposition_model(
      STL(
         ~ season(period = "week") + season(period = r_period()),
        robust = TRUE
      ),
      NAIVE(season_adjust)
    )) %>%
      forecast(h = r_steps()) %>%
      as_tsibble() %>%
      select(-.model) %>%
      select(.mean) %>%
      rename("value" = .mean) %>%
      mutate(Type = "Forecast") %>% 
      bind_rows(r_data4()) %>%
      autoplot() +
      scale_color_manual(values = c("light blue", "pink", "blue", "red")) +
      labs(title = title(), y = y_axis())
  }
  
  # Create a scatter Plot
  scatter_plot <- function() {
    r_data3() %>%
      mutate(Bearing = paste(Bearing, "Actual", sep ="/")) %>%
      ggplot(aes( x = Volume, y = Travel_Time, group = Bearing, color = Bearing)) +
      geom_point() +
      scale_color_manual(values = c("light blue", "pink")) +
      labs(title = "Volume vs Travel Time", y = "Travel Time")
  }
  
  output$plot <- plotly::renderPlotly(final_plot())
  
  output$scatter_plot <- plotly::renderPlotly(scatter_plot())
  
  output$summary <- renderPrint({
    summary(r_data3())
  })
  
  output$table <- DT::renderDataTable({
    r_data3()
  })
  
  output$download_data <- downloadHandler(
    filename = "download_data.csv",
    content = function(file) {
      data <- r_data3()
      write.csv(data, file, row.names = FALSE)
    }
  )
  
  
}

# Run the application
shinyApp(ui = ui, server = server)

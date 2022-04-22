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

# Define UI for application
ui <- fluidPage(

titlePanel("Traffic Forecasting"),
selectInput("level", "Select Aggregation Level", c("15 Minute", "Hourly", "Daily", "Weekly")),
selectInput("location", "Select Traffic Signal Location", unique(dataset$TSSU)),
dateRangeInput("daterange", "Date Range", start = "2020-01-01", end = "2022-04-01"),
plotOutput("testplot")


)



# Define server logic
server <- function(input, output) {


    output$testplot <- renderPlot({
      dataset %>%
        filter(TSSU == input$location) %>%
        autoplot()
    })

    
}

# Run the application 
shinyApp(ui = ui, server = server)

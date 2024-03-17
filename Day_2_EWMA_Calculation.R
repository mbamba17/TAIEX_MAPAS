# Install and load required packages
if (!requireNamespace("fredr", quietly = TRUE)) install.packages("fredr")
if (!requireNamespace("ggplot2", quietly = TRUE)) install.packages("ggplot2")
if (!requireNamespace("TTR", quietly = TRUE)) install.packages("TTR")

library(shiny)
library(ggplot2)
library(quarks) # For EWMA calculations
library(fredr)

# Set FRED API key
fredr_set_key("18bd98ad8b7870466ec5ddff1d8eae37") 

# Define UI
ui <- fluidPage(
  titlePanel("S&P 500 Analysis from St. Louis FED"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("lambdaInput", "Lambda for EWMA", min = 0.90, max = 0.99, value = 0.94, step = 0.01),
      actionButton("loadData", "Load S&P 500 Data")
    ),
    mainPanel(
      plotOutput("sp500Plot"),
      plotOutput("volatilityPlot")
    )
  )
)

# Define server logic
server <- function(input, output) {
  observeEvent(input$loadData, {
    # Download S&P 500 data from FRED
    sp500 <- fredr(series_id = "SP500", observation_start = as.Date("2010-01-01"))
    
    # Process and store the data for plotting
    sp500Data <- data.frame(Date = as.Date(sp500$date), Value = as.numeric(sp500$value)) %>% na.omit()
    
    # Plot S&P 500 time series
    output$sp500Plot <- renderPlot({
      ggplot(sp500Data, aes(x = Date, y = Value)) +
        geom_line() +
        labs(title = "S&P 500 Time Series", x = "Date", y = "Close Price") +
        theme_minimal()
    })
    
    # Plot EWMA Volatility
    output$volatilityPlot <- renderPlot({
      dailyReturns <- diff(log(sp500Data$Value))
      lambda <- input$lambdaInput
      
      # Calculate EWMA volatility
      volatilityData <- ewma(dailyReturns, lambda = lambda)
      
      # Prepare the data for plotting
      volatilityDataFrame <- data.frame(
        Date = sp500Data$Date[-1], # Exclude the first date since diff reduces the length by 1
        Volatility = volatilityData
      )
      
      ggplot(volatilityDataFrame, aes(x = Date, y = Volatility)) +
        geom_line() +
        labs(title = "EWMA Volatility of S&P 500 Returns", x = "Date", y = "Volatility") +
        theme_minimal()
    })
  })
}

# Run the application
shinyApp(ui = ui, server = server)

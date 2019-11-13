#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(DT)
library(ggplot2)
library(FinCal)
library(tidyr)
library(shinythemes)
library(scales)
library(gridExtra)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Workout 02 app"),
   
   fluidRow(
     column(3,
            sliderInput("initialamount", "Initial amount (in dollars)",
                        min = 1, max = 10000,
                        value = 1000, step = 0.5),
            
            sliderInput("annualcontribution", "Annual contribution",
                        min = 0, max = 5000,
                        value = 200, step = 0.5),
            
            sliderInput("annualgrowthrate", "Annual growth rate (in %)",
                        min = 0, max = 20,
                        value = 3, step = 0.5)
     ),
     column(3,
            sliderInput("highyieldannualrate", "High yield annual rate (in %)",
                        min = 0, max = 20,
                        value = 2, step = 0.5),
            
            sliderInput("fixedincomeannualrate", "Fixed income annual rate (in %)",
                        min = 0, max = 20,
                        value = 5, step = 0.5),
            
            sliderInput("USequityannualrate", "US equity annual rate (in %)",
                        min = 0, max = 20,
                        value = 10, step = 0.5)
     ),
     column(3,
            sliderInput("highyieldvolatility", "High yield volatility (in %)",
                        min = 0, max = 20,
                        value = 0.1, step = 0.5),
            
            sliderInput("fixedincomevolatility", "Fixed income volatility (in %)",
                        min = 0, max = 20,
                        value = 4.5, step = 0.5),
            
            sliderInput("USequityvolatility", "US equity volatility (in %)",
                        min = 0, max = 20,
                        value = 15, step = 0.5)
    ),
    column(3,
           sliderInput("years", "Years",
                       min = 0, max = 50,
                       value = 10, step = 1),
           
           textInput("randomseed", "Random seed", value = "12345", width = NULL,
                     placeholder = NULL),
           
           selectInput("facet", "Facet?", c('Yes', 'No'), selected = NULL, multiple = FALSE,
                       selectize = TRUE, width = NULL, size = NULL),
    )
            
  ),
  mainPanel(plotOutput("Plot"), width = 28)
)

# Define server logic required to draw a histogram
server <- shinyServer(function(input, output) {
  output$Plot <- renderPlot({
    
    # processing
    set.seed(input$randomseed)
    highyield <- c(input$initialamount)
    usbonds <- c(input$initialamount)
    usstocks <- c(input$initialamount)
    firsthighyield <- input$initialamount
    firstusbonds <- input$initialamount
    firstusstocks <- input$initialamount
    i <- 1
    while (i < input$years) {
      highyield <- append(highyield, firsthighyield + (input$initialamount + input$annualcontribution) * rnorm(n = 1, mean = input$highyieldannualrate, sd = input$highyieldvolatility))
      firsthighyield <- highyield[i]
      usbonds <- append(usbonds, firstusbonds + (input$initialamount + input$annualcontribution) * rnorm(n = 1, mean = input$fixedincomeannualrate, sd = input$fixedincomevolatility))
      firstusbonds <- usbonds[i]
      usstocks <- append(usstocks, firstusstocks + (input$initialamount + input$annualcontribution) * rnorm(n = 1, mean = input$USequityannualrate, sd = input$USequityvolatility))
      firstusstocks <- usstocks[i]
      i = i + 1
      }
    highyield<- data.frame(highyield)
    usbonds <- data.frame(usbonds)
    usstocks <- data.frame(usstocks)
    df <- data.frame(period = 1:(input$years), highyield, usbonds, usstocks)
    
    #plotting
    if(input$facet == 'No') {
    ggplot(df, aes(x = period)) + 
      geom_point(aes(y = highyield, color = "High yield"), size = .85) +
      geom_point(aes(y = usbonds, color = "US bonds"), size = .85) +
      geom_point(aes(y = usstocks, color = "US Stocks"), size = .85) +
     geom_line(aes(y = highyield, color = "High yield"), size = .85) +
      geom_line(aes(y = usbonds, color = "US bonds"), size = .85) +
      geom_line(aes(y = usstocks, color = "US Stocks"), size = .85) +
      labs(x = "year",
          y = "amount" ) + 
      ggtitle("Three indices") +
      scale_y_continuous(labels = dollar) +
      theme(legend.position="bottom") +
      theme(legend.title = element_blank()) +
      theme_minimal()
    }
    else {
      p1 <- ggplot(df, aes(x = period)) + 
        geom_point(aes(y = highyield), col = "red", size = .85) +
        geom_line(aes(y = highyield), col = "red", size = .85) +
        labs(x = "year",
             y = "amount" ) + 
        ggtitle("High yield") +
        scale_y_continuous(labels = dollar) +
        theme(legend.position="bottom") +
        theme(legend.title = element_blank()) +
        theme_minimal()
      p2 <- ggplot(df, aes(x = period)) + 
        geom_point(aes(y = usbonds), col = "green", size = .85) +
        geom_line(aes(y = usbonds), col = "green", size = .85) +
        labs(x = "year",
             y = "amount" ) + 
        ggtitle("US Bonds") +
        scale_y_continuous(labels = dollar) +
        theme(legend.position="bottom") +
        theme(legend.title = element_blank()) +
        theme_minimal()
      p3 <- ggplot(df, aes(x = period)) + 
        geom_point(aes(y = usstocks), col = "blue", size = .85) +
        geom_line(aes(y = usstocks), col = "blue", size = .85) +
        labs(x = "year",
             y = "amount" ) + 
        ggtitle("US Stocks") +
        scale_y_continuous(labels = dollar) +
        theme(legend.position="bottom") +
        theme(legend.title = element_blank()) +
        theme_minimal()
      grid.arrange(p1,p2,p3, nrow = 1)
    }
  })
})

# Run the application
shinyApp(ui = ui, server = server)

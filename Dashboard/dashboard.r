library(shiny)
library(dplyr)
require(shinydashboard)
library(ggplot2)
# path to the csv file ---
bcl <- read.csv("F:/ADMIN/Documents/bcl-data.csv", header = TRUE)

ui <- fluidPage(
  
  titlePanel("BC liquor Store Prices"),
  
  sidebarLayout(
    
    sidebarPanel(
      
      sliderInput(inputId = "price",
                  label = "Price :",
                  min = 0,
                  max = 200,
                  value = c(10,30),
                  pre = "$"),
      
      radioButtons(inputId = "product",
                   label = "Product type",
                   choices = c("BEER",
                               "REFRESHMENT",
                               "WINE",
                               "SPIRITS") 
      ),
      
      selectInput(inputId = "country",
                  label = "Country",
                  choices = c("Canada" = "CANADA",
                              "Italy" = "ITALY",
                              "France" = "FRANCE",
                              "U.S.A" = "UNITED STATES OF AMERICA",
                              "Japan" = "JAPAN",
                              "U.K" = "UNITED KINGDOM",
                              "Mexico" = "MEXICO",
                              "Spain" = "SPAIN",
                              "Portugal" = "PORTUGAL",
                              "Brazil" = "BRAZIL",
                              "Australia" = "AUSTRALIA")
      )
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Histogram ----
      plotOutput(outputId = "distPlot")
      
    )
  )
)

# Define server logic required to draw a histogram ----
server <- function(input, output) {
  
  output$distPlot <- renderPlot({
    
    #comning conditions for histogram --
    
    filtered <- 
      bcl %>%  
      filter(  Price >= input$price[1] ,
               Price <= input$price[2] ,
               Type == input$product ,
               Country == input$country)
    
    #x <- filtered$Alcohol_Content
    #hist(x, breaks = 50, col = "#75AADB", border = "white",
    # xlab = "Alcohol content")
    ggplot(filtered, aes(Alcohol_Content)) + geom_histogram(bins = 30)
  })
  
}

# Create Shiny app ----
shinyApp(ui = ui, server = server)

library(shiny)
library(shinydashboard)
library(dbplyr)
library(dplyr)
library(ggplot2)
library(gridExtra)

mtg <- DBI::dbConnect(
  drv = RSQLite::SQLite(), 
  dbname = "AllPrintings.sqlite")

ui <- dashboardPage(
  dashboardHeader(title = "MTG Statistics"),
  dashboardSidebar(),
  dashboardBody(
    fluidRow(
      box(plotOutput("histogram", height=250)),
      box(
        title = "Number of Bins:",
        sliderInput("bins",
                    "Number of bins:",
                    min = 1,
                    max = 50,
                    value = 30)
      ),
      box(
        title = "MTG Card Prices",
        sliderInput("price", "Price Range:", min = 0, max = 10000, value = c(20,30))
      )
    )
  )
)

server <- function(input, output) {
  plotInput <- reactive({
    list(cardprice = (mtg %>% dbReadTable("prices")),
         dat = subset(cardprice, price<input$price[2] & price>input$price[1])
    )
  })
  
  output$histogram <- renderPlot({
    
    # bigcardprice <- with(cardprice, price[price<input$price[2] & price>input$price[1]])
    selectprice <- plotInput()$dat$price
    # generate bins based on input$bins from ui.R
    bins <- seq(min(selectprice), max(selectprice), length.out = input$bins + 1)
    # draw the histogram with the specified number of bins
    hist(selectprice, breaks = bins, col = 'cornflowerblue', border = 'white', 
         xlab = "Price of Magic: The Gathering Cards (US Dollars)",
         main = "Histogram of MTG card prices")
    
    # ggplot(plotInput$dat, aes(type, price, col=type))  + geom_boxplot() + scale_y_log10()
  })
}

shinyApp(ui, server)
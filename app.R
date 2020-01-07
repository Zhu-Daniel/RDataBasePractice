#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(dbplyr)
library(dplyr)
library(ggplot2)
library(gridExtra)

mtg <- DBI::dbConnect(
    drv = RSQLite::SQLite(), 
    dbname = "AllPrintings.sqlite")

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("MTG Price Data"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("bins",
                        "Number of bins:",
                        min = 1,
                        max = 50,
                        value = 30),
        
            sliderInput("price",
                        "Price Range:",
                        min = 1,
                        max = 10000,
                        value = c(20,30))
        ),

        # Show a plot of the generated distribution
        mainPanel(
            fluidRow(
                splitLayout(cellWidths = c("50%","50%"),
           plotOutput("distPlot"),
           plotOutput("boxPlot"))
            )
        )
    )
    
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    plotInput <- reactive({
        list(cardprice = (mtg %>% dbReadTable("prices")),
            dat = subset(cardprice, price<input$price[2] & price>input$price[1])
        )
    })
    
    output$distPlot <- renderPlot({
        
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
    
    output$boxPlot <- renderPlot({
        origplot <- ggplot(plotInput()$dat, aes(type, price, col=type)) + geom_boxplot() + scale_y_log10()
        newplot <- origplot + theme(axis.text = element_text(size = 10))
        print(newplot)
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)

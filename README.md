# R Shiny Markdown Practice
## A project designed to help me understand the nuances of using R Shiny Dashboard and its applications 

R Shiny Dashboard is a flexible tool that allows you to build dashboards that are interactable. It allows one to display information in a website format (.html). Not only can it display charts, data points, and animations, it allows users to create interactable widgets which can be used to alter the output of the data and give more dynamic information. This will chronicle my journey in using R Shiny Dashboard.

### The Beginning

I began by familiarizing myself with R Shiny, which is an R package that allows you to use many of the features of R Shiny Dashboard, without the improved GUI.

I decided to analyze a database in R Shiny, specifically a database for the trading card game Magic: The Gathering using RSQLite. 

```{r}
library(shiny)
library(dbplyr)
library(dplyr)
library(ggplot2)
library(gridExtra)

mtg <- DBI::dbConnect(
    drv = RSQLite::SQLite(), 
    dbname = "AllPrintings.sqlite")

```


```

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

```
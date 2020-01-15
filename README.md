# R Shiny Markdown Practice
## A project designed to help me understand the nuances of using R Shiny Dashboard and its applications 

R Shiny Dashboard is a flexible tool that allows you to build dashboards that are interactable. It allows one to display information in a website format (.html). Not only can it display charts, data points, and animations, it allows users to create interactable widgets which can be used to alter the output of the data and give more dynamic information. This will chronicle my journey in using R Shiny Dashboard.

### The Beginning

I began by familiarizing myself with R Shiny, which is an R package that allows you to use many of the features of R Shiny Dashboard, without the improved GUI.

I decided to analyze a database in R Shiny, specifically a database for the trading card game Magic: The Gathering (MTG) using RSQLite. 

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
First, I extracted the information from the database into "mtg", which is an SQLite connection.

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
```
The ui is the part of shiny that governs the input, i.e interactive widgets the user can use to change the output. In this example, I created sliders to control a histogram and a boxplot to represent the price trends in different types of MTG cards.

```
# Define server logic required to draw a histogram
server <- function(input, output) {
    
    plotInput <- reactive({
        list(cardprice = (mtg %>% dbReadTable("prices")),
            dat = subset(cardprice, price<input$price[2] & price>input$price[1])
        )
    })
    
    output$distPlot <- renderPlot({
        
        
        hist(selectprice, breaks = bins, col = 'cornflowerblue', border = 'white', 
             xlab = "Price of Magic: The Gathering Cards (US Dollars)",
             main = "Histogram of MTG card prices")
        
        
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
Here, the server takes the input from the ui and adjusts the image of the charts. Generally, you want to put any changeable value into a reactive({}) function, which can [make the app faster](https://shiny.rstudio.com/tutorial/written-tutorial/lesson6/).

Output:
![](2020-01-15-12-44-51.png)

Pretty cool, right?
You can find the code under app.r.

I decided to go one step further and try to implement an R Shiny Dashboard 

```{r}
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
    
    hist(selectprice, breaks = bins, col = 'cornflowerblue', border = 'white', 
         xlab = "Price of Magic: The Gathering Cards (US Dollars)",
         main = "Histogram of MTG card prices")

  })
}

shinyApp(ui, server)
```

It's very similar to R Shiny, and the only changes you need to make is to the ui by adding the various dashboardHeader, dashboardBody, box, etc.

You can try this out under RDatabasePractice.r
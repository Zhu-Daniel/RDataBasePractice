library(tidyverse)
library(shiny)
library(shinyjs)
library(shinydashboard)
library(SyncMove)
library(geosphere)
library(gganimate)
library(gifski)
library(shinycssloaders)

#ggplot(gazelleRelocations,  aes(x=utm.easting, y=utm.northing, group=individual.local.identifier, color= individual.local.identifier)) + geom_point(alpha=0.7) + geom_path(alpha=0.7, arrow = arrow()) + facet_wrap(~individual.local.identifier) 
ui <- dashboardPage(
  dashboardHeader(
    title = "Mongolian Gazelle Movement", 
    titleWidth = 300
  ),
  dashboardSidebar(
    conditionalPanel(condition = "!output.setupComplete",
                     box(title = "loading"))
  ),
  dashboardBody(
    
    fluidRow(
      box(height = 200, width=6, imageOutput("animation") %>% withSpinner(color="blue", type=6)),
      box(width=6, plotOutput("scatterplot"))
    ),
    fluidRow(
      box(
        title = "Location of Gazelle on specific days.", width = 4,
        dateRangeInput("daterange", "Date:", start = "2007-09-05" , end = "2008-07-26", min = "2007-09-05", max = "2008-07-26")
      ),
      box(width = 4,
        radioButtons("id", "Distance traveled by ", choices = as.list(unique(gazelleRelocations$individual.local.identifier)))
      ),
    
   
      box(
        infoBoxOutput("distance")
      )
      
    )
  )
)

server <- function(input, output) {
  rv <- reactiveValues()
  rv$setupComplete <- FALSE
  dat <- gazelleRelocations %>% setNames(c("subject", "gender", "time", "x1", "y1")) %>% mutate(x2=lead(x1), y2=lead(y1)) %>% mutate(time=strptime(time, format = "%Y-%m-%d %H:%M:%S") %>% as.POSIXct)
  
  plotInput <- reactive({
   changingdat <- subset(x=dat,(dat$time >= input$daterange[1]) & (dat$time <= input$daterange[2]))
  })
  
 
  
  output$animation <- renderImage({
    #outfile <- tempfile(fileext='.gif')
    
    #gazgraph <- ggplot(dat)  + geom_path(aes(x = x1, y = y1, colour = subject), arrow=arrow(length = unit(5, "points")), alpha=0.7) 
    #gazgraph <- gazgraph + facet_wrap(~subject) + theme_bw() + labs(title = "Mongolian Gazelle Movement", subtitle = "Based on data from William Fagan's lab", x = "Easting Coordinates", y = "Northing Coordinates", caption = "Reference point for the coordinates is unknown.") + transition_reveal(along=date)
    
    #print(gazgraph)
    #gg_animate(gazgraph,"outfile.gif")
    #animate(gazgraph, fps=5, renderer = gifski_renderer("imgs/anim.gif"))
    
   # Return a list containing the filename
    rv$setupComplete <- TRUE
    
    list(src = "www/gazelleanimation.gif",
         contentType = 'image/gif'
         # width = 400,
         # height = 300,
         # alt = "This is alternate text"
    )
  },deleteFile = FALSE) # end of renderImage
  
  output$scatterplot <- renderPlot({
    gazellegraph <- ggplot(plotInput())  + geom_segment(aes(x = x1, y = y1, xend=x2, yend=y2, colour = subject), arrow=arrow(length = unit(5, "points")), alpha=0.7) 
    gazellegraph <- gazellegraph + facet_wrap(~subject) + theme_bw() + labs(title = "Mongolian Gazelle Movement", subtitle = "Based on data from William Fagan's lab", x = "Easting Coordinates", y = "Northing Coordinates", caption = "Reference point for the coordinates is unknown.")
    print(gazellegraph)
  })
  
  
  
  
  output$distance <- renderInfoBox({
    dat<-subset(plotInput(), subject==input$id)
    
    tripLength <- round(sqrt((dat$x2-dat$x1)**2 + (dat$y2-dat$y1)**2),digits=1)
    infoBox(
      paste(unique(dat$subject),"Traveled",sep=" "), value = paste(sum(tripLength),"KM",sep=" "), color = "red", width = 4, fill = FALSE
      #title = "test", value="4 KM", color = "red", width = 4, fill = TRUE
    )
  })
  output$setupComplete <- reactive({
    return(rv$setupComplete)
  })
  outputOptions(output, 'setupComplete', suspendWhenHidden=FALSE)
  
}

shinyApp(ui, server)
#dat <- gazelleRelocations %>% setNames(c("subject", "gender", "time", "x1", "y1")) %>% mutate(x2=lead(x1), y2=lead(y1))
#ggplot(dat)  + geom_segment(aes(x = x1, y = y1, xend = x2, yend = y2, colour = subject), arrow=arrow(length = unit(5, "points")), alpha=0.7) + facet_wrap(~subject) + theme_bw()

                            
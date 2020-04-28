# Load neccessary dependencies

library(shiny)
library(shinythemes)
library(tidyverse)
library(ggplot2)
library(ggthemes)

# load necessary dependencies

map_1910 <- read_rds("map_1910.rds")
map_1920 <- read_rds("map_1920.rds")
map_1930 <- read_rds("map_1930.rds")
map_1940 <- read_rds("map_1940.rds")

# Define UI for application that draws a histogram
ui <- fluidPage(theme = shinytheme("sandstone"),
    # Add line breaks for aesthetic purposes
    br(),
    navbarPage("Climate Change: The Data Speaks The Truth", 
               tabPanel("Heat Map Visualizations",                        
                        h3("Change in Global Temperatures by Decade"),
                        strong("The blue color becoming lighter indicates temperature rise."),
                        
                        br(),
                        br(),
                        
                        sidebarPanel(
                          radioButtons(inputId = "heat_year",
                                       label = "Select a decade:",
                                       choices = c("1910","1920","1930","1940"))),
                        
                        mainPanel(
                          
                          
                          plotOutput("heatmap"),
                          
                          br()
                          
                        )),
  
                        
              tabPanel("Role of CO2 Emissions"),
              tabPanel("Who Is Responsible?")
              
              ),
    

)

# Define server logic required to draw a histogram
server <- function(input, output) {
  #Output for historical heat day event map in the US
  
  output$heatmap <- renderPlot({
    if(input$heat_year == "1910"){
      map_1910
    }
    else if(input$heat_year == "1920"){
      map_1920
    }
    else if(input$heat_year == "1930"){
      map_1930
    }
    else if(input$heat_year == "1940"){
      map_1940
    }
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

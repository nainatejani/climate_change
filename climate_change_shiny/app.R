# Load neccessary dependencies

library(shiny)
library(shinythemes)
library(tidyverse)
library(ggplot2)
library(ggthemes)
library(plotly)

# load necessary dependencies


CO2Plot <- read_rds("co2_temp_plot.rds")
emissions_and_oil_consumption <- read_rds("C02_Emissions_And_OIL.rds")
country_names <- read_rds("country_names.rds")

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
              sidebarPanel(radioButtons(inputId = "heat_year",
                                       label = "Select a decade:",
                                       choices = c("1910","1920","1930","1940",
                                                   "1950","1960","1970","1980",
                                                   "1990","2000","2010","2014"))
                           ),
              mainPanel(imageOutput("heatmap"))
              ),
      tabPanel("Effect of CO2 Emissions",
               h3("How does change in CO2 affect global temperature?"),
               br(),
               br(),
              mainPanel(plotOutput("CO2plot"))
              ),
      tabPanel("Trends By Country",
               h3("Relation Between Oil Consumption and CO2 Emissions"),
               br(),
               br(),
               sidebarPanel(
                 selectInput(
                   "country", "Select a Country", country_names
                 )
               ),
               plotOutput("countrywisePlot")
                         
                       
               )
    )
)
       



# Define server logic required to draw a histogram
server <- function(input, output, session) {

  
  output$heatmap <- renderImage({
    width  <- session$clientData$output_heatmap_width
    height <- session$clientData$output_heatmap_height
    
    pixelratio <- session$clientData$pixelratio
    
    filename <- normalizePath(file.path('./maps',
                                        paste('map', input$heat_year, '.jpeg', sep='')))
    # Return a list containing the filename and alt text
    filename
    list(src = filename,
         alt = paste("Image number", input$heat_year),
         height = height+200,
         width = width)
  }, deleteFile = FALSE)
  
  output$CO2plot <- renderPlot({
    CO2Plot
  })
  output$countrywisePlot <- renderPlot({
    "hello i am here"
    plot <- emissions_and_oil_consumption %>%
               filter(country_name == input$country) %>% 
               ggplot(aes(x = energy_use, 
                          y = emissions_per_capita)) +
               geom_point() 
               # theme_classic() + 
               # labs(title = "Change in CO2 Emissions with Rising Oil Consumption", 
               #              x = "Energy Use(kg of oil equivalent per capita)",
               #              y = "Emissions of CO2(metric tons per capita)"
               #         )
    plot
    


  })

  
}

# Run the application 
shinyApp(ui = ui, server = server)

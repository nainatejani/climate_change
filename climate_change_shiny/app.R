# Load neccessary dependencies

library(shiny)
library(shinythemes)
library(tidyverse)
library(ggplot2)
library(ggthemes)
library(DT)


# load necessary dependencies

# import needed files(mostly plots and tables) for the app

CO2Plot <- read_rds("co2_temp_plot.rds")
forestPlot <- read_rds("forest_temp_plot.rds")
emissions_and_oil_consumption <- read_rds("C02_Emissions_And_OIL.rds")
country_names <- read_rds("country_names.rds")
climate_change_plot <- read_rds("climate_change_plt.rds")
co2_regression <- read_rds("regression_co2.rds")


# Define UI for application that draws a histogram
ui <- fluidPage(theme = shinytheme("sandstone"),
                
                # Add line breaks for aesthetic purposes
                
                br(),
                
                # I had a hard time coming up with a good title for the project. This will have to suffice 
                # for now I guess.
                
                navbarPage("It's not a myth: Climate Change and Correlated Factors", 
                           
                           # Okay, with the heat visualizations, there is good news and bad news.
                           # I had several different issues. Firstly, I was not able to get them 
                           # on the main panel, which was very frustrating and it was too late to go 
                           # to office hours. If you notice, all the maps are a little bit to the bottom
                           # and the dimensions are not exactly correct. This is because instead of uploading 
                           # the maps as rds, I uploaded them as images. Unlike the nice features that rds comes
                           # with handily, I learned that images need to be positioned carefully on Shiny Apps.
                           
                           # Secondly, I had data for years 1900 to 1914. Ideally, I would have loved to create an
                           # animation. However, since this entailed dealing with a huge amount of data(remember each 
                           # map has 85784 points for each latitude and longitude and doing that for 114 years was something
                           # R was having a hard time with). I eventually settled for the decadely plots you see on the app.
                           
                           
                           # The good news is that this part of the project was the most fulfilling to me. I learnt how
                           # to plot points on a shapefile and to deal with maps in general. I learnt a lot about extracting 
                           # data as well since I was dealing with 114 separate temperature files.
                           
                           
                           
                           tabPanel("Heat Map Visualizations",                        
                                    h3("Change in Global Temperatures by Decade"),
                                    strong("The temperature rise is indicated by the increasing reddish color."),
                                    br(),
                                    br(),
                                    # I could have had a slider instead of a radio button. This was supposed to 
                                    # be temporary. However, I ran out of time for doing this.
                                    
                                    sidebarPanel(radioButtons(inputId = "heat_year",
                                                              label = "Select a decade:",
                                                              choices = c("1910","1920","1930","1940",
                                                                          "1950","1960","1970","1980",
                                                                          "1990","2000","2010","2014"))
                                    ),
                                    mainPanel(imageOutput("heatmap")),
                                    p("Each of the heap maps is a result of plotting temperatures 
                for each longitude and latitude in a year. Each annual temperature for each 
                longitude and latitude was calculated by finding the average of the monthly temperatures
                for the year")
                           ),
                           tabPanel("What Causes It?",
                                    tabsetPanel(
                                      tabPanel("Effect of CO2 Emissions",
                                               h3("How does change in CO2 affect global temperature?"),
                                               br(),
                                               br(),
                                               mainPanel(p("One of the most commonly associated causes to climate change and
              global warning is CO2. Here, I plot the affect of CO2 on changes in global
              temperature. It is important to note that there is a positive correlation
              between the two."),
                                                         p("Note that the data points are for the years 1960 to 2014."),
                                                         plotOutput("CO2plot"),
                                                         h4("Relationship between CO2 Emissions and Temperature Change Across the Past Century"),
                                                         h5("Emissions in Tonnes Per Capita has been logged"),
                                                         dataTableOutput("co2_regression"),
                                               )
                                      ),
                                      tabPanel("Effect of Forest Area",
                                               h3("How does change in forest area affect global temperature?"),
                                               br(),
                                               br(),
                                               mainPanel(plotOutput("forestPlot"))
                                      ))),
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
                                    
                                    
                           ),
                           tabPanel("About",
                                    mainPanel(
                                      h3("Climate Change: !"),
                                      p("In this project, the first goal is make it visually clear through a series
                  of time intervals that climate change is a reality. Second, I analyze several
                  factors that may, directly or indirectly be associated with climate change"),
                                      plotOutput("climate_change_plot"),
                                      
                                      
                                    )
                                    
                                    
                           )
                ))




# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  
  output$heatmap <- renderImage({
    width  <- session$clientData$output_heatmap_width
    height <- session$clientData$output_heatmap_height
    
    pixelratio <- session$clientData$pixelratio
    
    filename <- normalizePath(file.path('./maps',
                                        paste('map', input$heat_year, '.jpeg', sep='')))
    # Return a list containing the filename and alt text
    list(src = filename,
         alt = paste("Image number", input$heat_year),
         height = height+200,
         width = width)
  }, deleteFile = FALSE)
  
  output$CO2plot <- renderPlot({
    CO2Plot
  })
  
  
  output$co2_regression <- renderDataTable(datatable({
    co2_regression
    
  }))
  
  output$climate_change_plot <- renderPlot({
    climate_change_plot
  })
  output$forestPlot <- renderPlot({
    forestPlot
  })
  output$countrywisePlot <- renderPlot({
    country <- input$country
    data <- emissions_and_oil_consumption %>%
      filter(country_name == country)
    ggplot(data,aes(x = energy_use, y = emissions_per_capita)) +
      geom_point(color = 'red') +
      geom_smooth(method = 'lm', se = FALSE) +
      theme_classic() +
      
      labs(title = "Change in CO2 Emissions with Rising Oil Consumption",
           x = "Energy Use(kg of oil equivalent per capita)",
           y = "Emissions of CO2(metric tons per capita)"
      )
    
    
  })
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)

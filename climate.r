#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

#Author- Swayam Sethi
#Student ID- 220011796

# Important imports required for the app
require(shiny)
require(dplyr)
require(ggplot2)
require(tidyverse)
require(mapview)
require(leaflet.extras)

# Reading data from datasets stored at particular locations in the system
data_BT <- read.csv('./data/app_data.csv', header = T) # study level metadata
data_2014 <- read.csv('./data/2014BT.csv', header = T) # metrics from Dornelas et al. 2014
pal <- colorFactor(palette = 'Set3', levels = unique(data_full$TAXA))
data_full<- inner_join(data_2014, data_BT, by = c('ID' = 'STUDY_ID')) %>%
  mutate(TAXA = if_else(TAXA == "All", "Multiple Taxa", TAXA))


climate_list = unique(data_full$CLIMATE)
names(climate_list) <- unique(data_full$CLIMATE)
values <- c(unique(data_full$TAXA))
names(values)<- c(unique(data_full$TAXA))

# User Interface code for the application
ui <- fluidPage(

  # Sidebar with a slider input
  sidebarLayout(
    sidebarPanel(
      
      # Application title``
      h1("Species richness change over time"),
      h4("Using BioTIME data from the Dornelas et al. 2014 Science paper"),
      # Creating a slider for years
      sliderInput("years", # input ID
                  "Years to display:", # display title
                  min = min(data_2014$Year),
                  max = max(data_2014$Year),
                  step = 1,
                  value = c(min(data_2014$Year), max(data_2014$Year))),
      # Creating check boxes for taxa 
      checkboxGroupInput("taxa",
                         "Checkbox for Taxa",
                         choices=values,
                         selected=unique(data_full$TAXA)
      ),
      #Creating drop down input for climate 
      selectInput("CLIMATE", label = h3("Climate"), 
                  choices = climate_list,
                  selected = unique(data_full$CLIMATE))
    ),

    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("distPlot"),
      leafletOutput("map")
    )
  )
)

# Server code for the application
server <- function(input, output) {
  
  # Creating the data to be reactive so that it changes with every input
  data_full_reactive <- reactive({data_full %>% filter(CLIMATE %in% input$CLIMATE & TAXA %in% input$taxa)
  })

  # Displaying the map to display taxa
  output$map <- renderLeaflet({
    mapview(data_full_reactive(), zcol = "TAXA", xcol = "CENT_LONG", ycol = "CENT_LAT",color = pal , crs = 4269, grid = FALSE,  label = paste(data_full_reactive()$TAXA), legend = FALSE)@map
  })
  
  # Displaying the graph
  output$distPlot <- renderPlot({
    
    ggplot(data = data_full_reactive()) +
      geom_line(aes(x = Year, y = S, color = TAXA, group = ID)) +
      geom_smooth(aes(x = Year, y = S), method = "loess") +
      theme_bw(base_size = 13) +
      scale_x_continuous(limits = input$years) +
      scale_y_log10()
    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

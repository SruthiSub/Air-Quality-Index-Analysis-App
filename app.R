# Install packages if not already installed
#install.packages(c("ggplot2", "rnaturalearth", "rnaturalearthdata", "sf", "viridis"))
#install.packages("shiny")

# Load libraries
library(ggplot2)
library(rnaturalearth)
library(rnaturalearthdata)
library(sf)
library(viridis)
library(dplyr)
library(tidyverse)
library(shiny)

city_locations <- read.csv("worldcities.csv")
AQI_by_city <- read.csv("AQI_by_city.csv")
# Data Scraped for PM2.5, so need to adjust by a factor to get AQI
AQI_by_city$aqi_table.X2024 <- AQI_by_city$aqi_table.X2024*50/16
maxAQI <- which(AQI_by_city$aqi_table.X2024 == max(AQI_by_city$aqi_table.X2024))
minAQI <- which(AQI_by_city$aqi_table.X2024 == min(AQI_by_city$aqi_table.X2024))
maxcity <- AQI_by_city$cities[maxAQI]
mincity <- AQI_by_city$cities[minAQI]
maxcountry <- AQI_by_city$countries[maxAQI]
mincountry <- AQI_by_city$countries[minAQI]
maxAQI <- AQI_by_city$aqi_table.X2024[maxAQI]
minAQI <- AQI_by_city$aqi_table.X2024[minAQI]
common_cities <- intersect(city_locations$city, AQI_by_city$cities)
latitudes <- city_locations$lat[which(city_locations$city %in% common_cities)]
longitudes <- city_locations$lng[which(city_locations$city %in% common_cities)]
city_list <- city_locations$city[which(city_locations$city %in% common_cities)]
AQI_by_city <- AQI_by_city[which(AQI_by_city$cities %in% common_cities),]
geodata <- data.frame(latitudes, longitudes)
geodata$cities <- city_list
geodata <- merge(geodata, AQI_by_city, ID = "cities")

world_data <- data.frame(
  lat = geodata$latitudes,
  lon = geodata$longitudes,
  AQI = geodata$aqi
)

# correlation plots:
country_wise_parameters <- read.csv("Country_wise_parameters.csv")
country_wise_parameters <- country_wise_parameters[,-1]
AQI_by_country <- AQI_by_city %>%
  group_by(countries) %>%
  summarise(mean_aqi = mean(aqi_table.X2024, na.rm = TRUE))
common_countries <- intersect(country_wise_parameters$countries, AQI_by_country$countries)
country_wise_parameters <- country_wise_parameters[which(country_wise_parameters$countries %in% common_countries),]
AQI_by_country <- AQI_by_country[which(country_wise_parameters$countries %in% common_countries),]
data <- merge(country_wise_parameters, AQI_by_country, ID = "countries")
colnames(data) <- c('country','population','latitude','longitude','GDP_per_capita','temperature','avg_precipitation','avg_rel_humidity','monthly_sunshine','mean_aqi')
data['population'] <-  as.numeric(gsub(",","",data$population))
data["GDP_per_capita"] <- as.numeric(gsub("[$,]","",data$GDP_per_capita))
data <- data[,-c(1)]
corrs <- cor(data, use = "pairwise.complete.obs")
target_var <- "mean_aqi"
corr_vec <- corrs[target_var, , drop = FALSE]
corr_vec <- corr_vec[,-c(length(corr_vec))]

#APP CODE
ui <- navbarPage("Air Quality Index",
                 tabPanel("About",
                          fluidPage(
                            h2("About this App"),
                            p("This app visualizes AQI (Air Quality Index) distribution around the world and explores how different parameters correlate with AQI."),
                            p("Use the tabs above to explore a global heatmap of AQI and correlation relationships through correlation values and regression plots."),
                            h2("What will each tab show me?"),
                            p("Global Heatmap of AQI - gives a heatmap of AQI all over the world, marking the AQI city-wise."),
                            p("Correlation with parameters - gives an overview of the correlation of AQI with various parameters like temperature, population, monthly sunshine, longitude, latitude, GDP per capita, average relative humidity and average precipitation. It also allows you to choose a parameter and view the correlation plot of it with AQI, with or without the corresponding regression line. Here we are comparing country-wise parameter data with the average AQI of the respective countries.")
                          )
                 ),
                 tabPanel("Global Heatmap of AQI",
                          fluidPage(
                            titlePanel("AQI Distribution in the World"),
                            h4("Max AQI is", maxAQI, "at", maxcity, "in", maxcountry,"."),
                            h4("Min AQI is", minAQI, "at", mincity, "in", mincountry,"."),
                            plotOutput("distribution_plot", height = '500px')
                          )
                 ),
                 tabPanel("Correlation with parameters",
                          fluidPage(
                            titlePanel("Correlation with other Parameters"),
                            
                            sidebarLayout(
                              sidebarPanel(
                                selectInput(inputId = "param",
                                            label = "Choose a parameter:",
                                            choices = c("temperature","population","GDP_per_capita","avg_rel_humidity","monthly_sunshine","avg_precipitation")),
                                checkboxInput(inputId = "lm",
                                              label = "Show regression line",
                                              value = TRUE),
                                hr(),
                                h4("Correlation Overview"),
                                plotOutput("lollipop", height = '300px')
                                
                              ),
                              
                              mainPanel(
                                h4("Select a parameter from the sidebar to view its correlation plot with Mean AQI."),
                                hr(),
                                h4("Parameter vs Mean AQI"),
                                plotOutput("scatter", height = '300px')
                              )
                            )
                          )
                 )
)


#server logic
server <- function(input,output){
  
  output$distribution_plot <- renderPlot({
    
    #drawing the world wide distribution
    world <- ne_countries(scale = "medium", returnclass = "sf")
    
    # Plot
    ggplot(data = world) +
      geom_sf(fill = "gray90", color = "gray70") +
      geom_point(data = world_data, aes(x = lon, y = lat, color = AQI), size = 1.5, alpha = 0.6) +
      scale_color_viridis(option = "inferno", name = "AQI", direction = -1) +
      theme_minimal() +
      labs(title = "Global Air Quality (AQI Heatmap)",
           x = "Longitude", y = "Latitude")
    
  })
  
  output$lollipop <- renderPlot({
    x <- names(corr_vec)
    y <- as.numeric(corr_vec)
    temp <- data.frame(x,y)
    ggplot(temp, aes(x = x, y = y)) +
      geom_segment( aes ( x = x, xend = x,y = 0, yend = y), color = 'skyblue') +
      geom_point(color = 'blue', size = 4, alpha = 0.6) +
      labs(y = "Correlation with Mean AQI", x = "Parameters") +
      theme_light() +
      coord_flip() 
  })
  
  output$scatter <- renderPlot({
    p <- ggplot(data, aes_string(x= input$param, y= "mean_aqi")) +
      geom_point(color = 'gold',alpha = 0.6) +
      theme_light() +
      labs(title = paste("AQI vs", input$param),
           x = input$param ,
           y = "Mean AQI")
    if (input$lm) {
      p <- p + geom_smooth(method = 'lm',color ='black') 
    }
    p
  })
}

#run the app
shinyApp(ui = ui, server = server)










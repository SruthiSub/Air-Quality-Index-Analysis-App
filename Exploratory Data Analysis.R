# Install packages if not already installed
#install.packages(c("ggplot2", "rnaturalearth", "rnaturalearthdata", "sf", "viridis"))
#install.packages("shiny")

# Set working directly to location of extracted file
if (requireNamespace("rstudioapi", quietly = TRUE)) {
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
}
# Load libraries
library(ggplot2)
library(rnaturalearth)
library(rnaturalearthdata)
library(sf)
library(viridis)
library(dplyr)
library(tidyverse)

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
paste("Max AQI is", maxAQI, "at", maxcity, "in", maxcountry,".")
paste("Min AQI is", minAQI, "at", mincity, "in", mincountry,".")

common_cities <- intersect(city_locations$city, AQI_by_city$cities)
latitudes <- city_locations$lat[which(city_locations$city %in% common_cities)]
longitudes <- city_locations$lng[which(city_locations$city %in% common_cities)]
city_list <- city_locations$city[which(city_locations$city %in% common_cities)]
AQI_by_city <- AQI_by_city[which(AQI_by_city$cities %in% common_cities),]
geodata <- data.frame(latitudes, longitudes)
geodata$cities <- city_list
geodata <- merge(geodata, AQI_by_city, ID = "cities")
geodata

world_data <- data.frame(
  lat = geodata$latitudes,
  lon = geodata$longitudes,
  AQI = geodata$aqi
)

# heatmap plot:
world <- ne_countries(scale = "medium", returnclass = "sf")
ggplot(data = world) +
  geom_sf(fill = "gray90", color = "gray70") +
  geom_point(data = world_data, aes(x = lon, y = lat, color = AQI), size = 1.5, alpha = 0.6) +
  scale_color_viridis(option = "inferno", name = "AQI", direction = -1) +
  theme_minimal() +
  labs(title = "Global Air Quality (AQI Heatmap)",
       x = "Longitude", y = "Latitude")

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
# plotting as a lollipop plot
x <- names(corr_vec)
y <- as.numeric(corr_vec)
temp <- data.frame(x,y)
ggplot(temp, aes(x = x, y = y)) +
  geom_segment( aes ( x = x, xend = x,y = 0, yend = y), color = 'skyblue') +
  geom_point(color = 'blue', size = 4, alpha = 0.6) +
  theme_light() +
  coord_flip() 

#temperature vs aqi graph
ggplot(data,aes(x = temperature, y = mean_aqi)) +
  geom_point(color = 'tomato',alpha = 0.6) +
  geom_smooth(method = 'lm', color = 'black') +
  theme_light() +
  labs( title = "AQI vs Temperature",
        x = "Mean Daily Temperature (deg C)",
        y = "Mean AQI")

#population vs aqi graph
ggplot(data,aes (x = population, y = mean_aqi)) +
  geom_point(color = 'orange', alpha = 0.6) +
  geom_smooth(method = 'lm', color = 'black') +
  scale_x_log10() +
  theme_light() +
  labs (title = "AQI vs Population",
        x = "population (log scale)",
        y = "Mean AQI")

#GDP per capita vs aqi graph
ggplot(data,aes(x = GDP_per_capita , y=mean_aqi))+
  geom_point(color = 'seagreen', alpha = 0.6) +
  geom_smooth(method = 'lm',color = 'black') +
  theme_light() +
  labs(title = 'AQI vs GDP per capita',
       x = "GDP per capita",
       y = "Mean AQI")

#Humidity vs aqi graph
ggplot(data, aes(x= avg_rel_humidity, y= mean_aqi)) +
  geom_point(color = 'dodgerblue',alpha = 0.6) +
  geom_smooth(method = 'lm',color ='black') +
  theme_light() +
  labs(title = "AQI vs Humidity",
       x = "Average Relative Humidity",
       y = "Mean AQI")

#Sunshine Hours vs aqi graph
ggplot(data, aes(x= monthly_sunshine, y= mean_aqi)) +
  geom_point(color = 'gold',alpha = 0.6) +
  geom_smooth(method = 'lm',color ='black') +
  theme_light() +
  labs(title = "AQI vs Monthly Sunshine Hours",
       x = "Mean Monthly Sunshine (Hours)",
       y = "Mean AQI")


#Precipitation vs aqi graph
ggplot(data, aes(x= avg_precipitation, y= mean_aqi)) +
  geom_point(color = 'purple',alpha = 0.6) +
  geom_smooth(method = 'lm',color ='black') +
  theme_light() +
  labs(title = "AQI vs Precipitation",
       x = "Average Precipitation",
       y = "Mean AQI")


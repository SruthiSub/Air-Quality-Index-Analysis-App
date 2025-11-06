# MTH208 Course Project - Team 7
# Data Scraping and Cleaning Script

library(rvest)
library(tidyverse)
library(chromote)
library(stringr)

# Extracting AQI for cities all over the world
url <- "https://www.iqair.com/world-most-polluted-cities"
b <- ChromoteSession$new()
b$Page$navigate("https://www.iqair.com/world-most-polluted-cities")
Sys.sleep(5)  
html <- b$DOM$getDocument()
html_content <- b$DOM$getOuterHTML(html$root$nodeId)[["outerHTML"]]
page <- read_html(html_content)
aqi_table <- page %>% html_table() 
aqi_table <- data.frame(aqi_table)
cities <- str_split(aqi_table$City, ",\\s*", simplify = TRUE)[,1]
countries <- str_split(aqi_table$City, ",\\s*", simplify = TRUE)[,2]
AQI_by_city <- data.frame(cities, countries, aqi_table$X2024)
AQI_by_city
# Extracting the data in batches to prevent timeout
for (i in 2:60){
url <- paste("https://www.iqair.com/world-most-polluted-cities?page=", as.character(i))
b <- ChromoteSession$new()
b$Page$navigate(url)
Sys.sleep(8)  
html <- b$DOM$getDocument()
html_content <- b$DOM$getOuterHTML(html$root$nodeId)[["outerHTML"]]
page <- read_html(html_content)
aqi_table <- page %>% html_table() 
aqi_table <- data.frame(aqi_table)
cities <- str_split(aqi_table$City, ",\\s*", simplify = TRUE)[,1]
countries <- str_split(aqi_table$City, ",\\s*", simplify = TRUE)[,2]
AQI_by_city_new <- data.frame(cities, countries, aqi_table$X2024)
AQI_by_city <- rbind(AQI_by_city, AQI_by_city_new)
}
write.csv(AQI_by_city, "AQI_by_city.csv")
# done in different sessions, as otherwise it was timing out
AQI_by_city <- read.csv("AQI_by_city.csv")
AQI_by_city <- AQI_by_city[,-1] # it adds a column of indices when saving
for (i in 61:120){
  url <- paste("https://www.iqair.com/world-most-polluted-cities?page=", as.character(i))
  b <- ChromoteSession$new()
  b$Page$navigate(url)
  Sys.sleep(8) 
  html <- b$DOM$getDocument()
  html_content <- b$DOM$getOuterHTML(html$root$nodeId)[["outerHTML"]]
  page <- read_html(html_content)
  aqi_table <- page %>% html_table() 
  aqi_table <- data.frame(aqi_table)
  cities <- str_split(aqi_table$City, ",\\s*", simplify = TRUE)[,1]
  countries <- str_split(aqi_table$City, ",\\s*", simplify = TRUE)[,2]
  AQI_by_city_new <- data.frame(cities, countries, aqi_table$X2024)
  AQI_by_city <- rbind(AQI_by_city, AQI_by_city_new)
}
write.csv(AQI_by_city, "AQI_by_city.csv")
AQI_by_city <- read.csv("AQI_by_city.csv")
AQI_by_city <- AQI_by_city[,-1] # it adds a columns with indices when saving
for (i in 121:180){
  url <- paste("https://www.iqair.com/world-most-polluted-cities?page=", as.character(i))
  b <- ChromoteSession$new()
  b$Page$navigate(url)
  Sys.sleep(8) 
  html <- b$DOM$getDocument()
  html_content <- b$DOM$getOuterHTML(html$root$nodeId)[["outerHTML"]]
  page <- read_html(html_content)
  aqi_table <- page %>% html_table() 
  aqi_table <- data.frame(aqi_table)
  cities <- str_split(aqi_table$City, ",\\s*", simplify = TRUE)[,1]
  countries <- str_split(aqi_table$City, ",\\s*", simplify = TRUE)[,2]
  AQI_by_city_new <- data.frame(cities, countries, aqi_table$X2024)
  AQI_by_city <- rbind(AQI_by_city, AQI_by_city_new)
}
write.csv(AQI_by_city, "AQI_by_city.csv")

# Extracting Weather and Climate Data:
url <- "https://weatherandclimate.com/"
page <- read_html(url)
countries_links <- page %>% html_elements("li") %>% html_elements("a") 
countries_links <- countries_links %>% html_attr("href")
countries_links <- c(countries_links[6:74], countries_links[79:324])
countries_links <- c(countries_links[1:113], countries_links[115:315])
countries_links <- c(countries_links[1:223], countries_links[225:314])
countries <- page %>% html_elements("span") %>% html_text()
countries <- c(countries[2:69],countries[71:317])
countries <- c(countries[1:113],countries[115:315])
countries <- c(countries[1:223],countries[225:314])
countries
i <- 1
url <- countries_links[i]
page <- read_html(url)
text <- page %>% html_elements("td") %>% html_text()
# Let us extract the following data points from the table:
# Daily mean C (temperature)
i1 <- which(text == "Daily mean °C (°F)")
A1 <- text[i1+13]
A1 <- str_split(A1, "\\(", simplify = TRUE)[1,1]
# Average Precipitation mm 
i2 <- which(text == "Average precipitation mm (inches)")
A2 <- text[i2+13]
A2 <- str_split(A2, "\\(", simplify = TRUE)[1,1]
# Average Relative Humidity (%)
i3 <- which(text == "Average relative humidity (%)")
A3 <- text[i3+13]
# Mean monthly sunshine hours
i4 <- which(text == "Mean monthly sunshine hours")
A4 <- text[i4+13]
climate_data <- c(countries[i],A1,A2,A3,A4)

for (i in 2:313){
url <- countries_links[i]
page <- read_html(url)
text <- page %>% html_elements("td") %>% html_text()
# Let us extract the following data points from the table:
# Daily mean C (temperature)
i1 <- which(text == "Daily mean °C (°F)")
A1 <- text[i1+13]
A1 <- str_split(A1, "\\(", simplify = TRUE)[1,1]
# Average Precipitation mm 
i2 <- which(text == "Average precipitation mm (inches)")
A2 <- text[i2+13]
A2 <- str_split(A2, "\\(", simplify = TRUE)[1,1]
# Average Relative Humidity (%)
i3 <- which(text == "Average relative humidity (%)")
A3 <- text[i3+13]
# Mean monthly sunshine hours
i4 <- which(text == "Mean monthly sunshine hours")
A4 <- text[i4+13]
data <- c(countries[i],A1,A2,A3,A4)
climate_data <- rbind(climate_data, data)
data
}
climate_data
climate_data <- data.frame(climate_data)
cnames <- c("countries","Daily mean temperature (C)", "Average precipitation (mm)", "Average relative humidity (%)", "Mean monthly sunshine hours")
colnames(climate_data) <- cnames
rownames(climate_data) <- 1:313
write.csv(climate_data, "Countrywise_Climate_Data.csv")

climate_data <- read.csv("Countrywise_Climate_Data.csv")
# Scraping country wise population
url <- "https://www.worldometers.info/world-population/population-by-country/"
page <- read_html(url)
dat <- page %>% html_table()
dat <- data.frame(dat[[1]])
countries <- dat$Country..or.dependency.
population <- dat$Population.2025
country_wise_population <- data.frame(countries, population)
common_countries <- intersect(country_wise_population$countries, climate_data$countries)

# Scraping Geological Data
url <- "https://developers.google.com/public-data/docs/canonical/countries_csv"
page <- read_html(url)
geo <- page %>% html_table()
geo <- data.frame(geo[[1]])
common_countries <- intersect(geo$name, common_countries)
latitude <- geo$latitude
longitude <- geo$longitude
countries <- geo$name
geological_data <- data.frame(countries, latitude, longitude)
geological_data <- geological_data[-which(is.na(geological_data$latitude)),]

# Scraping GDP per capita to get a measure of industrial growth of the country
url <- "https://worldpopulationreview.com/countries/by-gdp"
page <- read_html(url)
GDP <- page %>% html_table()
GDP <- data.frame(GDP[[1]])
countries <- GDP$Country
GDP_IMF_25 <- GDP$GDP..IMF..25.
GDP_per_capita <- GDP$GDP.Per.Capita
country_wise_gdp <- data.frame(countries , GDP_per_capita)
common_countries <- intersect(country_wise_gdp$countries, common_countries)
geological_data <- geological_data[which(geological_data$countries %in% common_countries),]
country_wise_population <- country_wise_population[which(country_wise_population$countries %in% common_countries),]
country_wise_gdp <- country_wise_gdp[which(country_wise_gdp$countries %in% common_countries),]
climate_data <- climate_data[which(climate_data$countries %in% common_countries),]

# merging all the data into one table
country_wise_parameters <- merge(country_wise_population, geological_data, by = "countries")
country_wise_parameters <- merge(country_wise_parameters, country_wise_gdp, by = "countries")
country_wise_parameters <- merge(country_wise_parameters, climate_data, by = "countries")

country_wise_parameters <- country_wise_parameters[,-6]
country_wise_parameters <- country_wise_parameters[!duplicated(country_wise_parameters$countries),]
write.csv(country_wise_parameters, "Country_wise_parameters.csv")
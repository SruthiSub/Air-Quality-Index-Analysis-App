Air Quality Index Analysis App - to analyse global trends and variations in the Air Quality Index, and to check correlation of the Air Quality Index with various environmental and socio-economic factors like temperature, humidity, rainfall, sunshine, GDP per capita, and population. Visualization is done through heatmaps, and correlation plots, integrated in an RShiny App.

Group project for the course MTH208 (Semester I 2025-26, IIT Kanpur), taken by Professor Akash Anand.

Execution of files:
- Download the zip file “MTH208 - Team 7 - Project” and extract the contents.
- Install the packages listed below if not already installed, by executing the lines:
install.packages(c("ggplot2", "rnaturalearth", "rnaturalearthdata", "sf", "viridis"))
install.packages("shiny")

To run the app:
- Run the following commands in the console:
setwd(“path to the extracted file”)
shiny::runApp(“app.R”)
- You can now see the app. You can go through the various tabs and view the visualizations. In the tab “Correlation with parameters”, you can also use the controls on the left top of the screen to pick which parameter you would like to see the correlation of AQI with, and you can choose to have a regression line or not in that correlation plot.

To see Exploratory Data Analysis:
- Within the R file “MTH208 - Team 7 - Exploratory Data Analysis.R”, you can also see how we built up the plot codes. We have included all the graph codes for reference. Before executing this, make sure that you change the working directory at the top of the code to the extracted file path.

For Data Scraping:
- The files “Country_wise_parameters.csv” and “AQI_by_city.csv”, have been scraped.
- The code for this can be seen in “MTH208 - Team 7 - Data Scraping Code.R”. Before executing this, make sure that you change the working directory at the top of the code to the extracted file path. On executing this file, you will be able to get the same csv files that we have saved in the folder. The scraping process took a lot of time, and the session had to be renewed to prevent timeouts a few times. Hence, we are attaching the files that are already scraped so that the app can be run directly using these. This will save computational power and is hence more efficient.
- “worldcities.csv” was taken from an online database where the geological data (latitude and longitude) of cities is maintained. That was not scraped by us. We directly downloaded that file and used the coordinates for plotting the AQI in the heatmap.

library(shiny)



# libraries ---------------------------------------------------------------

library(tidyverse)
library(readr)
library(rvest)
library(stats)
library(readxl)
library(dplyr)
library(stringr)
library(ggplot2)
library(ggthemes)
library(stringr)
library(data.table)
library(geojsonio)
library(leaflet)
library(rgdal)
library(haven)



# read data ---------------------------------------------------------------

cz.mobility <- read_csv('cz.mobility.csv')

cz.geojson <- geojson_read("cz1990.json",
                           what = "sp")

cz.conversions <- read_xls('cz00_eqv_v1.xls')



# Converting geo cz1990 to cz2000 and adding cz to geo --------------------

cz.conversions <- cz.conversions[, c(2:4)]
cz.conversions$`Commuting Zone ID, 1990` <- as.numeric(cz.conversions$`Commuting Zone ID, 1990`)
cz.conversions$`Commuting Zone ID, 1980` <- as.numeric(cz.conversions$`Commuting Zone ID, 1980`)
colnames(cz.conversions)[2] <- 'cz1990'
colnames(cz.conversions)[1] <- 'cz2000'
colnames(cz.conversions)[3] <- 'cz1980'

head(cz.geojson@data)
cz.geo <- cz.geojson
colnames(cz.mobility)[1] <- 'cz2000'

cz.geo@data <- left_join(cz.geo@data,
                         cz.conversions[, -3],
                         by = c('cz' = 'cz1990'))
cz.geo@data <- left_join(cz.geo@data,
                         cz.mobility,
                         by = 'cz2000')



ui <- fluidPage(
  
  titlePanel("College Proximity and Mobility in the U.S."),
  mainPanel(
    tabsetPanel(
      tabPanel("Tab1", 
               leafletOutput(outputId = "map1", width = "150%")), 
      tabPanel("Tab2", 
               leafletOutput(outputId = "map2", width = "150%"))),
  )
  
)

server <- function(input, output, session) {
  
}

shinyApp(ui, server)

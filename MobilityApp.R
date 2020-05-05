# libraries ---------------------------------------------------------------

library(shiny)
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

setwd("C:/Users/neris/OneDrive - Middlebury College/Last Semester/ECON Inequality & Justice/econ0401")

cz.geo <- geojson_read("cz_geo.geojson",
                       what = "sp")

cz <- read_dta('cz.dta')



# Create input choices ----------------------------------------------------

mapinputs <- c("Number of Colleges", "Number of Elite Colleges", 
                "Number of Public 2-year and 4-year Colleges")
mapmatches <- c("ncollege", "nelite", "npub")
mapchoices <- data.frame(mapinputs, mapmatches)

# App ---------------------------------------------------------------------

ui <- fluidPage(
  
  titlePanel("College Proximity and Mobility in the U.S."),
  mainPanel(
    tabsetPanel(
      tabPanel("Tab1",
               selectInput(inputId = "input1",
                           label = "Select a City",
                           choices = mapinputs),
               leafletOutput(outputId = "map1", width = "150%")), 
      tabPanel("Tab2", 
               dataTableOutput(outputId = "table1"))),
  )
  
)

server <- function(input, output) {
  
  output$map1 <- renderLeaflet({
    
    title1 <- input1
    
    mapchoice <- mapchoices %>%
      filter(mapinputs == input1)
    
    mapvar <- mapchoice[2]
    
    bins1 <- c(1, 5, 10, 20, 40, 60, 90)
    
    colors1 <- colorBin(bins = bins1,
                        palette = "YlOrRd",
                        domain = cz.geo@data$mapvar)
    
    cz.geo %>%
      leaflet() %>%
      addTiles() %>%
      addPolygons(fillColor = ~colors1(cz.geo@data$mapvar),
                  weight = 1,
                  color = "white",
                  opacity = 0.5,
                  fillOpacity = .7) %>%
      setView(-96, 37.8, 3) %>%
      addLegend(pal = colors1,
                values = cz.geo@data$mapvar,
                title = input1)
    
  })
  
  output$table1 <- renderTable(txttable.nelite)
  
}

shinyApp(ui, server)

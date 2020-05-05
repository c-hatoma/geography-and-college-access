# libraries ---------------------------------------------------------------

library(shiny)
library(tidyverse)
# library(readr)
library(rvest)
library(stats)
library(readxl)
library(dplyr)
# library(stringr)
library(ggplot2)
library(ggthemes)
library(geojsonio)
library(leaflet)
# library(rgdal)
# library(haven)
library(ggimage)


# read data ---------------------------------------------------------------

setwd("C:/Users/neris/OneDrive - Middlebury College/Last Semester/ECON Inequality & Justice/econ0401")

cz.geo <- geojson_read("cz_geo.geojson",
                       what = "sp")

cz <- read_dta('cz.dta')

tables <- read_csv('outputtables.csv')

# Create input choices ----------------------------------------------------





# App ---------------------------------------------------------------------

ui <- fluidPage(
  
  titlePanel("College Proximity and Mobility in the U.S."),
  mainPanel(
    tabsetPanel(
      tabPanel("Geographic Analysis",
               radioButtons("map", "Select a Topic to Map:",
                            c("Number of Colleges in Commuting Zone", "Other")),
               leafletOutput(outputId = "map1", width = "150%")), 
      tabPanel("Regression Outputs", 
               selectInput(inputId = "imagename",
                           label = "Select a Regression Output",
                           choices = tables$tablename),
               imageOutput(outputId = "image1")),
      tabPanel("About",
        mainPanel(
        h2("About the Project"),
        p("This the final project for Professor Peter Hans Matthews' ECON401: Inequality and Justice Seminar at Middlebury College. 
                         We were interested in looking at the relationship between inequality, social mobility, and higher education."),
        br(),
        h2("About Us"),
        p("Chica Morrow and Ivy Yang are two senior economics majors at Middlebury College. 
                         Chica likes trains and urban planning. Ivy likes the smell of rain and ice cream."),
        br(),
        h2("References"),
        p("Chetty, Raj, et. al. All Outcomes by Commuting Zone, Race, Gender and Parental Income Percentile. Opportunity Insights. 2018."),
        a("https://opportunityinsights.org/data/?geographic_level=0&topic=0&paper_id=1652#resource-listing"),
        p("Chetty, Raj, et. al. Mobility Report Cards: The Role of Colleges in Intergenerational Mobility. Opportunity Insights. 2017."),
        a("https://opportunityinsights.org/data/?geographic_level=0&topic=0&paper_id=536#resource-listing"),
        p("Currie, Janet and Enrico Moretti. Mother's Education and the Intergenerational Transmission of Human Capital: Evidence from College Openings. The Quarterly Journal of Economics, Volume 118, Issue 4, November 2003. Pages 1495–1532. https://doi.org/10.1162/003355303322552856"),
        p("Greenstone, Michael, Adam Looney, Jeremey Pastashnik, and Muxin Yu. “Thirteen Economic Facts about Social Mobility and the Role of Education.” The Brookings Institution. June 26, 2013."),
        a("https://www.brookings.edu/research/thirteen-economic-facts-about-social-mobility-and-the-role-of-education/#cancel"),
        p("Wilkinson, Richard and Kate Pickett. The Spirit Level. New York: Bloomsbury Press, 2009."),
        
        br(),
        h2("Acknowledgements"),
        p("We would like to thank Professor Matthews for his support and guidance throughout this project and during the entire semester.")))
  
)))

server <- function(input, output) {
  
  output$map1 <- renderLeaflet({
    
    if (input$map == "Number of Colleges") {
      
      bins1 <- c(1, 5, 10, 20, 40, 60, 82)
      
      colors1 <- colorBin(bins = bins1,
                          palette = "YlOrRd",
                          domain = cz.geo@data$ncollege)
      
      cz.geo %>%
        leaflet() %>%
        addTiles() %>%
        addPolygons(fillColor = ~colors1(cz.geo@data$ncollege),
                    weight = 1,
                    color = "white",
                    opacity = 0.5,
                    fillOpacity = .7) %>%
        setView(-96, 37.8, 3) %>%
        addLegend(pal = colors1,
                  values = cz.geo@data$ncollege,
                  title = "Number of Colleges in Commuting Zones Across the US")
      
    } else if (input$map == "Other") {
      
      bins2 <- c(0.1, 0.2, 0.3, 0.4, 0.5, 0.59392)
      
      colors2 <- colorBin(bins = bins2,
                          palette = "YlOrRd",
                          domain = cz.geo@data$`Income Age 26 Slope`)
      
      cz.geo %>%
        leaflet() %>%
        addTiles() %>%
        addPolygons(fillColor = ~colors2(cz.geo@data$`Income Age 26 Slope`),
                    weight = 1,
                    color = "white",
                    opacity = 0.5,
                    fillOpacity = .7) %>%
        setView(-96, 37.8, 3) %>%
        addLegend(pal = colors2,
                  values = cz.geo@data$`Income Age 26 Slope`,
                  title = "Slope of Income at Age 26 in Commuting Zones Across the US")
      
      }
    })
  
  output$image1 <- renderImage({
    
    if (is.null(input$imagename))
    return(NULL)
    
    printtable <- tables %>%
      filter(tablename == input$imagename)
    
    printpath <- paste(printtable[2])
    
    list(src = printpath,
         contentType = 'image/png',
         width = 900,
         height = 500,
         alt = "This is alternate text")
    
  })
}

shinyApp(ui, server)

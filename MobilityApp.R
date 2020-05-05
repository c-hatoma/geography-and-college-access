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
  navbarPage("College and Inequality",
             tabPanel("Intro",
                      mainPanel(
                        h2("Welcome!"),
                        p("think more about what you want to write and put it here")
                      )),
             tabPanel("Data"),
             tabPanel("Results"), 
             tabPanel("Maps",
              titlePanel("College Proximity and Mobility in the U.S."),
                 mainPanel(
                 tabsetPanel(
                     tabPanel("Tab1", 
                             leafletOutput(outputId = "map1", width = "150%")), 
                     tabPanel("Tab2", 
                             leafletOutput(outputId = "map2", width = "150%"))),
                 )
             ),
             tabPanel("Conclusion"),
             tabPanel("About",
                    mainPanel(
                      h2("About the Project"),
                      p("This the final project for Professor Matthews' ECON401: Inequality and Injustice Seminar at Middlebury College. 
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
                      p("We would like to thank Professor Matthews for his support and guidance throughout this project and during the entire semester.")
                      
                    ) )
  )
  
            
)

server <- function(input, output, session) {
  
}

shinyApp(ui, server)

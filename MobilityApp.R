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

#setwd("C:/Users/neris/OneDrive - Middlebury College/Last Semester/ECON Inequality & Justice/econ0401")

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
      tabPanel("Intro",
               mainPanel(
                 h2("Introduction"),
                 p("The United States has high levels of inequality. One reason Americans may tolerate this is because of the American Dream, 
                 the idea that an individual may increase their social standing through hard work. 
                 However, social mobility and inequality are inversely linked (Wilkinson and Pickett 2009), 
                 which means that the high levels of inequality in the US also mean low levels of social mobility. 
                 Education is often seen as the key to upward social mobility, as a college degree can open doors to higher-paying jobs (Greenstone et. al 2013). 
                 "),
                 p("Inspired by Wilkinson and Pickett, our research examines the effect of college accessibility on social mobility. 
                   Are commuting zones with colleges more likely to have higher rates of social mobility than those that do not? 
                   Living near a college and college attendance are correlated (Currie and Moretti 2003). 
                   If there are more college graduates in an area, they increase the relative social mobility of the area. 
                   This relationship is important because it affects the opportunity costs of going to college for both the poor and wealthy students. 
                   If attending college does not change social mobility, then the poor have little incentive to go to college if their circumstances will not improve, 
                   whereas the rich also may not have to go to college to maintain their wealth. "))
               ),
      tabPanel("Model",
        mainPanel(
          h2("Empirical Model"),
          p("We use a simple Ordinary Least Squares (OLS) regression to test the relationship between number of colleges and social mobility. 
            Our estimating equation is: "),
          #### INSERT AND IMAGE OF THE ESTIMATING EQUATION PLEASE HELP!! FILE IS CALLED "equation.png"
          p("Where Yi = the social mobility estimate for a given parent income rank, either kfr, kfr_black, kfr_white, or kfr_top20 
            for a commuting zone i. Xi is the number of colleges, four year colleges, four year private colleges, elite schools, public schools,
            or binary indicator for any college in a commuting zone. PopDensity is the population density in 2010, 
            and Med_hhinc is the median household income in 2016. We control for population density and median household income in 2016, 
            two factors that would plausibly impact both the presence of colleges in an area and the level of social mobility.")
        )),
      tabPanel("Geographic Analysis",
               radioButtons("map", "Select a Topic to Map:",
                            c("Number of Colleges in Commuting Zone", "Other")),
               leafletOutput(outputId = "map1", width = "150%")), 
      tabPanel("Data"),
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
  
# map outputs -------------------------------------------------------------
  
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


# Regression table outputs ------------------------------------------------
    
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

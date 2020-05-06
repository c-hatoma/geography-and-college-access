
# libraries ---------------------------------------------------------------

library(shiny)
library(tidyverse)
library(rvest)
library(stats)
library(readxl)
library(dplyr)
library(ggplot2)
library(ggthemes)
library(geojsonio)
library(leaflet)
library(ggimage)


# read data ---------------------------------------------------------------

cz.geo <- geojson_read("cz_geo.geojson",
                       what = "sp")

cz <- read_dta('cz.dta')

tables <- read_csv('regouttables.csv')



# App ---------------------------------------------------------------------


ui <- fluidPage(
  
  titlePanel("College Proximity and Mobility in the U.S."),
  mainPanel(
    tabsetPanel(
      tabPanel("Intro",
               leafletOutput(outputId = "map1", width = "150%"),
               mainPanel(
                 h2("Introduction"),
                 p("The United States has high levels of inequality. One reason Americans may 
                 tolerate this is because of the American Dream, the idea that an individual 
                 may increase their social standing through hard work. However, social 
                 mobility and inequality are inversely linked (Wilkinson and Pickett 2009), 
                 which means that the high levels of inequality in the US also mean low levels
                 of social mobility. Education is often seen as the key to upward social 
                 mobility, as a college degree can open doors to higher-paying jobs 
                 (Greenstone et. al 2013)."),
                 p("Inspired by Wilkinson and Pickett, our research examines the effect of 
                   college accessibility on social mobility. Are commuting zones with colleges
                   more likely to have higher rates of social mobility than those that do not?
                   Living near a college and college attendance are correlated (Currie and 
                   Moretti 2003). If there are more college graduates in an area, they 
                   increase the relative social mobility of the area. This relationship is 
                   important because it affects the opportunity costs of going to college for 
                   both the poor and wealthy students. If attending college does not change 
                   social mobility, then the poor have little incentive to go to college if 
                   their circumstances will not improve, whereas the rich also may not have 
                   to go to college to maintain their wealth.")
                 )
               ),
      tabPanel("Model",
        mainPanel(
          h2("Empirical Model"),
          p("We use a simple Ordinary Least Squares (OLS) regression to test the relationship between number of colleges and social mobility. 
            Our estimating equation is: "),
          HTML("<div style = 'height: 50px; '>"),
          imageOutput(outputId = "modelimg"),
          HTML("</div>"),
          p("Where ", 
            tags$b("Yi"),
            " = the social mobility estimate for a given parent income rank, either ",
            tags$b("kfr, kfr_black, kfr_white, "),
            "or ",
            tags$b("kfr_top20"),
            "for a commuting zone ",
            tags$b("i"),
            ". ",
            tags$b("Xi"),
            " is the number of colleges, four year colleges, four year private colleges, 
            elite schools, public schools, or binary indicator for any college in a 
            commuting zone i. ",
            tags$b("PopDensity"),
            " is the population density in 2010, and ",
            tags$b("Med_hhinc"),
            " is the median household income in 2016. We control for population density and 
            median household income in 2016, two factors that would plausibly impact both 
            the presence of colleges in an area and the level of social mobility.")
          )
        ),
      tabPanel("Data",
               mainPanel(
                 h2("Opportunity Insights"),
                 p("Our data come from Opportunity Insights, a project based out of Harvard 
                   University led by Raj Chetty dedicated to improving economic opportunity 
                   for Americans. We use their statistics on", 
                   tags$b("predicted income outcomes for children based on parent income percentile
                          by commuting zone"),
                          "as well as their data on colleges from their", 
                   tags$b('“Mobility Report Cards.”'),
                   "We aggregate the college data to the commuting zone level and create several
                   variables counting the",
                   tags$b("number of colleges, four year colleges, elite colleges, and public 
                   colleges"),
                   "in a commuting zone. These variables form our group of independent variables."),
                   p("Our dependent variables are the predicted mean percentile rank in the 
                   national distribution of household income measured as earnings in 2014-15",
                     tags$b("(“kfr”)"),
                     "for children of all races and all genders, the kfr for black 
                   children, the kfr for white children, and the mean predicted probability 
                   of reaching the top quintile of national household income distribution 
                   (“kfr_top20”). All estimates of dependent variables are conditional on ",
                     tags$b("parent income"),
                     ", of which we use the 1st, 25th, 50th, 75th, and 100th percentiles, 
                     with 1 being the lowest and 100 being the highest. In addition, we 
                     control for ",
                     tags$b("population density"),
                     "and ",
                     tags$b("median household income"),
                     "in 2016, two factors that could plausibly impact opportunities
                     for social mobility."),
                 p("The table below shows summary statistics for variables of interest. As 
                 an example, all parent percentiles are shown for kfr, or Kid Rank Given 
                 Parent Rank for all children. As the parent rank increases, the mean of the 
                 predicted child rank also increases, showing that children are likely to end 
                 up a similar income class as their parents. In addition, white children from 
                 the median parent income have a higher predicted rank (0.537) than black 
                 children from the median parent income (0.403)."),
                 imageOutput(outputId = "sumstats"),
               )
               ),
      tabPanel("Results",
                      selectInput(inputId = "imagename",
                           label = "Select a Regression Output",
                           choices = tables$tablename),
               HTML("<div style = 'height: 460px; '>"),
               imageOutput(outputId = "resultsimg"),
               HTML("</div>"),
               textOutput(outputId = "tabletext")),
      tabPanel("Conclusions",
                      HTML("<div style = 'height: 430px; '>"),
                      imageOutput(outputId = "plotimg"),
                      HTML("</div>"),
                 p("Overall, the results suggest that for those coming from the bottom 50% of 
                   the income distribution, having a public college nearby has a negative, 
                   although economically insignificant, effect on social mobility, with mixed
                   results for any college and four-year colleges. For those above the median,
                   having a college nearby does not have an effect. There are generally 
                   statistically insignificant results for those in the top 75% and above, 
                   meaning that the social mobility of those who have wealthy parents are not 
                   impacted by college location. These children are likely to go to college 
                   no matter where they live, and their parents have the resources to help 
                   them achieve their educational goals. Conversely, for those at the bottom 
                   end of the income distribution, having a college nearby has a negative, 
                   although economically low, impact. Perhaps the presence of a college only 
                   serves to increase divides, with those from the top maintaining their 
                   middle to upper class standing and those at the bottom staying at the 
                   bottom. As Haveman and Smeeding (2006) claim, higher education is currently
                   increasing social divides, not bridging them. "),
                 p("The results of this study should encourage administrators and policy 
                   makers to look into making college more accessible, and perhaps providing 
                   more outreach or scholarships for local, low-income children. However, 
                   there are likely larger, systemic problems that are the root cause of the
                   lack of educational opportunity and social mobility for those at the bottom.
                   Wilkinson and Pickett (2009) would suggest inequality.")),
      tabPanel("About",
        mainPanel(
        h2("About the Project"),
        p("This the final project for Professor Peter Hans Matthews' ECON401: Inequality and Justice Seminar at Middlebury College. 
                         We were interested in looking at the relationship between inequality, social mobility, and higher education."),
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



server <- function(input, output, session) {
  
# map outputs -------------------------------------------------------------
  
  output$map1 <- renderLeaflet({
      
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
        setView(-82, 37.8, 4) %>%
        addLegend(pal = colors1,
                  values = cz.geo@data$ncollege,
                  title = "Number of Colleges in Commuting Zones Across the US")
      
    })
  

# empirical model image ---------------------------------------------------
  
  output$modelimg <- renderImage({
    
    list(src = "C:\\\\Users\\neris\\OneDrive - Middlebury College\\Last Semester\\ECON Inequality & Justice\\econ0401\\equation.png",
         contentType = "image/png",
         width = 500,
         height = 50,
         alt = "Model")
    
    }, deleteFile = FALSE )
  

# Sum Stats image ---------------------------------------------------------
  
  output$sumstats <- renderImage({
    
    list(src = "C:\\\\Users\\neris\\OneDrive - Middlebury College\\Last Semester\\ECON Inequality & Justice\\econ0401\\sumstats.png",
         contentType = "image/png",
         width = 700,
         height = 450,
         alt = "Summary Statistics")
    
  }, deleteFile = FALSE )


# Regression table outputs ------------------------------------------------
    
  output$resultsimg <- renderImage({
    
    if (is.null(input$imagename))
    return(NULL)
    
    printtable <- tables %>%
      filter(tablename == input$imagename)
    
    printpath <- paste(printtable[2])
    
    list(src = printpath,
         contentType = 'image/png',
         width = 600,
         height = 450,
         alt = "Regression Results Table")
    
  }, deleteFile = FALSE )
  
  output$tabletext <- renderText({
    
    if(input$imagename == "Black Child Income Outcomes"){
      print("The table presents estimates of the effect of the number of colleges, 
            four year, or public colleges on the mean predicted income rank of Black 
            children with parent income percentile as defined by the column headers. 
            Controls are population density and median household income. The results 
            suggest that having any college or a public college nearby has a slightly 
            negative effect for children from below the median but has less effect on 
            children from above the median, and four year colleges have no significant 
            effect at all. Although statistically significant, the largest coefficient 
            is the effect of number of public colleges on the top 1%, which is -0.0041, 
            or 2.7% of a standard deviation, which is rather small." )}
    
    else if(input$imagename == "White Child Income Outcomes"){
      print("The table presents estimates of the effect of the number of colleges, four year,
            or public colleges on the mean predicted income rank of white children with parent
            income percentile as defined by the column headers. Controls are population 
            density and median household income. The results suggest that having any college
            or a four year college nearby have little to no effect on income rank, and but 
            that public colleges may have a slightly negative effect for those at the 75th 
            percentile and below. Although statistically significant, the largest coefficient
            is the effect of number of public colleges on the bottom 1%, which is -0.0053, or
            7.7% of a standard deviation, which is rather small.")}
    
    else if(input$imagename == "All Child Income Outcomes"){
      print("The table presents estimates of the effect of the number of colleges, four year,
            or public colleges on the mean predicted income rank of children with parent 
            income percentile as defined by the column headers. Controls are population 
            density and median household income. The results suggest that having a college 
            nearby has a slightly negative effect for children from below the median but has
            less effect on children from above the median. Although statistically significant,
            the largest coefficient is the effect of number of public colleges on the bottom 
            1%, which is -0.0068, or 9.3% of a standard deviation, which is rather small. This
            means that with an increase of one public school, the mean percentile rank is 
            predicted to decrease by 0.68 percentage points.")}
    
    else if(input$imagename == "All Child Probability of Reaching Top 20%"){
      print("The table presents estimates of the effect of the number of colleges, four year, 
            or public colleges on the mean predicted probability that a child with parents 
            from a given income percentile reaches the top income quintile. Controls are 
            population density and median household income.  The results suggest that having 
            a college nearby has a slightly negative effect for children from below the median
            but has less effect on children from above the median, with number of public 
            colleges having the greatest effect. Although statistically significant, the 
            largest coefficient is the effect of number of public colleges on the bottom 1%, 
            which is -0.0039, or only 7.3% of a standard deviation. ")}
    
    else { print(".") }
    
  })
  

# kfr plot ----------------------------------------------------------------

  output$plotimg <- renderImage({
    
    list(src = "C:\\\\Users\\neris\\OneDrive - Middlebury College\\Last Semester\\ECON Inequality & Justice\\econ0401\\Rplot_kfr.jpeg",
         contentType = "image/jpeg",
         width = 500,
         height = 400,
         alt = "Plot")
    
  }, deleteFile = FALSE )
  
  
}

shinyApp(ui, server, options = list(width = 960))

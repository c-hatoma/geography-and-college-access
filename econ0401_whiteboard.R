# R script where we can test all of our functions/manipulate data before pasting in Shinyapp


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


# data --------------------------------------------------------------------

# 
percentiles.data <- read_csv('national_percentile_outcomes.csv')
percentiles <- percentiles.data

# https://www.cbpp.org/research/state-budget-and-tax/funding-down-tuition-up
funding.data <- readxl::read_xlsx('PublicCollegeFundingEdited.xlsx')
funding <- funding.data

# opportunity insights 'absolute income mobility by child birth cohort and state'
state.abs.mobility.data <- readxl::read_xlsx('table2_state_absmob_by_cohort.xlsx')
state.abs.mobility <- state.abs.mobility.data

# state name to abbreviation conversion data (USPS)
url1 <- "https://pe.usps.com/text/pub28/28apb.htm"
url1.table <- url1 %>%
                read_html() %>%
                html_nodes(xpath = '//*[@id="ep18684"]') %>%
                # what exactly am I looking for?
                html_table(fill = TRUE) %>%
                .[[1]]
colnames(url1.table) <- url1.table[1, ]
url1.table1 <- url1.table[-1,]

# collegeboard scorecard data
College.scorecard.data <- read.csv('CollegeScorecard_Most-Recent-Cohorts-All-Data-Elements.csv')
college.scorecard <- College.scorecard.data

# GDP up to Feb 2020
# https://ihsmarkit.com/products/us-monthly-gdp-index.html
gdp.index.data <- readxl::read_xlsx('US-Monthly-GDP-History-Data.xlsx', sheet = 3)
gdp.index <- gdp.index.data
colnames(gdp.index)[1] <- "Y_M"
year.month <- str_split_fixed(gdp.index$Y_M, ' - ', 2)
colnames(year.month) <- c('Year', 'Month')
gdp.index <- cbind(year.month, gdp.index[, -1])
gdp.annual <- gdp.index %>%
  group_by(Year) %>%
  summarize(MaxGDP = max(`Monthly Real GDP Index`),
            MinGDP = min(`Monthly Real GDP Index`))

# https://nces.ed.gov/programs/digest/d18/tables/dt18_306.10.asp
enrollment.data <- read_xls('tabn306.10.xls')
enrollment <- enrollment.data[1:12]
# enrollment is in thousands
enrollment <- enrollment[-c(1, 3, 15, 27, 39, 51, 63, 75, 99, 111, 123, 135:139), ]
col1 <- data.frame(str_remove_all(enrollment[[1]], '\\.'), stringsAsFactors = FALSE)
col1[2, 1] <- "All_Students"
enrollment <- cbind(col1, enrollment[, -1])
enrollment <- t(enrollment)
rownames(enrollment) <- c()
colnames(enrollment) <- enrollment[1, ]
enrollment <- data.frame(enrollment)
colnames(enrollment)[1] <- 'Year'
enrollment <- enrollment[-1, ]
Years <- as.numeric(str_extract(enrollment$Year, "[:digit:]{4}"))
enrollment <- cbind(Years, enrollment[, -1])
enrollment <- data.frame(lapply(enrollment, function(x){ 
  gsub("---", NA, x)
}))
str(enrollment)


# data cleaning -----------------------------------------------------------

# match state names and abbreviations to data by state name
funding.abbrev <- left_join(funding, url1.table1,
                            by = c("State" = "State/Possession"))

# join funding data and absolute mobility data by state
funding.abbrev2016 <- funding.abbrev %>%
  filter(Year == 2016)

state.abs.mobility1980 <- state.abs.mobility %>%
  filter(cohort == 1980)

mobility.funding.2016.1980 <- full_join(funding.abbrev2016, state.abs.mobility1980,
                              by = c("Abbreviation" = 'state_name'))

# models ------------------------------------------------------------------

# test model 1
percentile.model1 <- lm(kfr_pooled_pooled ~ par_pctile,
                        data = percentiles)

summary(percentile.model1)

# test model 2: state funding & mobility by state

statefunding.model1 <- lm(cohort_mean ~ `Constant SEA/FTE`,
                          data = mobility.funding.2016.1980)
summary(statefunding.model1)

mobility.funding.2016.1980 %>%
  ggplot(aes(x = `Constant SEA/FTE`,
             y = as.numeric(cohort_mean))) +
  geom_point() +
  ylim(0.37, 0.63) +
  geom_abline(intercept = 5.160e-01,
         slope = -1.150e-06)

# test model 3: 

# first making a graph of annual GDP highs and lows -- maybe a temporary proxy for recessions?
gdp.annual %>%
  ggplot() +
  geom_line(mapping = aes(x = Year,
                 y = MaxGDP,
                 group = 1)) +
  geom_line(mapping = aes(x = Year,
                          y = MinGDP,
                          group = 1)) +
  theme_economist() +
  ylab('Real GDP')

enrollment1 <- enrollment[, 1:2]
gdp.annual1 <- gdp.annual
all.students <- as.numeric(enrollment[4:11, 2])

# annual enrollment graph
enrollment1 %>%
  ggplot() +
  geom_line(mapping = aes(x = Years,
                          y = All_Students,
                          group = 1)) +
  theme_economist() +
  ylab('Enrollment')

gdp.annual$Year <- as.factor(gdp.annual$Year)
enrollment1$Years <- as.factor(enrollment1$Years)
enrollment1$All_Students <- as.numeric(as.character(enrollment1$All_Students))

test <- inner_join(enrollment1, gdp.annual1,
          by = c("Years" = "Year"))

lm1 <- lm(All_Students ~ MaxGDP,
          data = test,
          na.action = na.omit)
summary(lm1)

#
test %>%
  ggplot() +
  geom_line(aes(x = Years,
                 y = All_Students,
                group = 1)) +
  theme_economist()

test %>%
  ggplot() +
  geom_line(aes(x = Years,
                 y = MaxGDP,
                 group = 1)) +
  # geom_abline(slope = 0.8015, intercept = 6316.7207) +
  theme_economist()









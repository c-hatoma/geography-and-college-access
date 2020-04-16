# R script where we can test all of our functions/manipulate data before pasting in Shinyapp


# libraries ---------------------------------------------------------------

library(tidyverse)
library(readr)
library(rvest)
library(stats)


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

# test model 2

statefunding.model1 <- lm(cohort_mean ~ 'Constant SEA/FTE',
                          data = mobility.funding.2016.1980)






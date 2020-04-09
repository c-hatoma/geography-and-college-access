# R script where we can test all of our functions/manipulate data before pasting in Shinyapp


# libraries ---------------------------------------------------------------

library(tidyverse)
library(readr)


# data --------------------------------------------------------------------

percentiles.data <- read_csv('national_percentile_outcomes.csv')
percentiles <- percentiles.data



# models ------------------------------------------------------------------


percentile.model1 <- lm(kfr_pooled_pooled ~ par_pctile,
                        data = percentiles)

summary(percentile.model1)

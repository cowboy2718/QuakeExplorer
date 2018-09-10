context("Test the time series plotting feature")

library(QuakeExplorer)
library(ggplot2)
library(dplyr)

results <- read_delim("~/Data/Statistical Projects/R Projects/QuakeExplorer/tests/testthat/results.csv", "\t", escape_double = FALSE, trim_ws = TRUE)

any_name_df<-eq_clean_data(results)

p1<- any_name_df %>% filter(COUNTRY =="CHINA" | COUNTRY=="USA" | COUNTRY=="GREECE") %>% filter(datevalue > '1900-01-01', datevalue < '1950-01-01') %>% 
ggplot()
  
p1<-p1 + geom_timeline(aes(x = datevalue, y = COUNTRY, color = TOTAL_DEATHS, size = EQ_PRIMARY)) 


test_that("Time Series plot",{
  expect_is(p1, "ggplot")
})


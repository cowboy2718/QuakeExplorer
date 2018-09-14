library(QuakeExplorer)
library(readr)
library(dplyr)
library(ggplot2)
library(lubridate)
context("checking time series plotting")

results <- read_delim('results.csv', delim = '\t')
results<-eq_clean_data(results)

test_plt<-results %>% filter(COUNTRY =="CHINA" | COUNTRY=="USA") %>%  filter(datevalue > '1900-01-01', datevalue < '1950-01-01') %>% 
ggplot() + geom_timeline(aes(x = datevalue, y = COUNTRY, color = TOTAL_DEATHS, size = EQ_PRIMARY)) 

test_that("time series only",{
  expect_is(test_plt, "ggplot")
})


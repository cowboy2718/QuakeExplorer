library(QuakeExplorer)
library(readr)
library(dplyr)
library(ggplot2)
library(lubridate)

context("checking time series plotting and labeling routines")

results <- read_delim('results.csv', delim = '\t')
results<-eq_clean_data(results)

test_plt<-results %>% filter(COUNTRY =="CHINA" | COUNTRY=="USA") %>%  filter(datevalue > '1900-01-01', datevalue < '1950-01-01') %>% 
ggplot() + geom_timeline(aes(x = datevalue, y = COUNTRY, color = TOTAL_DEATHS, size = EQ_PRIMARY)) 

test_that("time series only",{
  expect_is(test_plt, "ggplot")
})

test_plot2<- results %>% filter(COUNTRY =="CHINA" | COUNTRY=="USA" | COUNTRY=="GREECE") %>% 
filter(datevalue > '1900-01-01', datevalue < '1950-01-01') %>% 
ggplot() + geom_timeline(aes(x = datevalue, y = COUNTRY, color = TOTAL_DEATHS, size = EQ_PRIMARY)) +
  geom_timeline_label(data=results,aes(x = datevalue, y = COUNTRY, magnitude = EQ_PRIMARY,label = location_name, max_labels= 5))
 

test_that("labeled time series plot",{
  expect_is(test_plot2, "ggplot")
  
})


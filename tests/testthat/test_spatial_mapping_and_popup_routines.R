library(QuakeExplorer)
library(readr)
library(dplyr)
library(ggplot2)
library(lubridate)
library(leaflet)

context("checking spatial mapping and popup development routines")

results <- read_delim('results.csv', delim = '\t')
results<-eq_clean_data(results)

test_spatial_map<-results%>%filter(COUNTRY=="GREECE")%>%filter(datevalue > '1900-01-01', datevalue < '1950-01-01') %>% eq_map(annot_col = "datevalue")

test_that("spatial map produced",{
  expect_is(test_spatial_map, "leaflet")
})

results<-results%>%filter(COUNTRY=="GREECE")%>% filter(datevalue > '1900-01-01', datevalue < '1950-01-01')
results<-results%>%mutate(popup_info=eq_create_label(results))
test_spatial_map<-results%>%leaflet() %>% leaflet::addTiles()%>%addCircleMarkers(lng = ~LONGITUDE,lat = ~LATITUDE,radius = ~EQ_PRIMARY,weight=1,popup=~popup_info)

test_that("spatial map with pop up info is produced",{
  expect_is(test_spatial_map, "leaflet")
})
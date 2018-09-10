context("Test the spatial mapping feature")

library(QuakeExplorer)
library(ggplot2)
library(dplyr)
library(leaflet)

results <- read_delim("~/Data/Statistical Projects/R Projects/QuakeExplorer/tests/testthat/results.csv", "\t", escape_double = FALSE, trim_ws = TRUE)

any_name_df<-eq_clean_data(results)

any_name_df%>%filter(COUNTRY=="GREECE")%>%filter(datevalue > '1900-01-01', datevalue < '1950-01-01') 
any_name_df<-any_name_df%>%mutate(popup_info=eq_create_label(any_name_df))
any_name_df%>%leaflet() %>% leaflet::addTiles()%>%addCircleMarkers(lng = ~LONGITUDE,lat = ~LATITUDE,radius = ~EQ_PRIMARY,weight=1,popup=~popup_info)

test_that("A leaflet object is produced",{
  expect_is(earthmap, "leaflet")
})


## ----warning=FALSE, message=FALSE----------------------------------------
library(QuakeExplorer)
library(ggplot2)
library(dplyr)
library(readr)
library(leaflet)

## ----message=FALSE, eval=FALSE-------------------------------------------
#  library(readr)
#  
#  import_quake_data("results.csv")

## ----message=FALSE, eval=FALSE-------------------------------------------
#  library(dplyr)
#  
#  any_name_df<-eq_clean_data(results)

## ----message=FALSE, eval=FALSE-------------------------------------------
#  
#  # For BC date ranges, use a one sided inequality e.g. datevalue < '0000-01-01'
#  
#  library(dplyr)
#  library(ggplot2)
#  
#  any_name_df %>% filter(COUNTRY =="CHINA" | COUNTRY=="USA") %>% filter(datevalue > '1900-01-01', datevalue < '1950-01-01') %>% ggplot() + geom_timeline(aes(x = datevalue, y = COUNTRY, color = TOTAL_DEATHS, size = EQ_PRIMARY)) + ggtitle("NOAA Significant Earthquake Data Set")+ylab("Country")+xlab("Date") + scale_size_continuous(name = 'Richter') + scale_color_continuous(name = 'Mortality (deaths)') +theme(axis.text.x=element_text(angle=90,hjust=3))
#  

## ----message=FALSE, eval=FALSE-------------------------------------------
#  
#  # For BC date ranges, use a one sided inequality e.g. datevalue < '0000-01-01'
#  
#  library(dplyr)
#  library(ggplot2)
#  
#  any_name_df %>% filter(COUNTRY =="CHINA" | COUNTRY=="USA") %>% filter(datevalue > '1900-01-01', datevalue < '1950-01-01') %>% ggplot() +geom_timeline_label(aes(x = datevalue, y = COUNTRY, magnitude = EQ_PRIMARY,label = location_name, max_labels= 4))
#  

## ----warning=FALSE, message=FALSE----------------------------------------
library(QuakeExplorer)
library(ggplot2)
library(dplyr)
library(readr)
library(leaflet)

## ----message=FALSE, eval=FALSE-------------------------------------------
#  library(readr)
#  
#  import_quake_data("results.csv")

## ----message=FALSE, eval=FALSE-------------------------------------------
#  library(dplyr)
#  
#  any_name_df<-eq_clean_data(results)

## ----message=FALSE, eval=FALSE-------------------------------------------
#  
#  # Data input file is first cleaned
#  # For BC date ranges, use a one sided inequality e.g. datevalue < '0000-01-01'
#  
#  library(dplyr)
#  library(ggplot2)
#  
#  any_name_df %>% filter(COUNTRY =="CHINA" | COUNTRY=="USA") %>% filter(datevalue > '1900-01-01', datevalue < '1950-01-01') %>% ggplot() + geom_timeline(aes(x = datevalue, y = COUNTRY, color = TOTAL_DEATHS, size = EQ_PRIMARY)) + ggtitle("NOAA Significant Earthquake Data Set")+ylab("Country")+xlab("Date") + scale_size_continuous(name = 'Richter') + scale_color_continuous(name = 'Mortality (deaths)') +theme(axis.text.x=element_text(angle=90,hjust=3))
#  

## ----message=FALSE, eval=FALSE-------------------------------------------
#  
#  # For BC date ranges, use a one sided inequality e.g. datevalue < '0000-01-01'
#  
#  library(dplyr)
#  library(ggplot2)
#  
#  any_name_df %>% filter(COUNTRY =="CHINA" | COUNTRY=="USA") %>% filter(datevalue > '1900-01-01', datevalue < '1950-01-01') %>% ggplot() +geom_timeline_label(aes(x = datevalue, y = COUNTRY, magnitude = EQ_PRIMARY,label = location_name, max_labels= 4))
#  

## ----message = FALSE, eval=FALSE-----------------------------------------
#  
#  # User inputs a country and range of date to be explored.
#  # The earth map will plot the points as a leaflet with a pop up based on the date of the event.
#  # For BC date ranges, use a one sided inequality e.g. datevalue < '0000-01-01'
#  
#  library(leaflet)
#  library(dplyr)
#  
#  any_name_df%>%filter(COUNTRY=="GREECE")%>%filter(datevalue > '1900-01-01', datevalue < '1950-01-01') %>% eq_map(annot_col = "datevalue")
#  

## ----message=FALSE, eval=FALSE-------------------------------------------
#  
#  # User inputs a country and range of dates to be explored.
#  # The earth map will plot the points as a leaflet with a pop up based on the date of the event.
#  # For BC date ranges, use a one sided inequality e.g. datevalue < '0000-01-01'
#  
#  library(leaflet)
#  library(dplyr)
#  
#  any_name_df<-eq_clean_data(results)
#  
#  any_name_df<-any_name_df%>%filter(COUNTRY=="GREECE")%>% filter(datevalue > '1900-01-01', datevalue < '1950-01-01')
#  any_name_df<-any_name_df%>%mutate(popup_info=eq_create_label(any_name_df))
#  any_name_df%>%leaflet() %>% leaflet::addTiles()%>%addCircleMarkers(lng = ~LONGITUDE,lat = ~LATITUDE,radius = ~EQ_PRIMARY,weight=1,popup=~popup_info)


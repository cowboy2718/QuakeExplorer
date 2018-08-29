## ------------------------------------------------------------------------
library(QuakeExplorer)
library(ggplot2)
library(dplyr)
library(readr)

## ----message=FALSE, eval=FALSE-------------------------------------------
#  import_quake_data("results.csv")

## ----message=FALSE, eval=FALSE-------------------------------------------
#  any_name_df<-eq_clean_data(results)

## ----message=FALSE, eval=FALSE-------------------------------------------
#  
#  any_name_df %>% filter(COUNTRY =="CHINA" | COUNTRY=="USA") %>% filter(datevalue > '1900-01-01', datevalue < '1950-01-01') %>% ggplot() + geom_timeline(aes(x = datevalue, y = COUNTRY, color = TOTAL_DEATHS, size = EQ_PRIMARY)) + ggtitle("NOAA Significant Earthquake Data Set")+ylab("Country")+xlab("Date") + scale_size_continuous(name = 'Richter') + scale_color_continuous(name = 'Mortality (deaths)') +theme(axis.text.x=element_text(angle=90,hjust=3))
#  

## ----message=FALSE, eval=FALSE-------------------------------------------
#  
#  any_name_df %>% filter(COUNTRY =="CHINA" | COUNTRY=="USA") %>% filter(datevalue > '1900-01-01', datevalue < '1950-01-01') %>% ggplot() +geom_timeline_label(aes(x = datevalue, y = COUNTRY, magnitude = EQ_PRIMARY,label = location_name, max_labels= 4))
#  


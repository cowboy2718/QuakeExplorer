# Tony Gojanovic
# Coursera "R Programming Capstone Project"
# Final project
# August 2018

#' Routine for spatial analysis of earthquake data (earth mapping)
#' 
#' @description This function provides a spatial interpretation of earthquake data with a popup based on datevalue.
#' @note For BC date ranges, use a one sided inequality e.g. datevalue < '0000-01-01'
#'  
#' @export 
#' @importFrom dplyr mutate filter
#' @importFrom magrittr "%>%"
#' @import leaflet
#' @references 
#' @examples 
#' \dontrun{
#' 
#' # User inputs a country and range of date to be explored.
#' # The earth map will plot the points as a leaflet with a pop up based on the date of the event. 
#' # For BC date ranges, use a one sided inequality e.g. datevalue < '0000-01-01'
#' 
#' library(leaflet)
#' library(dplyr)
#' 
#' any_name_df%>%filter(COUNTRY=="GREECE")%>%filter(datevalue > '1900-01-01', datevalue < '1950-01-01') %>% eq_map(annot_col = "datevalue")
#' 
#' } 
#' 
#' 

eq_map <- function(data, annot_col = NULL) {
  earth_map <- data %>% leaflet::leaflet() %>%
    leaflet::addTiles()
    earth_map <- earth_map %>% leaflet::addCircleMarkers(lng = ~LONGITUDE,
                                           lat = ~LATITUDE,
                                           radius = ~EQ_PRIMARY,
                                           weight = 1,
                                           popup = data[[annot_col]])
  return(earth_map)
}

#' Function for labeling spatial analysis of earthquake data (earth mapping)
#'
#' @description This function provides a spatial interpretation of earthquake data with popups of location, magnitude and mortality.
#' @note For BC date ranges, use a one sided inequality e.g. datevalue < '0000-01-01'
#' @export 
#' @importFrom dplyr mutate filter
#' @importFrom magrittr "%>%"
#' @import leaflet
#' @references 
#' @examples 
#' \dontrun{
#' 
#' # User inputs a country and range of dates to be explored.
#' # The earth map will plot the points as a leaflet with a pop up based on the date of the event.
#' # For BC date ranges, use a one sided inequality e.g. datevalue < '0000-01-01'
#' 
#' library(leaflet)
#' library(dplyr)
#' 
#' any_name_df<-eq_clean_data(results)
#' 
#' any_name_df<-any_name_df%>%filter(COUNTRY=="GREECE")%>% filter(datevalue > '1900-01-01', datevalue < '1950-01-01')
#' any_name_df<-any_name_df%>%mutate(popup_info=eq_create_label(any_name_df))
#' any_name_df%>%leaflet() %>% leaflet::addTiles()%>%addCircleMarkers(lng = ~LONGITUDE,lat = ~LATITUDE,radius = ~EQ_PRIMARY,weight=1,popup=~popup_info)
#' 
#'}
#'

eq_create_label <- function(data) {
 
  dStr <- paste('<b>Date:</b>',data[['datevalue']])
  aStr <- paste('<b>Location:</b>',data[['location_name']])
  bStr <- paste('<b>Magnatude:</b>', data[['EQ_PRIMARY']])
  cStr <- paste('<b>Mortality (total deaths):</b>', data[['TOTAL_DEATHS']])
  
  # Missing data

  aStr[is.na(data[['LOCATION_NAME']])] <- ''
  bStr[is.na(data[['EQ_PRIMARY']])] <- ''
  cStr[is.na(data[['TOTAL_DEATHS']])] <- ''
  
  # assemble with line breaks
  return(paste(dStr, aStr, bStr, cStr, sep = '<br/>'))
}




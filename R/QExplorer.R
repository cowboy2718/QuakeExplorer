# Tony Gojanovic
# Coursera "R Programming Capstone Project"
# Final project
# August 2018

#' Reading an earth quake datadata set
#' 
#' @description This function reads in an exported data set from the NOAA Signficant Earthquake Database.
#' @param filename Filename in csv format to be read. The file must be located in the working directory.
#' @importFrom readr read_delim
#' @return Returns a data frame from a csv file into tabular format.
#' @note Data must have a .csv extension.  The NOAA data is exported as a tab delimited file called "results" without a file extension designation.
#' @export 
#' @references  

import_quake_data<-function(x){                                            #Import the NOAA data set called "results.csv"
  library(readr)
  results <- read_delim(x, "\t", escape_double = FALSE, trim_ws = TRUE)    #Use readr to import the tab delimited file and call it results
  View(results)
  
  }

#' Simple capitalization routine
#' 
#' @description This simple function capitalizes the first letter of a string (if it's in lower case).
#' @param Input A generic string.
#' @return A character vector in which the first letter is capitialized of each string segment.
#' 
simpleCap <- function(x) {
s <- strsplit(x, " ")[[1]]
paste(toupper(substring(s, 1,1)), substring(s, 2),
      sep="", collapse=" ")
}


#' Cleaning the data set
#'
#' @description
#' This function cleans the original raw data set from the NOAA Signficant Earthquake Database and formats some columns to be used in exploratory analysis.
#' @return A location name that has a properly formatted text for analysis and graphing, latitude and longditude are numeric and the date is properly formatted.
#' @importFrom lubridate ymd origin
#' @importFrom stringr str_pad
#' @importFrom dplyr mutate 
#' @note This function cleans key fields of the earth quake data set for exploratory analysis.
#' 
eq_clean_data<-function(df){
  
  # Clean location names and make then lower case, less the country code, and capitalize the first part of the string
  
  df$location_name <- tolower(gsub("^.*:\\s*","",df$LOCATION_NAME))
  df$location_name <- sapply(df$location_name,simpleCap)

  # Assure that latitude and longitude are numeric

  df$lat<-as.numeric(df$LATITUDE)
  df$long<-as.numeric(df$LONGITUDE)
  
  BCE <- (df$YEAR <= 0)
  
  #Add in missing month and day of July 2, which is roughly the midpoint of the year.
  
  df$MONTH <- as.numeric(df$MONTH)
  df$DAY<- as.numeric(df$DAY)
  df <- df %>% dplyr::mutate(MONTH = if_else(is.na(MONTH), 7, MONTH))
  df <- df %>% dplyr::mutate(DAY = if_else(is.na(DAY), 2, DAY))
  
  # Format or tidy date sequences
  df$year <- stringr::str_pad(abs(df$YEAR), 4, "left","0")
  df$month <- stringr::str_pad(df$MONTH, 2, "left","0")
  df$day <- stringr::str_pad(df$DAY, 2, "left","0")
  
  # Create a common data value based on the year, month and day and use a hyphen to seperate them
  
  df$datevalue <- (lubridate::ymd(paste(df$year, df$month, df$day, sep = "-")))
  orig <- as.numeric(lubridate::ymd("0000-01-01"))
  df$CE_equivalent <- as.numeric(df$datevalue)
  df$datevalue[BCE] <- as.Date(orig - (df$CE_equivalent[BCE] - orig), origin = lubridate::origin)
  return(df)

}


# Tony Gojanovic
# Coursera "R Programming Capstone Project"
# Final project
# September 2018

#' Importing the NOAA Signficant Earthquake Database.
#' 
#' @description This function imports in the NOAA Signficant Earthquake Database as a tab delimited format and converts it to an R dataframe.  
#' @details 
#' The entire NOAA dataset can be used (about 5500 records).
#' Follow the instructions on the NOAA site for downloading the entire dataset to a tab delimited file.
#' The NOAA Signficiant Earthquake site is found at:
#' \url{https://www.ngdc.noaa.gov/nndc/struts/form?t=101650&s=1&d=1.}
#' @param filename Filename in csv format to be read. The file must be located in the working directory.
#' @importFrom readr read_delim
#' @return Returns a data frame from a csv file into a dataframd called "results".
#' @note Exported earthquake data must have a .csv extension.  
#' The NOAA data is exported as a tab delimited file called "results" without a file extension designation. 
#' Specifically,  
#' \itemize{
#' 1. Data will be downloaded as a tab delimited file called "results" from the NOAA site. 
#' 2. The exported tab delimited file is added to the working directory.
#' 3. The tab delimited file is given a .csv extension.
#' 4. Use the R routine to import the tab delimited file to a dataframe.
#' }
#' @examples 
#' # Downloaded NOAA file in tab delimited format with a .csv extension.
#' # Returns a dataframe with the default name "results."
#' \dontrun{
#' 
#' library(readr)
#' import_quake_data("results.csv")
#' 
#' }


import_quake_data<-function(x){                                            #Import the NOAA data set called "results.csv"
  library(readr)
  results <- read_delim(x, "\t", escape_double = FALSE, trim_ws = TRUE)    #Use readr to import the tab delimited file and call it results
  View(results)
    }

# Simple capitalization routine which capitalizes the first letter of a string (if it's in lower case).
 
simpleCap <- function(x) {
s <- strsplit(x, " ")[[1]]
paste(toupper(substring(s, 1,1)), substring(s, 2),
      sep="", collapse=" ")
}

#' Formatting the imported NOAA Database into a usable format.
#'
#' @description
#' This function modfies the original raw data set from the NOAA Signficant Earthquake Database by formatting key columns to be used in exploratory analysis.
#' @return A formatted dataframe is returned with a location name that has a properly formatted for graphing, latitude and longditude values are numeric and dates are formatted approriately.
#' @importFrom lubridate ymd origin
#' @importFrom stringr str_pad
#' @importFrom dplyr mutate 
#' @note This function modifies key fields of the imported NOAA earth quake data set for exploratory analysis.  It will also return a dataframe with a properly formatted date field called 'datevalue.'
#' @examples 
#' \dontrun{
#' 
#' # Use the "results" dataframe name from the import routine.
#' # Reformat select fields for graphical analysis.  
#' # Assign the newly formatted dataframe to a user chosen name.
#' 
#' library(dplyr)
#' any_name_df<-eq_clean_data(results)
#' 
#' } 
#' 
#' 
eq_clean_data<-function(df){
  
  # Clean location names and make then lower case, less the country code, and capitalize the first part of the string
  
  df$location_name <- tolower(gsub("^.*:\\s*","",df$LOCATION_NAME))
  df$location_name <- sapply(df$location_name,simpleCap)

  # Assure that latitude and longitude are numeric

  df$LATITUDE<-as.numeric(df$LATITUDE)
  df$LONGITUDE<-as.numeric(df$LONGITUDE)
  
  # This steps is used to clean up variable names in the dataset.  It's not mandatory but removes all upper case labeling and
  # creates easier to use all lower case variable names.
  
  # colnames(df)[colnames(df) == 'EQ_PRIMARY'] <- 'eq_primary'
  # colnames(df)[colnames(df) == 'COUNTRY'] <- 'country'
  # colnames(df)[colnames(df) == 'TOTAL_DEATHS'] <- 'deaths'
  # colnames(df)[colnames(df) == 'LATITUDE'] <- 'lat'
  # colnames(df)[colnames(df) == 'LONGITUDE'] <- 'long'

  BC <- (df$YEAR <= 0)
  
  #Add in missing month and day of July 2, which is roughly the midpoint of the year.
  
  df$MONTH <- as.numeric(df$MONTH)
  df$DAY<- as.numeric(df$DAY)
  df <- df %>% dplyr::mutate(MONTH = if_else(is.na(MONTH), 7, MONTH))
  df <- df %>% dplyr::mutate(DAY = if_else(is.na(DAY), 2, DAY))
  
  # Format or tidy date sequences
  df$year <- stringr::str_pad(abs(df$YEAR), 4, "left","0")
  df$month <- stringr::str_pad(df$MONTH, 2, "left","0")
  df$day <- stringr::str_pad(df$DAY, 2, "left","0")
  
  # Create a common data value based on the year, month and day and use a hyphen to seperate them into a new variable called datevalue
  
  df$datevalue <- (lubridate::ymd(paste(df$year, df$month, df$day, sep = "-")))
  orig <- as.numeric(lubridate::ymd("0000-01-01"))
  df$AD_date <- as.numeric(df$datevalue)
  df$datevalue[BC] <- as.Date(orig - (df$AD_date[BC] - orig), origin = lubridate::origin)
  return(df)

}

# Tony Gojanovic
# Coursera "R Programming Capstone Project"
# Final project
# September 2018

#' Importing the NOAA Signficant Earthquake Database.
#' 
#' @description This function imports in the NOAA Signficant Earthquake Database as a tab delimited format and converts it to an R dataframe.  
#' @details 
#' The entire NOAA dataset can be used (about 5500 records).
#' Follow the instructions on the NOAA site for downloading the entire dataset to a tab delimited file.
#' The NOAA Signficiant Earthquake site is found at:
#' \url{https://www.ngdc.noaa.gov/nndc/struts/form?t=101650&s=1&d=1.}
#' @param filename Filename in csv format to be read. The file must be located in the working directory.
#' @importFrom readr read_delim
#' @return Returns a data frame from a csv file into a dataframd called "results".
#' @note Exported earthquake data must have a .csv extension.  
#' The NOAA data is exported as a tab delimited file called "results" without a file extension designation. 
#' Specifically,  
#' \itemize{
#' 1. Data will be downloaded as a tab delimited file called "results" from the NOAA site. 
#' 2. The exported tab delimited file is added to the working directory.
#' 3. The tab delimited file is given a .csv extension.
#' 4. Use the R routine to import the tab delimited file to a dataframe.
#' }
#' @examples 
#' # Downloaded NOAA file in tab delimited format with a .csv extension.
#' # Returns a dataframe with the default name "results."
#' \dontrun{
#' 
#' library(readr)
#' import_quake_data("results.csv")
#' 
#' }


import_quake_data<-function(x){                                            #Import the NOAA data set called "results.csv"
  library(readr)
  results <- readr::read_delim(x, "\t", escape_double = FALSE, trim_ws = TRUE)    #Use readr to import the tab delimited file and call it results
 }

# Simple capitalization routine which capitalizes the first letter of a string (if it's in lower case).
 
simpleCap <- function(x) {
s <- strsplit(x, " ")[[1]]
paste(toupper(substring(s, 1,1)), substring(s, 2),
      sep="", collapse=" ")
}

#' Formatting the imported NOAA Database into a usable format.
#'
#' @description
#' This function modfies the original raw data set from the NOAA Signficant Earthquake Database by formatting key columns to be used in exploratory analysis.
#' @return A formatted dataframe is returned with a location name that has a properly formatted for graphing, latitude and longditude values are numeric and dates are formatted approriately.
#' @importFrom lubridate ymd origin
#' @importFrom stringr str_pad
#' @importFrom dplyr mutate 
#' @note This function modifies key fields of the imported NOAA earth quake data set for exploratory analysis.  It will also return a dataframe with a properly formatted date field called 'datevalue.'
#' @examples 
#' \dontrun{
#' 
#' # Use the "results" dataframe name from the import routine.
#' # Reformat select fields for graphical analysis.  
#' # Assign the newly formatted dataframe to a user chosen name.
#' 
#' library(dplyr)
#' any_name_df<-eq_clean_data(results)
#' 
#' } 
#' 
#' 
eq_clean_data<-function(df){
  
  # Clean location names and make then lower case, less the country code, and capitalize the first part of the string
  
  df$location_name <- tolower(gsub("^.*:\\s*","",df$LOCATION_NAME))
  df$location_name <- sapply(df$location_name,simpleCap)

  # Assure that latitude and longitude are numeric

  df$LATITUDE<-as.numeric(df$LATITUDE)
  df$LONGITUDE<-as.numeric(df$LONGITUDE)
  
  # This steps is used to clean up variable names in the dataset.  It's not mandatory but removes all upper case labeling and
  # creates easier to use all lower case variable names.
  
  # colnames(df)[colnames(df) == 'EQ_PRIMARY'] <- 'eq_primary'
  # colnames(df)[colnames(df) == 'COUNTRY'] <- 'country'
  # colnames(df)[colnames(df) == 'TOTAL_DEATHS'] <- 'deaths'
  # colnames(df)[colnames(df) == 'LATITUDE'] <- 'lat'
  # colnames(df)[colnames(df) == 'LONGITUDE'] <- 'long'

  BC <- (df$YEAR <= 0)
  
  #Add in missing month and day of July 2, which is roughly the midpoint of the year.
  
  df$MONTH <- as.numeric(df$MONTH)
  df$DAY<- as.numeric(df$DAY)
  df <- df %>% dplyr::mutate(MONTH = if_else(is.na(MONTH), 7, MONTH))
  df <- df %>% dplyr::mutate(DAY = if_else(is.na(DAY), 2, DAY))
  
  # Format or tidy date sequences
  df$year <- stringr::str_pad(abs(df$YEAR), 4, "left","0")
  df$day <- stringr::str_pad(df$DAY, 2, "left","0")
  df$month <- stringr::str_pad(df$MONTH, 2, "left","0")
 
  # Create a common data value based on the year, month and day and use a hyphen to seperate them into a new variable called datevalue
  
  df$datevalue <- (lubridate::ymd(paste(df$year, df$month, df$day, sep = "-")))
  orig <- as.numeric(lubridate::ymd("0000-01-01"))
  df$AD_date <- as.numeric(df$datevalue)
  df$datevalue[BC] <- as.Date(orig - (df$AD_date[BC] - orig), origin = lubridate::origin)
  return(df)

}


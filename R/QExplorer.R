# Tony Gojanovic
# Coursera "R Programming Capstone Project"
# Final project
# August 2018

#' Reading an earth quake datadata set
#' 
#' @description
#' This function reads in an exported data set from the NOAA Signficant Earthquake Database.
#' @param filename Filename in csv format to be read. The file must be located in the working directory.
#' @return Returns a data frame from a csv file into tabular format.
#' @note Data must have a .csv extension.  The NOAA data is exported as a tab delimited file type without a file extension designation.
#' @export 
#' @references  

import_quake_data<-function(filename){   
  earthquake <- readr::read_delim(filename, "\t", escape_double = FALSE, trim_ws = TRUE)
  View(earthquake)
  }

